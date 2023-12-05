{-# LANGUAGE NamedFieldPuns #-}

module Contracts.MyPlutusProjectMap where

import Contracts.MyNFTMinting qualified as NFT
import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils
import PlutusTx.AssocMap as AssocMap hiding (singleton)

--Custom Datum
data Task = Task
  { taskMap :: AssocMap.Map BuiltinByteString Bool,
    taskCounter :: Integer
    --owner :: PubKeyHash
  }
  deriving (ToJSON, FromJSON, Generic, Show)

unstableMakeIsData ''Task

-- Custom Redeemer
data TaskAction
  = AddTask BuiltinByteString
  | CompleteTask BuiltinByteString
  deriving (Generic, ToJSON, FromJSON)

makeIsDataIndexed ''TaskAction [('AddTask, 0), ('CompleteTask, 1)]

plutusProjectLambda :: PubKeyHash -> AssetClass -> Task -> TaskAction -> ScriptContext -> Bool
plutusProjectLambda owner proofToken task taskaction sc =
  spendsProof proofToken sc && locksProof owner proofToken task taskaction sc
{-# INLINEABLE plutusProjectLambda #-}

-- to check that the validating input contains proof token
spendsProof :: AssetClass -> ScriptContext -> Bool
spendsProof proofToken sc = case findOwnInput sc of
  Nothing -> trace "Input not found" False
  Just (TxInInfo _ txOut) ->
    traceIfFalse "Proof token not spent" $
      assetClassValueOf (txOutValue txOut) proofToken #== 1
{-# INLINEABLE spendsProof #-}

-- check that a single output is locked to the script with proof token and valid state
locksProof :: PubKeyHash -> AssetClass -> Task -> TaskAction -> ScriptContext -> Bool
locksProof owner proofToken task action sc@(ScriptContext txinfo _) = case getContinuingOutputs sc of
  [o] -> case (txOutDatum o, assetClassValueOf (txOutValue o) proofToken, action) of
    (OutputDatum (Datum d), qty, AddTask desc) ->
      traceIfFalse "Proof token not locked" (qty #== 1)
        && traceIfFalse "Only Owner can add tasks" (txSignedBy txinfo owner)
        && traceIfFalse "Task not added" (unsafeFromBuiltinData d #/= addTask task desc)
    (OutputDatum (Datum d), qty, CompleteTask desc) ->
      traceIfFalse "Proof token not locked" (qty #== 1)
        && traceIfFalse "Task not updated" (unsafeFromBuiltinData d #/= updateTask task desc)
    _noDatum -> trace "No inline datum on output" False
  [] -> trace "No continuing output" False
  _multipleOutputs -> trace "Multiple continuing outputs" False
{-# INLINEABLE locksProof #-}

addTask :: Task -> BuiltinByteString -> Bool
addTask (Task taskmap taskcounter) description =
  let newTask =
        Task
          { taskMap = AssocMap.insert description False taskmap,
            taskCounter = taskcounter #+ 1
          }
   in (taskCounter newTask #== taskcounter #+ 1) && (plength (taskMap newTask) #<= 10)
{-# INLINEABLE addTask #-}

updateTask :: Task -> BuiltinByteString -> Bool
updateTask (Task taskmap taskcounter) description =
  if taskcounter #> 5
    then
      let newTask =
            Task
              { taskMap = AssocMap.delete description taskmap,
                taskCounter = taskcounter #- 1
              }
       in taskMap newTask #/= taskmap
    else
      let lookupTask = AssocMap.lookup description taskmap
          taskToBeUpdated =
            Task
              { taskMap = AssocMap.delete description taskmap,
                taskCounter = taskcounter #- 1
              }
          newUpdatedTask =
            Task
              { taskMap = AssocMap.insert description True (taskMap taskToBeUpdated),
                taskCounter = taskcounter #+ 1
              }
       in lookupTask #== Just False
{-# INLINEABLE updateTask #-}

-- Convert lambda into "untyped" form before pre-compilation
plutusProjectUntyped :: PubKeyHash -> AssetClass -> UntypedValidator
plutusProjectUntyped owner proofToken = mkUntypedValidator (plutusProjectLambda owner proofToken)
{-# INLINEABLE plutusProjectUntyped #-}

-- Contract synonym
type MyProjectMap = ValidatorContract "my-plutus-project-map"

-- Compile the untyped lambda to a UPLC script and splice back to Haskell.
compileProject :: PubKeyHash -> AssetClass -> MyProjectMap
compileProject owner proofToken = mkValidatorContract ($$(compile [||plutusProjectUntyped||]) `applyCode` liftCode owner `applyCode` liftCode proofToken)

projectExport :: JambExports
projectExport =
  export
    (defExports $ compileProject owner proofToken)
      { dataExports =
          [ Task
              { taskMap = AssocMap.empty,
                taskCounter = 0
              }
              `toJSONfile` "projectemptydatum",
            Task
              { taskMap = AssocMap.fromList [("buy milk", False)],
                taskCounter = 1
              }
              `toJSONfile` "datumfirsttask",
            AddTask "buy milk" `toJSONfile` "addtaskmilk",
            Task
              { taskMap = AssocMap.fromList [("buy candy", False)],
                taskCounter = 1
              }
              `toJSONfile` "datumsecondtask",
            AddTask "buy candy" `toJSONfile` "addtaskcandy"
          ],
        emulatorTest = NFT.test <> testProject
        --emulatorTest = testProject
      }
  where
    owner = "b2ab8a34bf3f08f5d60a272454b8ab5a5fd3f93f43c47618c8cade11"
    proofToken = assetClass "d75b0ccc0d8ab17fb61afee706d68177d6cf74d5d821c3b3de2587e1" "countercoin"

instance ValidatorEndpoints MyProjectMap where
  data GiveParam MyProjectMap = Give {lovelace :: Integer, owner :: PubKeyHash, withDatum :: Task, proofToken :: AssetClass}
    deriving (ToJSON, FromJSON, Generic)

  data GrabParam MyProjectMap = AddOrMark {withRedeemer :: TaskAction, pt :: AssetClass}
    deriving (ToJSON, FromJSON, Generic)

  give :: GiveParam MyProjectMap -> ContractM MyProjectMap ()
  give (Give lovelace owner datum proofToken@(AssetClass (cs, tn))) = do
    let appliedScript = compileProject owner proofToken
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor appliedScript,
          constraints = mustPayScriptWithInlineDatum appliedScript datum (lovelaceValueOf lovelace <> singleton cs tn 1)
        }
    logStr $
      printf
        "Task intialised to an empty List"

  grab :: GrabParam MyProjectMap -> ContractM MyProjectMap ()
  grab (AddOrMark action proofToken@(AssetClass (cs, tn))) = do
    pkh <- getOwnPKH
    let appliedScript = compileProject pkh proofToken
    utxos <- getUtxosAt appliedScript
    let validUtxo = filterByValue ((cs, tn, 1) `elem`) utxos
    case action of
      AddTask desc -> do
        case Map.toList validUtxo of
          [] -> logStr "No UTxO with proof token"
          (_ : _ : _) -> logStr "Multiple UTxOs are avaialble"
          [(oref, txOut)] -> do
            let oldState = convertDecoratedTxOutDatum @Task txOut
            case oldState of
              Nothing -> logStr "No datum"
              Just oldState' -> do
                let newState = taskCounter oldState' + 1
                    oldMap = taskMap oldState'
                    newTask =
                      Task
                        { taskMap = AssocMap.insert desc False oldMap,
                          taskCounter = newState
                        }
                submitAndConfirm
                  Tx
                    { lookups = scriptLookupsFor appliedScript `andUtxos` validUtxo,
                      constraints =
                        mustSign pkh <> oref `mustBeSpentWith` AddTask desc
                          <> mustPayScriptWithInlineDatum
                            appliedScript
                            newTask
                            (lovelaceValueOf 2_000_000 <> singleton cs tn 1)
                    }
                logStr ("Task Added and Counter incremented " ++ show (taskCounter newTask))
      CompleteTask description -> do
        case Map.toList validUtxo of
          [] -> logStr "No UTxOs available"
          (_ : _ : _) -> logStr "Multiple UTxOs are available"
          [(oref, txOut)] -> do
            let oldState = convertDecoratedTxOutDatum @Task txOut
            case oldState of
              Nothing -> logStr "No datum"
              Just oldState' ->
                if taskCounter oldState' >= 5
                  then do
                    let newTask =
                          Task
                            { taskMap = AssocMap.delete description (taskMap oldState'),
                              taskCounter = taskCounter oldState' -1
                            }
                    submitAndConfirm
                      Tx
                        { lookups = scriptLookupsFor appliedScript `andUtxos` validUtxo,
                          constraints =
                            oref `mustBeSpentWith` CompleteTask description
                              <> mustPayScriptWithInlineDatum
                                appliedScript
                                newTask
                                (lovelaceValueOf 2_000_000 <> singleton cs tn 1)
                        }
                    logStr $ printf "Task with description %s marked as complete and deleted" (show description)
                  else do
                    let taskToBeUpdated =
                          Task
                            { taskMap = AssocMap.delete description (taskMap oldState'),
                              taskCounter = taskCounter oldState' #- 1
                            }
                        newUpdatedTask =
                          Task
                            { taskMap = AssocMap.insert description True (taskMap taskToBeUpdated),
                              taskCounter = taskCounter taskToBeUpdated #+ 1
                            }
                    submitAndConfirm
                      Tx
                        { lookups = scriptLookupsFor appliedScript `andUtxos` validUtxo,
                          constraints =
                            oref `mustBeSpentWith` CompleteTask description
                              <> mustPayScriptWithInlineDatum
                                appliedScript
                                newUpdatedTask
                                (lovelaceValueOf 2_000_000 <> singleton cs tn 1)
                        }
                    logStr $ printf "Task with description %s marked as complete and updated" (show description)

testProject :: EmulatorTest
testProject =
  initEmulator @MyProjectMap
    2
    [ Give
        { lovelace = 20000000,
          owner = pkhForWallet 2,
          withDatum =
            Task
              { taskMap = AssocMap.empty,
                taskCounter = 0
              },
          proofToken
        }
        `fromWallet` 1,
      AddOrMark {withRedeemer = AddTask "buy milk", pt = proofToken} `toWallet` 2,
      AddOrMark {withRedeemer = AddTask "buy candy", pt = proofToken} `toWallet` 2,
      AddOrMark {withRedeemer = CompleteTask "buy milk", pt = proofToken} `toWallet` 2,
      AddOrMark {withRedeemer = AddTask "buy eggs", pt = proofToken} `toWallet` 2,
      AddOrMark {withRedeemer = AddTask "buy phone", pt = proofToken} `toWallet` 2,
      AddOrMark {withRedeemer = AddTask "buy mat", pt = proofToken} `toWallet` 2,
      AddOrMark {withRedeemer = CompleteTask "buy mat", pt = proofToken} `toWallet` 2
    ]
  where
    proofToken =
      -- Copy currency symbol from NFT emulator test output
      assetClass "9554ee2ef856140fde90634e7f7f8e7ef2c90f5b71e01c27f9f2edb3" "countercoin"
