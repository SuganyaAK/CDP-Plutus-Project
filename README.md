# CDP-Plutus-Project
A Todo List based on Plutus Smart Contract  

This example Plutus smart contract demonstrates how to maintain authoritative ToDo List application state by way of an auxiliary proof token. A proof token is a NFT that remains at the script address at all times, and thus can be used to restrict valid application state to the datum of the particular UTxO on which it resides.
This Project is done by me as part of my Cardano Developer Professional Course. This is a To-Do List where we can add a new task, update the status of the task as completed and delete the task if the length of the list is above a certain specified condition.

**Representation of Datum**
  `data Task = Task {
                taskMap :: Map BuiltinByteString Bool,
                taskCounter :: Integer
                   }
  deriving (ToJSON, FromJSON, Generic, Show)`
Datum "Task" has 2 parameters.
 1. taskMap is of type Map with the key being the task and the value being a Bool represents0the completion status of the task. 
      "Buy Milk" False
      "Buy Candy" False
      "Buy Eggs" True
 2. The second parameter "taskCounter" is of type Integer which will hold the updated state of the datum whenever the task is added/updated or deleted

**Representation of Redeemer**
  `data TaskAction  = AddTask BuiltinByteString
                   | CompleteTask BuiltinByteString
  deriving (Generic, ToJSON, FromJSON)`

  The contract is parameterized by the *`AssetClass`* of this proof token. This produces a unique script where at any given time, only a single specific UTxO locked at the script address (containing the token) will be spendable. For a validating transaction to succeed, a single new UTxO must be locked at the script containing the same token by which the contract is parameterized, as well as an updated state in its datum.In this example I am using the taskCounter value in the datum to create a counter that increments or decrements with each spending transaction. 
