{-# LANGUAGE InstanceSigs #-}

module Todo
  ( TodoListM
  , runTodoList
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify,put, runStateT)
import qualified Data.ByteString.Char8 as B
import Data.Char (isLetter, toLower)
import Data.Function (on)
import Data.List (intersect)
import Data.List.NonEmpty (toList)

import Types (Index (..)
  , SearchWord (..)
  , Description (..)
  , Tag (..)
  , SearchParams (..)
  , TodoItem (..)
  , MonadTodoList (..))

data Task = Task Index Description [Tag]
  deriving (Show, Eq)

type TaskList = [Task]

data TodoList = TodoList
  { taskList :: TaskList
  , nextIndex :: Int
  }
  deriving(Eq,Show)

emptyTodoList :: TodoList
emptyTodoList = TodoList { taskList = [], nextIndex = 0 }


newtype TodoListM a = TodoListM { runTodoListM :: StateT TodoList IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList = void . flip runStateT emptyTodoList . runTodoListM

instance MonadTodoList TodoListM where
  add  :: Description -> [Tag] -> TodoListM Index
  add desc tags = TodoListM $ do
    state <- get
    let updatedIndex = Index (fromIntegral $ nextIndex state)
    let newTask = Task updatedIndex desc tags
        updatedList = taskList state ++ [newTask]
    modify $ (\s -> s {taskList = updatedList, nextIndex = nextIndex state +1})
    return $ Index (fromIntegral $ nextIndex state)
  
  done   :: Index -> TodoListM ()
  done (Index n) = TodoListM $ do
    state <- get
    let latestList = taskList state
        indexToDrop = Index n
        updatedList = filter (\(Task index _ _) -> index/=indexToDrop ) latestList 
    put $ state {taskList = updatedList}
    return ()

  search :: SearchParams -> TodoListM [TodoItem]
  search params = TodoListM $ do
    state <- get -- map taskToTodoItem 
    let items = map taskToTodoItem (taskList state)
    let searchWords = spWords params
        searchTags = spTags params
    let filteredTasks = if null searchWords && null searchTags then items else
                        filterBySubsequence searchWords searchTags items
    return filteredTasks

-- Helper Functions
taskToTodoItem :: Task -> TodoItem
taskToTodoItem (Task index description tags) =
  TodoItem index description tags

filterBySubsequence :: [SearchWord] -> [Tag] ->[TodoItem] -> [TodoItem]
filterBySubsequence searchWords searchTags items =
  let matchingItems = filter (\item -> any (\word -> isSubsequence word (tiDescription item)) searchWords
                              || any (\tag -> isSubsequenceOfTag tag (tiTags item)) searchTags) items
  in matchingItems

isSubsequence :: SearchWord -> Description -> Bool
isSubsequence (SearchWord word) (Description desc) =
  let lowerCaseWord = map toLower (B.unpack word)
      lowerCaseDesc = map toLower $ B.unpack desc
    in all (\_ ->B.isInfixOf (B.pack lowerCaseWord) (B.pack lowerCaseDesc)) lowerCaseDesc

isSubsequenceOfTag :: Tag -> [Tag] -> Bool
isSubsequenceOfTag (Tag searchtag) tagsFromList =
  let byte = (map (B.unpack . getTag) tagsFromList)
      str = unwords byte
      lowerCaseSearchTag = map toLower $ B.unpack searchtag
      lowerCaseTagList = map toLower str
  in all (\_ -> B.isInfixOf (B.pack lowerCaseSearchTag) (B.pack lowerCaseTagList)) tagsFromList

