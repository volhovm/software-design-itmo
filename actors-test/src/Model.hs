{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Model describing what we store and how to operate on it

module Model
       ( Task(..)
       , TodoList(..)
       , fromTDL
       , TodoRequest(..)
       , parseRequest
       , applyRequest
       ) where

import           Control.Lens (ix, makeLenses)
import           Data.Default (Default (def))
import           Data.List    (delete, (!!))
import qualified Data.Map     as M
import qualified Data.Text    as T
import           Universum

-- | Simple task with name and done/not done flag
data Task = Task
    { taskText       :: Text
    , taskIsComplete :: Bool
    } deriving (Show,Eq)

-- | List of tasks marked by names
newtype TodoList = TodoList
    { _fromTDL :: M.Map Text [Task]
    } deriving (Show)

makeLenses ''TodoList

instance Default TodoList where
    def = TodoList $ M.singleton "list0" [Task "create_some_tasks" False]

data TodoRequest = TodoRequest
  { todoRequest :: Text
  } deriving Show


-- | Describes modification on todo list
data TDLMod
    = TDLRemoveList Text
    | TDLAddList Text
    | TDLAddTask Text
                 Task
    | TDLToggleTask Text
                    Int
    deriving (Show)

parseRequest :: TodoRequest -> Maybe TDLMod
parseRequest (todoRequest -> req) =
    head reqW >>= \case
      "addlist" -> TDLAddList <$> e1
      "addtask" -> TDLAddTask <$> e1 <*> (flip Task False <$> e2)
      "rmlist" -> TDLRemoveList <$> e1
      "toggle" -> TDLToggleTask <$> e1 <*> (readMaybe =<< (T.unpack <$> e2))
      _ -> Nothing
  where
    reqW = words req
    e1 = reqW ^? ix 1
    e2 = reqW ^? ix 2

applyRequest :: TDLMod -> M.Map Text [Task] -> M.Map Text [Task]
applyRequest cmd =
    (case cmd of
         (TDLRemoveList listName)       -> M.delete listName
         (TDLAddList listName)          -> M.insert listName []
         (TDLAddTask listName task)     -> M.adjust (++[task]) listName
         (TDLToggleTask listName index) -> M.adjust (modTask $ pred index) listName)
  where
    modTask i taskList
        | i < 0 || i >= length taskList = taskList
    modTask i taskList =
        let t = taskList !! i
            t' = t { taskIsComplete = not (taskIsComplete t) }
        in take i taskList ++ [t'] ++ drop (i+1) taskList
