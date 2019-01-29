module Todomvc.Db (
    DB(..)
  , debug
  , mkDB
  , insertItem
  , setItem
  , lookupItem
  , allItemIds
  , allItems
  , deleteItem
  , exampleTodoItem
) where

import Todomvc.Data

import Control.Concurrent (MVar, modifyMVar_, modifyMVar, newMVar, readMVar)
import Data.Map
import Control.Monad.IO.Class (MonadIO(..))
import Servant (NoContent(..))


-- fake DB

data DB = DB (MVar (Map TodoItemId TodoItem))

debug :: DB -> IO ()
debug (DB mvar) = readMVar mvar >>= print

mkDB :: IO DB
mkDB = DB <$> newMVar empty

insertItem :: DB -> TodoItem -> IO TodoItemId
insertItem (DB mvar) new = modifyMVar mvar $ \ m -> do
  let newKey = case keys m of
        [] -> 0
        ks -> succ (maximum ks)
  return (insert newKey new m, newKey)

setItem :: DB -> TodoItemId -> TodoItem -> IO TodoItemId
setItem (DB mvar) iid i = modifyMVar mvar $ \ m -> do
  return (insert iid i m, iid)

lookupItem :: DB -> TodoItemId -> IO (Maybe TodoItem)
lookupItem (DB mvar) i = do
  Data.Map.lookup i <$> readMVar mvar

allItemIds :: DB -> IO [TodoItemId]
allItemIds (DB mvar) =
  keys <$> readMVar mvar

allItems :: DB -> IO [(TodoItemId, TodoItem)]
allItems (DB mvar) =
  toList <$> readMVar mvar

deleteItem :: MonadIO m => DB -> TodoItemId -> m NoContent
deleteItem (DB mvar) i = liftIO $ do
  modifyMVar_ mvar $ \ m -> return (delete i m)
  return NoContent


exampleTodoItem :: TodoItem
exampleTodoItem = TodoItem "example item" InProgress
