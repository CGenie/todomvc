module Todomvc.Server.Api (
  todoItemsApi
) where

import Todomvc.Data
import Todomvc.Db

import Control.Monad.IO.Class (liftIO)
import Servant


todoItemsApi db =
       getTodoItems db
  :<|> createTodoItem db
  :<|> todoItemOperations db

getTodoItems :: DB -> Handler [TodoItemWithId]
getTodoItems db = do
  items <- liftIO $ allItems db
  return $ Prelude.map (uncurry TodoItemWithId) items
  --return $ [TodoItemWithId 0 exampleTodoItem]

createTodoItem :: DB -> ItemDescription -> Handler TodoItemId
createTodoItem db d = do
  liftIO $ putStrLn $ "adding: " ++ show d
  itemId <- liftIO $ insertItem db $ TodoItem d Todo
  return itemId

--todoItemOperations :: DB -> TodoItemId -> Handler TodoItemWithId
todoItemOperations db iid =
                   getTodoItemById db iid
              :<|> setTodoItemState db iid

getTodoItemById :: DB -> TodoItemId -> Handler TodoItemWithId
getTodoItemById db iid = do
  mItem <- liftIO $ lookupItem db iid
  case mItem of
           Just i -> return $ TodoItemWithId iid i
           _      -> throwError err404

setTodoItemState :: DB -> TodoItemId -> TodoState -> Handler TodoItemWithId
setTodoItemState db iid s = do
  liftIO $ putStrLn $ "setting state of " ++ show iid ++ " to " ++ show s
  i <- getTodoItemById db iid
  let newI = (item i) { state = s }
  _ <- liftIO $ setItem db iid newI
  return $ TodoItemWithId iid newI

