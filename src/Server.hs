module Server
    ( run
    ) where

import Control.Concurrent (MVar(..), modifyMVar_, modifyMVar, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Data.Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified GHC.Generics as G
import Data.Aeson (FromJSON, ToJSON)
import qualified Network.Wai as NW
import qualified Network.Wai.Handler.Warp as NWW
--import Servant ((:<|>), (:>), Get, Capture, Proxy(..), JSON(..), serve, Server(..), Handler(..), err404)
import Servant
import qualified System.IO as SIO


data TodoState = Todo | InProgress | Done
  deriving (Eq, Show, G.Generic)

instance ToJSON TodoState
instance FromJSON TodoState

instance FromHttpApiData TodoState where
  parseUrlPiece :: T.Text -> Either T.Text TodoState
  parseUrlPiece "Todo" = Right Todo
  parseUrlPiece "InProgress" = Right InProgress
  parseUrlPiece "Done" = Right Done
  parseUrlPiece s = Left s

type TodoItemId = Int
type ItemDescription = String

data TodoItem = TodoItem {
  description :: ItemDescription,
  state       :: TodoState
} deriving (Eq, Show, G.Generic)

instance ToJSON TodoItem
instance FromJSON TodoItem

data TodoItemWithId = TodoItemWithId {
  todoId      :: TodoItemId,
  item        :: TodoItem
} deriving (Eq, Show, G.Generic)

instance ToJSON TodoItemWithId
instance FromJSON TodoItemWithId


type TodoItemApi =
  "api" :> (
    "todo" :> (
        Get '[JSON] [TodoItemWithId] :<|>
        ReqBody '[JSON] ItemDescription :> Post '[JSON] TodoItemId :<|>
        Capture "todoId" TodoItemId :> (
            Get '[JSON] TodoItemWithId :<|>
            "state" :> (ReqBody '[JSON] TodoState :> Post '[JSON] TodoItemWithId)
        )
    )
  )

todoItemApi :: Proxy TodoItemApi
todoItemApi = Proxy

run :: IO ()
run = do
  let port = 3000
  let settings =
        NWW.setPort port $
        NWW.setBeforeMainLoop (SIO.hPutStrLn SIO.stderr ("listening on port " ++ show port)) $
        NWW.defaultSettings
  NWW.runSettings settings =<< mkApp

mkApp :: IO NW.Application
mkApp = do
  db <- mkDB
  insertItem db exampleTodoItem
  --return $ serve todoItemApi server db
  liftIO $ putStrLn $ T.unpack $ layout todoItemApi
  return $ serve todoItemApi $ server db

server :: DB -> Server TodoItemApi
server db = todoItemsApi db

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
  liftIO $ setItem db iid newI
  return $ TodoItemWithId iid newI
  --return NoContent

exampleTodoItem :: TodoItem
exampleTodoItem = TodoItem "example item" InProgress

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
