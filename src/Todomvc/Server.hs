module Todomvc.Server
    ( run
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe ()
import qualified Data.Text as T
import qualified Network.Wai as NW
import qualified Network.Wai.Handler.Warp as NWW
--import Servant ((:<|>), (:>), Get, Capture, Proxy(..), JSON(..), serve, Server(..), Handler(..), err404)
import Servant
import Servant.HTML.Blaze (HTML(..))
import qualified System.IO as SIO
import Text.Blaze.Html5 (Html(..))

import Todomvc.Data
import Todomvc.Db (DB(..), mkDB, insertItem, exampleTodoItem)
import qualified Todomvc.Server.Api as TSA
import qualified Todomvc.Server.Html as TSH


type TodoItemServant =
  "index.html" :> Get '[HTML] Html :<|>
  "api" :> TodoItemApi :<|>
  "static" :> Raw

type TodoItemApi =
    "todo" :> (
        Get '[JSON] [TodoItemWithId] :<|>
        ReqBody '[JSON] ItemDescription :> Post '[JSON] TodoItemId :<|>
        Capture "todoId" TodoItemId :> (
            Get '[JSON] TodoItemWithId :<|>
            "state" :> (ReqBody '[JSON] TodoState :> Post '[JSON] TodoItemWithId)
        )
    )

todoItemServant :: Proxy TodoItemServant
todoItemServant = Proxy

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
  _ <- insertItem db exampleTodoItem
  --return $ serve todoItemApi server db
  liftIO $ putStrLn $ T.unpack $ layout todoItemServant
  return $ serve todoItemServant $ server db

server :: DB -> Server TodoItemServant
server db = TSH.index
  :<|> TSA.todoItemsApi db
  :<|> serveDirectoryWebApp "static"
