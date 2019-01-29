module Todomvc.Data (
    TodoState(..)
  , TodoItemId
  , ItemDescription
  , TodoItem(..)
  , TodoItemWithId(..)
) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified GHC.Generics as G
import Servant


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
