module Todomvc.Server.Html (index) where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5


index :: Handler Html
index = return $ docTypeHtml $
  body $ (
    h1 "Todomvc"
  )
