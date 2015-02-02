module Handler.Home where

import Data.Default
import Yesod
import Yesod.Default.Util

import Foundation

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEncType) <- generateFromPost uploadForm
    filenames <- getList
    defaultLayout $ do
        setTitle "File Processor"
        $(widgetFileNoReload def "home")

postHomeR :: Handler Html
postHomeR = do
  ((result, _),_) <- runFormPost uploadForm
  case result of
  FormSuccess fi -> do
    app <- getYesod
    addFile app $ fileName fi
  _ -> return ()
  redirect HomeR

uploadForm = renderDivs $ fileAFormReq "file"

