module Handler.Home where

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod
import Yesod.Default.Util
import Control.Monad.Trans.Resource(runResourceT)

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
    fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
    --addFile app $ StoredFile (fileName fi) fileBytes
    addFile app $ StoredFile (fileName fi) (fileContentType fi) fileBytes
  _ -> return ()
  redirect HomeR

uploadForm = renderDivs $ fileAFormReq "file"

