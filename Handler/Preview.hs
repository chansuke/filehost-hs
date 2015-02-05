module Handler.Preview where

import Control.Exception hiding (Handler)
import qualified Data.ByteString.Lazy as LB
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Text.Blaze
import Yesod
import Yesod.Default.Util

import Foundation

--getPreviewR :: Text -> Handler Html
getPreviewR :: Int -> Handler Html
-- getPreviewR field = do
getPreviewR ident = do
    --StoredFile filename bytes <- getById ident
    StoredFile filename _bytes <- getById ident
    defaultLayout $ do
        setTitle . toMarkup $ "File Processor - " `Text.append` filename
        --previewBlock <- liftIO $ preview bytes
        previewBlock <- liftIO $ preview ident contentType bytes
        $(widgetFileNoReload def "preview")

--preview :: LB.ByteString -> IO Widget
--preview bytes = do
preview :: Int -> LB.ByteString -> IO Widget
preview ident contentType bytes
  | "image/" `Text.isPrefixOf` contentType =
    return [whamlet|<img src=@{DownloadR ident}>|]
  | otherwise = do
    eText <- try . evaluate $ LT.decodeUtf8 bytes :: IO (Either SomeException LT.Text)
    return $ case eText of
      Left _ -> errorMessage
      Right text -> [whamlet|<pre>#{text}|]
  where
    errorMessage = [whamlet|<pre>Unable to display file contents.|]
