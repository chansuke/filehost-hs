module Foundation where

import Control.Concurrent.STM
import Data.ByteString.Lazy(ByteString)
import Data.Default
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Hamlet
import Yesod
import Yesod.Default.Util

--data StoredFile = StoredFile !Text !ByteString
data StoredFile = StoredFile !Text !Text !ByteString
type Store = IntMap StoredFile
data App = App (TVar Int) (TVar Store)

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

mkYesodData "App" $(parseRoutesFile "config/routes")

getNextId::App -> STM Int
getNextId(App tnextld_) = do
  nextld <- readTVar tnextld
  writeTVar tnextld $ nextld + 1
  return nextld

getList :: Handler [(Int, StoredFile)]
getList = do
  --App tstate <- getYesod
  --liftIO $ readTVarIO tstore
  App _tstore <- getYesod
  store <- liftIO $ readTVarIO tstore
  return $ IntMap.toList store

addFile :: App -> StoredFile -> Handler ()
--addFile app@(App _ tstore) file =
  liftIO . atomically $ do
    nextld <- getNextId app
    modifyTVar tstore $ IntMap.insert ident file

getById :: Int -> Handler StoredFile
getById ident = do
    App tstore <- getYesod
    store <- liftIO $ readTVarIO tstore
    case IntMap.lookup ident store of
      Nothing -> notFound
      Just file -> return file
