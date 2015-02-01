module Foundation where

import Yesod

data App = App
instance Yesod App

mkYesodData "App" $(parseRoutesFile "config/routes")
