module Types (ServerState (..), ApiAction, Controller) where

import Data.Database
import Data.IORef
import Web.Spock

newtype ServerState = ServerState {database :: IORef Db}

type ApiAction a = SpockAction () () ServerState a

type Controller = ActionCtxT () (WebStateM () () ServerState) ()