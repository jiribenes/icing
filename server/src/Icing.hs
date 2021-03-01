{-# LANGUAGE TypeApplications #-}

module Icing
  ( app
  ) where

import           Control.Concurrent
import           Control.Monad.Except           ( runExceptT )
import           Data.Proxy
import           Database.Selda
import           Database.Selda.SQLite          ( SQLite
                                                , withSQLite
                                                )
import           Icing.API
import           Icing.State                    ( State
                                                , makeState
                                                )
import           Servant                        ( Application
                                                , Server
                                                , ServerError
                                                , hoistServer
                                                , serve
                                                )

withDb :: (MonadIO m, MonadMask m) => SeldaT SQLite m a -> m a
withDb action = withSQLite "db.sqlite" action

hoisted :: MVar State -> Server API
hoisted var = hoistServer (Proxy @API) withDb $ server var

app :: IO Application
app = do
  stateVar <- makeState
  pure $ serve (Proxy @API) $ hoisted stateVar
