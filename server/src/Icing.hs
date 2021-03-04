{-# LANGUAGE TypeApplications #-}

module Icing
  ( app
  ) where

import           Control.Concurrent             ( MVar )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Database.Selda                 ( MonadIO
                                                , MonadMask
                                                , SeldaT
                                                )
import           Database.Selda.SQLite          ( SQLite
                                                , withSQLite
                                                )
import           Icing.API                      ( API
                                                , server
                                                )
import           Icing.State                    ( State
                                                , makeState
                                                )
import           Servant                        ( Application
                                                , Server
                                                , hoistServer
                                                , serve
                                                )

-- | Runs a computation with a SQLite database
--
-- Note: This is not used as of writing this comment.
-- Might be useful for the future, though!
withDb :: (MonadIO m, MonadMask m) => SeldaT SQLite m a -> m a
withDb action = withSQLite "db.sqlite" action

-- | Hoists the server so that it can use the database
hoisted :: MVar State -> Server API
hoisted var = hoistServer (Proxy @API) withDb $ server var

-- | Entry point to the server itself
app :: IO Application
app = do
  -- prepare the state variable
  stateVar <- makeState

  -- server just serves the api via 'hoisted'
  pure $ serve (Proxy @API) $ hoisted stateVar
