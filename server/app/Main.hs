{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Control.Monad.Except           ( replicateM
                                                , runExcept
                                                , runExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Foldable                  ( traverse_ )
import           Database.Selda                 ( MonadIO
                                                , MonadSelda
                                                )
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( withStdoutLogger )
import           Network.Wai.Middleware.Cors   as C
import           Servant                        ( ServerError )
import           Icing
import           Control.Monad                  ( when )

main :: IO ()
main = runServer

runServer :: IO ()
runServer = do
  let port = 8888

  putStrLn $ "Launching server on port " <> show port

  withStdoutLogger $ \logger -> do
    -- assemble settings
    let settings = setPort port $ setLogger logger defaultSettings

    runApp <- app

    -- run the application with `settings`
    runSettings settings $ runCors runApp
 where
    -- TODO(jb): check CORS settings properly!
  dumbCors :: CorsResourcePolicy
  dumbCors = simpleCorsResourcePolicy
    { corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    , corsMaxAge         = Just $ 60 * 60 * 24 -- a single day
        -- this breaks swagger.json , corsRequireOrigin  = True
    }

  runCors application = cors (const $ Just dumbCors) application
