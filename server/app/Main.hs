{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Network.Wai.Handler.Warp       ( defaultSettings
                                                , runSettings
                                                , setLogger
                                                , setPort
                                                )
import           Network.Wai.Logger             ( withStdoutLogger )
import           Network.Wai.Middleware.Cors   as C
                                                ( CorsResourcePolicy
                                                  ( corsMaxAge
                                                  , corsMethods
                                                  , corsRequestHeaders
                                                  )
                                                , cors
                                                , simpleCorsResourcePolicy
                                                )

import           Icing                          ( app )

main :: IO ()
main = runServer

-- | Sets the basic settings and runs the server
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
  dumbCors :: CorsResourcePolicy
  dumbCors = simpleCorsResourcePolicy
    { corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    , corsMaxAge         = Just $ 60 * 60 * 24 -- a single day
    }

  runCors application = cors (const $ Just dumbCors) application
