{-# LANGUAGE OverloadedStrings #-}
module Icing.Prolog where

import           Control.Monad.IO.Class
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Exit
import           System.Process

prologFile :: FilePath
prologFile = "editor-input.pl"

setPrologText :: MonadIO m => Text -> m ()
setPrologText = liftIO . T.writeFile prologFile

-- The caller has to call 'setPrologText' before this!
runPrologWithCommand :: MonadIO m => Text -> m Text
runPrologWithCommand input = do
  let process :: CreateProcess
      process = proc "swipl" ["-q", "-f", prologFile]

  (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode process $ T.unpack input
  case exitCode of
    ExitSuccess   -> case out of 
                       "\n" -> pure $ T.pack err
                       _ -> pure $ T.pack out
    ExitFailure i -> do
      liftIO $ print i
      liftIO $ putStrLn err
      pure $ "[ERROR]: " <> T.pack err
