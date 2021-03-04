{-# LANGUAGE OverloadedStrings #-}

module Icing.Prolog
  ( prologFile
  , setPrologText
  , runPrologWithQuery
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Exit                    ( ExitCode(..) )
import           System.Process                 ( CreateProcess
                                                , proc
                                                , readCreateProcessWithExitCode
                                                )

-- | Represents the path to the file into which the document is pasted.
prologFile :: FilePath
prologFile = "editor-input.pl"

-- | Writes given text into 'prologFile'.
--
-- Overwrites original contents.
setPrologText :: MonadIO m => Text -> m ()
setPrologText = liftIO . T.writeFile prologFile

-- | Runs a prolog interpreter with a given query while consulting 'prologFile'.
--
-- This is completely unsafe. You have been warned.
--
-- The caller has to call 'setPrologText' before this!
runPrologWithQuery :: MonadIO m => Text -> m Text
runPrologWithQuery input = do
  let process :: CreateProcess
      process = proc "swipl" ["-q", "-f", prologFile]

  (exitCode, out, err) <-
    liftIO $ readCreateProcessWithExitCode process $ T.unpack input
  case exitCode of
    ExitSuccess -> case out of
      "\n" -> pure $ T.pack err
      _    -> pure $ T.pack out
    ExitFailure i -> do
      liftIO $ print i
      liftIO $ putStrLn err
      pure $ "[ERROR]: " <> T.pack err
