{-# LANGUAGE OverloadedStrings #-}

module Icing.Haskell
  ( haskellFile
  , setHaskellText
  , runHaskellWithQuery
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
haskellFile :: FilePath
haskellFile = "editor-input.hs"

-- | Writes given text into 'haskellFile'.
--
-- Overwrites original contents.
setHaskellText :: MonadIO m => Text -> m ()
setHaskellText = liftIO . T.writeFile haskellFile

-- | Runs a haskell interpreter with a given query while consulting 'haskellFile'.
--
-- This is completely unsafe. You have been warned.
--
-- The caller has to call 'setHaskellText' before this!
runHaskellWithQuery :: MonadIO m => Text -> m Text
runHaskellWithQuery input = do
  let process :: CreateProcess
      process = proc "swipl" ["-q", "-f", haskellFile]

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
