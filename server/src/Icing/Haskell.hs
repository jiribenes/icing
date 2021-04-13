{-# LANGUAGE OverloadedStrings #-}

module Icing.Haskell
  ( haskellFile
  , setHaskellText
  , HaskellState
  , initHaskellState
  , stopHaskellState
  , reloadHaskell
  , queryHaskell
  , Ghcid.Load(..)
  , Ghcid.Severity(..)
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Internal.Search     as T
import qualified Data.Text.IO                  as T
import qualified Language.Haskell.Ghcid        as Ghcid

data HaskellState = HaskellState
  { haskellStateGhci :: Ghcid.Ghci
  }

initHaskellState :: MonadIO m => m HaskellState
initHaskellState = do
  let callback _ _ = pure ()
  (state, loads) <- liftIO
    $ Ghcid.startGhci "ghci editor-input.hs" (Just ".") callback
  liftIO $ T.putStrLn "Initializing Haskell ghcid!"
  liftIO $ T.putStrLn $ foldMap (T.pack . show) loads
  pure $ HaskellState state

-- TODO: Use this function when exiting the server!
stopHaskellState :: MonadIO m => HaskellState -> m ()
stopHaskellState = liftIO . Ghcid.stopGhci . haskellStateGhci

-- | Represents the path to the file into which the document is pasted.
haskellFile :: FilePath
haskellFile = "editor-input.hs"

-- | Writes given text into 'haskellFile'.
--
-- Overwrites original contents.
setHaskellText :: MonadIO m => Text -> m ()
setHaskellText = liftIO . T.writeFile haskellFile

-- | Reload a Haskell instance
reloadHaskell :: MonadIO m => HaskellState -> m [Ghcid.Load]
reloadHaskell = liftIO . Ghcid.reload . haskellStateGhci

queryHaskell :: MonadIO m => HaskellState -> Text -> m (Maybe Text)
queryHaskell state query 
  | isQueryBad query = pure Nothing
  | otherwise = liftIO . fmap (Just . concatStrings) . Ghcid.exec (haskellStateGhci state) . T.unpack $ query
 where
  concatStrings :: [String] -> Text
  concatStrings = T.unlines . fmap T.pack

  isQueryBad :: Text -> Bool
  isQueryBad q = any (flip isIn q) [":!", ":q", ":set"]

  isIn :: Text -> Text -> Bool
  isIn needle haystack = case T.indices needle haystack of
                           (_matchIndex:_) -> True
                           _ -> False
