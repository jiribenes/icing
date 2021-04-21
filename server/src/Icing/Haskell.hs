{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Icing.Haskell
  ( haskellFile
  , HaskellState
  , initHaskellState
  , stopHaskellState
  , reloadHaskellState
  , interruptHaskell
  , reloadHaskell
  , queryHaskell
  , QueryResult(..)
  , Ghcid.Load(..)
  , Ghcid.Severity(..)
  , FullLoopMsg
  , LoopMsg
  , sendReload
  , sendQuery
  , sendSaveFile
  , makeHaskellChan
  , haskellLoop
  ) where

import           Control.Concurrent.Chan.Unagi
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Internal.Search     as T
import qualified Language.Haskell.Ghcid        as Ghcid
import           System.Timeout                 ( timeout )

data HaskellState = HaskellState
  { haskellStateGhci :: Ghcid.Ghci
  }

data LoopMsg = SaveFile Text
             | Reload ([Ghcid.Load] -> IO ())
             | Query Text (QueryResult -> IO ())

type FullLoopMsg = (LoopMsg, HaskellState)

sendReload
  :: MonadIO m
  => InChan FullLoopMsg
  -> HaskellState
  -> ([Ghcid.Load] -> IO ())
  -> m ()
sendReload inChan haskellState k =
  liftIO $ writeChan inChan (Reload k, haskellState)

sendQuery
  :: MonadIO m
  => InChan FullLoopMsg
  -> HaskellState
  -> Text
  -> (QueryResult -> IO ())
  -> m ()
sendQuery inChan haskellState q k =
  liftIO $ writeChan inChan (Query q k, haskellState)

sendSaveFile
  :: MonadIO m
  => InChan FullLoopMsg
  -> HaskellState
  -> Text
  -> m ()
sendSaveFile inChan haskellState fileContents =
  liftIO $ writeChan inChan (SaveFile fileContents, haskellState)

makeHaskellChan :: MonadIO m => m (InChan FullLoopMsg, OutChan FullLoopMsg)
makeHaskellChan = liftIO newChan

haskellLoop :: OutChan FullLoopMsg -> IO ()
haskellLoop outChan = forever go
 where
  go = do
    (loopMsg, haskellState) <- readChan outChan
    case loopMsg of
      Reload k -> do
        loads <- reloadHaskell haskellState
        k loads
      Query q k -> do
        result <- queryHaskell haskellState q
        k result
      SaveFile contents -> T.writeFile haskellFile contents

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

reloadHaskellState :: MonadIO m => HaskellState -> m HaskellState
reloadHaskellState state = do
  stopHaskellState state
  initHaskellState

interruptHaskell :: MonadIO m => HaskellState -> m ()
interruptHaskell = liftIO . Ghcid.interrupt . haskellStateGhci

-- | Represents the path to the file into which the document is pasted.
haskellFile :: FilePath
haskellFile = "editor-input.hs"

-- | Reload a Haskell instance
reloadHaskell :: MonadIO m => HaskellState -> m [Ghcid.Load]
reloadHaskell = liftIO . Ghcid.reload . haskellStateGhci

data QueryResult = BadQuery | ReloadQuery | TimeoutQuery | SuccessfulQuery Text

queryHaskell :: MonadIO m => HaskellState -> Text -> m QueryResult
queryHaskell state query
  | isQueryBad query = pure BadQuery
  | isQueryReload query = pure ReloadQuery
  | otherwise = do
    result <- liftIO $ timeout (2 * oneSecond) (process query)
    pure $ maybe TimeoutQuery SuccessfulQuery result
 where
  process = fmap concatStrings . Ghcid.exec (haskellStateGhci state) . T.unpack

  oneSecond :: Int
  oneSecond = (10 :: Int) ^ (6 :: Int)

  concatStrings :: [String] -> Text
  concatStrings = T.unlines . fmap T.pack

  isQueryBad :: Text -> Bool
  isQueryBad q = any (flip isIn q) [":!", ":q", ":set"]

  isQueryReload :: Text -> Bool
  isQueryReload q = ":reload" `isIn` q

  isIn :: Text -> Text -> Bool
  isIn needle haystack = case T.indices needle haystack of
    (_matchIndex : _) -> True
    _                 -> False
