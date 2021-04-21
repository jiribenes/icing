{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Icing.State where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Chan.Unagi  ( InChan )
import           Control.Concurrent.MVar        ( MVar
                                                , newEmptyMVar
                                                , newMVar
                                                , putMVar
                                                , readMVar
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.List                      ( find )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Random                  ( Random(..)
                                                , getStdRandom
                                                )

import           Icing.Client
import           Icing.Haskell
import           Icing.Message
import           Icing.Operation

-- | Represents the server state
data State = State
  { stateClients       :: [Client]
  , stateDocument      :: DocumentState
  , stateHaskell       :: HaskellState
  , stateHaskellInChan :: InChan FullLoopMsg
  }

instance Show State where
  show st = unlines [show $ stateClients st, show $ stateDocument st]

-- | Creates a new state.
--
-- Also starts a Haskell loop
-- 
-- Beware, the strings absolutely need to be synchronized with the frontend strings.
-- Otherwise bad thingsTM happen.
makeState :: MonadIO m => m (MVar State)
makeState = do
  let initialDocumentState = DocumentState 1 "-- write here" []
  haskellState      <- initHaskellState
  (inChan, outChan) <- makeHaskellChan

  _                 <- liftIO $ forkIO $ haskellLoop outChan

  liftIO $ newMVar (State [] initialDocumentState haskellState inChan)

addClient :: State -> Client -> State
addClient st client = st { stateClients = client : oldClients }
  where oldClients = stateClients st

removeClient :: State -> Text -> State
removeClient st name = st
  { stateClients = filter (\c -> clientName c /= name) oldClients
  }
  where oldClients = stateClients st

getClientByName :: State -> Text -> Maybe Client
getClientByName state name =
  find (\c -> clientName c == name) $ stateClients state

getAllClientsExcept :: State -> Text -> [Client]
getAllClientsExcept state name =
  filter (\c -> clientName c /= name) $ stateClients state

getAllClients :: State -> [Client]
getAllClients = stateClients

-- | A wrapper over 'applyOperation' which takes care
-- of converting between 'SomeAction' and 'Action'.
-- It also takes care of rewrapping 'State'.
processActions
  :: State -> Revision -> [SomeAction] -> Either String ([SomeAction], State)
processActions state revision actions = do
  let operation = someActionToAction <$> actions
  (op', operationState') <- applyOperation (stateDocument state)
                                           revision
                                           operation
  let actions' = actionToSomeAction <$> op'
  let state'   = state { stateDocument = operationState' }
  pure (actions', state')

-- | Like 'processActions', except it runs in IO to print errors
processActions'
  :: MonadIO m
  => State
  -> Revision
  -> [SomeAction]
  -> m (State, ([SomeAction], Revision))
processActions' st rev actions = case processActions st rev actions of
  Left err -> do
    liftIO $ putStrLn "error happened while processing action:"
    liftIO $ putStrLn err
    pure (st, ([], docRevision $ stateDocument st))
  Right (actions', st') ->
    pure (st', (actions', docRevision $ stateDocument st'))

getCurrentText :: State -> Text
getCurrentText = docText . stateDocument

getCurrentRevision :: State -> Revision
getCurrentRevision = docRevision . stateDocument

-- | Assigns a name to a client
--
-- If the wantedUsername is already used,
-- tries to append a number
--
-- TODO: This function is buggy as all hell.
-- Figure out something better, please.
createValidName :: State -> Text -> Text
createValidName st wantedUsername
  | wantedUsername `Set.notMember` usedNames st = wantedUsername
  | otherwise = go st wantedUsername 1
 where
  go state username i
    | makeUsername username i `Set.notMember` usedNames state = makeUsername
      username
      i
    | otherwise = go state username (i + 1)
  makeUsername :: Text -> Int -> Text
  makeUsername username i = username <> " " <> T.pack (show i)

-- | Returns the set of used names.
usedNames :: State -> Set Text
usedNames state = Set.fromList $ clientName <$> stateClients state

-- | Returns the set of used colours
usedColours :: State -> Set Colour
usedColours state = Set.fromList $ clientColour <$> stateClients state

-- | Returns the set of free colours
freeColours :: State -> Set Colour
freeColours state = Set.difference darkPallete $ usedColours state

-- | Tries to pick a free 'Colour' from the pallete
--
-- If there are no more colours left, picks a random colour that has already been used
pickRandomColour :: MonadIO m => State -> m Colour
pickRandomColour state
  | null (freeColours state) = pickRandomFromSet darkPallete
  | otherwise                = pickRandomFromSet $ freeColours state
 where
  pickRandomFromSet set = liftIO $ do
    randomIndex <- getStdRandom (randomR (0, Set.size set - 1))
    pure $ Set.toList set !! randomIndex

processHaskell :: MonadIO m => State -> m [CompilerOutputMessage]
processHaskell state = do
  let inChan       = stateHaskellInChan state
  let haskellState = stateHaskell state
  runWithMVar (sendReload inChan haskellState)
    $ \loads -> pure $ loadToMessage <$> loads
 where
  loadToMessage :: Load -> CompilerOutputMessage
  loadToMessage (Loading _ _) =
    CompilerOutputMessage "Loaded!" 2 Nothing Nothing
  loadToMessage (LoadConfig _) =
    CompilerOutputMessage "Loaded config!" 2 Nothing Nothing
  loadToMessage (Eval eval) =
    CompilerOutputMessage (T.pack $ show eval) 3 Nothing Nothing
  loadToMessage (Message severity _ pos posEnd msgs)
    | pos == (0, 0) = CompilerOutputMessage (stringsToText msgs)
                                            (severityToKind severity)
                                            Nothing
                                            Nothing
    | pos == posEnd = CompilerOutputMessage (stringsToText msgs)
                                            (severityToKind severity)
                                            (Just pos)
                                            Nothing
    | otherwise = CompilerOutputMessage (stringsToText msgs)
                                        (severityToKind severity)
                                        (Just pos)
                                        (Just posEnd)

  severityToKind :: Severity -> Int
  severityToKind Error   = 0
  severityToKind Warning = 1

  stringsToText :: [String] -> Text
  stringsToText = T.unlines . fmap T.pack

processHaskellQuery :: MonadIO m => State -> Text -> m (State, Text)
processHaskellQuery state q = do
  let inChan       = stateHaskellInChan state
  let haskellState = stateHaskell state
  runWithMVar (sendQuery inChan haskellState q) callback
 where
  callback :: QueryResult -> IO (State, Text)
  callback queryResult = case queryResult of
    BadQuery     -> pure (state, "Invalid query. Please don't.")
    TimeoutQuery -> do -- TODO: use interrupt properly?
      -- interruptHaskell $ stateHaskell state
      newHaskellState <- reloadHaskellState $ stateHaskell state
      let newState = state { stateHaskell = newHaskellState }
      pure
        (newState, "Timeout after two seconds. Be careful when using laziness!")
    ReloadQuery -> do
      newHaskellState <- reloadHaskellState $ stateHaskell state
      let newState = state { stateHaskell = newHaskellState }
      pure (newState, "Ghci reloaded, you're welcome :)")
    SuccessfulQuery someResult -> pure (state, someResult)

-- | Takes an action requiring a unit callback and a callback returning a value
-- and pipes the result outward using a MVar (probably an overkill)
runWithMVar :: MonadIO m => ((t -> IO ()) -> m a) -> (t -> IO b) -> m b
runWithMVar action callback = do
  resultVar <- liftIO newEmptyMVar
  _         <- action $ \result -> do
    result' <- callback result
    putMVar resultVar result'
  liftIO $ readMVar resultVar


-------------------------
-- conversion utilities
-- ----------------------

-- | Converts a 'SomeAction' into an 'Action'
someActionToAction :: SomeAction -> Action
someActionToAction (ActionInsert ins) =
  Insert (insertPosition ins) (insertText ins)
someActionToAction (ActionDelete del) =
  Delete (deletePosition del) (deleteLen del)

-- | Converts an 'Action' to a 'SomeAction'
actionToSomeAction :: Action -> SomeAction
actionToSomeAction (Insert i t) = ActionInsert $ InsertAction i t
actionToSomeAction (Delete i l) = ActionDelete $ DeleteAction i l
