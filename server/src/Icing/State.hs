{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Icing.State where

import           Control.Concurrent.MVar        ( MVar
                                                , newMVar
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
import           Icing.Message
import           Icing.Operation

-- | Represents the server state
data State = State
  { stateClients  :: [Client]
  , stateDocument :: DocumentState
  }
  deriving stock Show

-- | Creates a new state.
-- 
-- Beware, the strings absolutely need to be synchronized with the frontend strings.
-- Otherwise bad thingsTM happen.
makeState :: MonadIO m => m (MVar State)
makeState = do
  let initialDocumentState = DocumentState 1 "% write here" []
  liftIO $ newMVar (State [] initialDocumentState)

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
tryPickColour :: MonadIO m => State -> m (Maybe Colour)
tryPickColour state
  | null free = pure Nothing
  | otherwise = do
    randomIndex <- liftIO $ getStdRandom (randomR (0, length free - 1))
    pure $ Just (free !! randomIndex)
  where free = Set.toList $ freeColours state

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
