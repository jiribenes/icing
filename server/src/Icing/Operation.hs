{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Icing.Operation
  ( Action(..)
  , Operation
  , applyAction
  , transformAction
  , Revision
  , DocumentState(..)
  , applyOperation
  ) where

import           Control.Monad.Except           ( runExcept
                                                , throwError
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Tuple                     ( swap )

-- | Action either inserts or deletes!
data Action = Insert Int Text
            | Delete Int Int
  deriving stock (Show, Eq, Ord)

type Operation = [Action]

-- | Applies an action to a given Text document
--
--  TODO: do a bounds check
applyAction :: Action -> Text -> Text
applyAction action doc = case action of
  Insert i t -> let (start, end) = T.splitAt i doc in start <> t <> end
  Delete i l -> let (start, end) = T.splitAt i doc in start <> T.drop l end

-- Takes actions @a@, @b@.
-- If we denote the transformation as @f@, satisfies @f b \circ a === f a \circ b@ 
--
-- Most of the operations are from [reduce98]
transformAction :: Action -> Action -> ([Action], [Action])
transformAction a b = case (a, b) of
  (Insert i ta, Insert j tb)
    | i <= j    -> ([Insert i ta], [Insert (j + T.length ta) tb])
    | otherwise -> ([Insert (i + T.length tb) ta], [Insert j tb])
  (Insert i ta, Delete j lb)
    | i <= j
    -> ([Insert i ta], [Delete (j + T.length ta) lb])
    | i > j + lb
    -> ([Insert (i - lb) ta], [Delete j lb])
    | otherwise
    -> ( [Insert j ta] -- we might lose some info here
       , [Delete j (i - j), Delete (i + T.length ta) (lb - (i - j))]
       )
  (Delete _ _ , Insert _ _ ) -> swap $ transformAction b a -- symmetric?
  (Delete i la, Delete j lb) -> ([dd i la j lb], [dd j lb i la]) -- the interaction of these is complicated, let's write a helpful lemma
 where
  -- loses information from third rule onwards!
  dd i la j lb | j >= (i + la)                  = Delete i la
               | i >= (j + lb)                  = Delete (i - lb) la
               | j <= i && (i + la) <= (j + lb) = Delete i 0
               | j <= i && (i + la) > (j + lb)  = Delete j (i + la - (j + lb))
               | j > i && (j + lb) >= (i + la)  = Delete i (j - i)
               | otherwise                      = Delete i (la - lb)

-- | Equivalent of 'transformAction' for an 'Operation'
transformOperation :: Operation -> Operation -> (Operation, Operation)
transformOperation xs ys = unwrap $ go xs ys
 where
  -- TODO: This function probably is in base, but I can't seem to find it...
  go :: [Action] -> [Action] -> [([Action], [Action])]
  go []       []       = [([], [])]
  go as       []       = [(as, [])]
  go []       bs       = [([], bs)]
  go (a : as) (b : bs) = transformAction a b : go as bs

  -- TODO: This function seems kinda inefficient...
  unwrap :: [([a], [b])] -> ([a], [b])
  unwrap zs = (zs >>= fst, zs >>= snd)

-- That ought to be enough revisions for everybody!
type Revision = Int

-- | Represents a given state of a document
data DocumentState = DocumentState
  { docRevision :: Revision
  , docText     :: Text
  , docOps      :: [Operation]
  }
  deriving stock (Eq, Show, Ord)

-- | Takes a document and a pair (clientRevision, clientOperation)
-- and applies the operation (or dies trying).
--
-- Returns a new operation that should be sent to all other clients
-- except the one who sent the processed message.
applyOperation
  :: DocumentState
  -> Revision -- ^ last revision that the client saw
  -> Operation -- ^ operation recieved from client
  -> Either String (Operation, DocumentState)
applyOperation operationState clientRev clientOp = runExcept $ do
  let numberOfRevs = docRevision operationState - clientRev
  concurrentOps :: [Operation] <-
    if clientRev > docRevision operationState || numberOfRevs > length
         (docOps operationState)
      then throwError "Invalid revision number!"
      else pure $ take numberOfRevs (docOps operationState)

  let megaOp :: Operation = concat $ reverse concurrentOps
  let op'                 = fst $ transformOperation clientOp megaOp
  let docText'            = applyAllActions (docText operationState) op'

  let operationState' = DocumentState
        { docRevision = docRevision operationState + 1
        , docText     = docText'
        , docOps      = op' : docOps operationState
        }
  pure (op', operationState')

-- | Applies all [Action]s into a document
applyAllActions :: Text -> [Action] -> Text
applyAllActions = foldl' (flip applyAction)
