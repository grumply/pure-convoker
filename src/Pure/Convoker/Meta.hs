module Pure.Convoker.Meta where

import Pure.Convoker.Comment

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Elm.Component hiding (not,key,pattern Meta)
import Pure.Data.JSON
import Pure.Data.Txt
import Pure.Data.Render

import Data.Hashable

import Control.Monad
import Data.Function (on)
import qualified Data.Graph as G
import Data.List as List
import Data.Typeable
import GHC.Generics hiding (Meta)

{-
Design notes:

  What is implemented here:

    data instance Context (Meta a)
    data instance Name (Meta a)

  What is not implemented here:

    data isntance Resource (Meta a)
    data instance Product (Meta a)
    data instance Preview (Meta a)
    instance Previewable (Meta a)
    instance Processable (Meta a)
    instance Producible (Meta a)
    instance Amendable (Meta a)
    data instance Action (Meta a)
    data instance Reaction (Meta a)

-}

data Meta (a :: *)

data instance Context (Meta a) = MetaContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (Meta a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (Meta a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (Meta a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (Meta a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (Meta a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (Meta a))

data instance Name (Meta a) = MetaName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Nameable (Meta a) where
  toName _ = MetaName

--------------------------------------------------------------------------------
-- Simple anonymous vote totals.

data Votes a = Votes
  { votes :: [(Key (Comment a),Int)]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data AmendVote a 
  = Vote Username (Key (Comment a)) Int
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

amendVotes :: AmendVote a -> Votes a -> Votes a
amendVotes (Vote _ target vote) (Votes votes) = Votes (go votes)
  where
    go [] = [(target,vote)]
    go (x : rest) 
      | fst x == target = fmap (+ vote) x : rest
      | otherwise       = x : go rest
