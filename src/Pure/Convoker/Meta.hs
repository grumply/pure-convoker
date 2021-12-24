module Pure.Convoker.Meta where

import Pure.Convoker.Comment
import Pure.Convoker.Admins

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

    data instance Context (Meta domain a)
    data instance Name (Meta domain a)

  What is not implemented here:

    data isntance Resource (Meta domain a)
    data instance Product (Meta domain a)
    data instance Preview (Meta domain a)
    instance Previewable (Meta domain a)
    instance Processable (Meta domain a)
    instance Producible (Meta domain a)
    instance Amendable (Meta domain a)
    data instance Action (Meta domain a)
    data instance Reaction (Meta domain a)

-}

data Meta (domain :: *) (a :: *)

data instance Context (Meta domain a) = MetaContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (Meta domain a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (Meta domain a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (Meta domain a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (Meta domain a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (Meta domain a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (Meta domain a))

data instance Name (Meta domain a) = MetaName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Nameable (Meta domain a) where
  toName _ = MetaName

instance Typeable domain => Ownable (Meta domain a) where
  isOwner un _ _ = isAdmin @domain un

--------------------------------------------------------------------------------
-- Simple anonymous vote totals.

data Votes domain a = Votes
  { votes :: [(Key (Comment domain a),Int)]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data AmendVote domain a 
  = Vote Username (Key (Comment domain a)) Int
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

amendVotes :: AmendVote domain a -> Votes domain a -> Votes domain a
amendVotes (Vote _ target vote) (Votes votes) = Votes (go votes)
  where
    go [] = [(target,vote)]
    go (x : rest) 
      | fst x == target = fmap (+ vote) x : rest
      | otherwise       = x : go rest
