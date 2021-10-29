module Pure.Convoker.DiscussionMeta where

import Pure.Convoker.Comment (Comment)

import Pure.Auth hiding (Key)
import Pure.Data.JSON (ToJSON(..),FromJSON(..),ToJSONKey(..),FromJSONKey(..))
import Pure.Elm.Component hiding (render,Node)
import Pure.Conjurer
import Pure.Data.Render

import Data.Hashable
import Data.Tree
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Data.Typeable
import GHC.Generics

data DiscussionMeta a

data instance Resource (DiscussionMeta a) = RawDiscussionMeta
  { created    :: Time
  , edited     :: Maybe Time
  , upvoters   :: Map (Key (Comment a)) (Map Username Time)
  , downvoters :: Map (Key (Comment a)) (Map Username Time)
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Product (DiscussionMeta a) = DiscussionMeta
  { created   :: Time
  , edited    :: Maybe Time
  , upvotes   :: Map (Key (Comment a)) Int
  , downvotes :: Map (Key (Comment a)) Int
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (DiscussionMeta a) = DiscussionMetaPreview
  { created   :: Time
  , edited    :: Maybe Time
  , upvotes   :: Map (Key (Comment a)) Int
  , downvotes :: Map (Key (Comment a)) Int
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

deriving instance (ToJSONKey (Key (Comment a)))
deriving instance (FromJSONKey (Key (Comment a)))
deriving instance (ToJSONKey Username)
deriving instance (FromJSONKey Username)

data instance Context (DiscussionMeta a) = DiscussionMetaContext (Context a)
  deriving stock Generic
deriving instance (Eq (Context a), Eq (Name a)) => Eq (Context (DiscussionMeta a))
deriving instance (Ord (Context a), Ord (Name a)) => Ord (Context (DiscussionMeta a))
deriving instance (ToJSON (Context a), ToJSON (Name a)) => ToJSON (Context (DiscussionMeta a))
deriving instance (FromJSON (Context a), FromJSON (Name a)) => FromJSON (Context (DiscussionMeta a))
deriving instance (Typeable a, Pathable (Context a), Pathable (Name a)) => Pathable (Context (DiscussionMeta a))
deriving instance (Hashable (Context a), Hashable (Name a)) => Hashable (Context (DiscussionMeta a))

data instance Name (DiscussionMeta a) = DiscussionMetaName (Name a)
  deriving stock Generic
deriving instance (Eq (Name a)) => Eq (Name (DiscussionMeta a))
deriving instance (Ord (Name a)) => Ord (Name (DiscussionMeta a))
deriving instance (ToJSON (Name a)) => ToJSON (Name (DiscussionMeta a))
deriving instance (FromJSON (Name a)) => FromJSON (Name (DiscussionMeta a))
deriving instance (Pathable (Name a)) => Pathable (Name (DiscussionMeta a))
deriving instance (Hashable (Name a)) => Hashable (Name (DiscussionMeta a))

instance Amendable (DiscussionMeta a) where
  data Amend (DiscussionMeta a) 
    = Upvote (Key (Comment a)) Time Username
    | Unvote (Key (Comment a)) Time Username
    | Downvote (Key (Comment a)) Time Username
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (Upvote k now un) cm 
    | Just _ <- Map.lookup un =<< Map.lookup k (upvoters cm) = 
      Nothing

    | Just _ <- Map.lookup un =<< Map.lookup k (downvoters cm) = 
      Just cm
        { downvoters = Map.adjust (Map.delete un) k (downvoters cm)
        , upvoters   = Map.alter (Just . maybe (Map.singleton un now) (Map.insert un now)) k (upvoters cm)
        }

    | otherwise = 
      Just cm
        { upvoters = Map.alter (Just . maybe (Map.singleton un now) (Map.insert un now)) k (upvoters cm) 
        }
     
  amend (Unvote k now un) cm
    | Just _ <- Map.lookup un =<< Map.lookup k (downvoters cm) = 
      Just cm
        { downvoters = Map.adjust (Map.delete un) k (downvoters cm) 
        }

    | Just _ <- Map.lookup un =<< Map.lookup k (upvoters cm) = 
      Just cm
        { upvoters = Map.adjust (Map.delete un) k (upvoters cm)
        }

    | otherwise = 
      Nothing

  amend (Downvote k now un) cm 
    | Just _ <- Map.lookup un =<< Map.lookup k (downvoters cm) = 
      Nothing

    | Just _ <- Map.lookup un =<< Map.lookup k (upvoters cm) = 
      Just cm
        { downvoters = Map.alter (Just . maybe (Map.singleton un now) (Map.insert un now)) k (downvoters cm)
        , upvoters   = Map.adjust (Map.delete un) k (upvoters cm)
        }

    | otherwise = 
      Just cm
        { downvoters = Map.alter (Just . maybe (Map.singleton un now) (Map.insert un now)) k (downvoters cm) 
        }
      
  amend _ _ = Nothing

