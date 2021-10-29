{-# language DuplicateRecordFields #-}
module Pure.Convoker.Comment where

import Pure.Auth hiding (Key)
import Pure.Data.JSON (ToJSON(..),FromJSON(..))
import Pure.Elm.Component as Pure hiding (render,Node)
import Pure.Conjurer
import Pure.Data.Render

import Data.Hashable
import Data.Tree
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Data.Typeable
import GHC.Generics
  
data Comment a

data instance Resource (Comment a) = RawComment
  { context :: (Context a,Name a)
  , parent  :: Maybe (Key (Comment a)) 
  , key     :: Key (Comment a)
  , author  :: Username
  , time    :: Time
  , content :: Maybe (Product a,Preview a)
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Product a), ToJSON (Preview a)) => ToJSON (Resource (Comment a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Product a), FromJSON (Preview a)) => FromJSON (Resource (Comment a))

data instance Product (Comment a) = Comment
  { context :: (Context a,Name a)
  , parent  :: Maybe (Key (Comment a))
  , key     :: Key (Comment a)
  , author  :: Username
  , time    :: Time
  , content :: Maybe (Product a)
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Product a)) => ToJSON (Product (Comment a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Product a)) => FromJSON (Product (Comment a))

data instance Preview (Comment a) = CommentPreview
  { context :: (Context a,Name a)
  , parent  :: Maybe (Key (Comment a))
  , key     :: Key (Comment a)
  , author  :: Username
  , time    :: Time
  , content :: Maybe (Preview a)
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Preview a)) => ToJSON (Preview (Comment a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Preview a)) => FromJSON (Preview (Comment a))

instance Amendable (Comment a) where
  data Amend (Comment a)
    = UpdateCommentContent (Maybe (Product a,Preview a))
    deriving stock Generic
    
  amend (UpdateCommentContent mpp) RawComment {..} = 
    Just RawComment
      { content = mpp 
      , ..
      }

deriving instance (ToJSON (Product a), ToJSON (Preview a)) => ToJSON (Amend (Comment a))
deriving instance (FromJSON (Product a), FromJSON (Preview a)) => FromJSON (Amend (Comment a))

instance Processable (Comment a) where
  process _ RawComment {..} = do
    now <- Pure.time
    k <- newKey
    pure $ Just RawComment
      { key = k 
      , time = now
      , ..
      }

instance Producible (Comment a) where
  produce live RawComment {..} = do
    pure Comment 
      { content = fmap fst content
      , ..
      }

instance Previewable (Comment a) where
  preview live RawComment {..} _ = 
    pure CommentPreview 
      { content = fmap snd content
      , ..
      }

data instance Context (Comment a) = CommentContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a), Eq (Name a)) => Eq (Context (Comment a))
deriving instance (Ord (Context a), Ord (Name a)) => Ord (Context (Comment a))
deriving instance (ToJSON (Context a), ToJSON (Name a)) => ToJSON (Context (Comment a))
deriving instance (FromJSON (Context a), FromJSON (Name a)) => FromJSON (Context (Comment a))
deriving instance (Typeable a, Pathable (Context a), Pathable (Name a)) => Pathable (Context (Comment a))
deriving instance (Hashable (Context a), Hashable (Name a)) => Hashable (Context (Comment a))

data instance Name (Comment a) = CommentName (Key (Comment a))
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

instance Nameable (Comment a) where
  toName RawComment {..} = CommentName key
