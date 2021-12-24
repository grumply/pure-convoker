module Pure.Convoker.Comment 
  ( Deleted(..)
  , Parents(..)
  , Edited(..)
  , Created(..)
  , Comment(..)
  , Product(..)
  , Preview(..)
  , Context(..)
  , Name(..)
  , canEditComment
  ) where

import Pure.Convoker.Mods
import Pure.Convoker.Admins

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Elm.Component hiding (not,key,pattern Meta)
import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt
import Pure.Data.Render

import Data.Hashable

import Control.Monad
import Data.List as List
import qualified Data.Graph as G
import Data.Typeable
import GHC.Generics hiding (Meta)

{-
Design notes:

  What is implemented here:

    data instance Product (Comment a)
    data instance Preview (Comment a)
    data instance Context (Comment a)
    data instance Name (Comment a)
    instance Previewable (Comment a)

  What is not implemented here:

    data instance Resource (Comment a)
    instance Processable (Comment a)
    instance Producible (Comment a)
    instance Amendable (Comment a)
    data instance Action (Comment a)
    data instance Reaction (Comment a)

  What is overridable with IncoherentInstances:

    instance Previewable (Comment a)

-}

data Comment (domain :: *) (a :: *)

instance Fieldable Username where
  field _ _ = Null

newtype Deleted = Deleted Bool
  deriving (Eq,Ord,ToJSON,FromJSON) via Bool

instance Fieldable Deleted where
  field _ _ = Null

newtype Created = Created Time
  deriving (Eq,Ord,ToJSON,FromJSON) via Time

instance Fieldable Created where
  field _ _ = Null

newtype Edited = Edited (Maybe Time)
  deriving (Eq,Ord,ToJSON,FromJSON) via Maybe Time

instance Fieldable Edited where
  field _ _ = Null

newtype Parents domain a = Parents [Key (Comment domain a)]
  deriving (ToJSON,FromJSON) via [Key (Comment domain a)]

instance Fieldable (Parents domain a) where
  field _ _ = Null

data instance Product (Comment domain a) = Comment
  { author   :: Username
  , key      :: Key (Comment domain a)
  , parents  :: Parents domain a
  , created  :: Created
  , edited   :: Edited
  , deleted  :: Deleted
  , content  :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (Comment domain a) = CommentPreview
  { author   :: Username
  , key      :: Key (Comment domain a)
  , parents  :: Parents domain a
  , created  :: Created
  , edited   :: Edited
  , deleted  :: Deleted
  , content  :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

instance Typeable domain => Ownable (Comment domain a) where 
  isOwner un _ _ = isAdmin @domain un

data instance Context (Comment domain a) = CommentContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (Comment domain a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (Comment domain a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (Comment domain a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (Comment domain a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (Comment domain a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (Comment domain a))

data instance Name (Comment domain a) = CommentName (Key (Comment domain a))
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Previewable (Comment domain a) where
  preview _ _ _ _ Comment {..} = pure CommentPreview {..}

canEditComment 
  :: forall domain a.
     ( Typeable domain
     , Typeable a
     , Pathable (Context a), Hashable (Context a), Ord (Context a)
     , Pathable (Name a), Hashable (Name a), Ord (Name a)
     ) => Context a -> Name a -> Key (Comment domain a) -> Username -> IO Bool
canEditComment ctx nm k un = 
  tryReadProduct (fullPermissions @(Comment domain a)) (callbacks @(Comment domain a) (Just un)) (CommentContext ctx nm) (CommentName k) >>= \case
    Just Comment {..} | author == un -> pure True
    _ -> isMod @domain ctx un
