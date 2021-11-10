module Pure.Convoker.Comment 
  ( Comment(..)
  , Product(..)
  , Preview(..)
  , Context(..)
  , Name(..)
  , canEditComment
  ) where

import Pure.Convoker.Mods

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Elm.Component hiding (not,key,pattern Meta)
import Pure.Data.JSON
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

data Comment (a :: *)

data instance Product (Comment a) = Comment
  { author   :: Username
  , key      :: Key (Comment a)
  , parents  :: [Key (Comment a)]
  , created  :: Time
  , edited   :: Maybe Time
  , content  :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (Comment a) = CommentPreview
  { author   :: Username
  , key      :: Key (Comment a)
  , parents  :: [Key (Comment a)]
  , created  :: Time
  , edited   :: Maybe Time
  , content  :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Context (Comment a) = CommentContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (Comment a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (Comment a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (Comment a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (Comment a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (Comment a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (Comment a))

data instance Name (Comment a) = CommentName (Key (Comment a))
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Previewable (Comment a) where
  preview _ _ Comment {..} = pure CommentPreview {..}

canEditComment 
  :: ( Typeable a
     , Pathable (Context a), Hashable (Context a)
     , Pathable (Name a), Hashable (Name a)
     ) => Context a -> Name a -> Key (Comment a) -> Username -> IO Bool
canEditComment ctx nm k un = 
  tryReadProduct fullPermissions def (CommentContext ctx nm) (CommentName k) >>= \case
    Just Comment {..} | author == un -> pure True
    _ -> isMod (ModsContext ctx) un
