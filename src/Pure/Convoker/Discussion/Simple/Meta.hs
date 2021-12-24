module Pure.Convoker.Discussion.Simple.Meta
  ( Meta(..)
  , Resource(..)
  , Product(..)
  , Preview(..)
  , Amend(..)
  , Action(..)
  , Reaction(..)
  , trySetVote
  ) where

import Pure.Auth hiding (Key)
import Pure.Convoker.Comment
import Pure.Convoker.Meta
import Pure.Convoker.Mods
import Pure.Convoker.UserVotes
import Pure.Elm.Component hiding (pattern Meta)

import Pure.Conjurer
import Pure.Data.JSON

import Data.Hashable

import Data.List as List
import Data.Maybe
import Data.Typeable
import GHC.Generics hiding (Meta)

{-
Design notes:

  What is inherited:

    data instance Context (Meta domain a)
    data instance Name (Meta domain a)

  What is implemented here:

    data instance Resource (Meta domain a)
    data instance Product (Meta domain a)
    data instance Preview (Meta domain a)
    instance Processable (Meta domain a)
    instance Producible (Meta domain a)
    instance Previewable (Meta domain a)
    instance Amendable (Meta domain a)
    data instance Action (Meta domain a)
    data instance Reaction (Meta domain a)
  
  What is overridable with IncoherentIntances:
    
    instance Previewable (Meta domain a)
    instance Processable (Meta domain a)
    instance Producible  (Meta domain a)
    
-}

data instance Resource (Meta domain a) = RawMeta
  { votes :: Votes domain a
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

instance Default (Resource (Meta domain a)) where
  def = RawMeta (Votes [])

data instance Product (Meta domain a) = Meta
  { votes :: Votes domain a
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (Meta domain a) = NoMetaPreview
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Processable (Meta domain a)

instance Producible (Meta domain a) where
  produce _ _ _ RawMeta {..} _ = pure Meta {..}

instance Previewable (Meta domain a) where
  preview _ _ _ _ _ = pure NoMetaPreview 

instance Amendable (Meta domain a) where
  data Amend (Meta domain a) 
    = SetVote (AmendVote domain a)
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (SetVote amnd) RawMeta {..} = 
    Just RawMeta 
      { votes = amendVotes amnd votes
      , ..
      }

data instance Action (Meta domain a) = NoMetaAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
data instance Reaction (Meta domain a) = NoMetaReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

trySetVote 
  :: ( Typeable domain
     , Typeable a 
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
     )
    => Permissions (Meta domain a) -> Callbacks (Meta domain a) -> Context a -> Name a -> Username -> Key (Comment domain a) -> Int -> IO Bool
trySetVote permissions callbacks ctx nm un k v = fmap isJust do
  tryAmend permissions callbacks (MetaContext ctx nm) MetaName
    (SetVote (Vote un k v))

instance 
  ( Typeable domain
  , Typeable a
  , DefaultPermissions (Meta domain a), DefaultCallbacks (Meta domain a)
  , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
  , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
  ) => DefaultCallbacks (UserVotes domain a) 
  where
    callbacks Nothing = def
    callbacks (Just un) = def { onAmend = onAmend' }
      where
        onAmend' (UserVotesContext ctx nm) (UserVotesName un) res pro pre lst = \case
          Upvote comment -> void do
            tryAmend fullPermissions (callbacks (Just un)) (MetaContext ctx nm) MetaName 
              (SetVote (Vote un comment 1))

          Downvote comment -> void do
            tryAmend fullPermissions (callbacks (Just un)) (MetaContext ctx nm) MetaName 
              (SetVote (Vote un comment (-1)))