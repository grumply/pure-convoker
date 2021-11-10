module Pure.Convoker.Discussion.Threaded.Meta
  ( Meta(..)
  , Resource(..)
  , Product(..)
  , Preview(..)
  , Amend(..)
  , metaCallbacks 
  , metaPermissions
  , metaInteractions
  ) where

import Pure.Auth
import Pure.Convoker.Comment
import Pure.Convoker.Meta
import Pure.Convoker.Mods
import Pure.Convoker.UserVotes
import Pure.Elm.Component hiding (pattern Meta)

import Pure.Conjurer
import Pure.Data.JSON

import Data.Hashable

import Data.List as List
import Data.Typeable
import GHC.Generics hiding (Meta)

{-
Design notes:

  What is inherited:

    data instance Context (Meta a)
    data instance Name (Meta a)

  What is implemented here:

    data instance Resource (Meta a)
    data instance Product (Meta a)
    data instance Preview (Meta a)
    instance Processable (Meta a)
    instance Producible (Meta a)
    instance Previewable (Meta a)
    instance Amendable (Meta a)
    data instance Action (Meta a)
    data instance Reaction (Meta a)
  
  What is overridable with IncoherentIntances:
    
    instance Previewable (Meta a)
    instance Processable (Meta a)
    instance Producible  (Meta a)
    
-}

data instance Resource (Meta a) = RawMeta
  { votes :: Votes a
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Product (Meta a) = Meta
  { votes :: Votes a
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (Meta a) = NoMetaPreview
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Processable (Meta a)

instance Producible (Meta a) where
  produce _ RawMeta {..} = pure Meta {..}

instance Previewable (Meta a) where
  preview _ _ _ = pure NoMetaPreview 

instance Amendable (Meta a) where
  data Amend (Meta a) 
    = SetVote (AmendVote a)
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (SetVote amnd) RawMeta {..} = 
    Just RawMeta 
      { votes = amendVotes amnd votes
      , ..
      }

data instance Action (Meta a)
data instance Reaction (Meta a) 

metaInteractions :: Typeable a => Interactions (Meta a)
metaInteractions = def

metaPermissions :: Username -> Permissions (Meta a)
metaPermissions un = readPermissions { canAmend = canAmend' }
  where
    canAmend' _ _ = \case
      SetVote (Vote _ _ _) -> pure True
    
metaCallbacks :: Callbacks (Meta a)
metaCallbacks = def

userVotesCallbacks 
  :: ( Typeable a
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
     ) => Permissions (Meta a) -> Callbacks (Meta a) -> Callbacks (UserVotes a)
userVotesCallbacks metaPermissions metaCallbacks = def { onAmend = onAmend' }
  where
    onAmend' (UserVotesContext ctx nm) (UserVotesName un) res pro pre = \case
      Upvote comment -> void do
        tryAmend metaPermissions metaCallbacks (MetaContext ctx nm) MetaName 
          (SetVote (Vote un comment 1))

      Downvote comment -> void do
        tryAmend metaPermissions metaCallbacks (MetaContext ctx nm) MetaName 
          (SetVote (Vote un comment (-1)))
