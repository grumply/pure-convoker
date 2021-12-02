module Pure.Convoker.Discussion.Simple.Comment 
  ( Comment(..)
  , Resource(..)
  , Amend(..)
  , Action(..)
  , Reaction(..)
  , canEditComment
  ) where

import Pure.Convoker.Comment
import Pure.Convoker.Mods
import Pure.Convoker.Discussion.Shared.Markdown

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Data.JSON (ToJSON,FromJSON,traceJSON)
import Pure.Elm.Component hiding (pattern Delete,not)

import Data.Hashable

import Data.Coerce
import Data.List as List
import Data.Typeable
import GHC.Generics

import System.IO.Unsafe

{-
Design notes:

  What is inherited:

    data instance Product (Comment a)
    data instance Preview (Comment a)
    data instance Context (Comment a)
    data instance Name (Comment a)
    instance Previewable (Comment a)

  What is implemented here:

    data instance Resource (Comment a)
    instance Nameable (Comment a)
    instance Amendable (Comment a)
    data instance Action (Comment a)
    data instance Reaction (Comment a)
    instance Processable (Comment a)
    instance Producible (Comment a)
  
  What is overridable with IncoherentIntances:
    
    instance Nameable (Comment a)
    instance Previewable (Comment a)
    instance Processable (Comment a)
    instance Producible (Comment a)
    
-}

-- Overridable instances. These aren't used in the default discussion views.
instance {-# INCOHERENT #-} Typeable a => Component (Preview (Comment a))
instance {-# INCOHERENT #-} Typeable a => Component (Product (Comment a))

data instance Resource (Comment a) = RawComment
  { author   :: Username
  , key      :: Key (Comment a)
  , parents  :: Parents a
  , created  :: Created
  , edited   :: Edited
  , deleted  :: Deleted
  , content  :: Markdown
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

-- the key, author, and creation time are replaced on the server
instance Default (Resource (Comment a)) where
  def = RawComment 
    { author   = fromTxt ""
    , key      = unsafePerformIO newKey 
    , parents  = Parents []
    , created  = Created (unsafePerformIO time)
    , edited   = Edited Nothing
    , deleted  = Deleted False
    , content  = Markdown ""
    }

instance Nameable (Comment a) where
  toName RawComment {..} = CommentName key

instance Amendable (Comment a) where
  data Amend (Comment a) 
    = SetContent Markdown
    | Delete
    | Undelete
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (SetContent md) RawComment {..} | Deleted False <- deleted = 
    Just RawComment
      { content = md 
      , edited = Edited (Just (unsafePerformIO time))
      , ..
      }
      
  amend Delete RawComment {..} | Deleted False <- deleted =
    Just RawComment
      { deleted = Deleted True
      , ..
      }

  amend Undelete RawComment {..} | Deleted True <- deleted =
    Just RawComment
      { deleted = Deleted False
      , ..
      }

  amend _ _ = 
    Nothing

data instance Action (Comment a) = NoCommentAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
data instance Reaction (Comment a) = NoCommentReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Processable (Comment a) where
  process _ RawComment {..} = do
    let Parents ps = parents
    t <- time
    k <- newKey
    pure $ Just RawComment
      { key = k 
      , created = Created t
      , parents = Parents (List.take 1 ps)
      , edited = Edited Nothing
      , ..
      }

-- This can be overridden with incoherent instances to customize processing!
instance Producible (Comment a) where
  produce _ _ _ RawComment {..} =
    pure Comment
      { content = if deleted == Deleted True then [ "[ removed ]" ] else parseMarkdown content
      , ..
      }

instance 
  ( Typeable a 
  , Pathable (Context a), Hashable (Context a), Ord (Context a)
  , Pathable (Name a), Hashable (Name a), Ord (Name a)
  ) => DefaultPermissions (Comment a) 
  where
    permissions Nothing = readPermissions
    permissions (Just un) =
      readPermissions
        { canCreate = \_ _ RawComment {..} -> pure (author == un)
        , canUpdate = canUpdate'
        , canAmend  = canAmend'
        }
      where
        canUpdate' (CommentContext ctx nm) (CommentName k) = canEditComment ctx nm k un
        canAmend' (CommentContext ctx nm) (CommentName k) = \case
          SetContent _ -> canEditComment ctx nm k un
          _            -> isMod ctx un 

