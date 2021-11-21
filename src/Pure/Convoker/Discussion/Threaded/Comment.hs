module Pure.Convoker.Discussion.Threaded.Comment 
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
import Pure.Data.JSON
import Pure.Elm.Component hiding (pattern Delete,not)

import Data.Hashable

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

data instance Resource (Comment a) = RawComment
  { author   :: Username
  , key      :: Key (Comment a)
  , parents  :: [Key (Comment a)]
  , created  :: Time
  , edited   :: Maybe Time
  , deleted  :: Bool
  , content  :: Markdown
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

-- the key, author, and creation time are replaced on the server
instance Default (Resource (Comment a)) where
  def = RawComment 
    { author   = fromTxt ""
    , key      = unsafePerformIO newKey 
    , parents  = []
    , created  = unsafePerformIO time
    , edited   = Nothing
    , deleted  = False
    , content  = Markdown ""
    }

instance Nameable (Comment a) where
  toName RawComment {..} = CommentName key

instance Amendable (Comment a) where
  data Amend (Comment a) 
    = SetContent Markdown Time
    | Delete
    | Undelete
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (SetContent md t) RawComment {..} | not deleted = 
    Just RawComment
      { content = md 
      , edited = Just t
      , ..
      }
      
  amend Delete RawComment {..} | not deleted =
    Just RawComment
      { deleted = True
      , ..
      }

  amend Undelete RawComment {..} | deleted =
    Just RawComment
      { deleted = False 
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
    t <- time
    k <- newKey
    pure $ Just RawComment
      { created = t
      , parents = List.take 1 parents
      , edited = Nothing
      , key = k 
      , ..
      }

-- This can be overridden with incoherent instances to customize processing!
instance Producible (Comment a) where
  produce _ _ _ RawComment {..} =
    pure Comment
      { content = if deleted then [ "[ removed ]" ] else parseMarkdown content
      , ..
      }

instance 
  ( Typeable a 
  , Pathable (Context a), Hashable (Context a)
  , Pathable (Name a), Hashable (Name a)
  ) => DefaultPermissions (Comment a) 
  where
    permissions Nothing = readPermissions
    permissions (Just un) =
      readPermissions
        { canCreate = \_ _ _ -> pure True
        , canUpdate = canUpdate'
        , canAmend  = canAmend'
        }
      where
        canUpdate' (CommentContext ctx nm) (CommentName k) = canEditComment ctx nm k un
        canAmend' (CommentContext ctx nm) (CommentName k) = \case
          SetContent _ _ -> canEditComment ctx nm k un
          _              -> isMod ctx un 

