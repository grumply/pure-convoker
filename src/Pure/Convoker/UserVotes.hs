module Pure.Convoker.UserVotes 
  ( UserVotes(..)
  , Resource(..)
  , Product(..)
  , Preview(..)
  , Context(..)
  , Name(..)
  , Amend(..)
  , userVotesInteractions
  , userVotesPermissions
  , emptyUserVotes
  ) where

import Pure.Convoker.Comment
import Pure.Convoker.Meta

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Data.Bloom as Bloom
import Pure.Data.JSON
import Pure.Elm.Component

import Data.Hashable

import Control.Monad
import Data.Foldable
import Data.List as List
import Data.Typeable
import GHC.Generics hiding (Meta)
import System.IO.Unsafe

{-
Design notes:

  What is implemented here:

    data instance Resource (UserVotes a)
    data instance Product (UserVotes a)
    data instance Preview (UserVotes a)
    data instance Context (UserVotes a)
    data instance Name (UserVotes a)
    instance Amendable (UseerVotes a)
    instance Processable (UserVotes a)
    instance Producible (UserVotes a)
    instance Previewable (UserVotes a)
    
  I don't see a reason to override any of these.

-}

data UserVotes (a :: *)

data instance Resource (UserVotes a) = RawUserVotes
  { upvotes   :: [Key (Comment a)]
  , downvotes :: [Key (Comment a)]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Product (UserVotes a) = UserVotes
  { upvotes   :: Bloom
  , downvotes :: Bloom
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

instance ToJSON Bloom where
  toJSON = unsafePerformIO . Bloom.encode

instance FromJSON Bloom where
  parseJSON o = do
    case unsafePerformIO (Bloom.decode o) of
      Nothing -> mzero
      Just b  -> pure b

data instance Preview (UserVotes a) = NoUserVotesPreview
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Context (UserVotes a) = UserVotesContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (UserVotes a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (UserVotes a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (UserVotes a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (UserVotes a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (UserVotes a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (UserVotes a))

data instance Name (UserVotes a) = UserVotesName Username
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Amendable (UserVotes a) where
  data Amend (UserVotes a)
    = Upvote   (Key (Comment a)) 
    | Downvote (Key (Comment a)) 
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (Upvote target) RawUserVotes {..} 
    | target `elem` downvotes =
      Just RawUserVotes
        { downvotes = List.filter (/= target) downvotes 
        , ..
        }

    | target `notElem` upvotes = 
      Just RawUserVotes
        { upvotes = target : upvotes 
        , ..
        }

  amend (Downvote target) RawUserVotes {..} 
    | target `elem` upvotes =
      Just RawUserVotes
        { upvotes = List.filter (/= target) upvotes 
        , ..
        }

    | target `notElem` downvotes =
      Just RawUserVotes
        { downvotes = target : downvotes
        , ..
        }

  amend _ _ =
    Nothing

instance Processable (UserVotes a)

instance Producible (UserVotes a) where
  produce _ RawUserVotes {..} = do
    upvotes' <- new 0.0001 (List.length upvotes)
    for_ upvotes (add upvotes')

    downvotes' <- new 0.0001 (List.length downvotes)
    for_ downvotes (add downvotes')

    pure (UserVotes upvotes' downvotes')

instance Previewable (UserVotes a) where
  preview _ _ _ = pure NoUserVotesPreview

data instance Action (UserVotes a)
data instance Reaction (UserVotes a)

userVotesInteractions :: Typeable a => Interactions (UserVotes a)
userVotesInteractions = def

userVotesPermissions :: Typeable a => Username -> Permissions (UserVotes a)
userVotesPermissions un = readPermissions { canAmend = canAmend', canUpdate = canUpdate' }
  where
    canAmend' ctx (UserVotesName user) _ = pure (user == un)
    canUpdate' ctx (UserVotesName user) = pure (user == un)

emptyUserVotes :: Product (UserVotes a)
emptyUserVotes = unsafePerformIO do
  us <- new 0.0001 100
  ds <- new 0.0001 100
  pure (UserVotes us ds)