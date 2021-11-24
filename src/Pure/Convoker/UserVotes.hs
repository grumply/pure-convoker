module Pure.Convoker.UserVotes 
  ( UserVotes(..)
  , Resource(..)
  , Product(..)
  , Preview(..)
  , Context(..)
  , Name(..)
  , Amend(..)
  , emptyUserVotes
  , tryUpvote
  , tryDownvote
  ) where

import Pure.Convoker.Comment
import Pure.Convoker.Meta

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Data.JSON
import Pure.Elm.Component

import Data.Hashable

import Control.Monad
import Data.Foldable
import Data.List as List
import Data.Maybe
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
  { username  :: Username
  , upvotes   :: [Key (Comment a)]
  , downvotes :: [Key (Comment a)]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Product (UserVotes a) = UserVotes
  { username  :: Username
  , upvotes   :: [Key (Comment a)]
  , downvotes :: [Key (Comment a)]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

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

instance Nameable (UserVotes a) where
  toName RawUserVotes {..} = UserVotesName username

instance Ownable (UserVotes a) where
  isOwner un ctx (UserVotesName un') = pure (un == un')

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
  produce _ _ _ RawUserVotes {..} = pure UserVotes {..}

instance Previewable (UserVotes a) where
  preview _ _ _ _ _ = pure NoUserVotesPreview

data instance Action (UserVotes a) = NoUserVotesAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
data instance Reaction (UserVotes a) = NoUserVotesReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance 
  ( Typeable a
  , ToJSON (Context a), FromJSON (Context a), Hashable (Context a), Pathable (Context a), Ord (Context a)
  , ToJSON (Name a), FromJSON (Name a), Hashable (Name a), Pathable (Name a), Ord (Name a)
  ) => DefaultPermissions (UserVotes a) 
  where
    permissions Nothing = noPermissions
    permissions (Just un) = noPermissions 
      { canCreate = canCreate'
      , canRead = canRead'
      , canAmend = canAmend' 
      }
      where
        canCreate' _ (UserVotesName user) _ = pure (user == un)
        canRead' _ (UserVotesName user) = pure (user == un)
        canAmend' ctx (UserVotesName user) _ 
          | user == un = do
            tryReadResource (permissions (Just un)) (callbacks (Just un)) ctx (UserVotesName un) >>= \case
              Nothing -> void do
                tryCreate fullPermissions (callbacks (Just un)) ctx (RawUserVotes un [] [])
              Just _  ->
                pure ()
            pure True
          | otherwise =
            pure False

emptyUserVotes :: Username -> Product (UserVotes a)
emptyUserVotes un = UserVotes un [] []

tryUpvote 
  :: ( Typeable a
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
     ) => Permissions (UserVotes a) -> Callbacks (UserVotes a) -> Context a -> Name a -> Username -> Key (Comment a) -> IO Bool
tryUpvote permissions callbacks ctx nm un k = fmap isJust do
  tryAmend permissions callbacks (UserVotesContext ctx nm) (UserVotesName un)
    (Upvote k)

tryDownvote 
  :: ( Typeable a
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
     ) => Permissions (UserVotes a) -> Callbacks (UserVotes a) -> Context a -> Name a -> Username -> Key (Comment a) -> IO Bool
tryDownvote permissions callbacks ctx nm un k = fmap isJust do
  tryAmend permissions callbacks (UserVotesContext ctx nm) (UserVotesName un)
    (Downvote k)