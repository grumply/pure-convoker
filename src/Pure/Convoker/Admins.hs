{-# language CPP #-}
module Pure.Convoker.Admins 
  ( Admins(..)
  , Context(..)
  , Name(..)
  , Product(..)
  , Preview(..)
  , Amend(..)
  , tryCreateAdmins
  , tryAddAdmin
  , tryRemoveAdmin
  , isAdmin
  , adminPermissions
  ) where

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Elm.Component
import Pure.Data.JSON

import Data.Hashable

import Control.Monad (liftM2)
import Data.List as List
import Data.Maybe
import GHC.Generics

import Pure.WebSocket.Cache as WS

data Admins
data instance Resource Admins = RawAdmins
  { admins :: [Username]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Product Admins = Admins
  { admins :: [Username] 
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview Admins = NoAdminsPreview
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Context Admins = AdminsContext
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

data instance Name Admins = AdminsName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Routable Admins

instance Producible Admins where
  produce _ _ _ RawAdmins {..} _ = pure Admins {..}

instance Previewable Admins where
  preview _ _ _ _ _ = pure NoAdminsPreview

instance Nameable Admins where
  toName _ = AdminsName

instance Processable Admins

instance Ownable Admins where
  isOwner un _ _ = isAdmin un

instance Amendable Admins where
  data Amend Admins = AddAdmin Username | RemoveAdmin Username
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (AddAdmin un) RawAdmins {..} | un `notElem` admins = 
    Just RawAdmins { admins = un : admins }

  amend (RemoveAdmin un) RawAdmins {..} | un `elem` admins, List.length admins > 1, List.last admins /= un = 
    Just RawAdmins { admins = List.filter (/= un) admins }

  amend _ _ = 
    Nothing

data instance Action Admins = NoAdminsAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Reaction Admins = NoAdminsReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance DefaultPermissions Admins where
  permissions Nothing = readPermissions
  permissions (Just un) = readPermissions { canAmend = canAmend' }
    where
      canAmend' _ _ _ = isAdmin un

tryCreateAdmins :: [Username] -> IO Bool
tryCreateAdmins admins = fmap isJust do
  tryCreate fullPermissions (callbacks Nothing) AdminsContext (RawAdmins admins)

tryAddAdmin :: Permissions Admins -> Callbacks Admins -> Username -> IO Bool
tryAddAdmin permissions callbacks un = fmap isJust do
  tryAmend permissions callbacks AdminsContext AdminsName 
    (AddAdmin un)

tryRemoveAdmin :: Permissions Admins -> Callbacks Admins -> Username -> IO Bool
tryRemoveAdmin permissions callbacks un = fmap isJust do
  tryAmend permissions callbacks AdminsContext AdminsName
    (RemoveAdmin un)

isAdmin :: Username -> IO Bool
isAdmin un = 
#ifdef __GHCJS__
  WS.req WS.Cached (readingAPI @Admins) (readProduct @Admins) (AdminsContext,AdminsName) >>= \case
    Nothing -> pure False
    Just Admins {..} -> pure (un `elem` admins)
#else
  tryReadProduct readPermissions (callbacks Nothing) AdminsContext AdminsName >>= \case
    Just Admins {..} -> pure (un `elem` admins)
    _ -> pure False
#endif

defaultIsOwner :: Ownable a => Username -> Context a -> Name a -> IO Bool
defaultIsOwner un ctx nm = liftM2 (||) (isOwner un ctx nm) (isAdmin un)

adminPermissions :: Username -> Permissions resource
adminPermissions un = Permissions {..}
  where
    canRead     ctx nm      = isAdmin un
    canCreate   ctx nm res  = isAdmin un
    canUpdate   ctx nm      = isAdmin un
    canAmend    ctx nm amnd = isAdmin un
    canInteract ctx nm actn = isAdmin un
    canDelete   ctx nm      = isAdmin un
    canList     ctx         = isAdmin un
    canEnum                 = isAdmin un

