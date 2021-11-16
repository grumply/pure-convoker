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
  , adminsPermissions
  , adminsCallbacks
  , isAdmin
  , adminPermissions
  ) where

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Elm.Component
import Pure.Data.JSON

import Data.Hashable

import Data.List as List
import Data.Maybe
import GHC.Generics

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
  produce _ _ _ RawAdmins {..} = pure Admins {..}

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

tryCreateAdmins :: [Username] -> IO Bool
tryCreateAdmins admins = fmap isJust do
  tryCreate fullPermissions def AdminsContext (RawAdmins admins)

tryAddAdmin :: Permissions Admins -> Callbacks Admins -> Username -> IO Bool
tryAddAdmin permissions callbacks un = fmap isJust do
  tryAmend permissions callbacks AdminsContext AdminsName 
    (AddAdmin un)

tryRemoveAdmin :: Permissions Admins -> Callbacks Admins -> Username -> IO Bool
tryRemoveAdmin permissions callbacks un = fmap isJust do
  tryAmend permissions callbacks AdminsContext AdminsName
    (RemoveAdmin un)

adminsPermissions :: Username -> Permissions Admins
adminsPermissions un = readPermissions { canAmend = canAmend' }
  where
    canAmend' _ _ _ = isAdmin un

adminsCallbacks :: Callbacks Admins
adminsCallbacks = def

isAdmin :: Username -> IO Bool
isAdmin un = 
  tryReadProduct fullPermissions def AdminsContext AdminsName >>= \case
    Just Admins {..} | un `elem` admins -> pure True
    _ -> pure False

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

adminsInteractions :: Interactions Admins
adminsInteractions = def 