module Pure.Convoker.Mods
  ( Mods(..)
  , Resource(..)
  , Product(..)
  , Preview(..)
  , Amend(..)
  , Context(..)
  , Name(..)
  , tryCreateMods
  , tryAddMod
  , tryRemoveMod
  , modsPermissions
  , modsCallbacks
  , isMod
  ) where

import Pure.Convoker.Admins hiding (Add,Remove)

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Data.JSON
import Pure.Elm.Component

import Data.Hashable

import Data.List as List
import Data.Maybe
import Data.Typeable
import GHC.Generics

data Mods a
data instance Resource (Mods a) = RawMods
  { mods :: [Username]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Product (Mods a) = Mods
  { mods :: [Username]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (Mods a) = NoModsPreview
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Context (Mods a) = ModsContext (Context a)
  deriving stock Generic
deriving instance Eq (Context a) => Eq (Context (Mods a))
deriving instance Ord (Context a) => Ord (Context (Mods a))
deriving instance Hashable (Context a) => Hashable (Context (Mods a))
deriving instance (Typeable a, Pathable (Context a)) => Pathable (Context (Mods a))
deriving instance ToJSON (Context a) => ToJSON (Context (Mods a))
deriving instance FromJSON (Context a) => FromJSON (Context (Mods a))

data instance Name (Mods a) = ModsName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Processable (Mods a)

instance Producible (Mods a) where
  produce _ RawMods {..} = pure Mods {..}

instance Previewable (Mods a) where
  preview _ _ _ = pure NoModsPreview

instance Nameable (Mods a) where
  toName _ = ModsName

data instance Action (Mods a)
data instance Reaction (Mods a)

instance Amendable (Mods a) where
  data Amend (Mods a) = Add Username | Remove Username
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (Add un) RawMods {..} | un `notElem` mods = 
    Just RawMods { mods = un : mods }
  
  amend (Remove un) RawMods {..} | un `elem` mods, List.length mods > 1, List.last mods /= un =
    Just RawMods { mods = List.filter (/= un) mods }

  amend _ _ = 
    Nothing

tryCreateMods 
  :: ( Typeable (a :: *)
     , Processable (Mods a)
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     ) => Permissions (Mods a) -> Callbacks (Mods a) -> Context a -> [Username] -> IO Bool
tryCreateMods permissions callbacks ctx mods = fmap isJust do
  tryCreate permissions callbacks (ModsContext ctx) (RawMods mods)

tryAddMod 
  :: ( Typeable (a :: *)
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     ) => Permissions (Mods a) -> Callbacks (Mods a) -> Context a -> Username -> IO Bool
tryAddMod permissions callbacks ctx mod = fmap isJust do
  tryAmend permissions callbacks (ModsContext ctx) ModsName (Add mod)

tryRemoveMod 
  :: ( Typeable (a :: *)
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     ) => Permissions (Mods a) -> Callbacks (Mods a) -> Context a -> Username -> IO Bool
tryRemoveMod permissions callbacks ctx mod = fmap isJust do
  tryAmend permissions callbacks (ModsContext ctx) ModsName (Remove mod)

modsPermissions 
  :: ( Typeable (a :: *)
     , Pathable (Context (Mods a)), Hashable (Context (Mods a))
     ) => Username -> Permissions (Mods a)
modsPermissions un = readPermissions { canAmend = canAmend' }
  where
    canAmend' ctx _ _ = isMod ctx un

modsCallbacks :: Callbacks (Mods a)
modsCallbacks = def

isMod :: (Pathable (Context a), Hashable (Context a), Typeable a) => Context a -> Username -> IO Bool
isMod ctx un =
  tryReadProduct fullPermissions def (ModsContext ctx) ModsName >>= \case
    Just Mods {..} | un `elem` mods -> pure True
    _ -> isAdmin un