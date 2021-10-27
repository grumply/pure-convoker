module Pure.Convoker where

import Pure.Auth
import Pure.Data.JSON (ToJSON(..),FromJSON(..))
import Pure.Elm.Component hiding (render,Node)
import Pure.Conjurer
import Pure.Data.Render

import Data.Hashable
import Data.Tree

import Data.Typeable
import GHC.Generics

{-
This should be all the impelementation that is required to add
comments to an `Article` resource with full customization of 
content processing.

instance Renderable (Comment Article) where
  data Content (Comment Article) = RawArticleComment Markdown
  data Rendered (Comment Article) = ArticleComment [View]
  render (RawArticleComment md) = ArticleComment <$> fromMarkdown md
  
TODO:

Add comment administration. 

Add comment processing callbacks that add comments to threads.

-}

class Renderable a where
  data Content a :: *
  data Rendered a :: *
  render :: Content a -> IO (Rendered a)

--------------------------------------------------------------------------------
-- CommentMeta

data CommentMeta

data instance Resource CommentMeta = RawCommentMeta
  { verified :: Bool
  , created  :: Time
  , edited   :: Maybe Time
  , deleted  :: Bool
  , votes    :: (Int,Int)
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

-- The product may be vote-fuzzed. 
data instance Product CommentMeta = CommentMeta
  { verified :: Bool
  , created  :: Time
  , edited   :: Maybe Time
  , deleted  :: Bool
  , votes    :: (Int,Int)
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

-- The preview may be vote-fuzzed.
data instance Preview CommentMeta = CommentMeta
  { verified :: Bool
  , created  :: Time
  , edited   :: Maybe Time
  , deleted  :: Bool
  , votes    :: (Int,Int)
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

--------------------------------------------------------------------------------
-- Comment

data Comment a

data instance Resource (Comment a) = RawComment
  { context :: (Context a,Name a)
  , parent  :: Maybe (Key (Comment a)) 
  , key     :: Key (Comment a)
  , author  :: Username
  , content :: Maybe (Content a)
  , meta    :: CommentMeta
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Content a), ToJSON (Rendered a)) => ToJSON (Resource (Comment a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Content a), FromJSON (Rendered a)) => FromJSON (Resource (Comment a))

data instance Product (Comment a) = Comment
  { context :: (Context a,Name a)
  , parent  :: Maybe (Key (Comment a))
  , key     :: Key (Comment a)
  , author  :: Username
  , content :: Maybe (Rendered a)
  , meta    :: CommentMeta
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Rendered a)) => ToJSON (Product (Comment a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Rendered a)) => FromJSON (Product (Comment a))

data instance Preview (Comment a) = CommentPreview
  { context :: (Context a,Name a)
  , parent  :: Maybe (Key (Comment a))
  , key     :: Key (Comment a)
  , author  :: Username
  , content :: Maybe (Rendered a)
  , meta    :: CommentMeta
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Rendered a)) => ToJSON (Preview (Comment a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Rendered a)) => FromJSON (Preview (Comment a))

instance Amendable (Comment a) where
  data Amend (Comment a) 
    = VerifyComment
    | UpvoteComment
    | DownvoteComment
  

instance Renderable a => Producible (Comment a) where
  produce live RawComment {..} = do
    c <- render content
    pure Comment 
      { content = c
      , ..
      }

instance Previewable (Comment a) where
  preview live _ Comment {..} = pure CommentPreview {..}

data instance Context (Comment a) = CommentContext (Context a) (Name a) (Key (Thread a))
  deriving stock Generic
deriving instance (Eq (Context a), Eq (Name a)) => Eq (Context (Comment a))
deriving instance (Ord (Context a), Ord (Name a)) => Ord (Context (Comment a))
deriving instance (ToJSON (Context a), ToJSON (Name a)) => ToJSON (Context (Comment a))
deriving instance (FromJSON (Context a), FromJSON (Name a)) => FromJSON (Context (Comment a))
deriving instance (Typeable a, Pathable (Context a), Pathable (Name a)) => Pathable (Context (Comment a))
deriving instance (Hashable (Context a), Hashable (Name a)) => Hashable (Context (Comment a))

data instance Name (Comment a) = CommentName (Key (Comment a))
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

instance (Typeable a, Pathable (Context a), Pathable (Name a)) => Routable (Comment a)

instance Nameable (Comment a) where
  toName RawComment {..} = CommentName key

instance Fieldable (Context a,Name a) where
  field _ _ = Null

instance Fieldable (Maybe (Key a)) where
  field _ _ = Null

instance (Fieldable (Content a), Typeable a) => Formable (Resource (Comment a))
instance (Fieldable (Content a), Typeable a) => Fieldable (Resource (Comment a)) where
  field onsubmit initial = form onsubmit onpreview initial
    where onpreview _ = pure Null -- TODO

instance 
  ( Typeable a
  , Theme (Comment a)
  , ToJSON (Context a), FromJSON (Context a), Eq (Context a)
  , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
  , ToJSON (Rendered a), FromJSON (Rendered a)
  , Component (Product (Comment a))
  ) => Readable (Comment a)

instance 
  ( Typeable a
  , Theme (Comment a)
  , Pathable (Context a), ToJSON (Context a), FromJSON (Context a), Eq (Context a)
  , Pathable (Name a), ToJSON (Name a), FromJSON (Name a), Eq (Name a)
  , FromJSON (Rendered a)
  , Component (Preview (Comment a))
  ) => Listable (Comment a)

instance 
  ( Typeable _role, Typeable a
  , Theme (Comment a)
  , Eq (Context a), Pathable (Context a), ToJSON (Context a), FromJSON (Context a)
  , Eq (Name a), Pathable (Name a), ToJSON (Name a), FromJSON (Name a)
  , Fieldable (Content a), ToJSON (Content a), FromJSON (Content a)
  , ToJSON (Rendered a), FromJSON (Rendered a)
  , Component (Preview (Comment a)) 
  , Component (Product (Comment a)) 
  ) => Updatable _role (Comment a)

instance 
  ( Typeable _role, Typeable a
  , Theme (Comment a)
  , Default (Resource (Comment a))
  , Pathable (Context a), ToJSON (Context a), FromJSON (Context a)
  , Pathable (Name a), ToJSON (Name a), FromJSON (Name a)
  , ToJSON (Content a), FromJSON (Content a)
  , ToJSON (Rendered a), FromJSON (Rendered a)
  , Fieldable (Content a)
  , Component (Preview (Comment a))
  , Component (Product (Comment a))
  ) => Creatable _role (Comment a)

--------------------------------------------------------------------------------
-- Thread

data Thread a

data instance Resource (Thread a) = RawThread
  { context  :: (Context a,Name a)
  , key      :: Key (Thread a)
  , comments :: Tree (Resource (Comment a))
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Content a), ToJSON (Rendered a)) => ToJSON (Resource (Thread a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Content a), FromJSON (Rendered a)) => FromJSON (Resource (Thread a))

data instance Product (Thread a) = Thread
  { context  :: (Context a,Name a)
  , key      :: Key (Thread a)
  , comments :: Tree (Product (Comment a))
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Rendered a)) => ToJSON (Product (Thread a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Rendered a)) => FromJSON (Product (Thread a))

data instance Preview (Thread a) = ThreadPreview
  { context  :: (Context a,Name a)
  , key      :: Key (Thread a)
  , comments :: Tree (Preview (Comment a))
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Rendered a)) => ToJSON (Preview (Thread a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Rendered a)) => FromJSON (Preview (Thread a))

data instance Context (Thread a) = ThreadContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a), Eq (Name a)) => Eq (Context (Thread a))
deriving instance (Ord (Context a), Ord (Name a)) => Ord (Context (Thread a))
deriving instance (ToJSON (Context a), ToJSON (Name a)) => ToJSON (Context (Thread a))
deriving instance (FromJSON (Context a), FromJSON (Name a)) => FromJSON (Context (Thread a))
deriving instance (Typeable a, Pathable (Context a), Pathable (Name a)) => Pathable (Context (Thread a))
deriving instance (Hashable (Context a), Hashable (Name a)) => Hashable (Context (Thread a))

data instance Name (Thread a) = ThreadName (Key (Thread a))
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

zipTrees :: Tree a -> Tree b -> Tree (a,b)
zipTrees (Node a ts) (Node b ts') = Node (a,b) (zipForest ts ts')
  where
    zipForest :: Forest a -> Forest b -> Forest (a,b)
    zipForest (a : as) (b : bs) = zipTrees a b : zipForest as bs
    zipForest _ _ = []

instance Renderable a => Producible (Thread a) where
  produce live RawThread {..} = do
    cs <- traverse (produce live) comments
    pure Thread 
      { comments = cs
      , ..
      }

instance Previewable (Thread a) where
  preview live RawThread { comments = rawComments } Thread {..} = do
    cs <- traverse (uncurry (preview live)) (zipTrees rawComments comments)
    pure ThreadPreview 
      { comments = cs
      , ..
      }

instance Nameable (Thread a) where
  toName RawThread {..} = ThreadName key

instance (Typeable a, Pathable (Context a), Pathable (Name a)) => Routable (Thread a)

instance Fieldable a => Fieldable (Tree a) where
  field _ _ = Null -- TODO

instance (Fieldable (Resource (Comment a)), Typeable (Content a), Typeable a) => Formable (Resource (Thread a))

instance 
  ( Typeable a
  , Theme (Thread a)
  , ToJSON (Context a), FromJSON (Context a), Eq (Context a)
  , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
  , ToJSON (Rendered a), FromJSON (Rendered a)
  , Component (Product (Thread a))
  ) => Readable (Thread a)

instance 
  ( Typeable a
  , Theme (Thread a)
  , Pathable (Context a), ToJSON (Context a), FromJSON (Context a), Eq (Context a)
  , Pathable (Name a), ToJSON (Name a), FromJSON (Name a), Eq (Name a)
  , FromJSON (Rendered a)
  , Component (Preview (Thread a))
  ) => Listable (Thread a)

instance Amendable (Thread a) where
  data Amend (Thread a) 
    = AddComment    (Comment a)
    | UpdateComment (Comment a)
    
  amend (AddComment c)    RawThread {..} = RawThread { comments = addComment c comments, .. }
  amend (UpdateComment c) RawThread {..} = RawThread { comments = updateComment c comments, .. }

addComment :: Resource (Comment a) -> Tree (Resource (Comment a)) -> Tree (Resource (Comment a))
addComment new@RawComment { parent } tree | Just p <- parent = go p tree
  where
    go p (Node c cs) 
      | p == key c = Node c (cs ++ [new])
      | otherwise  = Node c (fmap (go p) cs)
addComment _ tree = tree

updateComment :: Resource (Comment a) -> Tree (Resource (Comment a)) -> Tree (Resource (Comment a))
updateComment res@RawComment { key = self } = go 
  where
    go (Node c cs)
      | self == key c = Node res cs
      | otherwise     = Node c (fmap go cs)

data Config = Config
  { verifying   :: Bool
  , editing     :: Bool
  , reverifying :: Bool
  , upvoting    :: Bool
  , downvoting  :: Bool
  }

instance Ownable (Comment a) where
  isOwner un ctx nm =
    tryReadResource ctx nm >>= \case
      Just RawComment {..} | un == author -> pure True
      _ -> pure False

commentPermissions :: Username -> Permissions (Comment a)
commentPermissions un = defaultPermissions (Just un)

commentAdminPermissions :: Permissions (Comment a)
commentAdminPermissions = fullPermissions

commentCallbacks :: Config -> Callbacks (Comment a)
commentCallbacks Config {..} = Callbacks {..}
  where
    onCreate (CommentContext ctx nm _) (CommentName key) res pro pre 
      | Just p <- parent res = do
        -- amend existing thread
        thread <- newKey
        tryAmend threadCallbacks (ThreadContext ctx nm) (ThreadName thread) (AddComment res)

      | otherwise = do
        _
        
    onUpdate ctx nm res pro pre
      | _ = do
        _

    onDelete ctx nm res pro pre 
      | _ = do
        _

    onRead _ _ _ = def
    onList _ _ = def

  