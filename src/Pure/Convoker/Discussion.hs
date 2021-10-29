module Pure.Convoker.Discussion where

import Pure.Convoker.Comment
import Pure.Convoker.DiscussionMeta

import Pure.Auth hiding (Key)
import Pure.Data.JSON (ToJSON(..),FromJSON(..))
import Pure.Elm.Component hiding (render,Node)
import Pure.Conjurer
import Pure.Data.Render
import Pure.Maybe
import Pure.Sync
import Pure.WebSocket
import Pure.WebSocket.Cache

import Data.Hashable
import Data.Tree
import Data.Map.Strict

import Data.Typeable
import GHC.Generics

data Discussion a

data instance Resource (Discussion a) = RawDiscussion
  { context  :: Context a
  , name     :: Name a
  , comments :: Forest (Product (Comment a),Preview (Comment a))
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Product (Comment a)),ToJSON (Preview (Comment a))) => ToJSON (Resource (Discussion a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Product (Comment a)),FromJSON (Preview (Comment a))) => FromJSON (Resource (Discussion a))

-- Use meta in Resource (Discussion a) to order the threads here and 
-- apply some vote fuzzing.
data instance Product (Discussion a) = Discussion
  { comments :: Forest (Product (Comment a))
  } deriving stock Generic
deriving instance (ToJSON (Product (Comment a))) => ToJSON (Product (Discussion a))
deriving instance (FromJSON (Product (Comment a))) => FromJSON (Product (Discussion a))

-- Pare down the comments to the most impactful - maybe the highest-ranked thread
-- and up to some limit of comments as a threshold based on the ranking of the
-- thread?
data instance Preview (Discussion a) = DiscussionPreview
  { comments :: Forest (Preview (Comment a))
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Preview a), ToJSON (Product (Comment a))) => ToJSON (Preview (Discussion a))
deriving instance (FromJSON (Preview a), FromJSON (Context a), FromJSON (Name a), FromJSON (Product (Comment a))) => FromJSON (Preview (Discussion a))

data instance Context (Discussion a) = DiscussionContext (Context a)
  deriving stock Generic
deriving instance (Eq (Context a)) => Eq (Context (Discussion a))
deriving instance (Ord (Context a)) => Ord (Context (Discussion a))
deriving instance (ToJSON (Context a)) => ToJSON (Context (Discussion a))
deriving instance (FromJSON (Context a)) => FromJSON (Context (Discussion a))
deriving instance (Typeable a, Pathable (Context a)) => Pathable (Context (Discussion a))
deriving instance (Hashable (Context a)) => Hashable (Context (Discussion a))

data instance Name (Discussion a) = DiscussionName (Name a) 
  deriving stock Generic
deriving instance (Eq (Name a)) => Eq (Name (Discussion a))
deriving instance (Ord (Name a)) => Ord (Name (Discussion a))
deriving instance (ToJSON (Name a)) => ToJSON (Name (Discussion a))
deriving instance (FromJSON (Name a)) => FromJSON (Name (Discussion a))
deriving instance (Typeable a, Pathable (Name a)) => Pathable (Name (Discussion a))
deriving instance (Hashable (Name a)) => Hashable (Name (Discussion a))

instance Nameable (Discussion a) where
  toName RawDiscussion {..} = DiscussionName name

instance {-# OVERLAPPABLE #-} Processable (Discussion a)

instance {-# OVERLAPPABLE #-} Producible (Discussion a) where
  produce _ RawDiscussion {..} = 
    pure Discussion 
      { comments = fmap (fmap fst) comments
      , ..
      }
    
instance {-# OVERLAPPABLE #-} Previewable (Discussion a) where
  preview _ RawDiscussion {..} _ = 
    pure DiscussionPreview 
      { comments = fmap (fmap snd) comments
      , ..
      }

instance {-# OVERLAPPABLE #-} Amendable (Discussion a) where
  data Amend (Discussion a) 
    = AddComment (Product (Comment a)) (Preview (Comment a))
    | UpdateComment (Product (Comment a)) (Preview (Comment a))

  amend (AddComment pro@Comment { parent } pre) RawDiscussion {..} =
    Just RawDiscussion
      { comments = insertComment parent (pro,pre) comments
      , ..
      }
  
  amend (UpdateComment pro@Comment { key } pre) RawDiscussion {..} =
    Just RawDiscussion
      { comments = updateComment key (pro,pre) comments
      , ..
      }

-- switch to short-circuiting foldr at some point; it shouldn't be important
-- since the linear traversal has to be performed anyways for serialization
insertComment 
  :: Maybe (Key (Comment a)) 
  -> (Product (Comment a),Preview (Comment a)) 
  -> Forest (Product (Comment a),Preview (Comment a)) 
  -> Forest (Product (Comment a),Preview (Comment a)) 
insertComment Nothing pp forest = Node pp [] : forest
insertComment (Just k) pp ts = go ts
  where
    go [] = []
    go ( Node (pro@Comment { key },pre) t : ts) 
      | k == key  = Node (pro,pre) (t ++ [Node pp []]) : ts
      | otherwise = Node (pro,pre) (go t) : go ts

-- switch to short-circuiting foldr at some point; it shouldn't be important
-- since the linear traversal has to be performed anyways for serialization
updateComment 
  :: Key (Comment a)
  -> (Product (Comment a),Preview (Comment a)) 
  -> Forest (Product (Comment a),Preview (Comment a)) 
  -> Forest (Product (Comment a),Preview (Comment a))
updateComment k pp forest = fmap go forest
  where
    go (Node (pro@Comment { key },pre) ts) 
      | key == k  = Node pp ts
      | otherwise = Node (pro,pre) (fmap go ts)

instance {-# OVERLAPPABLE #-}
  ( Typeable a
  , ToJSON (Context a), FromJSON (Context a), Eq (Context a)
  , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
  , FromJSON (Product a)
  , Component (Product (Comment a))
  , Theme (Discussion a)
  ) => Readable (Discussion a) 
  where
    toRead ws ctx nm = producingKeyed (ctx,nm) producer (\_ -> consuming consumer)
      where
        producer = sync .
          request (readingAPI @(Discussion a)) ws 
            (readProduct @(Discussion a))

        consumer = maybe "Not Found" defaultDiscussionView 

catchingToRead 
  :: forall a. 
    ( Typeable a
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , FromJSON (Product a)
    , Component (Product (Comment a))
    , Theme (Discussion a)
    ) => WebSocket -> Context (Discussion a) -> Name (Discussion a) -> View
catchingToRead _ ctx nm = 
  producingKeyed (ctx,nm) producer (\_ -> consuming consumer)
  where
    producer =
      req Cached (readingAPI @(Discussion a))
        (readProduct @(Discussion a)) 

    consumer = 
      maybe "Not Found" defaultDiscussionView

defaultDiscussionView 
  :: forall a. 
    ( Component (Product (Comment a))
    , Theme (Discussion a)
    ) => Product (Discussion a) -> View
defaultDiscussionView Discussion {..} =
  Div <| Themed @(Discussion a) . Themed @Reading |>
    (forest comments)
  where
    forest :: Forest (Product (Comment a)) -> [View]
    forest trees = [ tree t | t <- comments ]

    tree :: Tree (Product (Comment a)) -> View
    tree (Node a trees) =
      Div <||>
        ( run a
        : forest trees 
        )