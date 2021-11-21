module Pure.Convoker.Discussion where

import Pure.Convoker.Admins
import Pure.Convoker.Comment
import Pure.Convoker.Meta
import Pure.Convoker.Mods
import Pure.Convoker.UserVotes

import Pure.Auth (Username,Token(..),withToken)
import Pure.Conjurer
import Pure.Elm.Component as Pure hiding (not,key,pattern Meta)
import Pure.Data.JSON
import Pure.Data.Txt
import Pure.Data.Render
import Pure.Hooks
import Pure.Maybe
import Pure.Sync
import Pure.WebSocket

import Data.Hashable

import Control.Concurrent
import Control.Monad
import Data.List as List
import Data.Maybe
import qualified Data.Graph as G
import Data.Typeable
import GHC.Generics hiding (Meta)

data Discussion (a :: *)

data instance Resource (Discussion a) = RawDiscussion
  { context  :: Context a
  , name     :: Name a
  , comments :: [(Product (Comment a),Preview (Comment a))]
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Product (Comment a)),ToJSON (Preview (Comment a))) => ToJSON (Resource (Discussion a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Product (Comment a)),FromJSON (Preview (Comment a))) => FromJSON (Resource (Discussion a))

data instance Product (Discussion a) = Discussion
  { context  :: Context a
  , name     :: Name a
  , comments :: [Product (Comment a)]
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Product (Comment a))) => ToJSON (Product (Discussion a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Product (Comment a))) => FromJSON (Product (Discussion a))

data instance Preview (Discussion a) = DiscussionPreview
  { context  :: Context a
  , name     :: Name a
  , comments :: [Preview (Comment a)]
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Preview (Comment a))) => ToJSON (Preview (Discussion a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Preview (Comment a))) => FromJSON (Preview (Discussion a))

data instance Context (Discussion a) = DiscussionContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (Discussion a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (Discussion a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (Discussion a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (Discussion a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (Discussion a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (Discussion a))

data instance Name (Discussion a) = DiscussionName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Nameable (Discussion a) where
  toName _ = DiscussionName

instance Amendable (Discussion a) where
  data Amend (Discussion a) 
    = SetComment (Product (Comment a)) (Preview (Comment a))
    deriving stock Generic
    
  amend (SetComment pro@Comment { key = target } pre) RawDiscussion {..}
    | let match (Comment { key },_) = key == target
    , List.any match comments
    = Just RawDiscussion { comments = fmap (\x -> if match x then (pro,pre) else x) comments, .. }

    | otherwise 
    = Just RawDiscussion { comments = (pro,pre) : comments, .. }

deriving instance (ToJSON (Product (Comment a)), ToJSON (Preview (Comment a))) => ToJSON (Amend (Discussion a))
deriving instance (FromJSON (Product (Comment a)), FromJSON (Preview (Comment a))) => FromJSON (Amend (Discussion a))

instance Processable (Discussion a)

instance Producible (Discussion a) where
  produce _ _ _ RawDiscussion {..} =
    pure Discussion { comments = fmap fst comments, .. }

instance Previewable (Discussion a) where
  preview _ _ _ RawDiscussion {..} _ =
    pure DiscussionPreview { comments = fmap snd comments, .. }

data instance Action (Discussion a) = NoDiscussionAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Reaction (Discussion a) = NoDiscussionReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Ownable (Discussion a) where
  isOwner un _ _ = isAdmin un

type DiscussionViewer a = WebSocket -> Context a -> Name a -> Maybe Username -> Refresher -> Maybe (Product Admins,Product (Mods a),Product (UserVotes a),Product (Meta a),Product (Discussion a)) -> View
type Refresher = IO ()
type CommentSorter a b = Ord b => Product (Meta a) -> Product (Comment a) -> b
type CommentViewer a = WebSocket -> Context a -> Name a -> Maybe Username -> Refresher -> Maybe (Key (Comment a)) -> Product Admins -> Product (Mods a) -> Product (UserVotes a) -> Product (Meta a) -> Product (Comment a) -> View

discussion 
  :: forall _role a. 
    ( Typeable a
    , Typeable (_role :: *)
    , ToJSON (Context a), FromJSON (Context a), Eq (Context a)
    , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
    , FromJSON (Product (Meta a))
    ) => WebSocket -> Context a -> Name a -> DiscussionViewer a -> View
discussion ws ctx nm viewer = 
  withToken @_role $ \mt ->
    useState False $ \State {..} ->
      let 
        refresh = modify Prelude.not
        mun = fmap (\(Token (un,_)) -> un) mt
        tagged
          | state     = Tagged @True
          | otherwise = Tagged @False
      in 
        tagged do
          producingKeyed (ctx,nm) (producer mun) $ \(ctx,nm) -> 
            consuming (viewer ws ctx nm mun refresh)
  where
    -- what an annoying type signature
    async :: forall x. ((x -> IO ()) -> IO ()) -> IO (IO x)
    async f = do
      mv <- newEmptyMVar
      f (putMVar mv)
      pure (takeMVar mv)

    producer mun (ctx,nm) = do

      getAdmins <- async do
        request (readingAPI @Admins) ws 
          (readProduct @Admins) 
          (AdminsContext,AdminsName)

      getMods <- async do
        request (readingAPI @(Mods a)) ws 
          (readProduct @(Mods a)) 
          (ModsContext ctx,ModsName)
     
      getVotes <- async do
        maybe ($ Nothing)
          (\un -> 
            request (readingAPI @(UserVotes a)) ws
              (readProduct @(UserVotes a))
              (UserVotesContext ctx nm,UserVotesName un)
          ) mun
 
      getMeta <- async do
        request (readingAPI @(Meta a)) ws 
          (readProduct @(Meta a)) 
          (MetaContext ctx nm,MetaName)

      getDiscussion <- async do
        request (readingAPI @(Discussion a)) ws 
          (readProduct @(Discussion a)) 
          (DiscussionContext ctx nm,DiscussionName)

      (admins,mods,votes,meta,discussion) <- (,,,,) <$> getAdmins <*> getMods <*> getVotes <*> getMeta <*> getDiscussion
      pure ((,,,,) <$> admins <*> mods <*> pure (fromMaybe (emptyUserVotes (fromTxt def)) votes) <*> meta <*> discussion)

-- A simple approach using the components of a transposed graph of comments.
-- Takes a comment sorter that can define an arbitrary sort based on Ord using
-- the comment and the discussion's meta information.
threads 
  :: forall (_role :: *) a b. 
    ( Typeable a
    , Typeable _role
    , Theme (Comment a)
    , Pathable (Context a), ToJSON (Context a), FromJSON (Context a)
    , Pathable (Name a), ToJSON (Name a), FromJSON (Name a)
    , ToJSON (Resource (Comment a)), FromJSON (Resource (Comment a))
    , Formable (Resource (Comment a))
    , Default (Resource (Comment a))
    , Component (Preview (Comment a))
    , Component (Product (Comment a))
    , Ord b
    ) => CommentSorter a b -> CommentViewer a -> DiscussionViewer a
threads _ _ _ _ _ _ _ Nothing = "Nothing in threads"
threads sorter viewer ws ctx nm mun refresh (Just (admins,mods,votes,meta,Discussion {..})) = 
  Div <||> 
    [ toCreate @_role ws (CommentContext ctx nm)
    , Div <||> forest Nothing threads
    ]
  where
    edges = fmap (\(comment@Comment { key, parents }) -> (comment,key,parents)) comments
    
    (graph,nodeFromVertex,_) = G.graphFromEdges edges

    -- transpose because each vertex in our graph points to predecessors
    threads = G.components (G.transposeG graph) 

    forest mparent ts = 
      let look (G.Node n _) = let (comment,_,_) = nodeFromVertex n in comment
      in [ tree mparent t | t <- List.sortOn (sorter meta . look) ts ]
    
    tree mparent (G.Node (nodeFromVertex -> (comment@Comment { key },_,_)) sub) =
      Div <||>
        ( viewer ws ctx nm mun refresh mparent admins mods votes meta comment
        : forest (Just key) sub 
        )

extendCommentCallbacks 
  :: forall a. 
    ( Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    ) => Permissions (Discussion a) -> Callbacks (Discussion a) -> Callbacks (Comment a) -> Callbacks (Comment a)
extendCommentCallbacks discussionPermissions discussionCallbacks cbs = cbs
  { onCreate = \(CommentContext ctx nm) cnm res pro pre lst -> void do
    onCreate cbs (CommentContext ctx nm) cnm res pro pre lst
    tryAmend @(Discussion a) discussionPermissions discussionCallbacks 
      (DiscussionContext ctx nm) DiscussionName 
        (SetComment pro pre)
  
  , onUpdate = \(CommentContext ctx nm) cnm res pro pre lst -> void do
    onUpdate cbs (CommentContext ctx nm) cnm res pro pre lst
    tryAmend @(Discussion a) discussionPermissions discussionCallbacks 
      (DiscussionContext ctx nm) DiscussionName 
        (SetComment pro pre)
  }

createDiscussion 
  :: forall a. 
    ( Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    , Default (Resource (Meta a))
    , Amendable (Meta a)
    , Processable (Meta a)
    , Previewable (Meta a)
    , Producible (Meta a)
    , FromJSON (Resource (Meta a)), ToJSON (Resource (Meta a))
    , FromJSON (Product (Meta a)), ToJSON (Product (Meta a))
    , FromJSON (Preview (Meta a)), ToJSON (Preview (Meta a))
    , FromJSON (Amend (Meta a)), ToJSON (Amend (Meta a))
    ) => Context a -> Name a -> [Username] -> IO ()
createDiscussion ctx nm mods = void do
  tryCreate @(Discussion a) fullPermissions def (DiscussionContext ctx nm) (RawDiscussion ctx nm []) 
  tryCreate @(Mods a) fullPermissions def (ModsContext ctx) (RawMods mods)
  tryCreate @(Meta a) fullPermissions def (MetaContext ctx nm) (def :: Resource (Meta a))

addDiscussionCreationCallbacks 
  :: forall a. 
    ( Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    , Default (Resource (Meta a))
    , Amendable (Meta a)
    , Processable (Meta a)
    , Previewable (Meta a)
    , Producible (Meta a)
    , FromJSON (Resource (Meta a)), ToJSON (Resource (Meta a))
    , FromJSON (Product (Meta a)), ToJSON (Product (Meta a))
    , FromJSON (Preview (Meta a)), ToJSON (Preview (Meta a))
    , FromJSON (Amend (Meta a)), ToJSON (Amend (Meta a))
    ) => [Username] -> Callbacks a -> Callbacks a
addDiscussionCreationCallbacks mods cbs = cbs { onCreate = onCreate' }
  where
    onCreate' ctx nm res pro pre lst = do
      createDiscussion ctx nm mods
      onCreate cbs ctx nm res pro pre lst

