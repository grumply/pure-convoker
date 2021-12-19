module Pure.Convoker.Discussion where

import Pure.Convoker.Admins
import Pure.Convoker.Comment
import Pure.Convoker.Meta
import Pure.Convoker.Mods
import Pure.Convoker.UserVotes

import Pure.Auth (Username,Token(..),Access(..),withToken,authorize,defaultOnRegistered)
import Pure.Conjurer
import Pure.Elm.Application (storeScrollPosition,restoreScrollPosition)
import Pure.Elm.Component as Pure hiding (not,key,pattern Meta)
import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt
import Pure.Data.Render
import Pure.Hooks
import Pure.Maybe
import Pure.Router as R
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
  produce _ _ _ RawDiscussion {..} _ =
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

data DiscussionBuilder _role a = DiscussionBuilder
  { socket :: WebSocket
  , context :: Context a
  , name :: Name a
  , user :: Maybe Username
  , root :: Maybe (Key (Comment a))
  , admin :: Bool
  , mod :: Bool
  , onRefresh :: IO ()
  , withAuthor :: Username -> View
  , withContent :: [View] -> [View]
  , admins :: Product Admins
  , mods :: Product (Mods a)
  , votes :: Maybe (Product (UserVotes a))
  , meta :: Product (Meta a)
  , full :: Product (Discussion a)
  }

-- Core discussion view generator. Given an active connection, a resource's
-- context and name, and a method of rendering a `DiscussionBuilder`, this
-- will build the `DiscussionBuilder` context and render it if the necessary
-- resources can be retrieved.
discussion 
  :: forall _role a. 
    ( Typeable a
    , Typeable (_role :: *)
    , ToJSON (Context a), FromJSON (Context a), Eq (Context a)
    , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
    , FromJSON (Product (Meta a))
    ) => WebSocket -> Context a -> Name a -> Maybe (Key (Comment a)) -> (Username -> View) -> ([View] -> [View]) -> (DiscussionBuilder _role a -> View) -> View
discussion socket ctx nm root withAuthor withContent viewer = 

  withToken @_role $ \mt ->
    let user = fmap (\(Token (un,_)) -> un) mt in 

    -- simple switch used to trigger re-request and re-render
    -- See `tagged` and `refresh` below for use.
    useState False $ \State {..} ->

      let 
        onRefresh = modify Prelude.not

        tagged
          | state     = Tagged @True
          | otherwise = Tagged @False

      in 

        -- When `tagged` changes, when `refresh` is called, everything 
        -- wrapped by tagged is fully re-rendered.
        tagged do

          producingKeyed (ctx,nm,user) producer $ \(context,name,user) -> 

            let 
              consumer :: Maybe (Product Admins,Product (Mods a),Maybe (Product (UserVotes a)),Product (Meta a),Product (Discussion a)) -> View
              consumer Nothing = Null
              consumer (Just (admins,mods,votes,meta,full)) =
                let
                  admin = 
                    case user of
                      Just un | Admins as <- admins -> un `elem` as
                      _ -> False

                  mod =
                    case user of
                      Just un | Mods ms <- mods -> un `elem` ms
                      _ -> False

                in
                  viewer (DiscussionBuilder {..} :: DiscussionBuilder _role a)

            in
              consuming consumer

  where
    producer :: (Context a,Name a,Maybe Username) -> IO (Maybe (Product Admins,Product (Mods a),Maybe (Product (UserVotes a)),Product (Meta a),Product (Discussion a)))
    producer (ctx,nm,mun) = do

      let 
        getProduct 
          :: forall a. 
            ( Typeable a
            , ToJSON (Context a), FromJSON (Product a)
            , ToJSON (Name a), FromJSON (Context a)
            ) => Context a -> Name a -> IO (IO (Maybe (Product a)))
        getProduct ctx nm = async (request (readingAPI @a) socket (readProduct @a) (ctx,nm))

      getAdmins     <- getProduct AdminsContext AdminsName
      getMods       <- getProduct (ModsContext ctx) ModsName 
      getVotes      <- maybe (pure (pure Nothing)) (\un -> getProduct (UserVotesContext ctx nm) (UserVotesName un)) mun 
      getMeta       <- getProduct (MetaContext ctx nm) MetaName
      getDiscussion <- getProduct (DiscussionContext ctx nm) DiscussionName

      (admins,mods,votes0,meta,full) <- 
        (,,,,) 
          <$> getAdmins 
          <*> getMods 
          <*> getVotes 
          <*> getMeta 
          <*> getDiscussion
          
      let 
        -- if no votes are found, seed the UserVotes with either Nothing if the
        -- user is not logged in, or an empty userVotes if they are
        votes = maybe (maybe Nothing (Just . emptyUserVotes) mun) Just votes0

      pure $ 
        (,,,,) 
          <$> admins 
          <*> mods 
          <*> pure votes
          <*> meta 
          <*> full

    -- what an annoying type signature
    async :: forall x. ((x -> IO ()) -> IO ()) -> IO (IO x)
    async f = do
      mv <- newEmptyMVar
      f (putMVar mv)
      pure (takeMVar mv)

type CommentSorter a b = Ord b => Product (Meta a) -> Product (Comment a) -> b

data CommentBuilder _role a = CommentBuilder
  { socket :: WebSocket
  , context :: Context a
  , name :: Name a
  , user :: Maybe Username
  , admin :: Bool
  , mod :: Bool
  , onRefresh :: IO ()
  , children :: [View]
  , withAuthor :: Username -> View
  , withContent :: [View] -> [View]
  , root :: Maybe (Key (Comment a))
  , parent :: Maybe (Key (Comment a)) 
  , previous :: Maybe (Key (Comment a))
  , next :: Maybe (Key (Comment a))
  , size :: Int
  , admins :: Product Admins
  , mods :: Product (Mods a)
  , votes :: Maybe (Product (UserVotes a))
  , meta :: Product (Meta a)
  , comment :: Product (Comment a)
  }

data CommentFormBuilder _role a = CommentFormBuilder
  { socket :: WebSocket
  , context :: Context a
  , name :: Name a
  , user :: Maybe Username
  , onRefresh :: IO ()
  , onCancel :: IO ()
  , withContent :: [View] -> [View]
  , parent :: Maybe (Key (Comment a))
  , viewer :: CommentBuilder _role a -> View
  , comment :: Maybe (Resource (Comment a))
  }

type DiscussionLayout (_role :: *) a b =
    ( Typeable a
    , Typeable _role
    , Theme (Comment a)
    , Pathable (Context a), ToJSON (Context a), FromJSON (Context a)
    , Pathable (Name a), ToJSON (Name a), FromJSON (Name a)
    , ToJSON (Resource (Comment a)), FromJSON (Resource (Comment a))
    , Formable (Resource (Comment a))
    , Default (Resource (Comment a))
    , Ord b
    ) => CommentSorter a b -> (CommentFormBuilder _role a -> View) -> (CommentBuilder _role a -> View) -> (DiscussionBuilder _role a -> View)

linear :: DiscussionLayout _role a b
linear sorter runCommentFormBuilder runCommentBuilder DiscussionBuilder {..} | Discussion {..} <- full =
  Div <||>
    (( useState False $ \State {..} ->
        if state then
          runCommentFormBuilder CommentFormBuilder 
            { parent = Nothing
            , viewer = runCommentBuilder
            , onCancel = modify (const False)
            , comment = Nothing
            , ..
            } 
        else
          Button <| OnClick (\_ -> modify (const True)) |> [ "Add Comment" ]
     )
           
    : fmap comment (List.sortOn (sorter meta) comments)
    )
  where
    comment c = 
      runCommentBuilder CommentBuilder 
        { root = Nothing
        , parent = Nothing
        , previous = Nothing
        , next = Nothing
        , children = []
        , size = 0
        , comment = c
        , ..
        }

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
  
  , onAmend = \(CommentContext ctx nm) cnm res pro pre lst amnd -> void do
    onAmend cbs (CommentContext ctx nm) cnm res pro pre lst amnd
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
  tryReadResource fullPermissions def (ModsContext ctx) ModsName >>= \case
    Just RawMods {} -> pure ()
    _ -> void (tryCreate @(Mods a) fullPermissions def (ModsContext ctx) (RawMods mods))
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

