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

data Discussion (domain :: *) (a :: *)

instance {-# OVERLAPPABLE #-} (Typeable domain, Typeable a) => Theme (Discussion domain a)

data instance Resource (Discussion domain a) = RawDiscussion
  { context  :: Context a
  , name     :: Name a
  , comments :: [(Product (Comment domain a),Preview (Comment domain a))]
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Product (Comment domain a)),ToJSON (Preview (Comment domain a))) => ToJSON (Resource (Discussion domain a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Product (Comment domain a)),FromJSON (Preview (Comment domain a))) => FromJSON (Resource (Discussion domain a))

data instance Product (Discussion domain a) = Discussion
  { context  :: Context a
  , name     :: Name a
  , comments :: [Product (Comment domain a)]
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Product (Comment domain a))) => ToJSON (Product (Discussion domain a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Product (Comment domain a))) => FromJSON (Product (Discussion domain a))

data instance Preview (Discussion domain a) = DiscussionPreview
  { context  :: Context a
  , name     :: Name a
  , comments :: [Preview (Comment domain a)]
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Preview (Comment domain a))) => ToJSON (Preview (Discussion domain a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Preview (Comment domain a))) => FromJSON (Preview (Discussion domain a))

data instance Context (Discussion domain a) = DiscussionContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (Discussion domain a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (Discussion domain a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (Discussion domain a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (Discussion domain a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (Discussion domain a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (Discussion domain a))

data instance Name (Discussion domain a) = DiscussionName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Nameable (Discussion domain a) where
  toName _ = DiscussionName

instance Amendable (Discussion domain a) where
  data Amend (Discussion domain a) 
    = SetComment (Product (Comment domain a)) (Preview (Comment domain a))
    deriving stock Generic
    
  amend (SetComment pro@Comment { key = target } pre) RawDiscussion {..}
    | let match (Comment { key },_) = key == target
    , List.any match comments
    = Just RawDiscussion { comments = fmap (\x -> if match x then (pro,pre) else x) comments, .. }

    | otherwise 
    = Just RawDiscussion { comments = (pro,pre) : comments, .. }

deriving instance (ToJSON (Product (Comment domain a)), ToJSON (Preview (Comment domain a))) => ToJSON (Amend (Discussion domain a))
deriving instance (FromJSON (Product (Comment domain a)), FromJSON (Preview (Comment domain a))) => FromJSON (Amend (Discussion domain a))

instance Processable (Discussion domain a)

instance Producible (Discussion domain a) where
  produce _ _ RawDiscussion {..} _ =
    pure Discussion { comments = fmap fst comments, .. }

instance Previewable (Discussion domain a) where
  preview _ _ RawDiscussion {..} _ =
    pure DiscussionPreview { comments = fmap snd comments, .. }

data instance Action (Discussion domain a) = NoDiscussionAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Reaction (Discussion domain a) = NoDiscussionReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Typeable domain => Ownable (Discussion domain a) where
  isOwner un _ _ = isAdmin @domain un

data DiscussionBuilder domain a = DiscussionBuilder
  { socket :: WebSocket
  , context :: Context a
  , name :: Name a
  , user :: Maybe Username
  , root :: Maybe (Key (Comment domain a))
  , admin :: Bool
  , mod :: Bool
  , onRefresh :: IO ()
  , onVote :: Amend (UserVotes domain a) -> IO ()
  , withAuthor :: Username -> View
  , withContent :: [View] -> [View]
  , admins :: Product (Admins domain)
  , mods :: Product (Mods domain a)
  , votes :: Maybe (Product (UserVotes domain a))
  , meta :: Product (Meta domain a)
  , full :: Product (Discussion domain a)
  }

-- Core discussion view generator. Given an active connection, a resource's
-- context and name, and a method of rendering a `DiscussionBuilder`, this
-- will build the `DiscussionBuilder` context and render it if the necessary
-- resources can be retrieved.
discussion 
  :: forall domain a. 
    ( Typeable a
    , Typeable (domain :: *)
    , ToJSON (Context a), FromJSON (Context a), Eq (Context a)
    , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
    , FromJSON (Product (Meta domain a))
    ) => WebSocket -> Context a -> Name a -> Maybe (Key (Comment domain a)) -> (Username -> View) -> ([View] -> [View]) -> (DiscussionBuilder domain a -> View) -> View
discussion socket ctx nm root withAuthor withContent viewer = 

  withToken @domain $ \mt ->
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
              consumer :: Maybe (Product (Admins domain),Product (Mods domain a),Maybe (Product (UserVotes domain a)),Product (Meta domain a),Product (Discussion domain a)) -> View
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
                  useState votes $ \State {..} ->
                    let
                      onVote (Upvote k) =
                        modify $ \case
                          Just UserVotes {..}
                            | k `elem` downvotes -> Just UserVotes { downvotes = List.filter (/= k) downvotes, .. }
                            | k `notElem` upvotes -> Just UserVotes { upvotes = k : upvotes, .. }
                          x -> x
                      onVote (Downvote k) =
                        modify $ \case
                          Just UserVotes {..}
                            | k `elem` upvotes -> Just UserVotes { upvotes = List.filter (/= k) upvotes, .. }
                            | k `notElem` downvotes -> Just UserVotes { downvotes = k : downvotes, .. }
                          x -> x
                    in
                      viewer (DiscussionBuilder {..} :: DiscussionBuilder domain a)

            in
              consuming consumer

  where
    producer :: (Context a,Name a,Maybe Username) -> IO (Maybe (Product (Admins domain),Product (Mods domain a),Maybe (Product (UserVotes domain a)),Product (Meta domain a),Product (Discussion domain a)))
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

data CommentBuilder domain a = CommentBuilder
  { socket :: WebSocket
  , context :: Context a
  , name :: Name a
  , user :: Maybe Username
  , admin :: Bool
  , mod :: Bool
  , onRefresh :: IO ()
  , onVote :: Amend (UserVotes domain a) -> IO ()
  , children :: [(Int,View)]
  , withAuthor :: Username -> View
  , withContent :: [View] -> [View]
  , root :: Maybe (Key (Comment domain a))
  , parent :: Maybe (Key (Comment domain a)) 
  , previous :: Maybe (Key (Comment domain a))
  , next :: Maybe (Key (Comment domain a))
  , size :: Int
  , admins :: Product (Admins domain)
  , mods :: Product (Mods domain a)
  , votes :: Maybe (Product (UserVotes domain a))
  , meta :: Product (Meta domain a)
  , comment :: Product (Comment domain a)
  }

data CommentFormBuilder domain a = CommentFormBuilder
  { socket :: WebSocket
  , context :: Context a
  , name :: Name a
  , user :: Maybe Username
  , onRefresh :: IO ()
  , onCancel :: IO ()
  , withContent :: [View] -> [View]
  , parent :: Maybe (Key (Comment domain a))
  , viewer :: CommentBuilder domain a -> View
  , comment :: Maybe (Resource (Comment domain a))
  }

type DiscussionLayout (domain :: *) a b =
    ( Typeable a
    , Typeable domain
    , Theme (Discussion domain a)
    , Theme (Comment domain a)
    , Pathable (Context a), ToJSON (Context a), FromJSON (Context a)
    , Pathable (Name a), ToJSON (Name a), FromJSON (Name a)
    , ToJSON (Resource (Comment domain a)), FromJSON (Resource (Comment domain a))
    , Formable (Resource (Comment domain a))
    , Default (Resource (Comment domain a))
    , Ord b
    ) => CommentSorter domain a b -> (CommentFormBuilder domain a -> View) -> (CommentBuilder domain a -> View) -> (DiscussionBuilder domain a -> View)

linear :: forall domain a b. DiscussionLayout domain a b
linear sorter runCommentFormBuilder runCommentBuilder DiscussionBuilder {..} | Discussion {..} <- full =
  Div <| Themed @(Discussion domain a) |>
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
  :: forall domain a. 
    ( Typeable domain
    , Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    ) => Permissions (Discussion domain a) -> Callbacks (Discussion domain a) -> Callbacks (Comment domain a) -> Callbacks (Comment domain a)
extendCommentCallbacks discussionPermissions discussionCallbacks cbs = cbs
  { onCreate = \(CommentContext ctx nm) cnm res pro pre lst -> void do
    onCreate cbs (CommentContext ctx nm) cnm res pro pre lst
    tryAmend @(Discussion domain a) discussionPermissions discussionCallbacks 
      (DiscussionContext ctx nm) DiscussionName 
        (SetComment pro pre)
  
  , onUpdate = \(CommentContext ctx nm) cnm res pro pre lst -> void do
    onUpdate cbs (CommentContext ctx nm) cnm res pro pre lst
    tryAmend @(Discussion domain a) discussionPermissions discussionCallbacks 
      (DiscussionContext ctx nm) DiscussionName 
        (SetComment pro pre)
  
  , onAmend = \(CommentContext ctx nm) cnm res pro pre lst amnd -> void do
    onAmend cbs (CommentContext ctx nm) cnm res pro pre lst amnd
    tryAmend @(Discussion domain a) discussionPermissions discussionCallbacks 
      (DiscussionContext ctx nm) DiscussionName 
        (SetComment pro pre)
  
  }

createDiscussion 
  :: forall domain a. 
    ( Typeable domain
    , Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    , Default (Resource (Meta domain a))
    , Amendable (Meta domain a)
    , Processable (Meta domain a)
    , Previewable (Meta domain a)
    , Producible (Meta domain a)
    , FromJSON (Resource (Meta domain a)), ToJSON (Resource (Meta domain a))
    , FromJSON (Product (Meta domain a)), ToJSON (Product (Meta domain a))
    , FromJSON (Preview (Meta domain a)), ToJSON (Preview (Meta domain a))
    , FromJSON (Amend (Meta domain a)), ToJSON (Amend (Meta domain a))
    ) => Context a -> Name a -> [Username] -> IO ()
createDiscussion ctx nm mods = void do
  tryCreate @(Discussion domain a) fullPermissions def (DiscussionContext ctx nm) (RawDiscussion ctx nm []) 
  tryReadResource (fullPermissions @(Mods domain a)) def (ModsContext ctx) ModsName >>= \case
    Just RawMods {} -> pure ()
    _ -> void (tryCreate @(Mods domain a) fullPermissions def (ModsContext ctx) (RawMods mods))
  tryCreate @(Meta domain a) fullPermissions def (MetaContext ctx nm) (def :: Resource (Meta domain a))

addDiscussionCreationCallbacks 
  :: forall domain a. 
    ( Typeable domain
    , Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    , Default (Resource (Meta domain a))
    , Amendable (Meta domain a)
    , Processable (Meta domain a)
    , Previewable (Meta domain a)
    , Producible (Meta domain a)
    , FromJSON (Resource (Meta domain a)), ToJSON (Resource (Meta domain a))
    , FromJSON (Product (Meta domain a)), ToJSON (Product (Meta domain a))
    , FromJSON (Preview (Meta domain a)), ToJSON (Preview (Meta domain a))
    , FromJSON (Amend (Meta domain a)), ToJSON (Amend (Meta domain a))
    ) => [Username] -> Callbacks a -> Callbacks a
addDiscussionCreationCallbacks mods cbs = cbs { onCreate = onCreate' }
  where
    onCreate' ctx nm res pro pre lst = do
      createDiscussion @domain ctx nm mods
      onCreate cbs ctx nm res pro pre lst

