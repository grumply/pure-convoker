module Pure.Convoker.Discussion.Simple.Threaded where

import Pure.Convoker.Discussion.Threaded (threaded)
import Pure.Convoker as Export hiding (Upvote,Downvote,authenticatedEndpoints,unauthenticatedEndpoints,endpoints)
import qualified Pure.Convoker as Convoker

import Pure.Convoker.Discussion.Shared.Ago
import Pure.Convoker.Discussion.Shared.Markdown
import Pure.Convoker.Discussion.Shared.Total
import Pure.Convoker.Discussion.Simple.Comment hiding (Delete,Undelete)
import qualified Pure.Convoker.Discussion.Simple.Comment as Comment
import Pure.Convoker.Discussion.Simple.Meta hiding (Upvote,Downvote)

import Pure.Auth (Username,Token(..),Access(..),authorize,defaultOnRegistered)
import Pure.Conjurer
import Pure.Data.Bloom as Bloom
import Pure.Data.JSON hiding (Null)
import Pure.Elm.Component hiding (jump,pattern Meta,pattern Delete)
import Pure.Elm.Application (restoreScrollPosition,storeScrollPosition)
import Pure.Hooks (useEffectWith')
import Pure.Sync
import Pure.WebSocket

import Data.Hashable

import Control.Concurrent
import Data.Coerce
import qualified Data.Graph as G
import Data.List as List
import Data.Maybe
import Data.Typeable
import System.IO.Unsafe

{-
Designed to approximate the Hacker News implementation as a reasonable default.

Uses the approach in Pure.Convoker.Discussion.Threaded that sets up a basic
rendering order based on connected components using Data.Graph. The sorter
is relatively simple: sorts first by vote total and then by key (temporally 
ordered)
-}

simpleThreaded 
  :: forall _role a. 
    ( Typeable a
    , Typeable (_role :: *)
    , Theme (Comment a)
    , ToJSON (Resource (Comment a)), FromJSON (Resource (Comment a))
    , Formable (Resource (Comment a))
    , Default (Resource (Comment a))
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Eq (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Eq (Name a)
    ) => WebSocket -> Context a -> Name a -> (Username -> View) -> ([View] -> [View]) -> View
simpleThreaded ws ctx nm withAuthor withContent = threaded @_role ws ctx nm withAuthor withContent simpleSorter simpleCommentForm (run . SimpleComment)

newtype SimpleSorter a = SimpleSorter (Int,Key (Comment a))
instance Eq (SimpleSorter a) where
  (==) (SimpleSorter ss0) (SimpleSorter ss1) = ss0 == ss1
instance Ord (SimpleSorter a) where
  compare (SimpleSorter (c0,k0)) (SimpleSorter (c1,k1)) =
    case compare c1 c0 of
      EQ -> compare k0 k1
      x  -> x

simpleSorter :: CommentSorter a (SimpleSorter a)
simpleSorter Meta { votes = Votes vs } Comment { key } = SimpleSorter (fromMaybe 0 (List.lookup key vs),key)

simpleCommentForm 
  :: forall (_role :: *) (a :: *). 
    ( Typeable a
    , Typeable _role
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    ) => CommentFormBuilder _role a -> View
simpleCommentForm CommentFormBuilder {..} = authorize @_role (Access socket id defaultOnRegistered) go
  where
    go (Token (un,_)) =
      let 
        onPreview :: Resource (Comment a) -> IO View
        onPreview c = do
          r <- sync (request (publishingAPI @(Comment a)) socket (previewResource @(Comment a)) (CommentContext context name,c))
          case r of
            Nothing -> pure "Invalid comment."
            Just (_,_,_,comment,_) -> pure do
              Div <| Themed @Previewing |>
                [ let 
                    children = []
                    previous = Nothing
                    next = Nothing
                    size = 0
                    admins = Admins []
                    mods = Mods []
                    votes = Nothing
                    meta = Meta (Votes [])
                    admin = False
                    mod = False
                    withAuthor = txt
                  in
                    viewer CommentBuilder {..}
                ]

        onSubmit :: Resource (Comment a) -> IO ()
        onSubmit c = do
          mi <- sync (request (publishingAPI @(Comment a)) socket (createResource @(Comment a)) (CommentContext context name,c))
          for_ mi (\_ -> storeScrollPosition >> onRefresh >> restore)

      in 
        Div <| Themed @Creating |>
          [ form onSubmit onPreview (def :: Resource (Comment a))
          ]
          
    restore =
      forkIO do
        void do
          -- Should be sufficient for most devices?
          -- The failure mode is simply not restoring 
          -- the scroll position, which isn't too bad.
          delay (Millisecond * 100)
          addAnimation restoreScrollPosition

newtype SimpleComment (_role :: *) (a :: *) = SimpleComment (CommentBuilder _role a)

instance (Typeable _role, Typeable a, ToJSON (Context a), FromJSON (Context a), ToJSON (Name a), FromJSON (Name a)) => Component (SimpleComment _role a) where
  data Model (SimpleComment _role a) = SimpleCommentModel
    { total     :: Int
    , vote      :: Maybe Bool
    , replying  :: Bool
    , collapsed :: Bool
    , deleted   :: Bool
    }

  initialize (SimpleComment CommentBuilder { comment = Comment { key, deleted }, ..}) 
    | UserVotes {..} <- maybe (emptyUserVotes (maybe (fromTxt def) id user)) id votes
    , Meta { votes = Votes vs } <- meta 
    = let 
        replying = False
        collapsed = False
        total = fromMaybe 0 (List.lookup key vs)
        vote 
          | unsafePerformIO (Bloom.test upvotes key)   = Just True
          | unsafePerformIO (Bloom.test downvotes key) = Just False
          | otherwise                                  = Nothing
      in
        pure SimpleCommentModel 
          { deleted = coerce deleted
          , ..
          }

  data Msg (SimpleComment _role a) 
    = Upvote
    | Downvote
    | Unvote
    | Delete
    | Undelete
    | Collapse
    | Uncollapse
    | Replying

  upon Replying _ mdl = pure mdl { replying = Prelude.not (replying mdl) }

  upon Collapse _ mdl = pure mdl { collapsed = True }

  upon Uncollapse _ mdl = pure mdl { collapsed = True }

  upon Delete (SimpleComment CommentBuilder { socket, context, name, comment = Comment { key } }) mdl = do
    request 
      (publishingAPI @(Comment a)) 
      socket 
      (amendResource @(Comment a)) 
      (CommentContext context name,CommentName key,Comment.Delete) 
      def 
    pure (mdl :: Model (SimpleComment _role a)) { deleted = True }
    
  upon Undelete (SimpleComment CommentBuilder { socket, context, name, comment = Comment { key } }) mdl = do
    request 
      (publishingAPI @(Comment a)) 
      socket 
      (amendResource @(Comment a)) 
      (CommentContext context name,CommentName key,Comment.Undelete) 
      def 
    pure (mdl :: Model (SimpleComment _role a)) { deleted = False }
   
  upon msg (SimpleComment CommentBuilder { socket, context, name, user = Just username, comment = Comment { key } }) mdl@SimpleCommentModel { total, vote } = do
    let 
      rq v = 
        request 
          (publishingAPI @(UserVotes a)) 
          socket 
          (amendResource @(UserVotes a)) 
          (UserVotesContext context name,UserVotesName username,v key) 
          def 

    case msg of
      Upvote 
        | Just True <- vote -> pure ()
        | otherwise         -> rq Convoker.Upvote 

      Downvote
        | Just False <- vote -> pure ()
        | otherwise          -> rq Convoker.Downvote

      Unvote 
        | Just True  <- vote -> rq Convoker.Downvote
        | Just False <- vote -> rq Convoker.Upvote
        | otherwise          -> pure ()
            
    pure (mdl :: Model (SimpleComment _role a)) 
      { total = case msg of
          Upvote                 -> succ total
          Downvote               -> pred total
          Unvote 
            | Just False <- vote -> succ total
            | Just True  <- vote -> pred total
          _                      -> total
      , vote = case vote of
          Nothing 
            | Upvote   <- msg -> Just True
            | Downvote <- msg -> Just False
          _                   -> Nothing
      }
      
  upon _ _ mdl = pure mdl

  view (SimpleComment CommentBuilder {..}) SimpleCommentModel {..} =
    let 
      UserVotes { upvotes, downvotes } = maybe (emptyUserVotes (maybe (fromTxt def) id user)) id votes 
      Comment { author, created, edited, content, key } = comment
      Created c = created

      button name action = Button <| OnClick (\_ -> action) |> [ name ]
      
    in 
      Article <| Id (toTxt key) |>
        [ Footer <||>
          [ Span <||>
            [ txt (simplified total)

            , " | "

            , withAuthor author

            , " | " 

            , SimpleHTML "time" <| Attribute "pubdate" "" . DateTime (toZonedDateTime c) |> 
              [ txt (ago c) ]

            , case edited of
                Edited (Just _) -> " | edited | "
                _ -> " | "

            , case parent of
                Just k -> button "parent" (jump (toTxt k))
                _ -> Null

            , case previous of
                Just k -> button "prev" (jump (toTxt k))
                _ -> Null

            , case next of
                Just k -> button "next" (jump (toTxt k))
                _ -> Null
            
            , " | "

            -- When optimizing, collapse into a span with a single case of `vote`
            , case vote of
                Just _ -> button "unvote" (command Unvote)
                _ -> Null

            , case vote of
                Nothing -> button "upvote" (command Upvote)
                _  -> Null

            , case vote of
                Nothing -> button "downvote" (command Downvote)
                _  -> Null
            
            , " | "

            -- When optimizing, collapse into a span with a single case of `collapsed`
            , if collapsed then 
                button (fromTxt $ "[" <> toTxt size <> " more]") (command Uncollapse)
              else
                Null

            , if Prelude.not collapsed && size > 0 then
                button "[-]" (command Collapse)
              else
                Null

            -- When optimizing, collapse the admin controls into a span guarded with (admin || mod)
            , if admin || mod then
                " | "
              else
                Null
            
            , if admin || mod && Prelude.not deleted then
                button "delete" (command Delete)
              else
                Null

            , if admin || mod && deleted then
                button "undelete" (command Undelete)
              else
                Null
            ]
          ]

        , Section <||> 
          if deleted then -- to avoid a reload for moderators/admins
            [ "[ removed ]" ]
          else
            withContent content
        
        , Footer <||>
          [ button "reply" (command Replying) 
          ]
          
        , if Prelude.not replying then Null else
          Aside <||> -- Section? I kind of like the use of Aside here.
            [ simpleCommentForm @_role @a CommentFormBuilder
                { onCancel = command Replying
                , viewer = run . SimpleComment
                , .. 
                }
            ]

        , if Prelude.null children then Null else 
          Section <||> children 
        ]

#ifdef __GHCJS__
foreign import javascript unsafe
  "window.scrollTo(0,document.getElementById($1).offsetTop)"
    jump_js :: Txt -> IO ()
jump = jump_js
#else
jump _ = pure ()
#endif
