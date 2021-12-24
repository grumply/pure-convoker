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
import Pure.Data.JSON hiding (Null)
import Pure.Elm.Component hiding (jump,pattern Meta,pattern Delete)
import Pure.Elm.Application (restoreScrollPosition,storeScrollPosition)
import Pure.Hooks (useEffectWith')
import qualified Pure.Shadows as Shadows
import Pure.Sync
import Pure.WebSocket hiding (Reply,none)

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
  :: forall domain a. 
    ( Typeable a
    , Typeable (domain :: *)
    , Theme (Comment domain a)
    , ToJSON (Resource (Comment domain a)), FromJSON (Resource (Comment domain a))
    , Formable (Resource (Comment domain a))
    , Default (Resource (Comment domain a))
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Eq (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Eq (Name a)
    , Theme Edited, Theme Created, Theme Meta, Theme UserVotes, Theme Username, Theme Controls, Theme Reply, Theme Markdown, Theme Children, Theme (Comment domain a)
    , Fieldable Markdown
    ) => WebSocket -> Context a -> Name a -> Maybe (Key (Comment domain a)) -> (Username -> View) -> ([View] -> [View]) -> View
simpleThreaded ws ctx nm root withAuthor withContent = threaded @domain ws ctx nm root withAuthor withContent simpleSorter simpleCommentForm (run . SimpleComment)

newtype SimpleSorter domain a = SimpleSorter (Int,Key (Comment domain a))
instance Eq (SimpleSorter domain a) where
  (==) (SimpleSorter ss0) (SimpleSorter ss1) = ss0 == ss1
instance Ord (SimpleSorter domain a) where
  compare (SimpleSorter (c0,k0)) (SimpleSorter (c1,k1)) =
    case compare c1 c0 of
      EQ -> compare k0 k1
      x  -> x

simpleSorter :: CommentSorter domain a (SimpleSorter domain a)
simpleSorter Meta { votes = Votes vs } Comment { key } = SimpleSorter (fromMaybe 0 (List.lookup key vs),key)

simpleCommentForm 
  :: forall (domain :: *) (a :: *). 
    ( Typeable a
    , Typeable domain
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Fieldable Markdown
    ) => CommentFormBuilder domain a -> View
simpleCommentForm CommentFormBuilder {..} = authorize @domain (Access socket id defaultOnRegistered) go
  where
    go (Token (un,_)) =
      let 
        onPreview :: Resource (Comment domain a) -> IO View
        onPreview c = do
          r <- sync (request (publishingAPI @(Comment domain a)) socket (previewResource @(Comment domain a)) 
                (CommentContext context name,(c :: Resource (Comment domain a)) { author = un, parents = Parents $ maybeToList parent }))
          case r of
            Nothing -> pure "Invalid comment."
            Just (_,_,_,comment,_) -> pure do
              Div <| Themed @Previewing |>
                [ let 
                    root = Nothing
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

        onSubmit :: Resource (Comment domain a) -> IO ()
        onSubmit c@RawComment { content, key }
          | Just _ <- comment = do
            b <- sync do
              request (publishingAPI @(Comment domain a)) socket (amendResource @(Comment domain a)) 
                (CommentContext context name,CommentName key,SetContent content)
            if b then void do 
              storeScrollPosition >> onRefresh >> restore
            else
              pure ()
          | otherwise = do
            mi <- sync do
              request (publishingAPI @(Comment domain a)) socket (createResource @(Comment domain a)) 
                (CommentContext context name,(c :: Resource (Comment domain a)) { author = un, parents = Parents $ maybeToList parent })
            for_ mi (\_ -> storeScrollPosition >> onRefresh >> restore)

      in 
        Div <| Themed @Creating |>
          [ form onSubmit onPreview (fromMaybe def comment)
          ]
          
    restore =
      forkIO do
        void do
          -- Should be sufficient for most devices?
          -- The failure mode is simply not restoring 
          -- the scroll position, which isn't too bad.
          delay (Millisecond * 100)
          addAnimation restoreScrollPosition

newtype SimpleComment (domain :: *) (a :: *) = SimpleComment (CommentBuilder domain a)

instance 
  ( Typeable domain
  , Typeable a
  , ToJSON (Context a), FromJSON (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  , Theme Edited, Theme Created, Theme Meta, Theme UserVotes, Theme Username, Theme Controls, Theme Reply, Theme Markdown, Theme Children, Theme (Comment domain a)
  , Fieldable Markdown
  ) => Component (SimpleComment domain a) where
  data Model (SimpleComment domain a) = SimpleCommentModel
    { total     :: Int
    , vote      :: Maybe Bool
    , editing   :: Maybe (Resource (Comment domain a))
    , replying  :: Bool
    , collapsed :: Bool
    , deleted   :: Bool
    , comment   :: Product (Comment domain a)
    }

  initialize (SimpleComment CommentBuilder { comment = c@Comment { key, deleted }, ..}) 
    | UserVotes {..} <- maybe (emptyUserVotes (maybe (fromTxt def) id user)) id votes
    , Meta { votes = Votes vs } <- meta 
    = let 
        editing = Nothing
        replying = False
        collapsed = False
        total = fromMaybe 0 (List.lookup key vs)
        vote 
          | List.elem key upvotes   = Just True
          | List.elem key downvotes = Just False
          | otherwise               = Nothing
      in
        pure SimpleCommentModel 
          { comment = c
          , deleted = coerce deleted
          , ..
          }

  data Msg (SimpleComment domain a) 
    = Upvote
    | Downvote
    | Unvote
    | Delete
    | Undelete
    | Collapse
    | Uncollapse
    | Replying
    | Editing

  upon Editing (SimpleComment CommentBuilder { socket, context, name, comment = Comment { key } }) mdl = do
    case editing mdl of
      Nothing -> do
        mc <- sync do
          request
            (publishingAPI @(Comment domain a))
            socket
            (readResource @(Comment domain a))
            (CommentContext context name,CommentName key)
        pure mdl { editing = mc }
      Just _ -> 
        pure mdl { editing = Nothing }

  upon Replying _ mdl = pure mdl { replying = Prelude.not (replying mdl) }

  upon Collapse _ mdl = pure mdl { collapsed = True }

  upon Uncollapse _ mdl = pure mdl { collapsed = False }

  upon Delete (SimpleComment CommentBuilder { socket, context, name }) mdl@SimpleCommentModel { comment = Comment { key } } = do
    request 
      (publishingAPI @(Comment domain a)) 
      socket 
      (amendResource @(Comment domain a)) 
      (CommentContext context name,CommentName key,Comment.Delete) 
      def 
    pure (mdl :: Model (SimpleComment domain a)) { deleted = True }
    
  upon Undelete (SimpleComment CommentBuilder { socket, context, name }) mdl@SimpleCommentModel { comment = x@Comment { key } } = do
    sync do
      request 
        (publishingAPI @(Comment domain a)) 
        socket 
        (amendResource @(Comment domain a)) 
        (CommentContext context name,CommentName key,Comment.Undelete) 
    c <- sync do
      request
        (readingAPI @(Comment domain a))
        socket
        (readProduct @(Comment domain a))
        (CommentContext context name,CommentName key)
    pure (mdl :: Model (SimpleComment domain a)) { deleted = False, comment = fromMaybe x c }
   
  upon msg (SimpleComment CommentBuilder { socket, context, name, user = Just username }) mdl@SimpleCommentModel { comment = Comment { key }, total, vote } = do
    let 
      rq v = 
        request 
          (publishingAPI @(UserVotes domain a)) 
          socket 
          (amendResource @(UserVotes domain a)) 
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
            
    pure (mdl :: Model (SimpleComment domain a)) 
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

  view (SimpleComment CommentBuilder {..}) SimpleCommentModel { deleted = del, comment = cmt, ..} =
    let 
      UserVotes { upvotes, downvotes } = maybe (emptyUserVotes (maybe (fromTxt def) id user)) id votes 
      Comment { author, deleted, created, edited, content, key } = cmt
      Created c = created

      button name action = Button <| OnClick (\_ -> action) |> [ name ]
      
    in 
      Article <| Themed @(Comment domain a) . Id (toTxt key) |>
        [ Footer <| Themed @Meta |>
          [ if del then
              Null
            else 
              Section <| Themed @UserVotes |>
                [ case user of
                    Just _ ->
                      case vote of
                        Just False -> button "▼" (command Unvote)
                        Just True  -> button "▽" (command Unvote)
                        Nothing    -> button "▽" (command Downvote)
                    _ -> Null

                , txt (simplified total)
                  
                , case user of
                    Just _ ->
                      case vote of
                        Just True  -> button "▲" (command Unvote)
                        Just False -> button "△" (command Unvote)
                        Nothing    -> button "△" (command Upvote)
                    _ -> Null

                ]

          , Section <| Themed @Username |> [ withAuthor author ]

          , let e | Edited (Just _) <- edited = Themed @Edited | otherwise = id
            in Section <| Themed @Created . e |>
                [ SimpleHTML "time" <| Attribute "pubdate" "" . DateTime (toZonedDateTime c) |> 
                  [ txt (ago c) ]
                ]
          , Section <| Themed @Controls |>
            [ case previous of
                Just k -> button "←" (jump (toTxt k))
                _ -> Null

            , case root of
                Just k | root /= parent -> button "↸" (jump (toTxt k))
                _ -> Null
                
            , case parent of
                Just k -> button "↖︎" (jump (toTxt k))
                _ -> Null

            , case next of
                Just k -> button "→" (jump (toTxt k))
                _ -> Null
              
            , if collapsed then 
                button (fromTxt $ "[" <> toTxt (size + 1) <> " more]") (command Uncollapse)
              else 
                button "[-]" (command Collapse)

            , if Just author == user && isNothing editing then
                button "edit" (command Editing)
              else if Just author == user && isJust editing then
                button "cancel" (command Editing)
              else
                Null

            , if (admin || mod) && Prelude.not del then
                button "delete" (command Delete)
              else if (admin || mod) && del then
                button "undelete" (command Undelete)
              else
                Null

            ]

          ]

        , if collapsed then
            Null 
          else if del || deleted == Deleted True then -- del to avoid a reload for moderators/admins
            Section <| Themed @Markdown |> [ "[ removed ]" ]
          else 
            Section <| Themed @Markdown |> withContent content

        , if collapsed then
            Null
          else if replying then 
            Footer <| Themed @Reply |> [ button "cancel" (command Replying) ]
          else
            Footer <| Themed @Reply |> [ button "reply" (command Replying) ]

        , case replying of
            True | Nothing <- editing, False <- collapsed -> 
              Aside <||> -- Section? I kind of like the use of Aside here.
                [ simpleCommentForm @domain @a CommentFormBuilder
                    { parent = Just key
                    , onCancel = command Replying
                    , viewer = run . SimpleComment
                    , comment = Nothing
                    , .. 
                    }
                ]
            _ -> Null

        , case editing of
            Just c | False <- replying, False <- collapsed -> 
              Aside <||> -- Section? I kind of like the use of Aside here.
                [ simpleCommentForm @domain @a CommentFormBuilder
                    { parent = Just key
                    , onCancel = command Replying
                    , viewer = run . SimpleComment
                    , comment = Just c
                    , .. 
                    }
                ]
            _ -> Null

        , if Prelude.not (Prelude.null children) && Prelude.not collapsed then 
            Section <| Themed @Children |> children 
          else 
            Null 
        ]

data Controls
data Reply
data Children
instance {-# OVERLAPPABLE #-} (Typeable domain, Typeable a) => Theme (Comment domain a) where
  theme c =
    is c do
      margin-top =: 0.5em

      has (subtheme @Creating) do
        box-shadow =: Shadows.shadow Shadows.OffsetBottom 5 
        margin =: 1em
        border-radius =: 1em
        padding =: 1em

      has (subtheme @UserVotes) do
        display =: inline
        has (tag Button) do
          border =: none
          background-color =: inherit
          cursor =: pointer

          hover do
            text-decoration =: underline

      has (subtheme @Created) do
        display =: inline
        margin-left =: 0.5em
        margin-right =: 0.5em
        
        has "time" do
          display =: inline

      has (subtheme @Username) do
        display =: inline
        margin-left =: 0.5em
        margin-right =: 0.5em

      has (subtheme @Controls) do
        display =: inline
        has (tag Button) do
          border =: none
          background-color =: inherit
          cursor =: pointer

          hover do
            text-decoration =: underline

      has (subtheme @Markdown) do
        margin-left =: 1em

      has (subtheme @Reply) do
        has (tag Button) do
          border =: none
          background-color =: inherit
          cursor =: pointer

          hover do
            text-decoration =: underline

      has (subtheme @Children) do
        border-left =* [1px,solid,black]
        margin-left =: 1em
        padding-left =: 1em

      has ".RawComment" do
        child (tag H2) do
          display =: none
      
instance Theme Meta
instance Theme UserVotes
instance Theme Edited
instance Theme Created
instance Theme Username
instance Theme Controls
instance Theme Reply
instance Theme Markdown
instance Theme Children

#ifdef __GHCJS__
foreign import javascript unsafe
  "window.scrollTo(0,document.getElementById($1).offsetTop)"
    jump_js :: Txt -> IO ()
jump = jump_js
#else
jump _ = pure ()
#endif
