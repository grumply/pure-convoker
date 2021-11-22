module Pure.Convoker.Discussion.Threaded 
  ( module Pure.Convoker.Discussion.Threaded
  , module Export
  ) where

import Pure.Convoker as Export hiding (Upvote,Downvote,authenticatedEndpoints,unauthenticatedEndpoints,endpoints)
import qualified Pure.Convoker as Convoker
import Pure.Convoker.Discussion

import Pure.Auth (Username,Token(..))
import Pure.Conjurer
import Pure.Data.Bloom as Bloom
import Pure.Data.JSON hiding (Null)
import Pure.Elm.Component hiding (pattern Meta)
import Data.Foldable as Foldable
import Pure.Hooks (useState,State(..))
import Pure.WebSocket

import Data.Hashable

import qualified Data.Graph as G
import Data.List as List
import Data.Maybe
import Data.Typeable
import System.IO.Unsafe

unauthenticatedEndpoints
  :: forall a.
    ( Typeable a

    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Name a), FromJSON (Name a)
    
    , ToJSON (Product (Meta a)), FromJSON (Product (Meta a))
    , ToJSON (Preview (Meta a)), FromJSON (Preview (Meta a))

    ) => WebSocket -> Callbacks (Discussion a) -> Callbacks (Meta a) -> Callbacks (Mods a) ->  IO (IO ())
unauthenticatedEndpoints = Convoker.unauthenticatedEndpoints 

authenticatedEndpoints 
  :: forall a. 
    ( Typeable a

    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , Nameable    (Comment a)
    , Previewable (Comment a)
    , Processable (Comment a)
    , Producible  (Comment a)
    , Amendable   (Comment a)
    , Previewable (Meta a)
    , Processable (Meta a)
    , Producible  (Meta a)
    , Amendable   (Meta a)

    , ToJSON (Resource (Meta a)), FromJSON (Resource (Meta a))
    , ToJSON (Product (Meta a)), FromJSON (Product (Meta a))
    , ToJSON (Preview (Meta a)), FromJSON (Preview (Meta a))
    , ToJSON (Action (Meta a)), FromJSON (Action (Meta a))
    , ToJSON (Amend (Meta a)), FromJSON (Amend (Meta a))
    , FromJSON (Reaction (Meta a))

    , ToJSON (Resource (Comment a)), FromJSON (Resource (Comment a))
    , ToJSON (Action (Comment a)), FromJSON (Action (Comment a))
    , ToJSON (Reaction (Comment a)), FromJSON (Reaction (Comment a))
    , ToJSON (Reaction (Meta a))
    , ToJSON (Amend (Comment a)), FromJSON (Amend (Comment a))
      
    ) => WebSocket 
      -> Username 
      -> Callbacks (Discussion a) 
      -> Callbacks (Comment a)
      -> Callbacks (Meta a)
      -> Callbacks (Mods a)
      -> Callbacks (UserVotes a)
      -> IO (IO ())
authenticatedEndpoints ws un discussionCallbacks commentCallbacks metaCallbacks modsCallbacks userVotesCallbacks = 
  Convoker.authenticatedEndpoints ws un
    (permissions (Just un))
    (permissions (Just un))
    discussionCallbacks 
    (extendCommentCallbacks readPermissions discussionCallbacks commentCallbacks)
    metaCallbacks 
    modsCallbacks 
    userVotesCallbacks 
    (interactions (Just un))
    (interactions (Just un))

threaded 
  :: forall _role a b. 
    ( Typeable a
    , Typeable (_role :: *)
    , Theme (Comment a)
    , ToJSON (Resource (Comment a)), FromJSON (Resource (Comment a))
    , Formable (Resource (Comment a))
    , Default (Resource (Comment a))
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Eq (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Eq (Name a)
    , Default (Resource (Comment a))
    , Ord b
    , FromJSON (Product (Meta a))
    ) => WebSocket -> Context a -> Name a -> (Username -> View) -> ([View] -> [View]) -> (Product (Meta a) -> Product (Comment a) -> b) -> (CommentFormBuilder _role a -> View) -> (CommentBuilder _role a -> View) -> View
threaded ws ctx nm withAuthor withContent sorter commentFormBuilder commentBuilder = 
  discussion @_role @a ws ctx nm withAuthor withContent (threads @_role @a @b sorter commentFormBuilder commentBuilder)

threads :: DiscussionLayout _role a b
threads sorter runCommentFormBuilder runCommentBuilder DiscussionBuilder { full = Discussion { comments }, ..} =
  Div <||> 
    ( (useState False $ \State {..} ->
        if state then
          runCommentFormBuilder CommentFormBuilder 
            { parent = Nothing
            , viewer = runCommentBuilder
            , onCancel = modify (const False)
            , ..
            } 
        else
          Button <| OnClick (\_ -> modify (const True)) |> [ "Add Comment" ]
      )

    : forest Nothing threads
    )
  where
    edges = fmap (\(comment@Comment { key, parents = Parents ps }) -> (comment,key,ps)) comments
    
    (graph,nodeFromVertex,_) = G.graphFromEdges edges

    -- transpose because each vertex in our graph points to predecessors
    threads = G.components (G.transposeG graph) 

    forest parent ts = 
      let look (G.Node n _) = let (comment,_,_) = nodeFromVertex n in comment
          sorted = List.sortOn (sorter meta . look) ts 
      in 
        [ tree parent previous next t 
        | (t,pr,nx) <- zip3 sorted (Nothing : fmap Just sorted) (List.tail (fmap Just sorted ++ [Nothing])) 
        , let 
            previous = fmap (\(look -> Comment { key }) -> key) pr
            next = fmap (\(look -> Comment { key }) -> key) nx
        ]
    
    tree parent previous next node@(G.Node (nodeFromVertex -> (comment@Comment { key },_,_)) sub) =
      runCommentBuilder CommentBuilder 
        { children = forest (Just key) sub
        , size = Foldable.length node - 1 -- since children are passed lazily pre-rendered
        , ..
        }

    {- 
      What is the performance difference between the above and this?
      What is the difference heap-wise? My intuition says that they will be the same,
      but I'm erring on the side of caution here since this is a critical portion of the
      threaded layout. I'd like to test when I have some extensive mocking or a good 
      arbitrary instance or when I can look at some core.

      forest mparent ts = 
        let look (G.Node n _) = let (comment,_,_) = nodeFromVertex n in comment
        in [ tree t | t <- List.sortOn (sorter meta . look) ts ]
        where
          tree (G.Node (nodeFromVertex -> (Comment { key },_,_)) sub) =
            runCommentBuilder CommentBuilder 
              { children = forest (Just key) sub
              , size = Foldable.length node - 1
              , ..
              }
    -}