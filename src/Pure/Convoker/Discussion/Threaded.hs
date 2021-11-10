module Pure.Convoker.Discussion.Threaded where

import Pure.Convoker.Admins
import Pure.Convoker.Comment
import Pure.Convoker.Discussion
import Pure.Convoker.Discussion.Shared.Ago
import Pure.Convoker.Discussion.Shared.Markdown
import Pure.Convoker.Discussion.Shared.Total
import Pure.Convoker.Discussion.Threaded.Comment
import Pure.Convoker.Discussion.Threaded.Meta hiding (Upvote,Downvote)
import Pure.Convoker.Meta 
import Pure.Convoker.Mods
import Pure.Convoker.UserVotes hiding (Upvote,Downvote)
import qualified Pure.Convoker.UserVotes as UserVotes

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Data.Bloom as Bloom
import Pure.Data.JSON hiding (Null)
import Pure.Elm.Component hiding (pattern Meta)
import Pure.WebSocket

import Data.Hashable

import Data.List as List
import Data.Maybe
import Data.Typeable
import System.IO.Unsafe

threaded 
  :: forall _role a b. 
    ( Typeable a
    , Typeable (_role :: *)
    , ToJSON (Context a), FromJSON (Context a), Eq (Context a)
    , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
    , Ord b
    ) => WebSocket -> Context a -> Name a -> CommentSorter a b -> CommentViewer a -> View
threaded ws ctx nm sorter viewer = discussion @_role ws ctx nm (threads sorter viewer)

createDiscussion 
  :: forall a. 
    ( Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    ) => Context a -> Name a -> [Username] -> IO ()
createDiscussion ctx nm mods = void do
  tryCreate @(Discussion a) fullPermissions def (DiscussionContext ctx nm) (RawDiscussion ctx nm []) 
  tryCreate @(Mods a) fullPermissions def (ModsContext ctx) (RawMods mods)
  tryCreate @(Meta a) fullPermissions def (MetaContext ctx nm) (RawMeta (Votes []))

simpleThreaded 
  :: forall _role a. 
    ( Typeable a
    , Typeable (_role :: *)
    , ToJSON (Context a), FromJSON (Context a), Eq (Context a)
    , ToJSON (Name a), FromJSON (Name a), Eq (Name a)
    ) => WebSocket -> Context a -> Name a -> View
simpleThreaded ws ctx nm = threaded @_role ws ctx nm simpleSorter simpleComment 

simpleSorter :: CommentSorter a (Int,Key (Comment a))
simpleSorter Meta { votes = Votes vs } Comment { key } = (fromMaybe 0 (List.lookup key vs),key)

simpleComment :: (Typeable a, ToJSON (Context a), FromJSON (Context a), ToJSON (Name a), FromJSON (Name a)) => CommentViewer a
simpleComment ws ctx nm mun refresh mparent Admins {..} Mods {..} votes@UserVotes { upvotes, downvotes } meta comment@Comment {..} = 
  let 
    isMod = maybe False (`elem` admins) mun || maybe False (`elem` mods) mun
  in 
    Article <||>
      [ Header <||>
        [ Span <||>
          [ "Posted by "
          , Address <||> [ A <| Rel "author" |> [ txt author ] ]
          , " " 
          , SimpleHTML "time" <| Attribute "pubdate" "" . DateTime (toZonedDateTime created) |> [ txt (ago created) ]
          ]
        , maybe Null (\un -> simpleVotes ws ctx nm un key votes meta) mun
        ]
      , Section <||> content
      ]

simpleVotes :: (Typeable a, ToJSON (Context a), FromJSON (Context a), ToJSON (Name a), FromJSON (Name a)) => WebSocket -> Context a -> Name a -> Username -> Key (Comment a) -> Product (UserVotes a) -> Product (Meta a) -> View
simpleVotes socket context name username key UserVotes { upvotes, downvotes } Meta { votes = Votes vs } =
  let
    votes = fromMaybe 0 (List.lookup key vs)
    vote 
      | unsafePerformIO (Bloom.test upvotes key)   = Just True
      | unsafePerformIO (Bloom.test downvotes key) = Just False
      | otherwise                                  = Nothing
  in
    run SimpleVotes {..}

data SimpleVotes a = SimpleVotes
  { socket :: WebSocket
  , context :: Context a
  , name :: Name a
  , username :: Username
  , key :: Key (Comment a)
  , votes :: Int
  , vote :: Maybe Bool
  }

instance (Typeable a, ToJSON (Context a), FromJSON (Context a), ToJSON (Name a), FromJSON (Name a)) => Component (SimpleVotes a) where
  data Model (SimpleVotes a) = SimpleVotesModel
    { votes :: Int
    , vote :: Maybe Bool 
    }

  initialize SimpleVotes {..} = pure SimpleVotesModel {..}

  data Msg (SimpleVotes a) 
    = Upvote
    | Downvote
    | Unvote

  upon Upvote SimpleVotes { socket, context, name, username, key } mdl@SimpleVotesModel { votes, vote = v }
    | Just True <- v = pure mdl
    | otherwise = do
      request (publishingAPI @(UserVotes a)) socket (amendResource @(UserVotes a)) 
        (UserVotesContext context name,UserVotesName username,UserVotes.Upvote key) def
      pure (mdl :: Model (SimpleVotes a))
        { votes = votes + 1
        , vote = case v of
          Just False -> Nothing
          Nothing    -> Just True 
          _          -> v  
        }

  upon Downvote SimpleVotes { socket, context, name, username, key } mdl@SimpleVotesModel { votes, vote = v }
    | Just False <- v = pure mdl
    | otherwise = do
      request (publishingAPI @(UserVotes a)) socket (amendResource @(UserVotes a)) 
        (UserVotesContext context name,UserVotesName username,UserVotes.Downvote key) def
      pure (mdl  :: Model (SimpleVotes a))
        { votes = votes - 1
        , vote = case v of
          Just True -> Nothing
          Nothing   -> Just False
          _         -> v 
        }
        
  upon Unvote SimpleVotes { socket, context, name, username, key } mdl@SimpleVotesModel { votes, vote = v }
    | Nothing <- v = pure mdl 
    | Just True <- v = do
      request (publishingAPI @(UserVotes a)) socket (amendResource @(UserVotes a))
        (UserVotesContext context name,UserVotesName username,UserVotes.Downvote key) def
      pure (mdl :: Model (SimpleVotes a))
        { votes = votes - 1 
        , vote = Nothing
        }
    | Just False <- v = do
      request (publishingAPI @(UserVotes a)) socket (amendResource @(UserVotes a))
        (UserVotesContext context name,UserVotesName username,UserVotes.Upvote key) def
      pure (mdl :: Model (SimpleVotes a))
        { votes = votes + 1 
        , vote = Nothing
        }

  view _ SimpleVotesModel {..} 
    | Just _ <- vote = Span <||> [ Button <| OnClick (\_ -> command Unvote) |> [ "Unvote" ] ]
    | otherwise =
      Span <||>
        [ Button <| OnClick (\_ -> command Upvote)   |> [ "Upvote" ]
        , Button <| OnClick (\_ -> command Downvote) |> [ "Downvote" ]
        ]