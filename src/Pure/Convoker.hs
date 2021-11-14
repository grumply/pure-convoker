module Pure.Convoker (module Export, module Pure.Convoker) where

import Pure.Convoker.Comment as Export
import Pure.Convoker.Discussion as Export
import Pure.Convoker.Meta as Export
import Pure.Convoker.Mods as Export
import Pure.Convoker.Admins as Export
import Pure.Convoker.UserVotes as Export

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Data.JSON hiding (Null)
import Pure.Elm.Component (View,Default(..),pattern Null)
import Pure.Hooks (useEffectWith')
import Pure.Sorcerer
import Pure.WebSocket

import Data.Hashable

import Data.Typeable

convoke
  :: forall a.
    ( Typeable a

    , Pathable (Context a), Hashable (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , Amendable (Comment a)
    , ToJSON (Resource (Comment a)), FromJSON (Resource (Comment a))
    , ToJSON (Amend (Comment a)), FromJSON (Amend (Comment a))

    , Amendable (Meta a)
    , ToJSON (Resource (Meta a)), FromJSON (Resource (Meta a))
    , ToJSON (Preview (Meta a)), FromJSON (Preview (Meta a))
    , ToJSON (Product (Meta a)), FromJSON (Product (Meta a))
    , ToJSON (Amend (Meta a)), FromJSON (Amend (Meta a))

    ) => [Listener]
convoke = concat
  [ conjure @(Discussion a) 
  , conjure @(Comment a)
  , conjure @(Meta a)
  , conjure @(Mods a)
  , conjure @(UserVotes a)
  ]

endpoints 
  :: forall a. 
    ( Typeable a

    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , Nameable (Comment a)
    , Processable (Comment a)
    , Producible (Comment a)
    , Amendable (Comment a)
    , ToJSON (Resource (Comment a)), FromJSON (Resource (Comment a))
    , ToJSON (Action (Comment a)), FromJSON (Action (Comment a))
    , ToJSON (Reaction (Comment a)), FromJSON (Reaction (Comment a))
    , ToJSON (Amend (Comment a)), FromJSON (Amend (Comment a))

    , Processable (Meta a)
    , Producible (Meta a)
    , Previewable (Meta a)
    , Amendable (Meta a)
    , ToJSON (Resource (Meta a)), FromJSON (Resource (Meta a))
    , ToJSON (Product (Meta a)), FromJSON (Product (Meta a))
    , ToJSON (Preview (Meta a)), FromJSON (Preview (Meta a))
    , ToJSON (Action (Meta a)), FromJSON (Action (Meta a))
    , ToJSON (Reaction (Meta a)), FromJSON (Reaction (Meta a))
    , ToJSON (Amend (Meta a)), FromJSON (Amend (Meta a))

    ) => WebSocket
      -> Maybe Username
      -> Permissions (Comment a) 
      -> Permissions (Meta a) 
      -> Callbacks (Discussion a) 
      -> Callbacks (Comment a)
      -> Callbacks (Meta a)
      -> Callbacks (Mods a)
      -> Callbacks (UserVotes a)
      -> Interactions (Comment a) 
      -> Interactions (Meta a)
      -> View
endpoints ws mu commentPermissions metaPermissions discussionCallbacks commentCallbacks metaCallbacks modsCallbacks userVotesCallbacks commentInteractions metaInteractions = 
  useEffectWith' (effect mu) mu Null
  where
    effect = \case
      Just un -> authenticatedEndpoints ws un commentPermissions metaPermissions discussionCallbacks commentCallbacks metaCallbacks modsCallbacks userVotesCallbacks commentInteractions metaInteractions
      _ -> unauthenticatedEndpoints ws discussionCallbacks metaCallbacks modsCallbacks

-- | This should be considered an extensible API for managing an unauthenticated
-- user's discussion endpoints. You can choose to satisfy some of the 
-- constraints and partially apply this function and thereby reduce the required
-- implementation burden, e.g. as seen in Pure.Convoker.Discussion.Threaded
-- Meta has a concrete implementation that satisfies the required constraints.
--
-- Callbacks for Discussion, Meta, and Mods are required parameters since they 
-- are the extension points for the API, allowing, e.g. analytics analysis of
-- discussion reads, etc....
-- 
-- Returns a callback that will deactivate the endpoints.
unauthenticatedEndpoints 
  :: forall (a :: *). 
    ( Typeable a

    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , ToJSON (Product (Meta a)), FromJSON (Product (Meta a))
    , ToJSON (Preview (Meta a)), FromJSON (Preview (Meta a))

    ) => WebSocket -> Callbacks (Discussion a) -> Callbacks (Meta a) -> Callbacks (Mods a) ->  IO (IO ())
unauthenticatedEndpoints socket discussionCallbacks metaCallbacks modsCallbacks = do
  discussion <- enact socket (cachingReading @(Discussion a) readPermissions discussionCallbacks)
  meta       <- enact socket (cachingReading @(Meta a) readPermissions metaCallbacks)
  mods       <- enact socket (cachingReading @(Mods a) readPermissions modsCallbacks)
  pure do
    repeal discussion
    repeal meta
    repeal mods

-- | This should be considered an extensible API for managing an authenticated
-- user's discussion endpoints. You can choose to satisfy some of the 
-- constraints and partially apply this function and thereby reduce the required
-- implementation burden, e.g. as seen in Pure.Convoker.Discussion.Threaded 
-- where Comment and Meta have concrete implementations that satisfy all of the
-- required constraints and the only required parameters are the WebSocket, the
-- Username, and Callbacks for extensibility.
--
-- It would be easy to simplify the constraints here as `Conjurable (Meta a)`
-- and `Conjurable (Comment a)`, but that would obscure the nature of this
-- function by hiding what exactly needs to be implemented for a fully-custom 
-- discussion implemention. That is, this type signature can be read as a
-- guide for implementing your own, customized, discussion types, as they
-- are the types and instances that were omitted to maintain generality.  
--
-- Note that one place where this generality was not chased to the edge was
-- in the implementation of UserVotes. Creating a user-local sideband, much 
-- like the discussion-local sideband found in `Meta`, could be a good extension
-- point for more complex discussion dynamics, but would bring with it another 
-- extensive set of constraints. 
-- 
-- Permissions for Comment and Meta are required parameters since Comment and 
-- Meta are left open for custom implementation and it wouldn't make sense to 
-- have arbitrary defaults.
--
-- Callbacks for Discussion, Comment, Meta, Mods, and UserVotes are required 
-- parameters since they are extension points for the API, allowing, e.g.
-- user email notifications to be sent upon update or changes, or triggering 
-- of analysis on comment creation, etc....
--
-- Similarly to the case for permissioning for Comment and Meta, interactions
-- for Comment and Meta are required since it wouldn't make sense to have 
-- arbitrary defaults.
--
-- Returns a callback that will deactivate the endpoints.
authenticatedEndpoints 
  :: forall (a :: *). 
    ( Typeable a

    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , Nameable (Comment a)
    , Processable (Comment a)
    , Producible (Comment a)
    , Amendable (Comment a)
    , ToJSON (Resource (Comment a)), FromJSON (Resource (Comment a))
    , ToJSON (Action (Comment a)), FromJSON (Action (Comment a))
    , ToJSON (Reaction (Comment a)), FromJSON (Reaction (Comment a))
    , ToJSON (Amend (Comment a)), FromJSON (Amend (Comment a))

    , Processable (Meta a)
    , Producible (Meta a)
    , Previewable (Meta a)
    , Amendable (Meta a)
    , ToJSON (Resource (Meta a)), FromJSON (Resource (Meta a))
    , ToJSON (Product (Meta a)), FromJSON (Product (Meta a))
    , ToJSON (Preview (Meta a)), FromJSON (Preview (Meta a))
    , ToJSON (Action (Meta a)), FromJSON (Action (Meta a))
    , ToJSON (Reaction (Meta a)), FromJSON (Reaction (Meta a))
    , ToJSON (Amend (Meta a)), FromJSON (Amend (Meta a))

    ) => WebSocket 
      -> Username 
      -> Permissions (Comment a) 
      -> Permissions (Meta a) 
      -> Callbacks (Discussion a) 
      -> Callbacks (Comment a)
      -> Callbacks (Meta a)
      -> Callbacks (Mods a)
      -> Callbacks (UserVotes a)
      -> Interactions (Comment a) 
      -> Interactions (Meta a) 
      -> IO (IO ())
authenticatedEndpoints socket un commentPermissions metaPermissions discussionCallbacks commentCallbacks metaCallbacks modsCallbacks userVotesCallbacks commentInteractions metaInteractions = do 
  -- Notes:
  --   We don't cache Comment, since they're stored in Discussion.
  --   We don't cache UserVotes to reduce memory overhead.
  --   We don't enact a publishing endpoint for discussion because
  --     discussions are manually created when their linked resource 
  --     is created.

  discussionReading   <- enact socket (cachingReading @(Discussion a) readPermissions discussionCallbacks)
  commentReading      <- enact socket (reading @(Comment a) commentPermissions commentCallbacks)
  metaReading         <- enact socket (cachingReading @(Meta a) metaPermissions metaCallbacks)
  modsReading         <- enact socket (cachingReading @(Mods a) readPermissions modsCallbacks)
  userVotesReading    <- enact socket (reading @(UserVotes a) (userVotesPermissions un) userVotesCallbacks)

  commentPublishing   <- enact socket (publishing @(Comment a) commentPermissions commentCallbacks commentInteractions)
  metaPublishing      <- enact socket (cachingPublishing @(Meta a) metaPermissions metaCallbacks metaInteractions)
  modsPublishing      <- enact socket (cachingPublishing @(Mods a) (modsPermissions un) modsCallbacks modsInteractions)
  userVotesPublishing <- enact socket (publishing @(UserVotes a) (userVotesPermissions un) userVotesCallbacks userVotesInteractions)

  pure do
    repeal discussionReading
    repeal commentReading
    repeal metaReading
    repeal modsReading
    repeal userVotesReading

    repeal commentPublishing
    repeal metaPublishing
    repeal modsPublishing
    repeal userVotesPublishing 

convokerCache 
  :: forall a. 
    ( Typeable a

    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , ToJSON (Product (Meta a)), FromJSON (Product (Meta a))
    , ToJSON (Preview (Meta a)), FromJSON (Preview (Meta a))
    ) => IO ()
convokerCache = do
  cache @(Discussion a)
  cache @(Mods a)
  cache @(Meta a)