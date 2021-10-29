module Pure.Convoker (module Export,module Pure.Convoker) where

import Pure.Convoker.Comment as Export
import Pure.Convoker.Discussion as Export
import Pure.Convoker.DiscussionMeta as Export

import Pure.Auth (Username)
import Pure.Conjurer
import Pure.Elm.Component

{-

This approach is slightly more generic than necessary: the writer can create
an arbitrary resource type that is producible and previewable and it can be
used as the content of a comment system.

TODO:

Add comment administration. 

Add comment processing callbacks that add comments to discussions. (work started below)

-}

data Config = Config
  { editing     :: Bool
  , upvoting    :: Bool
  , downvoting  :: Bool
  , fuzzing     :: Bool
  }

addCommentCallbacks :: Config -> Callbacks a -> Callbacks a
addCommentCallbacks Config {..} cbs = Callbacks 
  { onCreate = \ctx nm res pro pre -> do
    onCreate cbs ctx nm res pro pre

  , onUpdate = \ctx nm res pro pre -> do
    onUpdate cbs ctx nm res pro pre

  , onDelete = \ctx nm res pro pre -> do
    onDelete cbs ctx nm res pro pre

  , onResource = \ctx nm res -> do
    onResource cbs ctx nm res

  , onRead = \ctx nm pro -> do
    onRead cbs ctx nm pro

  , onPreview = \ctx nm pre -> do
    onPreview cbs ctx nm pre

  , onList = \ctx lst -> do
    onList cbs ctx lst
  }

