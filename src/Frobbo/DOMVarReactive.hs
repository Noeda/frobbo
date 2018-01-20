{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Frobbo.DOMVarReactive
  ( div_
  , span_
  , spanS_
  , react
  , zoom
  , getTopDomElement
  , DOMPowered()
  , DOMBuild()
  , updateState
  , toDOMPowered )
  where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Functor.Contravariant
import Data.Dynamic
import Data.Monoid
import Data.Text ( Text, pack )

import Frobbo.DOMVar

data DOMBuild state
  = PlaceDiv [DOMBuild state]
  | PlaceSpan Text
  | forall key. (Eq key, Typeable key) => Zoom (state -> key) (key -> DOMBuild key)
  | forall key. (Eq key, Typeable key) => React (state -> key) (key -> DOMBuild state)

instance Contravariant DOMBuild where
  contramap co (PlaceDiv inners) = PlaceDiv $ fmap (contramap co) inners
  contramap _ (PlaceSpan txt) = PlaceSpan txt
  contramap co (Zoom state inner) =
    Zoom
    (\new_state -> state (co new_state))
    inner
  contramap co (React state inner) =
    React
    (\new_state -> state (co new_state))
    (\key -> contramap co $ inner key)

react :: (Eq key, Typeable key) => (state -> key) -> (key -> DOMBuild state) -> DOMBuild state
react builder = React builder

zoom :: (Eq key, Typeable key) => (state -> key) -> (key -> DOMBuild key) -> DOMBuild state
zoom zoomer = Zoom zoomer

div_ :: [DOMBuild a] -> DOMBuild a
div_ = PlaceDiv

span_ :: Text -> DOMBuild state
span_ = PlaceSpan

spanS_ :: String -> DOMBuild state
spanS_ = PlaceSpan . pack

data Trigger state = Trigger
  { areEqualStates   :: state -> state -> Bool
  , getKey           :: state -> Dynamic
  , updateTrigger    :: state -> Dynamic -> IO ()
  , getChildTriggers :: IO (Triggers state) }

data Triggers state = Triggers [Trigger state]

instance Contravariant Trigger where
  contramap co trigger = Trigger
    { areEqualStates   = \st1 st2 -> (areEqualStates trigger) (co st1) (co st2)
    , getKey           = \st1 -> (getKey trigger) (co st1)
    , updateTrigger    = \st1 dyn -> (updateTrigger trigger) (co st1) dyn
    , getChildTriggers = contramap co <$> getChildTriggers trigger }

instance Contravariant Triggers where
  contramap co (Triggers lst) = Triggers $ fmap (contramap co) lst

data DOMPowered state = DOMPowered
  { topLevelTriggers    :: Triggers state
  , topDom              :: DOMVar
  , domPoweredLockState :: MVar state }

getTopDomElement :: DOMPowered state -> DOMVar
getTopDomElement (DOMPowered _ dom _) = dom

toDOMPowered :: MonadIO m => state -> DOMBuild state -> m (DOMPowered state)
toDOMPowered initial_state dombuild = liftIO $ do
  lock <- newMVar initial_state
  (var, triggers) <- runStateT (toDOMVar' initial_state dombuild) (Triggers [])
  return DOMPowered
    { topLevelTriggers    = triggers
    , topDom              = var
    , domPoweredLockState = lock }

updateState :: MonadIO m => state -> DOMPowered state -> m ()
updateState new_state (DOMPowered (Triggers triggers) _ lock) = liftIO $
  modifyMVar_ lock $ \old_state -> do
    let walk triggers = for_ triggers $ \trigger -> do
          unless (areEqualStates trigger old_state new_state) $
            updateTrigger trigger new_state (getKey trigger new_state)
          Triggers more_triggers <- getChildTriggers trigger
          walk more_triggers
     in walk triggers
    return new_state

toDOMVar' :: MonadIO m => state -> DOMBuild state -> StateT (Triggers state) m DOMVar
toDOMVar' _ (PlaceSpan txt) = do
  var <- newDOMVar Span []
  setDOMVarText var txt
  return var
toDOMVar' state (PlaceDiv inner) = do
  inner_tags <- traverse (toDOMVar' state) inner
  div <- newDOMVar Div inner_tags
  return div
toDOMVar' state (Zoom keyer inner) = do
  let keyed = keyer state
  (inner_stuff, Triggers inner_triggers) <- lift $
    runStateT (toDOMVar' keyed (inner keyed)) (Triggers [])
  modify $ \(Triggers old_triggers) -> Triggers $
    old_triggers <>
    (fmap (contramap keyer) inner_triggers)
  return inner_stuff
toDOMVar' state (React keyer by_state) = do
  let keyed = keyer state
      first_inner = by_state keyed

  (inner_stuff, inner_triggers) <- lift $ runStateT (toDOMVar' state first_inner) (Triggers [])
  inner_triggers_tvar <- liftIO $ newTVarIO inner_triggers

  div <- newDOMVar Div [inner_stuff]

  modify $ \(Triggers triggers) -> Triggers $
    (:triggers) $ Trigger
      { areEqualStates = \state1 state2 -> keyer state1 == keyer state2
      , getKey         = toDyn . keyer
      , updateTrigger  = \new_state new_key_dynamic -> do
         let Just new_key = fromDynamic new_key_dynamic
             new_inner = by_state new_key
         (inner_stuff, inner_triggers) <- runStateT (toDOMVar' new_state new_inner) (Triggers [])
         atomically $ writeTVar inner_triggers_tvar inner_triggers
         modifyDOMVar div Div [inner_stuff]
      , getChildTriggers = atomically $ readTVar inner_triggers_tvar }

  return div

