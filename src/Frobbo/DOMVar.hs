{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Implements `DOMVar`, a mechanism over the Document Object Model on
-- webpages where dom elements can be replaced entirely with different tagged
-- items and not break the hierarchy.
--

module Frobbo.DOMVar
  (
  -- * Types
    DOMVar()
  , DOMTag(..)
  -- * Operations
  , newDOMVar
  , modifyDOMVar
  , setFinalizerDOMVar
  , setDOMVarText
  , setClassName
  , getClassName
  -- * Document body
  , getBody )
  where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import GHC.Generics
import GHCJS.Types
import Data.Data
import Data.Foldable
import Data.JSString
import Data.Text ( Text )
import qualified Data.Text as T

foreign import javascript "$r = document.createElement($1);" jsCreateElement :: JSString -> IO JSVal
foreign import javascript "$1.appendChild($2);" jsAppendChild :: JSVal -> JSVal -> IO ()
foreign import javascript "$1.replaceChild($2, $3);" jsReplaceChild :: JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript "if ( $1.contains($2) ) { $1.removeChild($2); };" jsRemoveChild :: JSVal -> JSVal -> IO ()
foreign import javascript "$1.textContent = $2;" jsSetText :: JSVal -> JSString -> IO ()
foreign import javascript "$r = $1.textContent;" jsGetText :: JSVal -> IO JSString
foreign import javascript "$r = document.body;" jsGetBody :: IO JSVal
foreign import javascript "$r = $1.className;" jsGetClassName :: JSVal -> IO JSString
foreign import javascript "$1.className = $2;" jsSetClassName :: JSVal -> JSString -> IO ()
foreign import javascript "while ($1.firstChild) { $1.removeChild($1.firstChild); };" jsRemoveAllChildren :: JSVal -> IO ()

newtype DOMElement = DOMElement JSVal

data DOMVar = DOMVar (MVar (DOMElement, DOMElement -> Maybe DOMElement -> IO (), DOMTag))
  deriving ( Eq, Typeable, Generic )

-- | These are all the tags supported by `DOMVar`.
data DOMTag
  = Div
  | Br
  | Span
  | P
  | Body
  | Hr
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

tagToName :: DOMTag -> JSString
tagToName Div  = pack "div"
tagToName Span = pack "span"
tagToName P    = pack "p"
tagToName Body = pack "body"
tagToName Br   = pack "br"
tagToName Hr   = pack "hr"

getBody :: MonadIO m => m DOMVar
getBody = liftIO $ do
  body_ref <- jsGetBody
  DOMVar <$> newMVar (DOMElement body_ref, \_ _ -> return (), Body)

-- | Creates a new `DOMVar`, with its tag and children.
newDOMVar :: MonadIO m => DOMTag -> [DOMVar] -> m DOMVar
newDOMVar tag children = liftIO $ mask_ $ do
  new_tag_ref <- jsCreateElement (tagToName tag)

  fitTag new_tag_ref children

  DOMVar <$> newMVar (DOMElement new_tag_ref, \_ _ -> return (), tag)

setFinalizerDOMVar :: MonadIO m => DOMVar -> IO () -> m ()
setFinalizerDOMVar (DOMVar mvar) finalizer = liftIO $
  void $ mkWeakMVar mvar finalizer

fitTag :: JSVal -> [DOMVar] -> IO ()
fitTag new_tag_ref children =
  for_ children $ \(DOMVar child_mvar) -> do
    modifyMVar_ child_mvar $ \(DOMElement child_dom_element, update_ref, child_tag) -> do
      update_ref (DOMElement child_dom_element) Nothing
      jsAppendChild new_tag_ref child_dom_element
      return (DOMElement child_dom_element,
              childUpdater new_tag_ref,
              child_tag)
 where
  childUpdater :: JSVal -> DOMElement -> Maybe DOMElement -> IO ()
  childUpdater new_tag_ref
               (DOMElement dom_element)
               Nothing =
    jsRemoveChild new_tag_ref dom_element

  childUpdater new_tag_ref
               (DOMElement dom_element)
               (Just (DOMElement new_dom_element)) =
    jsReplaceChild new_tag_ref new_dom_element dom_element

-- | Modifies the tag and children of a `DOMVar`. Does not modify the text.
modifyDOMVar :: MonadIO m => DOMVar -> DOMTag -> [DOMVar] -> m ()
modifyDOMVar (DOMVar mvar) tag new_children = liftIO $ mask_ $
  modifyMVar_ mvar $ \(DOMElement old_ref, update_ref, old_tag) -> do
    old_text <- jsGetText old_ref

    -- Remove all children upfront
    for_ new_children $ \(DOMVar child_mvar) ->
      modifyMVar_ child_mvar $ \(child_dom_element, update_ref, child_tag) -> do
        update_ref child_dom_element Nothing
        return (child_dom_element, \_ _ -> return (), child_tag)

    -- If DOM tag hasn't changed, re-use previous DOM element, and don't call
    -- updater for the reference.
    tag_ref <- if old_tag /= tag
                 then do old_class_name <- jsGetClassName old_ref
                         new_ref <- jsCreateElement (tagToName tag)
                         jsSetClassName new_ref old_class_name
                         jsSetText new_ref old_text
                         update_ref (DOMElement old_ref) (Just $ DOMElement new_ref)
                         return new_ref

                 else do jsRemoveAllChildren old_ref
                         return old_ref

    fitTag tag_ref new_children

    return (DOMElement tag_ref, update_ref, tag)

-- | Sets the text on a `DOMVar`.
setDOMVarText :: MonadIO m => DOMVar -> Text -> m ()
setDOMVarText (DOMVar mvar) new_text = liftIO $
  withMVar mvar $ \(DOMElement tag_ref, _, _) ->
    jsSetText tag_ref (pack $ T.unpack new_text)

-- | Sets new CSS class name for a `DOMVar`.
setClassName :: MonadIO m => DOMVar -> Text -> m ()
setClassName (DOMVar mvar) class_name = liftIO $
  withMVar mvar $ \(DOMElement tag_ref, _, _) ->
    jsSetClassName tag_ref (pack $ T.unpack class_name)

-- | Gets the current CSS class name in a `DOMVar`.
getClassName :: MonadIO m => DOMVar -> m Text
getClassName (DOMVar mvar) = liftIO $
  withMVar mvar $ \(DOMElement tag_ref, _, _) ->
    T.pack . unpack <$> jsGetClassName tag_ref

