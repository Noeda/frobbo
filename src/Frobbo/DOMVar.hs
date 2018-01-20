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
  -- * Events
  , addClickEventHandler
  -- * Document body
  , getBody )
  where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import GHC.Generics
import GHCJS.Foreign.Callback
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
foreign import javascript "$1.addEventListener('click', $2);" jsAddClickEventHandler :: JSVal -> Callback (IO ()) -> IO ()

newtype DOMElement = DOMElement { toJSVal :: JSVal }

data DOMContents = DOMContents
  { domElement       :: DOMElement
  , updateReferences :: DOMElement -> Maybe DOMElement -> IO ()
  , domTag           :: DOMTag
  , clickEvents      :: [Callback (IO ())] }

data DOMVar = DOMVar (MVar DOMContents)
  deriving ( Eq, Typeable, Generic )

-- | These are all the tags supported by `DOMVar`.
data DOMTag
  = Div
  | Br
  | Span
  | P
  | Body
  | Hr
  | Button
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

tagToName :: DOMTag -> JSString
tagToName Div    = pack "div"
tagToName Span   = pack "span"
tagToName P      = pack "p"
tagToName Body   = pack "body"
tagToName Br     = pack "br"
tagToName Hr     = pack "hr"
tagToName Button = pack "button"

getBody :: MonadIO m => m DOMVar
getBody = liftIO $ do
  body_ref <- jsGetBody
  DOMVar <$> newMVar (DOMContents
    { domElement       = DOMElement body_ref
    , updateReferences = \_ _ -> return ()
    , domTag           = Body
    , clickEvents      = [] })

-- | Creates a new `DOMVar`, with its tag and children.
newDOMVar :: MonadIO m => DOMTag -> [DOMVar] -> m DOMVar
newDOMVar tag children = liftIO $ mask_ $ do
  new_tag_ref <- jsCreateElement (tagToName tag)

  fitTag new_tag_ref children

  DOMVar <$> newMVar (DOMContents
    { domElement       = DOMElement new_tag_ref
    , updateReferences = \_ _ -> return ()
    , domTag           = tag
    , clickEvents      = [] })

setFinalizerDOMVar :: MonadIO m => DOMVar -> IO () -> m ()
setFinalizerDOMVar (DOMVar mvar) finalizer = liftIO $
  void $ mkWeakMVar mvar finalizer

fitTag :: JSVal -> [DOMVar] -> IO ()
fitTag new_tag_ref children =
  for_ children $ \(DOMVar child_mvar) -> do
    modifyMVar_ child_mvar $ \domcontents -> do
      updateReferences domcontents (domElement domcontents) Nothing
      jsAppendChild new_tag_ref (toJSVal $ domElement domcontents)
      return $ domcontents { updateReferences = childUpdater new_tag_ref }
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
  modifyMVar_ mvar $ \domcontents -> do  -- (DOMElement old_ref, update_ref, old_tag) -> do
    old_text <- jsGetText $ toJSVal $ domElement domcontents

    -- Remove all children upfront
    for_ new_children $ \(DOMVar child_mvar) ->
      modifyMVar_ child_mvar $ \child_domcontents -> do
        updateReferences child_domcontents (domElement child_domcontents) Nothing
        return child_domcontents { updateReferences = \_ _ -> return () }

    -- If DOM tag hasn't changed, re-use previous DOM element, and don't call
    -- updater for the reference.
    tag_ref <- if domTag domcontents /= tag
                 then do old_class_name <- jsGetClassName (toJSVal $ domElement domcontents)
                         new_ref <- jsCreateElement (tagToName tag)
                         jsSetClassName new_ref old_class_name
                         jsSetText new_ref old_text
                         for_ (clickEvents domcontents) $ \click_event ->
                           jsAddClickEventHandler new_ref click_event
                         updateReferences domcontents (domElement domcontents) (Just $ DOMElement new_ref)
                         return new_ref

                 else do jsRemoveAllChildren $ toJSVal $ domElement domcontents
                         return $ toJSVal $ domElement domcontents

    fitTag tag_ref new_children

    return domcontents
      { domElement =  DOMElement tag_ref
      , domTag     =  tag }

-- | Sets the text on a `DOMVar`.
setDOMVarText :: MonadIO m => DOMVar -> Text -> m ()
setDOMVarText (DOMVar mvar) new_text = liftIO $
  withMVar mvar $ \domcontents ->
    jsSetText (toJSVal $ domElement domcontents) (pack $ T.unpack new_text)

-- | Sets new CSS class name for a `DOMVar`.
setClassName :: MonadIO m => DOMVar -> Text -> m ()
setClassName (DOMVar mvar) class_name = liftIO $
  withMVar mvar $ \domcontents ->
    jsSetClassName (toJSVal $ domElement domcontents) (pack $ T.unpack class_name)

-- | Gets the current CSS class name in a `DOMVar`.
getClassName :: MonadIO m => DOMVar -> m Text
getClassName (DOMVar mvar) = liftIO $
  withMVar mvar $ \domcontents ->
    T.pack . unpack <$> jsGetClassName (toJSVal $ domElement domcontents)

-- | Adds an event handler for clicks.
addClickEventHandler :: MonadIO m => DOMVar -> IO () -> m ()
addClickEventHandler (DOMVar mvar) event = liftIO $ mask_ $ do
  cb <- modifyMVar mvar $ \domcontents -> do
    cb <- syncCallback ContinueAsync event
    jsAddClickEventHandler
      (toJSVal $ domElement domcontents)
      cb
    return (domcontents
      { clickEvents = cb:clickEvents domcontents }, cb)
  void $ mkWeakMVar mvar $ releaseCallback cb

