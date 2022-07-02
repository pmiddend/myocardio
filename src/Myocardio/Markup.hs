module Myocardio.Markup
  ( Markup
  , markup
  , markupSet
  , fromText
  , (@?)
  , GetAttr(..)
  )
where

import Lens.Micro ((.~), (&), (^.))
import Control.Monad (forM)
import qualified Data.Text as T
import Myocardio.TextMarkup

import Graphics.Vty (Attr, vertCat, horizCat, text', defAttr)

import Brick.AttrMap
import Brick.Types

-- | A type class for types that provide access to an attribute in the
-- rendering monad.  You probably won't need to instance this.
class GetAttr a where
    -- | Where to get the attribute for this attribute metadata.
    getAttr :: a -> RenderM n Attr

instance GetAttr Attr where
    getAttr a = do
        c <- getContext
        return $ mergeWithDefault a (c^.ctxAttrMapL)

instance GetAttr AttrName where
    getAttr = lookupAttrName

-- | Build a piece of markup from text with an assigned attribute name.
-- When the markup is rendered, the attribute name will be looked up in
-- the rendering context's 'AttrMap' to determine the attribute to use
-- for this piece of text.
(@?) :: T.Text -> AttrName -> Markup AttrName
(@?) = (@@)

-- | Build a widget from markup.
markup :: (Eq a, GetAttr a) => Markup a -> Widget n
markup m =
    Widget Fixed Fixed $ do
      let markupLines = markupToList m
          mkLine pairs = do
              is <- forM pairs $ \(t, aSrc) -> do
                  a <- getAttr aSrc
                  return $ text' a t
              if null is
                 then do
                     def <- getAttr defAttr
                     return $ text' def $ T.singleton ' '
                 else return $ horizCat is
      lineImgs <- mapM mkLine markupLines
      return $ emptyResult & imageL .~ vertCat lineImgs
