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
import Control.Monad (forM, mapM)
import qualified Data.Text as T
import Myocardio.TextMarkup
    ( Markup, (@@), fromText, markupSet, markupToList )

import Graphics.Vty (Attr, vertCat, horizCat, text', defAttr)

import Brick.AttrMap ( AttrName, mergeWithDefault )
import Brick.Types
    ( Widget(Widget),
      lookupAttrName,
      ctxAttrMapL,
      emptyResult,
      getContext,
      imageL,
      RenderM,
      Size(Fixed) )
import Prelude(Applicative (pure), Foldable (null))
import Data.Eq (Eq)
import Data.Function (($))

-- | A type class for types that provide access to an attribute in the
-- rendering monad.  You probably won't need to instance this.
class GetAttr a where
    -- | Where to get the attribute for this attribute metadata.
    getAttr :: a -> RenderM n Attr

instance GetAttr Attr where
    getAttr a = do
        c <- getContext
        pure $ Brick.AttrMap.mergeWithDefault a (c^.ctxAttrMapL)

instance GetAttr Brick.AttrMap.AttrName where
    getAttr = lookupAttrName

-- | Build a piece of markup from text with an assigned attribute name.
-- When the markup is rendered, the attribute name will be looked up in
-- the rendering context's 'AttrMap' to determine the attribute to use
-- for this piece of text.
(@?) :: T.Text -> Brick.AttrMap.AttrName -> Markup Brick.AttrMap.AttrName
(@?) = (@@)

-- | Build a widget from markup.
markup :: (Eq a, GetAttr a) => Markup a -> Widget n
markup m =
    Widget Fixed Fixed $ do
      let markupLines = markupToList m
          mkLine pairs = do
              is <- forM pairs $ \(t, aSrc) -> do
                  a <- getAttr aSrc
                  pure $ text' a t
              if null is
                 then do
                     def <- getAttr defAttr
                     pure $ text' def $ T.singleton ' '
                 else pure $ horizCat is
      lineImgs <- mapM mkLine markupLines
      pure $ emptyResult & imageL .~ vertCat lineImgs
