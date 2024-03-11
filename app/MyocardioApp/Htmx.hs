module MyocardioApp.Htmx (useHtmx, hxPost_, hxTarget_, hxGet_, SwapType (..), hxSwap_) where

import Control.Monad (Monad)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Text (Text)
import Lucid (Attributes, Html, HtmlT, script_, src_)
import Lucid.Base (makeAttributes)

-- This is literally copied from lucid-htmx because this package depends on an old version of text
-- and I didn't want to migrate it. See
--
-- https://github.com/monadicsystems/lucid-htmx/issues/12
htmxSrc :: Text
htmxSrc = "https://unpkg.com/htmx.org"

useHtmx :: (Monad m) => HtmlT m ()
useHtmx = script_ [src_ htmxSrc] ("" :: Html ())

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "data-hx-post"

data SwapType
  = SwapInnerHtml
  | SwapOuterHtml
  | SwapAfterBegin
  | SwapBeforeBegin
  | SwapBeforeEnd
  | SwapAfterEnd
  | SwapDelete
  | SwapNone
  deriving (Eq)

hxSwap_ :: SwapType -> Attributes
hxSwap_ = makeAttributes "data-hx-swap" . swapToString
  where
    swapToString SwapInnerHtml = "innerHTML"
    swapToString SwapOuterHtml = "outerHTML"
    swapToString SwapAfterBegin = "afterbegin"
    swapToString SwapBeforeBegin = "beforebegin"
    swapToString SwapAfterEnd = "afterend"
    swapToString SwapBeforeEnd = "beforeend"
    swapToString SwapDelete = "delete"
    swapToString SwapNone = "none"

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "data-hx-get"

-- | <https://htmx.org/attributes/hx-target/>
hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "data-hx-target"
