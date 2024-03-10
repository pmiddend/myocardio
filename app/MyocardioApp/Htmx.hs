module MyocardioApp.Htmx (useHtmx, hxPost_, hxTarget_) where

import Control.Monad (Monad)
import Data.Text (Text)
import Lucid (Attribute, Html, HtmlT, script_, src_)
import Lucid.Base (makeAttribute)

-- This is literally copied from lucid-htmx because this package depends on an old version of text
-- and I didn't want to migrate it. See
--
-- https://github.com/monadicsystems/lucid-htmx/issues/12
htmxSrc :: Text
htmxSrc = "https://unpkg.com/htmx.org"

useHtmx :: (Monad m) => HtmlT m ()
useHtmx = script_ [src_ htmxSrc] ("" :: Html ())

hxPost_ :: Text -> Attribute
hxPost_ = makeAttribute "data-hx-post"

-- | <https://htmx.org/attributes/hx-target/>
hxTarget_ :: Text -> Attribute
hxTarget_ = makeAttribute "data-hx-target"
