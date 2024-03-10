{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad (Monad)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Eq ((==))
import Data.Foldable (mapM_)
import Data.Function (($), (.))
import Data.List (filter)
import Data.Maybe (Maybe, mapMaybe)
import Data.Ord (comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Time.Clock (getCurrentTime)
import Lucid (renderText)
import qualified Lucid as L
import MyocardioApp.Database (Database (sorenessHistory), Muscle, Soreness (Soreness, muscle, soreness, time), SorenessValue (LittleSore, VerySore), allMuscles, modifyDb, readDatabase)
import qualified MyocardioApp.Htmx as LX
import Safe (maximumByMay)
import System.IO (IO)
import Web.Scotty (Parsable (parseParam), get, html, param, post, readEither, scotty)
import Prelude (Show (show))

instance Parsable Muscle where
  parseParam = readEither

instance Parsable SorenessValue where
  parseParam = readEither

packShow :: (Show a) => a -> Text
packShow = pack . show

htmlSkeleton :: (Monad m) => L.HtmlT m a -> L.HtmlT m a
htmlSkeleton content = do
  L.doctypehtml_ $ do
    L.head_ $ do
      LX.useHtmx
      L.meta_ [L.charset_ "utf-8"]
      L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
      L.title_ "myocardio - power up your routines"
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css", L.rel_ "stylesheet"]
    L.body_ $ do
      L.div_ [L.class_ "container"] $ do
        content

sorenessValueToEmoji :: SorenessValue -> Text
sorenessValueToEmoji VerySore = "😭"
sorenessValueToEmoji LittleSore = "😕"
sorenessValueToEmoji _otherSoreness = ""

sorenessOutput :: forall m. (Monad m) => Database -> L.HtmlT m ()
sorenessOutput database = do
  let muscleToSoreness :: Muscle -> Maybe Soreness
      muscleToSoreness muscle' =
        maximumByMay (comparing (.time)) $
          filter (\historyEntry -> historyEntry.muscle == muscle') database.sorenessHistory
      sorenessToHtml :: Soreness -> L.HtmlT m ()
      sorenessToHtml soreness' = L.li_ do
        L.span_ [L.class_ "me-1"] (L.toHtml (sorenessValueToEmoji soreness'.soreness))
        L.strong_ [L.class_ "me-1"] (L.toHtml (packShow soreness'.muscle))
        L.a_
          [ LX.hxPost_ ("/reset-soreness?muscle=" <> packShow soreness'.muscle),
            L.href_ "#",
            LX.hxTarget_ "#soreness-output"
          ]
          "Reset"
  L.div_ [L.id_ "soreness-output"] do
    L.ul_ (mapM_ sorenessToHtml $ mapMaybe muscleToSoreness allMuscles)

urlNewSoreness :: (IsString a) => a
urlNewSoreness = "/new-soreness"

newSorenessMuscle :: (IsString a) => a
newSorenessMuscle = "muscle"

newSorenessHowSore :: (IsString a) => a
newSorenessHowSore = "how-sore"

sorenessInputAndOutput :: forall m. (Monad m) => Database -> L.HtmlT m ()
sorenessInputAndOutput database = do
  L.h1_ "Soreness"
  L.form_ [L.class_ "row"] do
    L.div_ [L.class_ "col"] do
      L.div_ [L.class_ "form-floating"] do
        let muscleOption :: Muscle -> L.HtmlT m ()
            muscleOption muscle' = L.option_ [L.value_ (packShow muscle')] (L.toHtml (packShow muscle'))
        L.select_ [L.class_ "form-select", L.id_ "i-am-sore", L.name_ newSorenessMuscle] (mapM_ muscleOption allMuscles)
        L.label_ [L.for_ "i-am-sore"] "I am sore here"
    L.div_ [L.class_ "col"] do
      L.div_ [L.class_ "form-check form-check-inline"] do
        L.input_ [L.class_ "form-check-input", L.type_ "radio", L.name_ newSorenessHowSore, L.id_ "very sore", L.value_ (packShow VerySore), L.checked_]
        L.label_ [L.class_ "form-check-label", L.for_ "very sore"] (L.toHtml $ sorenessValueToEmoji VerySore <> " VERY")
      L.div_ [L.class_ "form-check form-check-inline"] do
        L.input_ [L.class_ "form-check-input", L.type_ "radio", L.name_ newSorenessHowSore, L.id_ "a little", L.value_ (packShow LittleSore)]
        L.label_ [L.class_ "form-check-label", L.for_ "a little"] (L.toHtml $ sorenessValueToEmoji LittleSore <> " A LITTLE")
    L.div_ [L.class_ "col"] do
      L.button_
        [ L.class_ "btn btn-primary",
          L.type_ "submit",
          LX.hxPost_ urlNewSoreness,
          LX.hxTarget_ "#soreness-output"
        ]
        "Submit"
  sorenessOutput database

main :: IO ()
main = scotty 3000 do
  post urlNewSoreness do
    muscle' <- param newSorenessMuscle
    howSore' <- param newSorenessHowSore

    currentTime <- liftIO getCurrentTime

    db <-
      modifyDb
        ( \db ->
            db
              { sorenessHistory =
                  Soreness
                    { time = currentTime,
                      muscle = muscle',
                      soreness = howSore'
                    }
                    : db.sorenessHistory
              }
        )

    html $ renderText $ sorenessOutput db
  get "/" do
    db <- readDatabase
    html $ renderText $ htmlSkeleton $ sorenessInputAndOutput db
