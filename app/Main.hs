{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import CMarkGFM (commonmarkToHtml)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bool (Bool)
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable (Foldable (elem), find, forM_, mapM_)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (filter)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (Maybe (Just, Nothing), mapMaybe, maybe)
import Data.Monoid (Monoid (mempty))
import Data.Ord (comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text.IO (readFile)
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Lucid (renderText)
import qualified Lucid as L
import MyocardioApp.Database
  ( Database,
    DatabaseF (currentTraining, pastExercises, sorenessHistory),
    Exercise (category, description, muscles, name),
    ExerciseName (ExerciseName),
    ExerciseWithIntensity (ExerciseWithIntensity, exercise, intensity, time),
    Intensity (Intensity),
    Muscle,
    Soreness (Soreness, muscle, soreness, time),
    SorenessValue (LittleSore, NotSore, VerySore),
    allMuscles,
    exercises,
    intensityToText,
    modifyDb,
    readDatabase,
  )
import qualified MyocardioApp.Htmx as LX
import Network.HTTP.Types.Status (status400)
import Safe (maximumByMay)
import System.IO (IO)
import Web.Scotty (Parsable (parseParam), finish, get, html, param, post, readEither, scotty, status)
import Prelude (Either (Right), Fractional ((/)), RealFrac (round), Show (show))

instance Parsable Muscle where
  parseParam = readEither

instance Parsable SorenessValue where
  parseParam = readEither

instance Parsable Intensity where
  parseParam = Right . Intensity . TL.toStrict

instance Parsable ExerciseName where
  parseParam = Right . ExerciseName . TL.toStrict

packShow :: (Show a) => a -> Text
packShow = pack . show

htmlSkeleton :: L.Html a -> L.Html a
htmlSkeleton content = do
  L.doctypehtml_ $ do
    L.head_ $ do
      LX.useHtmx
      L.meta_ [L.charset_ "utf-8"]
      L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
      L.title_ "myocardio - power up your routines"
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css", L.rel_ "stylesheet"]
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css", L.rel_ "stylesheet"]
    L.body_ $ do
      L.div_ [L.class_ "container"] $ do
        content

sorenessValueToEmoji :: SorenessValue -> Text
sorenessValueToEmoji VerySore = "😭"
sorenessValueToEmoji LittleSore = "😕"
sorenessValueToEmoji _otherSoreness = ""

sorenessOutput :: Database -> L.Html ()
sorenessOutput database = do
  let muscleToSoreness :: Muscle -> Maybe Soreness
      muscleToSoreness muscle' =
        case maximumByMay (comparing (.time)) $ filter (\historyEntry -> historyEntry.muscle == muscle') database.sorenessHistory of
          -- If the latest value is not sore, then don't display soreness at all.
          Just (Soreness {soreness = NotSore}) -> Nothing
          otherValue -> otherValue
      sorenessToHtml :: Soreness -> L.Html ()
      sorenessToHtml soreness' = L.li_ do
        L.span_ [L.class_ "me-1"] (L.toHtml (sorenessValueToEmoji soreness'.soreness))
        L.strong_ [L.class_ "me-1"] (L.toHtml (packShow soreness'.muscle))
        L.a_
          [ LX.hxPost_ ("/reset-soreness?muscle=" <> packShow soreness'.muscle),
            L.href_ "#",
            LX.hxTarget_ ("#" <> anchorSorenessOutput)
          ]
          "Reset"
  L.div_ [L.id_ "soreness-output"] do
    L.ul_ (mapM_ sorenessToHtml $ mapMaybe muscleToSoreness allMuscles)

urlNewSoreness :: (IsString a) => a
urlNewSoreness = "/new-soreness"

urlAddToWorkout :: (IsString a) => a
urlAddToWorkout = "/add-to-workout"

addToWorkoutExerciseName :: (IsString a) => a
addToWorkoutExerciseName = "exercise-name"

addToWorkoutIntensity :: (IsString a) => a
addToWorkoutIntensity = "intensity"

newSorenessMuscle :: (IsString a) => a
newSorenessMuscle = "muscle"

newSorenessHowSore :: (IsString a) => a
newSorenessHowSore = "how-sore"

anchorSorenessOutput :: (IsString a) => a
anchorSorenessOutput = "soreness-output"

anchorCurrentWorkout :: (IsString a) => a
anchorCurrentWorkout = "current-workout"

sorenessInputAndOutput :: Database -> L.Html ()
sorenessInputAndOutput database = do
  L.h1_ do
    L.i_ [L.class_ "bi-graph-down-arrow me-2"] mempty
    L.span_ "Soreness"
  L.form_ [L.class_ "row"] do
    L.div_ [L.class_ "col"] do
      L.div_ [L.class_ "form-floating"] do
        let muscleOption :: Muscle -> L.Html ()
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
          LX.hxTarget_ ("#" <> anchorSorenessOutput)
        ]
        "Submit"
  sorenessOutput database

dayDiffText :: UTCTime -> UTCTime -> Text
dayDiffText currentTime before =
  let dayDiff = round $ diffUTCTime currentTime before / nominalDay
   in case dayDiff :: Int of
        0 -> "today"
        1 -> "yesterday"
        n -> packShow n <> " days ago"

currentWorkoutHtml :: Database -> L.Html ()
currentWorkoutHtml database =
  L.div_ [L.id_ anchorCurrentWorkout] do
    case database.currentTraining of
      [] -> mempty
      currentExercises -> do
        L.h1_ do
          L.i_ [L.class_ "bi-joystick me-2"] mempty
          L.span_ "Current Workout"
        L.ol_ $ forM_ currentExercises \exWithIn -> do
          L.li_ do
            L.span_ do
              L.a_ [L.href_ ("#description-" <> packShow exWithIn.exercise.name)] (L.strong_ (L.toHtml (packShow exWithIn.exercise.name)))
            L.span_ ", "
            L.span_ [L.class_ "me-2"] (L.toHtml (intensityToText exWithIn.intensity))
            L.a_
              [ LX.hxPost_ ("/reset-current-workout?exercise-name=" <> packShow exWithIn.exercise.name),
                L.href_ "#",
                LX.hxTarget_ ("#" <> anchorCurrentWorkout)
              ]
              "Reset"

trainingHtml :: UTCTime -> Database -> L.Html ()
trainingHtml currentTime database = do
  L.h1_ do
    L.i_ [L.class_ "bi-hand-thumbs-up me-2"] mempty
    L.span_ "Training"
  let exerciseWithIntensityTrainsMuscle :: Muscle -> ExerciseWithIntensity Exercise -> Bool
      exerciseWithIntensityTrainsMuscle muscle' e = muscle' `elem` e.exercise.muscles
      lastTraining :: Muscle -> L.Html ()
      lastTraining muscle' =
        case maximumByMay
          (comparing (.time))
          (filter (exerciseWithIntensityTrainsMuscle muscle') database.pastExercises) of
          Nothing -> L.p_ do
            L.strong_ "never trained!"
          Just exWithIntensity ->
            L.p_
              [L.class_ "text-muted"]
              ( L.toHtml $
                  "Last training: "
                    <> dayDiffText currentTime exWithIntensity.time
                    <> " "
                    <> packShow exWithIntensity.exercise.name
              )
      exercisesByCategory :: Muscle -> [NE.NonEmpty Exercise]
      exercisesByCategory muscle' = NE.groupAllWith (.category) $ filter (\e -> muscle' `elem` e.muscles) database.exercises
      outputExercise :: Exercise -> L.Html ()
      outputExercise e = do
        let pastExercise = maximumByMay (comparing (.time)) $ filter (\pe -> pe.exercise.name == e.name) database.pastExercises
        L.form_ do
          L.input_
            [ L.type_ "hidden",
              L.name_ addToWorkoutExerciseName,
              L.value_ (packShow e.name)
            ]
          L.div_ [L.class_ "input-group"] do
            L.button_
              [ L.type_ "button",
                LX.hxPost_ urlAddToWorkout,
                L.class_ "btn btn-sm btn-primary",
                LX.hxTarget_ ("#" <> anchorCurrentWorkout)
              ]
              do
                L.i_ [L.class_ "bi-journal-plus"] mempty
                L.span_ "Add to workout"
            L.input_
              [ L.class_ "form-control",
                L.value_ (maybe "" (intensityToText . (.intensity)) pastExercise),
                L.name_ addToWorkoutIntensity,
                L.type_ "text"
              ]
            L.span_ [L.class_ "input-group-text"] (L.toHtml $ packShow e.name)
            L.span_ [L.class_ "text-muted input-group-text"] $ L.toHtml $ case pastExercise of
              Nothing -> "never executed!"
              Just lastExecution -> dayDiffText currentTime lastExecution.time
      muscleToTrainingHtml :: Muscle -> L.Html ()
      muscleToTrainingHtml muscle' = do
        L.h2_ (L.toHtml $ packShow muscle')
        lastTraining muscle'
        forM_ (exercisesByCategory muscle') \exercises' -> do
          L.h3_ (L.toHtml $ packShow $ (.category) $ NE.head exercises')
          forM_ exercises' outputExercise

  mapM_ muscleToTrainingHtml allMuscles

exercisesHtml :: Database -> L.Html ()
exercisesHtml db = do
  L.h1_ do
    L.i_ [L.class_ "bi-box2-heart me-2"] mempty
    L.span_ "Exercise Descriptions"
  forM_ db.exercises \exercise' -> do
    L.h4_ [L.id_ ("description-" <> packShow exercise'.name)] (L.toHtml (packShow exercise'.name))
    L.toHtmlRaw $ commonmarkToHtml [] [] exercise'.description

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
  post "/reset-soreness" do
    muscle' <- param "muscle"

    currentTime <- liftIO getCurrentTime

    db <-
      modifyDb
        ( \db ->
            db
              { sorenessHistory =
                  Soreness
                    { time = currentTime,
                      muscle = muscle',
                      soreness = NotSore
                    }
                    : db.sorenessHistory
              }
        )

    html $ renderText $ sorenessOutput db
  post urlAddToWorkout do
    exerciseName :: ExerciseName <- param addToWorkoutExerciseName
    intensity' :: Intensity <- param addToWorkoutIntensity
    currentTime <- liftIO getCurrentTime
    readDb <- readDatabase
    case find (\e -> e.name == exerciseName) readDb.exercises of
      Nothing -> do
        status status400
        finish
      Just exercise' -> do
        db' <-
          modifyDb
            ( \db ->
                db
                  { currentTraining =
                      ExerciseWithIntensity
                        { exercise = exercise',
                          intensity = intensity',
                          time = currentTime
                        }
                        : db.currentTraining
                  }
            )
        html $ renderText $ currentWorkoutHtml db'

  post "/reset-current-workout" do
    exercise' :: ExerciseName <- param "exercise-name"
    db <-
      modifyDb
        ( \db ->
            db
              { currentTraining =
                  filter (\exWithIn -> exWithIn.exercise.name /= exercise') db.currentTraining
              }
        )
    html $ renderText $ currentWorkoutHtml db

  get "/" do
    db <- readDatabase
    currentTime <- liftIO getCurrentTime
    html $ renderText $ htmlSkeleton $ do
      sorenessInputAndOutput db
      L.hr_ [L.class_ "mb-3"]
      currentWorkoutHtml db
      L.hr_ [L.class_ "mb-3"]
      trainingHtml currentTime db
      L.hr_ [L.class_ "mb-3"]
      exercisesHtml db
