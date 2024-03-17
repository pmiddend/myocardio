{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import CMarkGFM (commonmarkToHtml)
import Control.Applicative (Applicative (pure))
import Control.Monad (when, (>>=))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Hash.SHA256 (hashlazy)
import Data.Bool (Bool (True), (&&))
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable (Foldable (elem), find, forM_, mapM_, notElem)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (filter, zip)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (Maybe (Just, Nothing), isJust, isNothing, mapMaybe, maybe)
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord ((<), (>)), comparing)
import Data.Semigroup (Semigroup ((<>)))
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text, pack, replace, toLower, unpack)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime (utctDay, utctDayTime), diffUTCTime, getCurrentTime, nominalDay)
import Lucid (renderText)
import qualified Lucid as L
import Lucid.Base (makeAttributes)
import MyocardioApp.Database
  ( Category (Endurance, Mobility, Strength, Stretch),
    Database,
    DatabaseF (currentTraining, pastExercises, sorenessHistory),
    Exercise (Exercise, category, description, fileReferences, muscles, name),
    ExerciseName (ExerciseName),
    ExerciseWithIntensity (ExerciseWithIntensity, exercise, intensity, time),
    FileReference (FileReference),
    Intensity (Intensity),
    Muscle,
    Soreness (Soreness, muscle, soreness, time),
    SorenessValue (LittleSore, NotSore, VerySore),
    allCategories,
    allMuscles,
    exercises,
    intensityToText,
    modifyDb,
    readDatabase,
  )
import MyocardioApp.Htmx (SwapType (SwapOuterHtml))
import qualified MyocardioApp.Htmx as LX
import Network.HTTP.Types.Status (status400)
import Network.Wai.Parse (FileInfo (fileContent))
import Safe (maximumByMay)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.IO (FilePath, IO)
import Web.Scotty (ActionM, File, Parsable (parseParam, parseParamList), file, files, finish, formParam, formParams, get, html, pathParam, post, readEither, scotty, status)
import Prelude (Either (Left, Right), Enum (succ), Fractional ((/)), RealFrac (round), Show (show), Traversable (traverse))

instance (Parsable a) => Parsable (NE.NonEmpty a) where
  parseParam v =
    case parseParamList v of
      Left e -> Left e
      Right v' -> case NE.nonEmpty v' of
        Nothing -> Left "list with no elements"
        Just nonEmptyList -> Right nonEmptyList

instance Parsable Muscle where
  parseParam = readEither

instance Parsable Category where
  parseParam = readEither

instance Parsable SorenessValue where
  parseParam = readEither

instance Parsable Intensity where
  parseParam = Right . Intensity . TL.toStrict

instance Parsable ExerciseName where
  parseParam = Right . ExerciseName . TL.toStrict

newtype HtmlId = HtmlId Text

htmlIdFromText :: Text -> Text
htmlIdFromText = replace " " "_"

makeId :: HtmlId -> L.Attributes
makeId (HtmlId i) = L.id_ i

makeTarget :: HtmlId -> L.Attributes
makeTarget (HtmlId i) = LX.hxTarget_ ("#" <> i)

packShow :: (Show a) => a -> Text
packShow = pack . show

idTopLevelContainer :: (IsString a) => a
idTopLevelContainer = "top-level-container"

htmlSkeleton :: CurrentPage -> L.Html () -> L.Html ()
htmlSkeleton page content = do
  L.doctypehtml_ $ do
    L.head_ $ do
      LX.useHtmx
      L.meta_ [L.charset_ "utf-8"]
      L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
      L.title_ "myocardio - power up your routines"
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css", L.rel_ "stylesheet"]
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css", L.rel_ "stylesheet"]
    L.body_ $ do
      headerHtml page
      L.div_ [L.class_ "container", L.id_ idTopLevelContainer] do
        content
      -- Not sure if we need the bootstrap JS, and it must save some bandwidth, so leave it out maybe
      L.script_ [L.src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"] ("" :: Text)

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

urlNewExercise :: (IsString a) => a
urlNewExercise = "/partial/new-exercise"

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

idCurrentWorkout :: HtmlId
idCurrentWorkout = HtmlId "current-workout"

icon :: Text -> L.Html ()
icon name' = L.i_ [L.class_ ("bi-" <> name' <> " me-2")] mempty

icon' :: Text -> L.Html ()
icon' name' = L.i_ [L.class_ ("bi-" <> name')] mempty

sorenessInputAndOutput :: Database -> L.Html ()
sorenessInputAndOutput database = do
  L.h1_ do
    icon "graph-down-arrow"
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
        L.input_ [L.class_ "form-check-input", L.type_ "radio", L.name_ newSorenessHowSore, L.id_ "verysore", L.value_ (packShow VerySore), L.checked_]
        L.label_ [L.class_ "form-check-label", L.for_ "verysore"] (L.toHtml $ sorenessValueToEmoji VerySore <> " VERY")
      L.div_ [L.class_ "form-check form-check-inline"] do
        L.input_ [L.class_ "form-check-input", L.type_ "radio", L.name_ newSorenessHowSore, L.id_ "alittle", L.value_ (packShow LittleSore)]
        L.label_ [L.class_ "form-check-label", L.for_ "alittle"] (L.toHtml $ sorenessValueToEmoji LittleSore <> " A LITTLE")
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

urlCommitWorkout :: (IsString a) => a
urlCommitWorkout = "/partials/commit-workout"

currentWorkoutHtml :: Database -> L.Html ()
currentWorkoutHtml database =
  L.div_ [makeId idCurrentWorkout] do
    case database.currentTraining of
      [] -> mempty
      currentExercises -> do
        L.a_ [L.href_ "#exercise-list", L.class_ "icon-link mb-2"] do
          "Go to exercise list"
          icon "arrow-right"
        L.h1_ do
          icon "joystick"
          L.span_ "Current Workout"
        let musclesInvolved = currentExercises >>= (NE.toList . (.muscles) . (.exercise))
            musclesMissing = Set.toList $ Set.fromList allMuscles `Set.difference` Set.fromList musclesInvolved
        L.div_ [L.class_ "gap-1 mb-3"] do
          L.span_ [L.class_ "text-muted me-1"] "Trained: "
          forM_ musclesInvolved \muscle' -> L.span_ [L.class_ "badge text-bg-success me-1"] (L.toHtml $ packShow muscle')
        L.div_ [L.class_ "gap-1 mb-3"] do
          L.span_ [L.class_ "text-muted me-1"] "Missing: "
          forM_ musclesMissing \muscle' -> L.span_ [L.class_ "badge text-bg-warning me-1"] (L.toHtml $ packShow muscle')
        forM_ currentExercises \exWithIn -> do
          L.div_ [L.class_ "mb-3 card"] do
            L.div_
              [ L.class_ "carousel slide",
                L.id_ ("carousel-" <> htmlIdFromText (packShow exWithIn.exercise.name)),
                makeAttributes "data-bs-theme" "dark"
              ]
              do
                L.div_
                  [L.class_ "carousel-inner"]
                  ( forM_ (zip [0 ..] exWithIn.exercise.fileReferences) \(i, FileReference fileRef) ->
                      L.div_
                        [ L.class_ "carousel-item" <> if i == (0 :: Int) then L.class_ "active" else mempty
                        ]
                        $ L.img_ [L.src_ ("/" <> pack uploadedFileDir <> "/" <> fileRef), L.class_ "d-block w-100"]
                  )
                L.button_
                  [ L.type_ "button",
                    L.class_ "carousel-control-prev",
                    makeAttributes "data-bs-target" ("#carousel-" <> htmlIdFromText (packShow exWithIn.exercise.name)),
                    makeAttributes "data-bs-slide" "prev"
                  ]
                  (L.span_ [L.class_ "carousel-control-prev-icon"] mempty)
                L.button_
                  [ L.type_ "button",
                    L.class_ "carousel-control-next",
                    makeAttributes "data-bs-target" ("#carousel-" <> htmlIdFromText (packShow exWithIn.exercise.name)),
                    makeAttributes "data-bs-slide" "next"
                  ]
                  (L.span_ [L.class_ "carousel-control-next-icon"] mempty)
            L.div_ [L.class_ "card-body"] do
              L.h6_ [L.class_ "card-title"] do
                L.strong_ (L.toHtml (packShow exWithIn.exercise.name))
              L.h6_ [L.class_ "card-subtitle"] (L.toHtml (intensityToText exWithIn.intensity))
              L.p_ [L.class_ "card-text"] do
                L.toHtmlRaw $ commonmarkToHtml [] [] exWithIn.exercise.description
                L.button_
                  [ LX.hxPost_ ("/reset-current-workout?exercise-name=" <> packShow exWithIn.exercise.name),
                    makeTarget idCurrentWorkout,
                    L.type_ "button",
                    L.class_ "btn btn-secondary"
                  ]
                  do
                    icon "trash"
                    L.span_ "Don't use"

        L.form_ do
          L.button_
            [ L.type_ "submit",
              L.class_ "btn btn-primary",
              LX.hxPost_ urlCommitWorkout,
              makeTarget idCurrentWorkout
            ]
            do
              icon "send"
              L.span_ "Commit"

trainingHtml :: UTCTime -> Database -> Category -> L.Html ()
trainingHtml currentTime database category' = do
  L.h1_ [L.id_ "exercise-list"] do
    icon "hand-thumbs-up"
    L.span_ "Exercise list"
  L.div_ [L.class_ "text-bg-light p-2"] do
    L.ul_ $ forM_ allMuscles \muscle' -> do
      L.li_ (L.a_ [L.href_ ("#training-section-" <> packShow muscle')] (L.toHtml (packShow muscle')))
  let exercisePool = filter (\e -> e.category == category') database.exercises
      exerciseWithIntensityTrainsMuscle :: Muscle -> ExerciseWithIntensity Exercise -> Bool
      exerciseWithIntensityTrainsMuscle muscle' e = muscle' `elem` e.exercise.muscles
      lastTraining :: Muscle -> L.Html ()
      lastTraining muscle' =
        case maximumByMay
          (comparing (.time))
          (filter (exerciseWithIntensityTrainsMuscle muscle') database.pastExercises) of
          Nothing -> L.p_ do
            L.em_ "never trained!"
          Just exWithIntensity ->
            L.p_
              [L.class_ "text-muted"]
              ( L.em_ $
                  L.toHtml $
                    "Last training: "
                      <> dayDiffText currentTime exWithIntensity.time
                      <> ", "
                      <> packShow exWithIntensity.exercise.name
                      <> " ("
                      <> toLower (packShow exWithIntensity.exercise.category)
                      <> ")"
              )
      exercisesForMuscle :: Muscle -> [Exercise]
      exercisesForMuscle muscle' = filter (\e -> muscle' `elem` e.muscles) exercisePool
      outputExercise :: Muscle -> Exercise -> L.Html ()
      outputExercise muscle' exerciseToOutput = do
        let lastExecutionOfThisExercise :: Maybe (ExerciseWithIntensity Exercise)
            lastExecutionOfThisExercise =
              maximumByMay (comparing (.time)) $ filter (\pe -> pe.exercise.name == exerciseToOutput.name) database.pastExercises
            beginningOfDayAfterExecution :: Maybe UTCTime
            beginningOfDayAfterExecution =
              (\x -> x.time {utctDay = succ x.time.utctDay, utctDayTime = 1}) <$> lastExecutionOfThisExercise
            nextExerciseAfterThisContainingThisMuscle :: Maybe (ExerciseWithIntensity Exercise)
            nextExerciseAfterThisContainingThisMuscle =
              beginningOfDayAfterExecution >>= \x -> find (\e -> e.time > x && muscle' `elem` e.exercise.muscles) database.pastExercises
            pastTimeReadable = case lastExecutionOfThisExercise of
              Nothing -> "never executed!"
              Just lastExecutionInstance ->
                let firstSorenessBetweenExecutions :: Maybe Soreness
                    firstSorenessBetweenExecutions =
                      find
                        ( \soreness' ->
                            soreness'.time
                              > lastExecutionInstance.time
                              && soreness'.muscle
                                == muscle'
                              && case nextExerciseAfterThisContainingThisMuscle of
                                Nothing -> True
                                Just nextExercise ->
                                  soreness'.time < nextExercise.time
                        )
                        database.sorenessHistory
                 in dayDiffText currentTime lastExecutionInstance.time <> case firstSorenessBetweenExecutions of
                      Nothing -> ", no soreness"
                      Just lastSoreness -> ", soreness: " <> sorenessValueToEmoji lastSoreness.soreness
        L.form_ do
          L.input_
            [ L.type_ "hidden",
              L.name_ addToWorkoutExerciseName,
              L.value_ (packShow exerciseToOutput.name)
            ]
          L.h5_ $ do
            L.span_ $ L.toHtml $ packShow exerciseToOutput.name
            when (isJust lastExecutionOfThisExercise && isNothing nextExerciseAfterThisContainingThisMuscle) do
              L.span_ [L.class_ "ms-2 badge text-bg-secondary"] "Last"
          L.p_ [L.class_ "text-muted"] (L.toHtml $ "Last: " <> pastTimeReadable)
          L.div_ [L.class_ "input-group mb-3"] do
            L.button_
              [ L.type_ "button",
                LX.hxPost_ urlAddToWorkout,
                L.class_ "btn btn-sm btn-primary",
                makeTarget idCurrentWorkout
              ]
              do
                icon "journal-plus"
                L.span_ "Add to workout"
            L.input_
              [ L.class_ "form-control",
                L.value_ (maybe "" (intensityToText . (.intensity)) lastExecutionOfThisExercise),
                L.name_ addToWorkoutIntensity,
                L.type_ "text",
                L.placeholder_ "Enter intensity here"
              ]
          L.button_
            [ L.class_ "btn btn-info",
              L.type_ "button btn-sm",
              LX.hxGet_ ("/partials/exercise-description/" <> packShow exerciseToOutput.name),
              LX.hxSwap_ SwapOuterHtml
            ]
            "Show details"
          L.hr_ []

      muscleToTrainingHtml :: Muscle -> L.Html ()
      muscleToTrainingHtml muscle' = do
        L.h2_ [L.id_ ("training-section-" <> htmlIdFromText (packShow muscle'))] (L.toHtml $ packShow muscle')
        L.div_ [L.class_ "ms-3"] do
          lastTraining muscle'
          L.div_ [L.class_ "text-bg-light p-3 border rounded"] $ forM_ (exercisesForMuscle muscle') (outputExercise muscle')

  mapM_ muscleToTrainingHtml allMuscles

urlNewExerciseCancel :: (IsString a) => a
urlNewExerciseCancel = "/partial/new-exercise-cancel"

urlNewExerciseSubmit :: (IsString a) => a
urlNewExerciseSubmit = "/partial/new-exercise-submit"

exerciseFormMusclesParam :: (IsString a) => a
exerciseFormMusclesParam = "muscles"

exerciseFormFilesToDeleteParam :: (IsString a) => a
exerciseFormFilesToDeleteParam = "filesToDelete"

exerciseFormCategoryParam :: (IsString a) => a
exerciseFormCategoryParam = "category"

exerciseFormNameParam :: (IsString a) => a
exerciseFormNameParam = "name"

exerciseFormDescriptionParam :: (IsString a) => a
exerciseFormDescriptionParam = "description"

idExerciseForm :: HtmlId
idExerciseForm = HtmlId "new-exercise-form"

exerciseFormHtml :: ExerciseName -> Category -> [Muscle] -> [FileReference] -> Text -> L.Html ()
exerciseFormHtml editName editCategory editMuscles fileRefs description' =
  L.form_ [LX.hxEncoding_ "multipart/form-data"] do
    L.div_ [L.class_ "form-floating mb-3"] do
      L.input_
        [ L.class_ "form-control",
          L.id_ "exercise-name",
          L.type_ "text",
          L.name_ exerciseFormNameParam,
          L.value_ (packShow editName)
        ]
      L.label_ [L.for_ "exercise-name"] "Name"

    L.h5_ "Category"
    L.div_ [L.class_ "d-flex justify-content-evenly mb-3"] $ forM_ allCategories \category' -> do
      L.input_
        [ L.class_ "btn-check",
          L.id_ ("category-" <> htmlIdFromText (packShow category')),
          L.type_ "radio",
          L.name_ exerciseFormCategoryParam,
          if category' == editCategory then L.checked_ else mempty,
          L.value_ (packShow category')
        ]
      L.label_
        [L.for_ ("category-" <> packShow category'), L.class_ "btn btn-outline-info"]
        $ L.toHtml
        $ packShow category'

    L.h5_ "Muscles involved"
    L.div_ [L.class_ "mb-3"] $ forM_ allMuscles \muscle' -> do
      L.input_
        [ L.class_ "btn-check",
          L.id_ ("muscle-" <> htmlIdFromText (packShow muscle')),
          L.type_ "checkbox",
          L.name_ exerciseFormMusclesParam,
          L.value_ (packShow muscle'),
          if muscle' `elem` editMuscles then L.checked_ else mempty
        ]
      L.label_
        [L.for_ ("muscle-" <> packShow muscle'), L.class_ "btn btn-outline-secondary me-2"]
        $ L.toHtml
        $ packShow muscle'

    L.textarea_
      [ L.class_ "form-control mb-3",
        L.placeholder_ "Description",
        L.name_ exerciseFormDescriptionParam
      ]
      (L.toHtml description')

    L.h5_ "Attached files"
    forM_ fileRefs \(FileReference fileRef) -> do
      L.div_ [L.class_ "d-flex flex-row mb-3 align-items-center justify-content-evenly"] do
        L.div_ do
          L.div_ [L.class_ "form-check"] do
            L.input_
              [ L.type_ "checkbox",
                L.class_ "form-check-input",
                L.name_ exerciseFormFilesToDeleteParam,
                L.id_ ("delete-" <> fileRef),
                L.value_ fileRef
              ]
            L.label_ [L.class_ "form-check-label", L.for_ ("delete-" <> fileRef)] "Delete this"
        L.div_ (exerciseImageHtml (FileReference fileRef))

    L.div_ do
      L.label_ [L.for_ "file-upload", L.class_ "form-label"] "Files to attach"
      L.input_ [L.class_ "form-control", L.type_ "file", L.multiple_ "true", L.id_ "file-upload", L.name_ "attached-files"]

    L.div_ [L.class_ "hstack gap-3"] do
      L.button_
        [ L.type_ "submit",
          L.class_ "btn btn-primary",
          LX.hxPost_ urlNewExerciseSubmit,
          LX.hxTarget_ ("#" <> idTopLevelContainer)
        ]
        "Submit"
      L.button_
        [ L.type_ "submit",
          L.class_ "btn btn-outline-warning",
          LX.hxPost_ urlNewExerciseCancel,
          makeTarget idExerciseForm
        ]
        "Cancel edit"

newExerciseButtonHtml :: L.Html ()
newExerciseButtonHtml =
  L.button_
    [ L.type_ "submit",
      L.class_ "btn btn-primary",
      LX.hxPost_ urlNewExercise,
      makeTarget idExerciseForm
    ]
    do
      icon "plus-lg"
      L.span_ "New exercise"

exerciseImageHtml :: FileReference -> L.Html ()
exerciseImageHtml (FileReference fileRef) = L.figure_ [L.class_ "figure"] $ L.img_ [L.src_ ("/" <> pack uploadedFileDir <> "/" <> fileRef), L.class_ "figure-img img-fluid rounded"]

exerciseDescriptionHtml :: Exercise -> L.Html ()
exerciseDescriptionHtml e = do
  L.toHtmlRaw $ commonmarkToHtml [] [] e.description
  forM_ e.fileReferences exerciseImageHtml

exercisesHtml :: Database -> L.Html ()
exercisesHtml db = do
  L.div_ [makeId idExerciseForm, L.class_ "mb-3"] newExerciseButtonHtml
  L.hr_ []
  L.h2_ do
    icon "box2-heart"
    L.span_ "Exercise Descriptions"
  forM_ db.exercises \exercise' -> do
    L.h3_ [L.id_ ("description-" <> htmlIdFromText (packShow exercise'.name))] do
      L.button_
        [ L.class_ "btn btn-sm btn-secondary me-2",
          LX.hxGet_
            ("/partial/edit-exercise/" <> packShow exercise'.name),
          makeTarget idExerciseForm
        ]
        (icon' "pencil-square")
      L.toHtml (packShow exercise'.name)
    L.div_ [L.class_ "hstack gap-1 mb-3"] do
      L.span_ [L.class_ "badge text-bg-dark"] (L.toHtml $ packShow exercise'.category)
      forM_ exercise'.muscles \muscle' -> L.span_ [L.class_ "badge text-bg-info"] (L.toHtml $ packShow muscle')
    L.div_ [L.class_ "alert alert-light"] (exerciseDescriptionHtml exercise')

data CurrentPage
  = PageSoreness
  | PageStrength
  | PageStretch
  | PageEndurance
  | PageMobility
  | PageExercises
  deriving (Eq)

headerHtml :: CurrentPage -> L.Html ()
headerHtml currentPage =
  let pages =
        [ (PageSoreness, "", "graph-down-arrow", "Soreness"),
          (PageStrength, "training/strength", "hammer", "Strength"),
          (PageEndurance, "training/endurance", "person-walking", "Endurance"),
          (PageStretch, "training/stretch", "rulers", "Stretch"),
          (PageMobility, "training/mobility", "airplane", "Mobility"),
          (PageExercises, "exercises", "box2-heart", "Exercises")
        ]
      makeItem :: (CurrentPage, Text, Text, Text) -> L.Html ()
      makeItem (enum, url, iconName, title) =
        L.li_ [L.class_ "nav-item"] do
          L.a_
            [ L.href_ ("/" <> url),
              L.class_ "nav-link",
              if currentPage == enum then L.class_ "active" else mempty
            ]
            do
              icon iconName
              L.toHtml title
   in L.header_ [L.class_ "d-flex justify-content-center py-3"] do
        L.ul_ [L.class_ "nav nav-pills"] (mapM_ makeItem pages)

uploadedFileDir :: FilePath
uploadedFileDir = "uploaded-files"

uploadSingleFile :: (MonadIO m) => File -> m FileReference
uploadSingleFile (_, fileInfo) = do
  let fileHashText :: Text
      fileHashText = TE.decodeUtf8 $ Base16.encode (hashlazy (fileContent fileInfo))
  liftIO $ do
    createDirectoryIfMissing True uploadedFileDir
    BSL.writeFile (uploadedFileDir <> "/" <> unpack fileHashText) (fileContent fileInfo)
  pure (FileReference fileHashText)

paramValues :: Text -> ActionM [Text]
paramValues desiredParamName =
  mapMaybe
    (\(paramName, paramValue) -> if paramName == desiredParamName then Just paramValue else Nothing)
    <$> formParams

main :: IO ()
main = scotty 3000 do
  post urlNewSoreness do
    muscle' <- formParam newSorenessMuscle
    howSore' <- formParam newSorenessHowSore

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
    muscle' <- formParam "muscle"

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
  post urlNewExerciseSubmit do
    uploadedFiles <- files
    writtenFiles <- traverse uploadSingleFile uploadedFiles
    musclesRaw <- paramValues exerciseFormMusclesParam
    case traverse (parseParam . TL.fromStrict) musclesRaw of
      Left _parseError -> do
        status status400
        finish
      Right muscles' -> do
        case NE.nonEmpty muscles' of
          Nothing -> do
            status status400
            finish
          Just muscles'' -> do
            category' <- formParam exerciseFormCategoryParam
            description' <- formParam exerciseFormDescriptionParam
            name' <- formParam exerciseFormNameParam
            toDelete <- paramValues exerciseFormFilesToDeleteParam
            forM_ toDelete (liftIO . removeFile . unpack . (\fn -> pack uploadedFileDir <> "/" <> fn))
            newDb <- modifyDb \db ->
              let existingExercise = find (\e -> e.name == name') db.exercises
               in db
                    { exercises =
                        -- Add a new one
                        Exercise
                          { muscles = muscles'',
                            category = category',
                            description = description',
                            name = name',
                            fileReferences =
                              maybe
                                []
                                (\existing -> filter (\(FileReference fileRef) -> fileRef `notElem` toDelete) existing.fileReferences)
                                existingExercise
                                <> writtenFiles
                          }
                          -- ...and filter out the existing one
                          : filter (\e -> e.name /= name') db.exercises
                    }
            html $ renderText $ exercisesHtml newDb

  post urlNewExerciseCancel do
    html $ renderText newExerciseButtonHtml

  get "/partial/edit-exercise/:exercise-name" do
    readDb <- readDatabase
    exerciseName <- pathParam "exercise-name"
    case find (\e -> e.name == exerciseName) readDb.exercises of
      Nothing -> do
        status status400
        finish
      Just exercise' -> do
        html $ renderText $ exerciseFormHtml exercise'.name exercise'.category (NE.toList exercise'.muscles) exercise'.fileReferences exercise'.description

  post urlNewExercise do
    html $ renderText $ exerciseFormHtml (ExerciseName "") Strength [] [] ""

  post urlAddToWorkout do
    exerciseName :: ExerciseName <- formParam addToWorkoutExerciseName
    intensity' :: Intensity <- formParam addToWorkoutIntensity
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

  post urlCommitWorkout do
    newDb <- modifyDb \db ->
      db {currentTraining = [], pastExercises = db.currentTraining <> db.pastExercises}
    html $ renderText $ currentWorkoutHtml newDb

  post "/reset-current-workout" do
    exercise' :: ExerciseName <- formParam "exercise-name"
    db <-
      modifyDb
        ( \db ->
            db
              { currentTraining =
                  filter (\exWithIn -> exWithIn.exercise.name /= exercise') db.currentTraining
              }
        )
    html $ renderText $ currentWorkoutHtml db

  get "/partials/exercise-description/:name" do
    exerciseName <- pathParam "name"
    readDb <- readDatabase
    case find (\e -> e.name == exerciseName) readDb.exercises of
      Nothing -> do
        status status400
        finish
      Just e -> html $ renderText $ exerciseDescriptionHtml e

  get "/exercises" do
    db <- readDatabase
    html $ renderText $ htmlSkeleton PageExercises $ exercisesHtml db

  get "/training/strength" do
    db <- readDatabase
    currentTime <- liftIO getCurrentTime
    html $ renderText $ htmlSkeleton PageStrength $ do
      currentWorkoutHtml db
      L.hr_ [L.class_ "mb-3"]
      trainingHtml currentTime db Strength

  get "/training/stretch" do
    db <- readDatabase
    currentTime <- liftIO getCurrentTime
    html $ renderText $ htmlSkeleton PageStretch $ do
      currentWorkoutHtml db
      L.hr_ [L.class_ "mb-3"]
      trainingHtml currentTime db Stretch

  get "/training/mobility" do
    db <- readDatabase
    currentTime <- liftIO getCurrentTime
    html $ renderText $ htmlSkeleton PageMobility $ do
      currentWorkoutHtml db
      L.hr_ [L.class_ "mb-3"]
      trainingHtml currentTime db Mobility

  get "/training/endurance" do
    db <- readDatabase
    currentTime <- liftIO getCurrentTime
    html $ renderText $ htmlSkeleton PageEndurance $ do
      currentWorkoutHtml db
      L.hr_ [L.class_ "mb-3"]
      trainingHtml currentTime db Endurance

  get "/" do
    db <- readDatabase
    html $ renderText $ htmlSkeleton PageSoreness $ sorenessInputAndOutput db

  get "/uploaded-files/:fn" do
    fileName <- pathParam "fn"
    file (uploadedFileDir <> "/" <> fileName)
