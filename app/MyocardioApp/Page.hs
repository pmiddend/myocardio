module MyocardioApp.Page(Page(..), isPageMain, isPageMuscles) where

import Prelude()

import qualified MyocardioApp.Pages.MainPage as MainPage
import qualified MyocardioApp.Pages.MusclesPage as MusclesPage
import Data.Bool (Bool (False, True))

data Page = PageMain MainPage.Model | PageMuscles MusclesPage.Model

isPageMain :: Page -> Bool
isPageMain (PageMain _) = True
isPageMain _ = False

isPageMuscles :: Page -> Bool
isPageMuscles (PageMuscles _) = True
isPageMuscles _ = False
