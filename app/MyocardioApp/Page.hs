module MyocardioApp.Page(Page(..)) where

import Prelude()

import qualified MyocardioApp.Pages.MainPage as MainPage
import qualified MyocardioApp.Pages.MusclesPage as MusclesPage

data Page = PageMain MainPage.Model | PageMuscles MusclesPage.Model
