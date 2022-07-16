module MyocardioApp.UpdateResult(UpdateResult(..)) where

import Prelude()

data UpdateResult model = UpdateResultContinue model | UpdateResultHalt model
