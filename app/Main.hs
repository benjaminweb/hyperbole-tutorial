module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Web.Hyperbole

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

main = do
  run 3000 $ do
    liveApp (basicDocument "Skeleton") (page $ centralPage)



data Central = Central
  deriving (Show, Read, ViewId)

data CentralAction = ChangeSelectedTo Selected
  deriving (Show, Read, ViewAction)

data Selected = A | B | C deriving (Show, Eq, Read)

instance HyperView Central where
  type Action Central = CentralAction
  type Require Central = '[Presets, Results]
  
central :: Central -> CentralAction -> Eff es (View Central ())
central _ (ChangeSelectedTo x) = pure $ centralView x

centralPage :: (Hyperbole :> es) => Page es '[Central, Presets, Results]
centralPage = do
  -- message listens for any actions that the centralView triggers
  handle central $ handle presets $ handle results $ load $ do
    pure $ do
      el bold "Message Page"
      hyper Central $ centralView A

centralView :: Selected -> View Central ()
centralView s = do
  col (border 3 . pad 10) $ do
    el_ $ text ("Selected: " `T.append` T.pack (show s))
    button (ChangeSelectedTo A) id "A"
    button (ChangeSelectedTo B) id "B"
    button (ChangeSelectedTo C) id "C"
    col (border 3 . pad 10 . gap 10) $ do
      hyper Presets $ presetsView s
      hyper Results $ resultsView Result1

data Presets = Presets
  deriving (Show, Read, ViewId)

data PresetsAction = View Selected deriving (Show, Read, ViewAction)

instance HyperView Presets where
  type Action Presets = PresetsAction

presets :: (Hyperbole :> es) => Presets -> PresetsAction -> Eff es (View Presets ())
presets _ (View s) = pure $ presetsView s

presetsView :: Selected -> View Presets ()
presetsView s = do
  text $ "viewing details for " `T.append` T.pack (show s)
  target Results $ button (ViewResults Result1) id "Result1"
  target Results $ button (ViewResults Result2) id "Result2"

data Results = Results
  deriving (Show, Read, ViewId)

data ResultVariant = Result1 | Result2 | Result3 deriving (Show, Eq, Read)

data ResultsAction = ViewResults ResultVariant deriving (Show, Read, ViewAction)

instance HyperView Results where
  type Action Results = ResultsAction

results :: (Hyperbole :> es) => Results -> ResultsAction -> Eff es (View Results ())
results _ (ViewResults x) = pure $ col (border 3 . pad 10) $ resultsView x

resultsView :: ResultVariant -> View Results ()
resultsView Result1 = text "one short result"
resultsView Result2 = text "a different result"
resultsView Result3 = text "this is a special result"
