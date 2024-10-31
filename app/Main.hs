module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Web.Hyperbole
import Data.Maybe (fromMaybe)

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
  type Require Central = '[Presets, Results, Sidebar]
  
central :: Central -> CentralAction -> Eff es (View Central ())
central _ (ChangeSelectedTo x) = pure $ centralView x

centralPage :: (Hyperbole :> es) => Page es '[Central, Presets, Results, Sidebar]
centralPage = do
  -- message listens for any actions that the centralView triggers
  handle central $ handle presets $ handle results $ handle sidebar $ load $ do
    pure $ do
      el bold "Message Page"
      row (border 3 . pad 10 . gap 10) $ do
        hyper Sidebar $ sidebarView Nothing
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
      hyper Results $ resultsView Nothing

data Presets = Presets
  deriving (Show, Read, ViewId)

data PresetsAction = View Selected deriving (Show, Read, ViewAction)

instance HyperView Presets where
  type Action Presets = PresetsAction

presets :: (Hyperbole :> es) => Presets -> PresetsAction -> Eff es (View Presets ())
presets _ (View s) = pure $ presetsView s

presetsView :: Selected -> View Presets ()
presetsView s = do
  col (border 3 . pad 10) $ do
    text $ "viewing details for " `T.append` T.pack (show s)
    let variant x t = target Results $ button (ViewResults $ Just x) id t
    case s of
      A -> col id $ variant Result1 "Result1"
      B -> col id $ do
                      variant Result2 "Result2"                    
                      variant Result3 "Result3"
      C -> col id $ do
                      variant Result4 "Result4"
                      variant Result5 "Result5"
                      variant Result6 "Result6"

data Results = Results
  deriving (Show, Read, ViewId)

data ResultVariant = Result1 | Result2 | Result3 | Result4 | Result5 | Result6 | Result7 | Result8 deriving (Show, Eq, Read)

data ResultsAction = ViewResults (Maybe ResultVariant) deriving (Show, Read, ViewAction)

instance HyperView Results where
  type Action Results = ResultsAction

results :: (Hyperbole :> es) => Results -> ResultsAction -> Eff es (View Results ())
results _ (ViewResults x) = pure $ col (border 3 . pad 10) $ resultsView x

resultsView :: Maybe ResultVariant -> View Results ()
resultsView Nothing = col (border 3 . pad 10) $ el_ $ text "no results!"
resultsView (Just x) = col (border 3 . pad 10) $ 
                         case x of
                           Result1 -> text "one short result"
                           Result2 -> text "a different result"
                           Result3 -> text "this is a special result"
                           Result4 -> text "THIS IS NUUMBER FOOOUR!"
                           Result5 -> text "give me a high five"
                           Result6 -> do
                                        target Sidebar $ button (UpdateSidebar (Just "surprise!!")) id "click me to get surprise in sidebar"
                           Result7 -> target Results $ onLoad (ViewResults (Just Result8)) 0 (el_ "loading…")
                           Result8 -> text "you unlocked the secret chamber"

data Sidebar = Sidebar
  deriving (Show, Read, ViewId)

data SidebarAction = UpdateSidebar (Maybe Text)
  deriving (Show, Read, ViewAction)

instance HyperView Sidebar where
  type Action Sidebar = SidebarAction

sidebar :: Sidebar -> SidebarAction -> Eff es (View Sidebar ())
sidebar _ (UpdateSidebar x) = pure $ sidebarView x

sidebarView :: Maybe Text -> View Sidebar ()
sidebarView x = col (border 3 . pad 10) $ do
                  text $ fromMaybe "N/A" x
                  case x of
                    Just "surprise!!" -> target Results $ button (ViewResults (Just Result7)) id "set to Result7"
                    Nothing -> none
