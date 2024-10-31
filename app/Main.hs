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
  type Require Central = '[Presets]
  
central :: Central -> CentralAction -> Eff es (View Central ())
central _ (ChangeSelectedTo x) = pure $ centralView x

centralPage :: (Hyperbole :> es) => Page es '[Central, Presets]
centralPage = do
  -- message listens for any actions that the centralView triggers
  handle central $ handle presets $ load $ do
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
    col (border 3 . pad 10) $ do
      hyper Presets $ presetsView s

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
