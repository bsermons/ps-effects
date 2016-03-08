module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Functor.Aff as Func

import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling as SCR

import Control.Monad.Aff as Aff
import Control.Monad.Aff (Aff(), Canceler(), forkAff, runAff, later')
import Control.Monad.Aff.AVar (AVAR, takeVar, putVar, makeVar)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION, throwException, error)

import DOM as DOM
import DOM.HTML as DOM
import DOM.HTML.Window as DOM
import DOM.HTML.Types as DOM

import Halogen
import Halogen.Query as Q
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

import Window (WINDOW, WindowRef, closeWindow, openWindow, getProperty, hasProperty)

type AppEffects = (window :: WINDOW)


data State
  = Init
  | Waiting WindowRef (Canceler (avar :: AVAR, window :: WINDOW))
  | Done String


data Query a
  = OpenWindow a
  | GotResult String a
  | CancelWaiting a


initialState :: State
initialState = Init


{-| Fork an aff and return a canceler. 
-}
forkQuery
  :: forall s a f eff
   . Aff (avar :: AVAR | eff) a
  -> (a -> Action f)
  -> ComponentDSL s f (Aff (avar :: AVAR | eff)) (Canceler (avar :: AVAR | eff))
forkQuery aff act = do
  v <- liftAff' makeVar

  canceler <- liftAff' $ forkAff do
    res <- aff
    putVar v res

  subscribe
    $ EventSource
    $ SCR.producerToStallingProducer
    $ produce \emit -> do
      flip (Aff.runAff (\_ -> emit $ Right unit)) (takeVar v) \res -> do
        emit $ Left $ act res unit

  pure canceler


{-| Monitor an open window for a global property to be set and return the value.
-}
monitor :: forall eff a
   . WindowRef
  -> String
  -> Aff (window :: WINDOW | eff) a
monitor win prop = do
  res <- checkLater
  if res
    then do liftEff $ getProperty win prop
    else monitor win prop
  where
    checkForResult :: forall e. Eff (window :: WINDOW | e) Boolean
    checkForResult = hasProperty win prop

    checkLater = later' 1000 (liftEff checkForResult)

ui :: forall eff. Component State Query (Aff (HalogenEffects (window :: WINDOW | eff)))
ui = component render eval
  where
    render :: State -> ComponentHTML Query
    render s =
      H.div_
      [ (showMsg s)
      , H.button
        [ E.onClick (E.input_ OpenWindow), P.disabled $ (isBusy s) ]
        [ H.text "Effects Test" ]
      , H.button
        [ E.onClick (E.input_ CancelWaiting), P.disabled $ not (isBusy s) ]
        [ H.text "Cancel" ]
      ]

    showMsg (Done s) = H.div_ [ H.text $ "Success! " ++ s ]
    showMsg otherwise = H.div_ [H.text ""]

    eval :: Natural Query (ComponentDSL State Query (Aff (HalogenEffects (window :: WINDOW | eff))))
    eval (OpenWindow next) = do
      win <- liftEff' $ openWindow "http://www.google.com" "TestWin" "width=300,height=400"
      case win of
        (Just w) -> do
          canceler <- forkQuery (monitor w "_testResult") GotResult
          modify (\_ -> Waiting w canceler)
        Nothing ->
          modify (\_ -> Done "Could not open window. Check your popup blocker.")

      pure next

    eval (GotResult res next) = do
      modify (\_ -> Waiting)

    eval (CancelWaiting next) = do
      get >>= liftEff' <<< cancelAuth
      modify (\_ -> Done "User canceled auth.")
      pure next


    isBusy :: State -> Boolean
    isBusy (Waiting _ _) = true
    isBusy _ = false

    -- TODO: cancel the running monitor
    -- cancelAuth :: forall eff1. State -> Eff (window :: WINDOW, err :: EXCEPTION, avar :: AVAR | eff1) Unit
    cancelAuth (Waiting win canceler) = do
      -- Aff.launchAff $ Aff.cancel canceler (error "User canceled auth")
      closeWindow win
    cancelAuth _ = pure unit

-- | Run the app
main :: Eff (Aff (HalogenEffects AppEffects)) Unit
main =
  Aff.runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
