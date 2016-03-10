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

import Halogen
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

import Window (WINDOW, WindowRef, closeWindow, openWindow, getProperty, hasProperty)

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (CONSOLE, log)

type IO = HalogenEffects (window :: WINDOW, console :: CONSOLE)


data State
  = Init
  | Waiting WindowRef (Canceler IO)
  | WaitingNoCanceler WindowRef
  | Done String


data Query a
  = OpenWindow a
  | OpenWindowNonCanceler a
  | GotResult String a
  | CancelWaiting a


initialState :: State
initialState = Init


{-| Fork an aff and return a canceler. 
-}
forkQuery
  :: forall s a f eff
   . Aff (avar :: AVAR, console :: CONSOLE | eff) a
  -> (a -> Action f)
  -> ComponentDSL s f (Aff (avar :: AVAR, console :: CONSOLE | eff)) (Canceler (avar :: AVAR, console :: CONSOLE | eff))

forkQuery aff act = do

  v <- liftAff' makeVar

  canceler <- liftAff' $ forkAff do
    res <- aff
    putVar v res

  subscribe
    $ EventSource
    $ SCR.producerToStallingProducer
    $ produce \emit -> do
      Aff.runAff
        (\_ -> do
            log "something wrong happened."
            emit $ Right unit)
        (\res -> do
            log "something went right."
            emit $ Left $ act res unit)
        (takeVar v)

  pure canceler


{-| Monitor an open window for a global property to be set and return the value.
-}
monitor :: forall eff a
   . WindowRef
  -> String
  -> Aff (window :: WINDOW, console :: CONSOLE | eff) a
monitor win prop = do
  liftEff $ log "Monitor called"
  res <- checkLater
  if res
    then do liftEff $ getProperty win prop
    else monitor win prop
  where
    checkForResult :: forall e. Eff (window :: WINDOW | e) Boolean
    checkForResult = hasProperty win prop

    checkLater = later' 1000 (liftEff checkForResult)

ui :: forall eff. Component State Query (Aff IO)
ui = component render eval
  where
    render :: State -> ComponentHTML Query
    render s =
      H.div_
      [ (showMsg s)
      , H.button
        [ E.onClick (E.input_ OpenWindow), P.disabled $ (isBusy s) ]
        [ H.text "Open Window" ]
      , H.button
        [ E.onClick (E.input_ OpenWindowNonCanceler), P.disabled $ (isBusy s) ]
        [ H.text "Open Window (no canceler)" ]
      , H.button
        [ E.onClick (E.input_ CancelWaiting), P.disabled $ not (isBusy s) ]
        [ H.text "Cancel" ]
      ]

    showMsg (Done s) = H.div_ [ H.text s ]
    showMsg otherwise = H.div_ [H.text ""]

    eval :: Natural Query (ComponentDSL State Query (Aff IO))
    eval (OpenWindow next) = do
      openWin true
      pure next
    eval (OpenWindowNonCanceler next) = do
      openWin false
      pure next

    eval (GotResult res next) = do
      set $ Done ("Got forked result: " ++ res)
      pure next

    eval (CancelWaiting next) = do
      liftEff' $ log "trying to cancel"
      state <- get
      liftAff' $ cancelAuth state
      set $ Done "User canceled auth."
      pure next

    openWin withCancel = do
      win <- liftEff' $ openWindow "http://www.purescript.org" "TestWin" "width=300,height=400"
      case win of
        Just w ->
          case withCancel of
            true -> do
              canceler <- forkQuery (monitor w "_testResult") GotResult
              set $ Waiting w canceler
            false -> do
              set $ WaitingNoCanceler w
              result <- liftAff' $ monitor w "_testResult"
              set $ Done ("Got immediate result: " ++ result)
        Nothing ->
          set $ Done "Could not open window. Check your popup blocker."

    isBusy :: State -> Boolean
    isBusy (Waiting _ _) = true
    isBusy (WaitingNoCanceler _) = true
    isBusy _ = false


    cancelAuth :: State -> Aff IO Unit
    cancelAuth (WaitingNoCanceler win) = do
      liftEff $ log "Cenceling no canceler"
      liftEff $ closeWindow win
      pure unit

    cancelAuth (Waiting win canceler) = do
      liftEff $ log "Canceling with canceler"
      liftEff $ closeWindow win
      cancelResult <- Aff.cancel canceler (error "User canceled auth")
      pure unit

    cancelAuth _ = pure unit

-- | Run the app
main :: Eff IO Unit
main =
  Aff.runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
