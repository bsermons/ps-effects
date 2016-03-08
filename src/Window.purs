module Window where

import Prelude

import Data.Maybe (Maybe())
import Data.Nullable (Nullable(), toMaybe)

import Control.Monad.Eff (Eff())
import Control.Monad.Aff.AVar

import Data.Function (Fn2, Fn3, runFn2, runFn3)
import Data.Nullable (Nullable)

foreign import data WINDOW :: !

foreign import data WindowRef :: *

foreign import _openWindow :: forall eff. Fn3
                              String String String
                              (Eff (window :: WINDOW | eff) (Nullable WindowRef))

foreign import closeWindow :: forall eff. WindowRef
                              -> Eff (window :: WINDOW | eff) Unit

foreign import _hasProperty :: forall eff.
                               WindowRef
                               -> String
                               -> Eff (window :: WINDOW | eff) Boolean

foreign import _getProperty :: forall eff a.
                               WindowRef
                               -> String
                               -> Eff (window :: WINDOW | eff) a


openWindow :: forall eff. String -> String -> String -> Eff (window :: WINDOW | eff) (Maybe WindowRef)
openWindow a b c = toMaybe <$> (runFn3 _openWindow a b c)

hasProperty :: forall eff. WindowRef -> String -> Eff (window :: WINDOW | eff) Boolean
hasProperty = _hasProperty

getProperty :: forall eff a. WindowRef -> String -> Eff (window :: WINDOW | eff) a
getProperty = _getProperty
