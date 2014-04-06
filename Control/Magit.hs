{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Control.Magit where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.AffineSpace
import Data.IORef
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text as T
import Data.Text.Encoding as T
import Data.Thyme
import Debug.Trace
import Prelude hiding (log)
import System.IO.Unsafe
import System.Locale
import System.Log.FastLogger
import Text.Regex.PCRE.Light

