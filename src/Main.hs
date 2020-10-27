{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Trans
import Data.Aeson hiding (json)
import Data.IORef
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as T

import Data.Text.Encoding
import Database.Persist hiding (get)
import qualified Database.Persist as P
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH
import GHC.Generics
import Network.HTTP.Types.Status
import Network.Wai (Middleware)
import Web.Spock
import Web.Spock.Config
import App (app)

main :: IO ()
main = runSpock 8080 app
