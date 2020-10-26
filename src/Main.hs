{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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


share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Person json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  name Text
  age Int
  deriving Show
|]

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = runSpock 8080 app

app :: IO Middleware
app = do
  ref <- newIORef 0
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg' <- defaultSpockCfg () (PCPool pool) ()
  let spockCfg = spockCfg' {spc_errorHandler = errorJson'}
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  spock spockCfg routes

routes :: Api
routes =
  do
    get "people" $ do
      allPeople <- runSQL $ selectList [] [Asc PersonId]
      json allPeople
    get ("people" <//> var) $ \personId -> do
      maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
      case maybePerson of
        Nothing -> do
          Web.Spock.setStatus notFound404
          errorJson 2 "Could not find a person with matching id"
        Just thePerson -> json thePerson
    post "people" $ do
      maybePerson <- jsonBody :: ApiAction (Maybe Person)
      case maybePerson of
        Nothing -> do
          Web.Spock.setStatus badRequest400
          errorJson 1 "Failed to parse request body as Person"
        Just thePerson -> do
          newId <- runSQL $ insert thePerson
          Web.Spock.setStatus created201
          json $ object ["result" .= String "success", "id" .= newId]
    put ("people" <//> var) $ \personId -> do
      maybePerson <- jsonBody :: ApiAction (Maybe Person)
      case maybePerson of
        Nothing -> do
          Web.Spock.setStatus badRequest400
          errorJson 3 "Failed to parse request body as Person"
        Just thePerson -> do
          res <- runSQL $ replace personId thePerson
          json $ object ["result" .= String "success", "id" .= personId, "res" .= res]
    Web.Spock.delete ("people" <//> var) $ \personId -> do
      maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
      case maybePerson of
        Nothing -> do
          Web.Spock.setStatus notFound404
          errorJson 4 "Could not find a person with matching id"
        Just thePerson -> do
          runSQL $ P.delete personId :: ApiAction ()
          json $ object ["result" .= String "success"]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson' :: Status -> ActionCtxT ctx IO ()
errorJson' (Status code message) = errorJson code (decodeUtf8 message)

errorJson :: MonadIO m => Int -> Text -> ActionCtxT ctx m ()
errorJson code message =
  json $
    object
      [ "result" .= String "failure",
        "error" .= object ["code" .= code, "message" .= message]
      ]
