{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import Control.Monad.Trans
import Data.Aeson hiding (json)
import Data.IORef
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as T
import GHC.Generics
import Web.Spock
import Web.Spock.Config
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist as P -- We'll be using P.get later for GET /people/<id>.
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  name Text
  age Int
  deriving Show
|]

type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = do
  ref <- newIORef 0
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

app :: Api
app =
  do
    get "people" $ do
      allPeople <- runSQL $ selectList [] [Asc PersonId]
      json allPeople
    get ("people" <//> var) $ \personId -> do
      maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
      case maybePerson of
        Nothing -> errorJson 2 "Could not find a person with matching id"
        Just thePerson -> json thePerson
    post "people" $ do
      maybePerson <- jsonBody :: ApiAction (Maybe Person)
      case maybePerson of
        Nothing -> errorJson 1 "Failed to parse request body as Person"
        Just thePerson -> do
          newId <- runSQL $ insert thePerson
          json $ object ["result" .= String "success", "id" .= newId]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]
