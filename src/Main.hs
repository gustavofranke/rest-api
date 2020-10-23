{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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

data Person = Person
  { name :: Text,
    age :: Int
  }
  deriving (Generic, Show)

instance ToJSON Person

instance FromJSON Person


-- *Main> :set -XOverloadedStrings
-- *Main> encode Person { name = "Leela", age = 25 }
-- "{\"age\":25,\"name\":\"Leela\"}"
-- *Main> decode "{ \"name\": \"Amy\", \"age\": 30 }" :: Maybe Person
-- Just (Person {name = "Amy", age = 30})
-- *Main> 
-- *Main> decode "{\"age\":25,\"name\":\"Leela\"}" :: Maybe Person
-- Just (Person {name = "Leela", age = 25})
-- *Main> 
-- *Main> decode "{ \"name\": \"Amy\", \"age\": 30 }" :: Maybe Value
-- Just (Object (fromList [("age",Number 30.0),("name",String "Amy")]))
-- *Main> 

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  ref <- newIORef 0
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
  runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
  do get root $
         text "Hello World!"
     get ("hello" <//> var) $ \name ->
       do (DummyAppState ref) <- getState
          visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
          text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
     get "people" $
       do json $ Person { name = "Fry", age = 25 }
