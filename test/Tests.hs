{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import App (app)
import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (spockAsApp app) $ do
    describe "GET /" $ do
      it "serves stuff" $
        get "/people" `shouldRespondWith` 200
