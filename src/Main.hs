module Main where

import App (app)
import Web.Spock

main :: IO ()
main = runSpock 8080 app
