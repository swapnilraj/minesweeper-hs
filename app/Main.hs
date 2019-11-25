{- | 'Main' module to start the 'Threepenny' server.

    Only responsible for starting the server and nothing else.
-}
module Main where

import Graphics (setup)

import Graphics.UI.Threepenny.Core

-- | Spins the 'Threepenny' GUI with 'setup' from 'Graphics' module.
main = startGUI defaultConfig setup
