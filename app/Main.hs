module Main (main) where

import UI (runApp)

main :: IO ()
main = do
    putStrLn "Welcome to the Moxfield-to-Forge Converter!"
    runApp