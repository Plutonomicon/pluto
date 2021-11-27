{-# LANGUAGE NoImplicitPrelude #-}


module PlutusCore.Assembler.EntryPoint (main) where


import System.IO (putStrLn)

import PlutusCore.Assembler.Prelude


main :: IO ()
main = putStrLn "hello world"
