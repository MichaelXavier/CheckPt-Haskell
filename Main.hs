module Main where

import CheckPt

import CheckPt.CLI (modes, dispatch)

import qualified System.Console.CmdArgs as Arg

main :: IO ()
main = Arg.cmdArgs_ modes >>= dispatch
