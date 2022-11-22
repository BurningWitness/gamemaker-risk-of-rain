{-# LANGUAGE OverloadedLabels
           , OverloadedStrings #-}

module Main where

import           GameMaker.RiskOfRain
import           GameMaker.RiskOfRain.Decompilation (unparsed)

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Lens.Micro
import           Options.Applicative



data Args =
       Args
         { aLoud :: Bool
         , aFile :: FilePath
         }

args :: Parser Args
args = Args
         <$> flag False True ( short 'l' <> long "loud" )
         <*> strArgument (metavar "FILE")

main :: IO ()
main = do
  Args loud file <- execParser $ info (args <**> helper) mempty
  raw <- BSL.readFile file
  BSC.putStrLn ""
  eiForm <- flip stream (decodeForm raw) $ \(n, s) ->
              when loud $
                BSL.putStrLn .
                  toLazyByteString $
                    "(" <> intDec n <> "/" <> intDec totalChunks <> ") " <> string8 s
  case eiForm of
    Left err   -> fail $ "Decoding error: " <> err
    Right form -> do
      _ <- evaluate $ force form
      BSC.putStrLn $ mconcat
                       [ form ^. #gen8 . #name
                       , ", build ", BSC.pack . show $ form ^. #gen8 . #build
                       , ", created at ", BSC.pack . show $ form ^. #gen8 . #timestamp
                       ]
      BSC.putStrLn $ mconcat
                       [ "Sound files: "
                       , BSC.pack . show . length $ form ^. #sond
                       ]
      BSC.putStrLn $ mconcat
                       [ "Sprites: "
                       , BSC.pack . show . length $ form ^. #sprt
                       ]
      BSC.putStrLn $ mconcat
                       [ "Backgrounds: "
                       , BSC.pack . show . length $ form ^. #bgnd
                       ]
      BSC.putStrLn $ mconcat
                       [ "Fonts: "
                       , BSC.pack . show . length $ form ^. #font
                       ]
      BSC.putStrLn $ mconcat
                       [ "Rooms: "
                       , BSC.pack . show . length $ form ^. #room
                       ]
      BSC.putStrLn $ mconcat
                       [ "Texture slices: "
                       , BSC.pack . show . length $ form ^. #tpag
                       ]
      BSC.putStrLn $ mconcat
                       [ "Unique function names: "
                       , BSC.pack . show . length $ form ^. #code . _Just . #elements
                       ]
      BSC.putStrLn $ mconcat
                       [ "Unique variable names: "
                       , BSC.pack . show . length $ form ^. #vari . _Just . #elements
                       ]
      BSC.putStrLn $ mconcat
                       [ "Unique GameMaker function names: "
                       , BSC.pack . show . length $ form ^. #func . _Just . #positions
                       ]
      BSC.putStrLn $ mconcat
                       [ "Strings: "
                       , BSC.pack . show . length $ form ^. #strg
                       ]
      BSC.putStrLn $ mconcat
                       [ "Textures: "
                       , BSC.pack . show . length $ form ^. #txtr
                       ]
      BSC.putStrLn $ mconcat
                       [ "Audio files: "
                       , BSC.pack . show . length $ form ^. #audo
                       ]
      case (form ^. #code, form ^. #vari, form ^. #func) of
        (Just cod, Just var, Just fun) -> do
          insts <- flip stream (extort $ expressions form cod var fun) $ \(n, s) ->
                     when loud $
                       BSL.putStrLn .
                         toLazyByteString $
                           "(" <> intDec n <> "/" <> intDec (totalFunctions cod) <> ") " <> byteString s
          case insts of
            Left err     -> fail $ "Expression parsing error: " <> err
            Right decomp -> do
              _ <- evaluate $ force decomp
              BSC.putStrLn $ mconcat
                               [ "Interpreted functions: "
                               , BSC.pack . show $ length decomp
                               ]
              BSC.putStrLn $ mconcat
                               [ "  of which unparsed: "
                               , BSC.pack . show . length $ unparsed decomp
                               ]

        _ -> BSC.putStrLn "No code to work with"
