{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GameMaker.RiskOfRain.Decompilation
import           GameMaker.RiskOfRain.Lens
import           GameMaker.RiskOfRain.Unpacking

import           Control.Lens hiding (elements)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Either (isRight)
import           Prelude
import           System.Environment



-- | Given a file name as a first argument checks whether we can decode the 'FORM'
--   and outputs some stats on it.
main :: IO ()
main = do
  mayArgs <- getArgs
  case mayArgs of
    []       -> fail "No arguments passed to the test. Please pass the program name."
    (path:_) -> do
      file <- BSL.readFile path
      case decodeForm file of
        Left err -> fail $ "Decoding error: " <> err
        Right form -> do
          BSC.putStrLn ""
          BSC.putStrLn $ mconcat
                           [ form^.gen8.name.unPointer
                           , ", build ", BSC.pack . show $ form^.gen8.build
                           , ", created at ", BSC.pack . show $ form^.gen8.timestamp
                           ]
          BSC.putStrLn $ mconcat
                           [ "Sound files: "
                           , BSC.pack . show $ lengthOf (sond.unSond.unDictionary.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Sprites: "
                           , BSC.pack . show $ lengthOf (sprt.unSprt.unDictionaryS.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Backgrounds: "
                           , BSC.pack . show $ lengthOf (bgnd.unBgnd.unDictionary.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Fonts: "
                           , BSC.pack . show $ lengthOf (font.unFont.unDictionary.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Rooms: "
                           , BSC.pack . show $ lengthOf (room.unRoom.unDictionary.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Texture slices: "
                           , BSC.pack . show $ lengthOf (tpag.unTpag.unDictionary.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Unique function names: "
                           , BSC.pack . show $ lengthOf (code._Just.elements.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Unique variable names: "
                           , BSC.pack . show $ lengthOf (vari._Just.elements.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Unique GameMaker function names: "
                           , BSC.pack . show $ lengthOf (func._Just.positions.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Strings: "
                           , BSC.pack . show $ lengthOf (strg.unStrg.unDictionary.each) form
                           ]

          BSC.putStrLn $ mconcat
                           [ "Textures: "
                           , BSC.pack . show $ lengthOf (txtr.unTxtr.unDictionaryS.each) form
                           ]
          BSC.putStrLn $ mconcat
                           [ "Audio files: "
                           , BSC.pack . show $ lengthOf (audo.unAudo.unDictionaryS.each) form
                           ]
          case expressions form of
            Left err     -> fail $ "Expression parsing error: " <> err
            Right decomp -> do
              BSC.putStrLn $ mconcat
                               [ "Interpreted functions: "
                               , BSC.pack . show $ length decomp
                               ]
              BSC.putStrLn $ mconcat
                               [ "  of which unparsed: "
                               , BSC.pack . show . length $ unparsed decomp
                               ]
