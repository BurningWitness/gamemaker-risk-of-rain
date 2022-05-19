{-# LANGUAGE OverloadedStrings #-}

module Coherence where

import           GameMaker.RiskOfRain.Decompilation
import           GameMaker.RiskOfRain.Lens hiding (path)
import           GameMaker.RiskOfRain.Unpacking

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Lens.Micro
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
                           , BSC.pack . show . length $ form^.sond.unSond.unDictionary
                           ]
          BSC.putStrLn $ mconcat
                           [ "Sprites: "
                           , BSC.pack . show . length $ form^.sprt.unSprt.unDictionaryS
                           ]
          BSC.putStrLn $ mconcat
                           [ "Backgrounds: "
                           , BSC.pack . show . length $ form^.bgnd.unBgnd.unDictionary
                           ]
          BSC.putStrLn $ mconcat
                           [ "Fonts: "
                           , BSC.pack . show . length $ form^.font.unFont.unDictionary
                           ]
          BSC.putStrLn $ mconcat
                           [ "Rooms: "
                           , BSC.pack . show . length $ form^.room.unRoom.unDictionary
                           ]
          BSC.putStrLn $ mconcat
                           [ "Texture slices: "
                           , BSC.pack . show . length $ form^.tpag.unTpag.unDictionary
                           ]
          BSC.putStrLn $ mconcat
                           [ "Unique function names: "
                           , BSC.pack . show . length $ form^.code._Just.elements
                           ]
          BSC.putStrLn $ mconcat
                           [ "Unique variable names: "
                           , BSC.pack . show . length $ form^.vari._Just.elements
                           ]
          BSC.putStrLn $ mconcat
                           [ "Unique GameMaker function names: "
                           , BSC.pack . show . length $ form^.func._Just.positions
                           ]
          BSC.putStrLn $ mconcat
                           [ "Strings: "
                           , BSC.pack . show . length $ form^.strg.unStrg.unDictionary
                           ]
          BSC.putStrLn $ mconcat
                           [ "Textures: "
                           , BSC.pack . show . length $ form^.txtr.unTxtr.unDictionaryS
                           ]
          BSC.putStrLn $ mconcat
                           [ "Audio files: "
                           , BSC.pack . show . length $ form^.audo.unAudo.unDictionaryS
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
