{-# LANGUAGE OverloadedLabels
           , OverloadedStrings #-}

module Main
  ( main
  ) where

import           GameMaker.RiskOfRain.Unpacking

import           Data.Binary.Get (ByteOffset)
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as Lazy
import           Data.Time.Format.ISO8601
import           Optics.Operators
import           System.Environment



main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> process file
    _      -> fail "Expected FILE as the only argument"



process :: FilePath -> IO ()
process path = do
  file <- Lazy.readFile path
  case parseChunks file of
    Failure off err -> fail . showString "Could not parse GameMaker data file (at "
                                . shows off . showString "): " $ show err
    Success chunks  -> do
      chunk "GEN8" (parseGen8 (chunks ^. #strg) (chunks ^. #gen8)) $ \gen8 -> do
        pure $
          "GEN8 | "
            <> (shortByteString $ gen8 ^. #name)
            <> ", build " <> word32Dec (gen8 ^. #build)
            <> ", created at " <> string8 (iso8601Show $ gen8 ^. #timestamp)

      Lazy.putStrLn "OPTN | unused"

      chunk "EXTN" (parseExtn (chunks ^. #strg) (chunks ^. #extn)) $ \extn -> do
        case stream_ (extn ^. #triplets) of
          Failure off err     -> failure ("stream EXTN" <>) off err
          Success ~(n, extn2) ->
            case stream_ (extn2 ^. #segments) of
              Failure off err -> failure ("stream EXTN/2" <>) off err
              Success ~(m, _) ->
                pure $
                  "EXTN | "
                    <> intDec n <> "/" <> word32Dec (extn ^. #count) <> " triplets, "
                    <> intDec m <> "/" <> word32Dec (extn2 ^. #count) <> " segments"

      chunk "SOND" (parseSond (chunks ^. #strg) (chunks ^. #sond)) $ \sond -> do
        case stream_ (sond ^. #elements) of
          Failure off err -> failure ("stream SOND" <>) off err
          Success ~(n, _) ->
            pure $
              "SOND | "
                <> intDec n <> "/" <> word32Dec (sond ^. #count) <> " sounds"

      Lazy.putStrLn "AGRP | unused"

      chunk "SPRT" (parseSprt (chunks ^. #strg) (chunks ^. #tpag) (chunks ^. #sprt)) $ \sprt -> do
        case stream_ (sprt ^. #elements) of
          Failure off err -> failure ("stream SPRT" <>) off err
          Success ~(n, _) ->
            pure $
              "SPRT | "
                <> intDec n <> "/" <> word32Dec (sprt ^. #count) <> " sprites"

      chunk "BGND" (parseBgnd (chunks ^. #strg) (chunks ^. #tpag) (chunks ^. #bgnd)) $ \bgnd -> do
        case stream_ (bgnd ^. #elements) of
          Failure off err -> failure ("stream BGND" <>) off err
          Success ~(n, _) ->
            pure $
              "BGND | "
                <> intDec n <> "/" <> word32Dec (bgnd ^. #count) <> " backgrounds"

      Lazy.putStrLn "PATH | unused"

      chunk "SCPT" (parseScpt (chunks ^. #strg) (chunks ^. #scpt)) $ \scpt -> do
        case stream_ (scpt ^. #bindings) of
          Failure off err -> failure ("stream SCPT" <>) off err
          Success ~(n, _) ->
            pure $
              "SCPT | "
                <> intDec n <> "/" <> word32Dec (scpt ^. #count) <> " bindings"

      Lazy.putStrLn "SHDR | unused"

      chunk "FONT" (parseFont (chunks ^. #strg) (chunks ^. #tpag) (chunks ^. #font)) $ \font -> do
        case stream_ (font ^. #elements) of
          Failure off err -> failure ("stream FONT" <>) off err
          Success ~(n, _) ->
            pure $
              "FONT | "
                <> intDec n <> "/" <> word32Dec (font ^. #count) <> " fonts"

      Lazy.putStrLn "TMLN | unused"

      chunk "OBJT" (parseObjt (chunks ^. #strg) (chunks ^. #objt)) $ \objt -> do
        case stream_ (objt ^. #elements) of
          Failure off err -> failure ("stream OBJT" <>) off err
          Success ~(n, _) ->
            pure $
              "OBJT | "
                <> intDec n <> "/" <> word32Dec (objt ^. #count) <> " objects"

      chunk "ROOM" (parseRoom (chunks ^. #strg) (chunks ^. #room)) $ \room -> do
        case stream_ (room ^. #elements) of
          Failure off err -> failure ("stream ROOM" <>) off err
          Success ~(n, _) ->
            pure $
              "ROOM | "
                <> intDec n <> "/" <> word32Dec (room ^. #count) <> " rooms"

      Lazy.putStrLn "DAFL | unused"

      chunk "TPAG" (parseTpag (chunks ^. #tpag)) $ \tpag -> do
        case stream_ (tpag ^. #elements) of
          Failure off err -> failure ("stream TPAG" <>) off err
          Success ~(n, _) ->
            pure $
              "TPAG | "
                <> intDec n <> "/" <> word32Dec (tpag ^. #count) <> " regions"

      chunk "CODE" (parseCode (chunks ^. #strg) (chunks ^. #code)) $ \mayCode ->
        mappend "CODE | " <$> do
          case mayCode of
            Nothing   -> pure "absent"
            Just code -> 
              case stream_ (code ^. #functions) of
                Failure off err -> failure ("stream CODE" <>) off err
                Success ~(n, _) ->
                  pure $
                    intDec n <> "/" <> word32Dec (code ^. #count) <> " slices"

      chunk "VARI" (parseVari (chunks ^. #strg) (chunks ^. #vari)) $ \mayVari ->
        mappend "VARI | " <$> do
          case mayVari of
            Nothing   -> pure "absent"
            Just vari -> 
              case stream_ (vari ^. #elements) of
                Failure off err -> failure ("stream VARI" <>) off err
                Success ~(n, _) ->
                  pure $
                    intDec n <> "/? variables"

      chunk "FUNC" (parseFunc (chunks ^. #strg) (chunks ^. #func)) $ \mayFunc ->
        mappend "FUNC | " <$> do
          case mayFunc of
            Nothing   -> pure "absent"
            Just func -> 
              case stream_ (func ^. #positions) of
                Failure off err     -> failure ("stream FUNC" <>) off err
                Success ~(n, func2) ->
                  case stream_ (func2 ^. #elements) of
                    Failure off err -> failure ("stream FUNC/2" <>) off err
                    Success ~(m, _) ->
                      pure $
                           intDec n <> "/" <> word32Dec (func ^. #count) <> " functions, "
                        <> intDec m <> "/" <> word32Dec (func2 ^. #count) <> " definitions"

      chunk "STRG" (parseStrg (chunks ^. #strg)) $ \strg -> do
        case stream_ (strg ^. #strings) of
          Failure off err -> failure ("stream STRG" <>) off err
          Success ~(n, _) ->
            pure $
              "STRG | "
                <> intDec n <> "/" <> word32Dec (strg ^. #count) <> " strings"

      chunk "TXTR" (parseTxtr (chunks ^. #txtr)) $ \txtr -> do
        case stream_ (txtr ^. #elements) of
          Failure off err -> failure ("stream TXTR" <>) off err
          Success ~(n, _) ->
            pure $
              "TXTR | "
                <> intDec n <> "/" <> word32Dec (txtr ^. #count) <> " files"

      chunk "AUDO" (parseAudo (chunks ^. #audo)) $ \audo -> do
        case stream_ (audo ^. #files) of
          Failure off err -> failure ("stream AUDO" <>) off err
          Success ~(n, _) ->
            pure $
              "AUDO | "
                <> intDec n <> "/" <> word32Dec (audo ^. #count) <> " files"



failure :: ShowS -> ByteOffset -> String -> IO a
failure name off err =
  fail . showString "Could not " . name . showString " (at "
           . shows off . showString "): " $ show err

chunk :: String -> Result a -> (a -> IO Builder) -> IO ()
chunk name m f =
  case m of
    Failure off err -> failure (("parse" <>) . (name <>) . (" chunk" <>)) off err
    Success res     -> do
      builder <- f res
      Lazy.putStrLn $ toLazyByteString builder



stream_ :: Stream a Result r -> Result (Int, r)
stream_ = go 0
  where
    go n s =
      case s of
        Yield _ s' -> let n' = n + 1
                      in n' `seq` go n' s'
        Effect m   -> go n =<< m
        End r      -> Success (n, r)
