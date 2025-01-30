{-# LANGUAGE OverloadedRecordDot
           , OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.ByteString.Short (ShortByteString)
import qualified Data.Patricia.Word.Strict as Patricia
import           Data.Primitive.SmallArray
import           Data.Time.Clock.POSIX
import           Data.Time.Format.ISO8601
import           GameMaker.RiskOfRain.Decoder as Decoder
import           System.Environment
import           System.Exit
import           System.IO
import           Parser.Lathe (Scrap (..))



main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> process file
    _      -> fail "Expected FILE as the only argument"



newline :: Builder
newline =
  case nativeNewline of
    LF   -> word8 0x0A
    CRLF -> word16BE 0x0D0A



process :: FilePath -> IO ()
process path = do
  file <- LB.readFile path

  let template = form
                   gen8
                   optn
                   extn
                   sond
                   discard
                   sprt
                   bgnd
                   discard
                   scpt
                   discard
                   font
                   discard
                   objt
                   room
                   discard
                   tpag
                   (Decoder.maybe code)
                   (Decoder.maybe vari)
                   (Decoder.maybe func)
                   strg
                   txtr
                   audo

  let (Scrap offset _ _, result) = decode template file
  case result of
    Left err -> do
      LB.hPutStrLn stderr $
        toLazyByteString $
          byteString "Could not parse GameMaker data file (at byte "
            <> int64Dec offset <> byteString "): " <> string8 (show err)

      exitWith $ ExitFailure 1

    Right out ->
      case lookupStrgRef out.strg out.gen8.name of
        Nothing -> do
          LB.hPutStrLn stderr $
            toLazyByteString $
              byteString "Could not parse GameMaker data file: \
                \game name specified in GEN8 is not in the STRG chunk"

          exitWith $ ExitFailure 1

        Just name ->
          display out name



display
  :: Form Gen8 Optn Extn Sond () Sprt Bgnd () Scpt () Font
          () Objt Room () Tpag (Maybe Code) (Maybe Vari) (Maybe Func) Strg Txtr Audo
  -> ShortByteString
  -> IO ()
display out name =
  LB.hPutStrLn stdout $
    toLazyByteString $
         byteString "GEN8 | "
      <> shortByteString name
      <> byteString ", build " <> word32Dec out.gen8.build
      <> byteString ", created at "
      <> string8 (iso8601Show . posixSecondsToUTCTime . fromIntegral $ out.gen8.timestamp)

      <> newline
      <> byteString "OPTN | unknown"

      <> newline
      <> byteString "EXTN | "
      <> intDec (sizeofSmallArray out.extn.unknown1) <> byteString " triplets, "
      <> intDec (sizeofSmallArray out.extn.unknown2) <> byteString " segments"

      <> newline
      <> byteString "SOND | "
      <> intDec (sizeofSmallArray out.sond.elements) <> byteString " sounds"

      <> newline
      <> byteString "AGRP | unused"

      <> newline
      <> byteString "SPRT | "
      <> intDec (sizeofSmallArray out.sprt.elements) <> byteString " sprites"

      <> newline
      <> byteString "BGND | "
      <> intDec (sizeofSmallArray out.bgnd.elements) <> byteString " backgrounds"

      <> newline
      <> byteString "PATH | unused"

      <> newline
      <> byteString "SCPT | "
      <> intDec (sizeofSmallArray out.scpt.bindings) <> byteString " bindings"

      <> newline
      <> byteString "SHDR | unused"

      <> newline
      <> byteString "FONT | "
      <> intDec (sizeofSmallArray out.font.elements) <> byteString " fonts"

      <> newline
      <> byteString "TMLN | unused"

      <> newline
      <> byteString "OBJT | "
      <> intDec (sizeofSmallArray out.objt.elements) <> byteString " objects"

      <> newline
      <> byteString "ROOM | "
      <> intDec (sizeofSmallArray out.room.elements) <> byteString " rooms"

      <> newline
      <> byteString "DAFL | unused"

      <> newline
      <> byteString "TPAG | "
      <> intDec (Patricia.size out.tpag.elements) <> byteString " texture regions"

      <> newline
      <> byteString "CODE | "
      <> case out.code of
           Nothing   -> byteString "empty"
           Just this ->
             intDec (sizeofSmallArray this.functions) <> byteString " code blobs"

      <> newline
      <> byteString "VARI | "
      <> case out.vari of
           Nothing   -> byteString "empty"
           Just this ->
             intDec (sizeofSmallArray this.elements) <> byteString " variables"

      <> newline
      <> byteString "FUNC | "
      <> case out.func of
           Nothing   -> byteString "empty"
           Just this ->
                intDec (sizeofSmallArray this.positions) <> byteString " functions, "
             <> intDec (sizeofSmallArray this.elements) <> byteString " definitions"

      <> newline
      <> byteString "STRG | "
      <> intDec (Patricia.size out.strg.strings) <> byteString " strings"

      <> newline
      <> byteString "TXTR | "
      <> intDec (sizeofSmallArray out.txtr.elements) <> byteString " textures"

      <> newline
      <> byteString "AUDO | "
      <> intDec (sizeofSmallArray out.audo.tracks) <> byteString " audio files"
