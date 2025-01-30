{-# LANGUAGE BangPatterns
           , OverloadedRecordDot
           , OverloadedStrings #-}

module Main
  ( main
  ) where

import           Decompile
import           Disassemble
import           Display

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Primitive.SmallArray
import           GameMaker.RiskOfRain.Decoder as Decoder
import           System.Environment
import           System.Exit
import           System.IO
import           Parser.Lathe (Scrap (..), ByteOffset)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> process file
    _      -> fail "Expected FILE as the only argument"



failure :: Builder -> IO ()
failure bldr = do
  LB.hPutStrLn stderr . toLazyByteString $ bldr
  exitWith $ ExitFailure 1



process :: FilePath -> IO ()
process path = do
  file <- LB.readFile path

  let template = form
                   discard
                   discard
                   discard
                   discard
                   discard
                   discard
                   discard
                   discard
                   discard
                   discard
                   discard
                   discard
                   objt
                   discard
                   discard
                   tpag
                   (Decoder.maybe code)
                   (Decoder.maybe vari)
                   (Decoder.maybe func)
                   strg
                   discard
                   discard

  let (Scrap offset _ _, result) = decode template file
  case result of
    Left e    -> parsingFailure offset e
    Right out ->
      case (,,) <$> out.code <*> out.vari <*> out.func of
        Nothing ->
          failure $
            byteString "GameMaker data file does not have CODE, VARI and FUNC chunks"

        Just (codes, varis, funcs) ->
          case makeFunctions out.strg funcs of
            Left (StrgRef ref) ->
              failure $
                byteString "Could not find reference to function "
                  <> word32Dec ref <> " in the STRG chunk"

            Right funs ->
              case makeVariables out.strg varis of
                Left (StrgRef ref) ->
                  failure $
                    byteString "Could not find reference to variable "
                      <> word32Dec ref <> " in the STRG chunk"

                Right vars ->
                  case makeArguments out.strg funcs of
                    Left (StrgRef ref) ->
                      failure $
                        byteString "Could not find reference to arguments for function "
                          <> word32Dec ref <> " in the STRG chunk"

                    Right args ->
                      let go !v !f n
                            | n >= sizeofSmallArray codes.functions = pure ()
                            | otherwise                             =
                                case disassemble out.strg out.objt args v f
                                       (indexSmallArray codes.functions n) of
                                  Left e              -> disassemblyFailure e
                                  Right (asm, v', f') ->
                                    case decompile out.strg out.objt asm of
                                      Left e    -> decompilationFailure e
                                      Right src -> do
                                        LB.hPutStrLn stdout $
                                          toLazyByteString $
                                            ( if n == 0
                                                then mempty
                                                else newline <> newline
                                            )
                                              <> display src

                                        go v' f' (n + 1)

                      in go vars funs 0


parsingFailure :: ByteOffset -> Decoder.Error -> IO ()
parsingFailure offset e =
  failure $
    byteString "Could not parse GameMaker data file (at byte "
      <> int64Dec offset <> byteString "):" <> newline <> byteString "  "
      <> case e of
           Decoder.AbruptEnd ->
             byteString "abrupt end of input"

           Mismatch a b ->
             byteString "expected chunk \"" <> chunk a <> byteString "\", but found \""
                                            <> chunk b <> byteString "\""

           Misaligned a size res ->
             byteString "chunk \"" <> chunk a <> byteString "\" has a size of "
               <> word32Dec size <> byteString " bytes, but parsing consumed "
               <> int64Dec res <> byteString " bytes"

           TrailingData ->
             byteString "trailing data"
  where
    chunk (ChunkName x) = word32BE x



disassemblyFailure :: Disassemble.Error -> IO ()
disassemblyFailure e =
  failure $
    case e of
      NoFunctionName (StrgRef ref) -> 
        byteString "Could not find reference to function "
          <> word32Dec ref <> " in the STRG chunk"

      NoArguments name ->
        byteString "Could not find arguments for function \""
                   <> shortByteString name <> byteString "\""

      Disassemble.Error name offset reason ->
        byteString "Disassembly error in function \"" <> shortByteString name
          <> byteString "\" at offset " <> int64Dec offset
          <> byteString ":" <> newline <> byteString "  "
          <> case reason of
               BadDataType a ->
                 byteString "bad datatype (0x" <> word8HexFixed a
                                               <> byteString ")"

               BadInstance a ->
                 byteString "bad instance (0x" <> word16HexFixed (fromIntegral a)
                                               <> byteString ")"

               BadVariableType a ->
                 byteString "bad variable type (0x" <> word8HexFixed a
                                                    <> byteString ")"

               BadComparison a ->
                 byteString "bad comparison (0x" <> word8HexFixed a
                                                 <> byteString ")"

               ObjectMiss a ->
                 byteString "referenced object is outside OBJT array bounds ("
                   <> int16Dec a <> byteString ")"

               VariableMiss a ->
                 byteString "referenced variable not in scope at current position ("
                   <> word32Dec a <> byteString ")"

               FunctionMiss a ->
                 byteString "referenced function not in scope at current position ("
                   <> word32Dec a <> byteString ")"

               StringMiss (StrgRef ref) ->
                 byteString "referenced string is not in the STRG chunk (global reference "
                   <> word32Dec ref <> byteString ")"

               StringIndexMiss a ->
                 byteString "referenced string is not in the STRG chunk (index "
                   <> word32Dec a <> byteString ")"

               BadPushable dt ->
                 byteString "unexpected datatype push (pushed "
                   <> ( case dt of
                          BadPushFloat    -> "float"
                          BadPushInt64    -> "int64"
                          BadPushBoolean  -> "bool"
                          BadPushInstance -> "instance"
                      )

                   <> byteString ")"

               IllegalOp a b c d ->
                 byteString "illegal operation ("
                   <> word8HexFixed a <> byteString " "
                   <> word8HexFixed b <> byteString " "
                   <> word8HexFixed c <> byteString " "
                   <> word8HexFixed d <> byteString ")"

               Disassemble.AbruptEnd ->
                 byteString "abrupt end of file"



decompilationFailure :: Decompile.Error -> IO ()
decompilationFailure (Decompile.Error name offset e) =
  failure $
    byteString "Decompilation error in function \"" <> shortByteString name
      <> byteString "\" at offset " <> word32Dec offset
      <> case e of
           Stage2Error s2e ->
                byteString ", stage 2:" <> newline <> byteString "  "
             <> case s2e of
                  BackjumpOutOfBounds ->
                    byteString "backjump points out of bounds"

                  BackjumpMissInbound loc ->
                    byteString "backjump does not point to a statement ("
                      <> word32Dec loc <> byteString ")"

                  BackjumpMissIntoCase loc -> 
                    byteString "backjump points inside a case statement ("
                      <> word32Dec loc <> byteString ")"

                  FrontjumpBeyondContext loc ->
                    byteString "frontjump points outside of context ("
                      <> word32Dec loc <> byteString ")"

                  RValueOutOfBounds ->
                    byteString "reached end looking for an r-value"

                  NotAnRValue loc ->
                    byteString "unknown r-value type ("
                      <> word32Dec loc <> byteString ")"

                  InvalidSecondDimAssignment loc ->
                    byteString "invalid second dimension array assignment ("
                      <> word32Dec loc <> byteString ")"

                  IncorrectSwitch loc ->
                    byteString "malformed case switch ("
                      <> word32Dec loc <> byteString ")"

                  ExpectedSwitch loc ->
                    byteString "expected a case switch ("
                      <> word32Dec loc <> byteString ")"

                  SurplusPopz loc ->
                    byteString "unexpected extra Popz statement ("
                      <> word32Dec loc <> byteString ")"

                  SurplusPopEnvAny loc ->
                    byteString "unexpected extra PopEnvAny statement ("
                      <> word32Dec loc <> byteString ")"

                  ExpectedPops loc ->
                    byteString "expected more Popz/PopEnvAny statements ("
                      <> word32Dec loc <> byteString ")"

                  EmptyCaseSlice loc ->
                    byteString "empty case slice ("
                      <> word32Dec loc <> byteString ")"

                  MalformedSwitch loc ->
                    byteString "malformed case switch ("
                      <> word32Dec loc <> byteString ")"

                  NoRValueBeforeCase loc ->
                    byteString "expected r-value before a case ("
                      <> word32Dec loc <> byteString ")"

                  ExpectedDupOnInstanceReassign loc ->
                    byteString "expected Dup on instance reassignment ("
                      <> word32Dec loc <> byteString ")"

                  ExpectedDupOnArrayReassign loc ->
                    byteString "expected Dup on array reassignment ("
                      <> word32Dec loc <> byteString ")"

                  ExpectedPopEnvAnyEscape loc ->
                    byteString "expected a PopEnvAny escape ("
                      <> word32Dec loc <> byteString ")"

                  ExpectedRepeatPopEnv loc ->
                    byteString "expected repeat PopEnv ("
                      <> word32Dec loc <> byteString ")"

                  MalformedRepeatPop loc ->
                    byteString "malformed repeat Pop statement ("
                      <> word32Dec loc <> byteString ")"

                  InconvertibleCallReturn loc ->
                    byteString "inconvertible call return ("
                      <> word32Dec loc <> byteString ")"

                  MalformedOr loc ->
                    byteString "malformed boolean or operation ("
                      <> word32Dec loc <> byteString ")"

                  MalformedAnd loc ->
                    byteString "malformed boolean and operation ("
                      <> word32Dec loc <> byteString ")"

                  UnknownStatement loc ->
                    byteString "unknown statement ("
                      <> word32Dec loc <> byteString ")"

           Stage3Error s3e ->
                byteString ", stage 3:" <> newline <> byteString "  "
             <> case s3e of
                  FrontjumpOutOfBounds ->
                    byteString "frontjump points out of bounds"

                  FrontjumpMissInbound loc ->
                    byteString "frontjump does not point to a statement ("
                      <> word32Dec loc <> byteString ")"

                  MalformedConditional loc ->
                    byteString "malformed conditional ("
                      <> word32Dec loc <> byteString ")"

                  ExpectedConditional loc ->
                    byteString "expected conditional ("
                      <> word32Dec loc <> byteString ")"

                  WrongConditionalJumpType loc ->
                    byteString "wrong conditional jump type ("
                      <> word32Dec loc <> byteString ")"

                  UnknownLoopType loc ->
                    byteString "unknown loop type ("
                      <> word32Dec loc <> byteString ")"

                  TrailingInRepeat loc ->
                    byteString "trailing statements inside a repeat ("
                      <> word32Dec loc <> byteString ")"

                  TrailingInCase loc ->
                    byteString "trailing statements inside a case ("
                      <> word32Dec loc <> byteString ")"

                  TrailingInDefault loc ->
                    byteString "trailing statements inside a default ("
                      <> word32Dec loc <> byteString ")"

                  TrailingInWith loc ->
                    byteString "trailing statements inside a with ("
                      <> word32Dec loc <> byteString ")"

                  TrailingInIf loc ->
                    byteString "trailing statements inside an if ("
                      <> word32Dec loc <> byteString ")"

                  TrailingInElse loc ->
                    byteString "trailing statements inside an else ("
                      <> word32Dec loc <> byteString ")"

                  TrailingInLoop loc ->
                    byteString "trailing statements inside a loop ("
                      <> word32Dec loc <> byteString ")"

                  TrailingStatements ->
                    byteString "trailing statements after processing"

                  UnhandledStatement loc ->
                    byteString "unknown statement ("
                      <> word32Dec loc <> byteString ")"
