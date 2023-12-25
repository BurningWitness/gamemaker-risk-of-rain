{-# LANGUAGE BangPatterns
           , OverloadedLabels
           , OverloadedStrings #-}

module Main
  ( main
  ) where

import           GameMaker.RiskOfRain.Decompilation
import           GameMaker.RiskOfRain.Disassembly hiding (Instruction (..), Variable (..))
import           GameMaker.RiskOfRain.Unpacking

import           Data.Binary.Get (ByteOffset)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy
import           Data.ByteString.Short (ShortByteString, fromShort)
import           Optics.Operators
import           System.Environment




main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      picked <- extract file
      case picked of
        YYC            -> Lazy.putStrLn "Empty CODE chunk, nothing to decompile"
        VM objs stream -> process objs stream

    _      -> fail "Expected FILE as the only argument"



data Extracted = VM Objects (Stream Assembly Result ())
               | YYC

extract :: FilePath -> IO Extracted
extract path = do
  file <- Lazy.readFile path
  case parseChunks file of
    Failure off err -> fail . showString "Could not parse GameMaker data file (at "
                                . shows off . showString "): " $ show err
    Success chunks  ->
      case parseCode (chunks ^. #strg) (chunks ^. #code) of
        Failure off err -> failure ("parse CODE chunk " <>) off err
        Success mayCode ->
          case mayCode of
            Nothing   -> pure YYC
            Just code -> do
              vars <- getVari chunks
              ~(funs, args) <- getFunc chunks
              objs <- getObjt chunks
              strs <- getStrg chunks
              pure (VM objs $ disassemble (chunks ^. #code) strs objs vars funs args code)



process :: Objects -> Stream Assembly Result () -> IO ()
process objs = go (pure ())
  where
    go between s =
      case s of
        Yield a s' ->
          case decompile objs a of
            Success b     -> do between
                                output b
                                go (Strict.putStr "\n") s'
            Failure o err ->
              fail $ showString "Could not decompile function "
                       . (Strict.unpack (fromShort (a ^. #name)) <>)
                       . showString " (" . shows o . showString "):  \n" $ err

        Effect m ->
          case m of
            Success s'    -> go between s'
            Failure o err ->
              fail $ showString "Unpacking error "
                       . showString " (" . shows o . showString "):  \n" $ err

        End ()   -> pure ()



getVari :: Chunks -> IO Variables
getVari chunks =
  case parseVari (chunks ^. #strg) (chunks ^. #vari) of
    Failure off err -> failure ("parse VARI chunk " <>) off err
    Success mayVari ->
      case mayVari of
        Nothing   -> fail "Empty VARI chunk, however CODE chunk is non-empty"
        Just vari ->
          case makeVariables vari of
            Failure off err -> failure ("process VARI chunk " <>) off err
            Success !vars   -> pure vars

getFunc :: Chunks -> IO (Functions, Arguments)
getFunc chunks =
  case parseFunc (chunks ^. #strg) (chunks ^. #func) of
    Failure off err -> failure ("parse FUNC chunk " <>) off err
    Success mayFunc ->
      case mayFunc of
        Nothing   -> fail "Empty FUNC chunk, however CODE chunk is non-empty"
        Just func ->
          case makeFunctions func of
            Failure off err         -> failure ("process FUNC chunk " <>) off err
            Success ~(!funs, func2) ->
              case makeArguments func2 of
                Failure off err -> failure ("process FUNC chunk " <>) off err
                Success !args   -> pure (funs, args)


getStrg :: Chunks -> IO Strings
getStrg chunks =
  case parseStrg (chunks ^. #strg) of
    Failure off err -> failure ("parse STRG chunk " <>) off err
    Success strg    ->
      case makeStrings strg of
        Failure off err -> failure ("process STRG chunk " <>) off err
        Success !strs   -> pure strs

getObjt :: Chunks -> IO Objects
getObjt chunks =
  case parseObjt (chunks ^. #strg) (chunks ^. #objt) of
    Failure off err -> failure ("parse OBJT chunk " <>) off err
    Success objt    ->
      case makeObjects objt of
        Failure off err -> failure ("process OBJT chunk " <>) off err
        Success !objs   -> pure objs



failure :: ShowS -> ByteOffset -> String -> IO a
failure name off err =
  fail . showString "Could not " . name . showString " (at "
           . shows off . showString "): " $ show err



newtype Offset = Offset Int

tab :: Offset -> Offset
tab (Offset n) = Offset (n + 1)

indent :: Offset -> Builder
indent (Offset n) = byteString $ Strict.replicate (n * 8) ' '



type Prec = Int

paren :: Bool -> Builder -> Builder
paren False b = b
paren True  b = "(" <> b <> ")"

inter :: Monoid m => m -> (a -> m) -> [a] -> m
inter i f xs =
  case xs of
    y:zs -> f y <> go1 zs
    []   -> mempty
  where
    go1 as =
      case as of
        a:bs -> i <> f a <> go1 bs
        []   -> mempty



output :: Source -> IO ()
output src =
  Lazy.putStrLn $
    toLazyByteString $
      "function " <> shortByteString (src ^. #name)
                       <> " (" <> arguments (src ^. #arguments) <> ") {"
        <> foldMap (\(At _ x) -> "\n" <> expr (Offset 1) x) (src ^. #expressions)
        <> "\n}"

arguments :: [ShortByteString] -> Builder
arguments xs =
  go $ case xs of
         "arguments" : ys -> ys
         _                -> xs
  where
    go as =
      case as of
        a:bs -> shortByteString a <> go1 bs
        []   -> mempty

    go1 as =
      case as of
        a : bs -> ", " <> shortByteString a <> go1 bs
        []     -> mempty




expr :: Offset -> Expr -> Builder
expr offset x =
  indent offset <>
    case x of
      Declare v          -> "var " <> assignment v <> ";"
      Assign v           -> assignment v <> ";"
      Reassign bin _ _ v -> reassignment bin v <> ";"
      Call name args     -> call name args <> ";"
      Return v           -> "return " <> rvalue 0 v <> ";"
      Exit               -> "exit;"
      Continue           -> "continue;"
      Break              -> "break;"

      If cond thn els    -> if_ offset cond thn els

      While cond xs      -> "while (" <> condition 0 cond <> ")" <> block offset xs

      Do (At _ cond) xs  ->
        "do"
          <> block offset xs
          <> ( case xs of
                 [_] -> "\n" <> indent offset <> "until ("
                 _   -> " until ("
             )
          <> condition 0 cond <> ");"

      For a (At _ cond) b xs ->
        "for (" <> assignment a <> "; " <> condition 0 cond <> "; " <> assignment b <> ")"
          <> block offset xs

      Repeat i xs        -> "repeat (" <> rvalue 0 i <> ")" <> block offset xs

      Switch r cs def    ->
        "switch (" <> rvalue 0 r <> ") {"
          <> ( flip foldMap cs $ \(Case (At _ c) xs) ->
                 "\n" <> indent offset <> "case " <> rvalue 11 c <> ":"
                      <> foldMap (\(At _ y) -> "\n" <> expr (tab offset) y) xs
             )
          <> ( case def of
                 NoDefault  -> mempty
                 Default xs ->
                   "\n" <> indent offset <> "default:"
                        <> foldMap (\(At _ y) -> "\n" <> expr (tab offset) y) xs
             )
          <> "\n" <> indent offset <> "}"

      With r vs          -> "with (" <> reference r <> ")" <> block offset vs
        where
          reference e =
            case e of
              Static i ->
                case i of
                  Object name -> shortByteString name
                  Self        -> "self"
                  Other       -> "other"
                  All         -> "all"
                  Noone       -> "noone"
                  Global      -> "global"
                  Local       -> "local"

              Dynamic (At _ v) -> rvalue 0 v



reassignment :: Binary -> Assignment -> Builder
reassignment bin (Assignment _ _ (At _ r) v) =
  varying r <> " " <> fst (binary bin) <> "= " <> rvalue 0 v

assignment :: Assignment -> Builder
assignment (Assignment _ _ (At _ r) v) =
  varying r <> " = " <> rvalue 0 v

block :: Offset -> [At Expr] -> Builder
block offset xs =
  case xs of
    [At _ x] -> "\n" <> expr (tab offset) x
    _        ->
      " {" <> foldMap (\(At _ x) -> "\n" <> expr (tab offset) x) xs
           <> "\n"
           <> indent offset <> "}"



if_ :: Offset -> Condition -> [At Expr] -> Else -> Builder
if_ offset cond thn els =
  "if (" <> condition 0 cond <> ")"
    <> block offset thn
    <> case els of
         NoElse    -> mempty

         Else [At _ (If cond' thn' els')] ->
              ( case thn of
                  [_] -> "\n" <> indent offset <> "else "
                  _   -> " else "
              )
           <> if_ offset cond' thn' els'

         Else more ->
              ( case thn of
                  [_] -> "\n" <> indent offset <> "else"
                  _   -> " else"
              )
           <> block offset more





condition :: Prec -> Condition -> Builder
condition p c =
  case c of
    Const _ _ r                -> rvalue p r
    Compare cmp _ _ l (At _ r) -> compare_ p cmp l r

    Every xs                   ->
      paren (p > 2) $ inter " && " (\(At _ x) -> condition 3 x) xs

    Any xs                     ->
      paren (p > 2) $ inter " || " (\(At _ x) -> condition 3 x) xs

    Not v                      -> "!" <> condition 11 v



rvalue :: Prec -> RValue -> Builder
rvalue p x =
  case x of
    Value (Constant v)          -> constant v
    Value (Varying v)           -> varying v
    RCompare cmp _ _ l (At _ r) -> compare_ p cmp l r
    Cast _ _ v                  -> rvalue p v
    Negate _ v                  -> paren (p > 10) $ "-" <> rvalue 11 v
    Complement _ v              -> paren (p > 10) $ "~" <> rvalue 11 v
    Binary bin _ _ l (At _ r)   ->
      let ~(op, prec) = binary bin
      in paren (p > prec) $ rvalue prec l <> " " <> op <> " " <> rvalue prec r

    RCall name args             -> call name args


constant :: Constant -> Builder
constant c =
  case c of
    CDouble d  -> formatDouble standardDefaultPrecision d
    CInt32 i   -> int32Dec i
    CInt16 i   -> int16Dec i
    CString bs -> "\"" <> shortByteString bs <> "\""


varying :: Variable -> Builder
varying d =
  case d of
    Var r name    -> reference r <> shortByteString name
    VArray ds r v -> reference r <> shortByteString v <> dims ds

  where
    reference r =
      case r of
        Static i ->
          case i of
            Object name -> shortByteString name <> "."
            Self        -> "self."
            Other       -> "other."
            All         -> "all."
            Noone       -> "noone."
            Global      -> "global."
            Local       -> mempty

        Dynamic (At _ v) -> "instance (" <> rvalue 0 v <> ")."

    dims (OneDim (At _ x))          = "[" <> rvalue 0 x <> "]"
    dims (TwoDim (At _ x) (At _ y)) = "[" <> rvalue 0 x <> "][" <> rvalue 0 y <> "]"



compare_ :: Prec -> Comparison -> RValue -> RValue -> Builder
compare_ p cmp l r =
  paren (p > 6) $ rvalue 6 l <> " " <> comparison cmp <> " " <> rvalue 6 r

comparison :: Comparison -> Builder
comparison cmp =
  case cmp of
    Lt -> "<"
    Le -> "<="
    Eq -> "=="
    Ne -> "!="
    Gt -> ">"
    Ge -> ">="


binary :: Binary -> (Builder, Prec)
binary bin =
  case bin of
    Mul -> ("*"  ,7)
    Div -> ("/"  ,7)
    Add -> ("+"  ,6)
    Sub -> ("-"  ,6)
    And -> ("&"  ,3)
    Or  -> ("|"  ,3)
    Xor -> ("^"  ,3)
    Shl -> ("<<" ,3)
    Shr -> (">>" ,3)
    Rem -> ("div",7)
    Mod -> ("%"  ,7)



call :: ShortByteString -> [At RValue] -> Builder
call name args =
  shortByteString name <> " (" <> inter ", " (\(At _ y) -> rvalue 0 y) args <> ")"
