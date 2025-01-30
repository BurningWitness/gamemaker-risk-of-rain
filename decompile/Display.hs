{-# LANGUAGE OverloadedRecordDot
           , OverloadedStrings #-}

module Display
  ( newline
  , display
  ) where

import           Decompile

import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Short (ShortByteString)
import           Data.Primitive.SmallArray
import           System.IO



newline :: Builder
newline =
  case nativeNewline of
    LF   -> word8 0x0A
    CRLF -> word16BE 0x0D0A



newtype Offset = Offset Int

tab :: Offset -> Offset
tab (Offset n) = Offset (n + 1)

indent :: Offset -> Builder
indent (Offset n) = byteString $ B.replicate n '\t'



type Prec = Int

paren :: Bool -> Builder -> Builder
paren False b = b
paren True  b = "(" <> b <> byteString ")"

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



display :: Source -> Builder
display src =
  byteString "function "
    <> shortByteString src.name
    <> byteString " (" <> arguments src.arguments
    <> byteString ")"
    <> newline
    <> byteString "{"
    <> foldMap (\(At _ x) -> newline <> expr (Offset 1) x) src.expressions
    <> newline <> byteString "}"

arguments :: SmallArray ShortByteString -> Builder
arguments arr =
  case sizeofSmallArray arr of
    0 -> mempty
    _ -> go True $ case indexSmallArray arr 0 of
                      "arguments" -> 1
                      _           -> 0
  where
    go isFirst n
      | n >= sizeofSmallArray arr = mempty
      | otherwise                 =
          ( if isFirst
              then id
              else (shortByteString ", " <>)
          ) $
            shortByteString (indexSmallArray arr n) <> go False (n + 1)



expr :: Offset -> Expr -> Builder
expr offset x =
  indent offset <>
    case x of
      Declare v          -> byteString "var " <> assignment v <> byteString ";"
      Assign v           -> assignment v <> byteString ";"
      Reassign bin _ _ v -> reassignment bin v <> byteString ";"
      Call name args     -> call name args <> byteString ";"
      Return v           -> byteString "return " <> rvalue 0 v <> byteString ";"
      Exit               -> byteString "exit;"
      Continue           -> byteString "continue;"
      Break              -> byteString "break;"

      If cond thn els    -> if_ offset cond thn els

      While cond xs      ->
        byteString "while (" <> condition 0 cond <> byteString ")"
          <> block offset xs

      Do (At _ cond) xs  ->
        "do"
          <> block offset xs
          <> ( case xs of
                 [_] -> newline <> indent offset <> byteString "until ("
                 _   -> byteString " until ("
             )
          <> condition 0 cond <> byteString ");"

      For a (At _ cond) b xs ->
        "for (" <> assignment a <> byteString "; "
                <> condition 0 cond <> byteString "; "
                <> assignment b <> byteString ")"
          <> block offset xs

      Repeat i xs        ->
        byteString "repeat (" <> rvalue 0 i <> byteString ")"
          <> block offset xs

      Switch r cs def    ->
        "switch (" <> rvalue 0 r <> byteString ") {"
          <> ( flip foldMap cs $ \(Case (At _ c) xs) ->
                 newline <> indent offset
                   <> byteString "case " <> rvalue 11 c <> byteString ":"
                        <> foldMap (\(At _ y) -> newline <> expr (tab offset) y) xs
             )
          <> ( case def of
                 NoDefault  -> mempty
                 Default xs ->
                   newline <> indent offset <> byteString "default:"
                     <> foldMap (\(At _ y) -> newline <> expr (tab offset) y) xs
             )
          <> newline <> indent offset <> byteString "}"

      With r vs          ->
        byteString "with (" <> reference r <> byteString ")"
          <> block offset vs
        where
          reference e =
            case e of
              Static i ->
                case i of
                  Object name -> shortByteString name
                  Self        -> byteString "self"
                  Other       -> byteString "other"
                  All         -> byteString "all"
                  Noone       -> byteString "noone"
                  Global      -> byteString "global"
                  Local       -> byteString "local"

              Dynamic (At _ v) -> rvalue 0 v



reassignment :: Binary -> Assignment -> Builder
reassignment bin (Assignment _ _ (At _ r) v) =
  varying r <> byteString " " <> fst (binary bin) <> byteString "= " <> rvalue 0 v

assignment :: Assignment -> Builder
assignment (Assignment _ _ (At _ r) v) =
  varying r <> byteString " = " <> rvalue 0 v

block :: Offset -> [At Expr] -> Builder
block offset xs =
  case xs of
    [At _ x] -> newline <> expr (tab offset) x
    _        ->
      " {" <> foldMap (\(At _ x) -> newline <> expr (tab offset) x) xs
           <> newline
           <> indent offset <> byteString "}"



if_ :: Offset -> Condition -> [At Expr] -> Else -> Builder
if_ offset cond thn els =
  "if (" <> condition 0 cond <> byteString ")"
    <> block offset thn
    <> case els of
         NoElse    -> mempty

         Else [At _ (If cond' thn' els')] ->
              ( case thn of
                  [_] -> newline <> indent offset <> byteString "else "
                  _   -> byteString " else "
              )
           <> if_ offset cond' thn' els'

         Else more ->
              ( case thn of
                  [_] -> newline <> indent offset <> byteString "else"
                  _   -> byteString " else"
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

    Not v                      -> byteString "!" <> condition 11 v



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
      in paren (p > prec) $
           rvalue prec l <> byteString " " <> op <> byteString " " <> rvalue prec r

    RCall name args             -> call name args


constant :: Constant -> Builder
constant c =
  case c of
    CDouble d  -> formatDouble standardDefaultPrecision d
    CInt32 i   -> int32Dec i
    CInt16 i   -> int16Dec i
    CString bs -> byteString "\"" <> shortByteString bs <> byteString "\""


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
            Object name -> shortByteString name <> byteString "."
            Self        -> byteString "self."
            Other       -> byteString "other."
            All         -> byteString "all."
            Noone       -> byteString "noone."
            Global      -> byteString "global."
            Local       -> mempty

        Dynamic (At _ v) -> byteString "instance (" <> rvalue 0 v <> byteString ")."

    dims (OneDim (At _ x))          = "[" <> rvalue 0 x <> byteString "]"
    dims (TwoDim (At _ x) (At _ y)) = "[" <> rvalue 0 x <> byteString "]["
                                          <> rvalue 0 y <> byteString "]"



compare_ :: Prec -> Comparison -> RValue -> RValue -> Builder
compare_ p cmp l r =
  paren (p > 6) $
    rvalue 6 l <> byteString " " <> comparison cmp <> byteString " " <> rvalue 6 r

comparison :: Comparison -> Builder
comparison cmp =
  case cmp of
    Lt -> byteString "<"
    Le -> byteString "<="
    Eq -> byteString "=="
    Ne -> byteString "!="
    Gt -> byteString ">"
    Ge -> byteString ">="


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
  shortByteString name <> byteString " (" <> inter ", " (\(At _ y) -> rvalue 0 y) args <> byteString ")"
