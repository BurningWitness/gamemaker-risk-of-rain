{-|
    This module decompiles raw bytecode into C-like.

    The bytecode is laughably poor, a direct interpretation of the original code.
    The few \"optimizations\" applied are easy to spot and relate to specific
    language constructs. As such to decompile the code no stack simulation is necessary,
    parsing suffices.

    Important notes:

      * Functions can receive arguments as variables named @argumentN@;

      * Calls read arguments in LIFO order; binary operations first consume
        the second argument, then the first one.
 -}

{-# LANGUAGE BangPatterns
           , DuplicateRecordFields
           , NoFieldSelectors
           , OverloadedRecordDot
           , OverloadedStrings #-}

module Decompile
  ( DataType (..)
  , Instance (..)
  , Asm.Comparison (..)
  , At (..)

  , Dimensions (..)
  , Constant (..)
  , Reference (..)
  , Variable (..)
  , Value (..)
  , Binary (..)
  , RValue (..)

  , Condition (..)

  , Else (..)

  , Case (..)
  , Default (..)

  , Assignment (..)
  , Expr (..)

  , Source (..)

  , Error (..)
  , StageError (..)
  , Stage2Error (..)
  , Stage3Error (..)
  , decompile
  ) where

import           Disassemble hiding (Error (..), Instruction (..), Variable (..))
import qualified Disassemble as Asm

import           Data.ByteString.Short (ShortByteString)
import           Data.Functor.Identity
import           Data.Int
import           Data.Primitive.SmallArray
import           Data.RadixTree.Word8.Strict (RadixTree)
import qualified Data.RadixTree.Word8.Strict as Radix
import           Data.Word
import           GameMaker.RiskOfRain.Decoder hiding (Error (..))



data Error = Error
               !ShortByteString
               !Word32
               !StageError
             deriving Show

data StageError = Stage2Error !Stage2Error
                | Stage3Error !Stage3Error
                  deriving Show

data Stage2Error = BackjumpOutOfBounds
                 | BackjumpMissInbound !Word32
                 | BackjumpMissIntoCase !Word32
                 | FrontjumpBeyondContext !Word32
                 | RValueOutOfBounds
                 | NotAnRValue !Word32
                 | InvalidSecondDimAssignment !Word32
                 | IncorrectSwitch !Word32
                 | ExpectedSwitch !Word32
                 | SurplusPopz !Word32
                 | SurplusPopEnvAny !Word32
                 | ExpectedPops !Word32
                 | EmptyCaseSlice !Word32
                 | MalformedSwitch !Word32
                 | NoRValueBeforeCase !Word32
                 | ExpectedDupOnInstanceReassign !Word32
                 | ExpectedDupOnArrayReassign !Word32
                 | ExpectedPopEnvAnyEscape !Word32
                 | ExpectedRepeatPopEnv !Word32
                 | MalformedRepeatPop !Word32
                 | InconvertibleCallReturn !Word32
                 | MalformedOr !Word32
                 | MalformedAnd !Word32
                 | UnknownStatement !Word32
                   deriving Show

data Stage3Error = FrontjumpOutOfBounds
                 | FrontjumpMissInbound !Word32
                 | MalformedConditional !Word32
                 | ExpectedConditional !Word32
                 | WrongConditionalJumpType !Word32
                 | UnknownLoopType !Word32
                 | TrailingInRepeat !Word32
                 | TrailingInCase !Word32
                 | TrailingInDefault !Word32
                 | TrailingInWith !Word32
                 | TrailingInIf !Word32
                 | TrailingInElse !Word32
                 | TrailingInLoop !Word32
                 | TrailingStatements
                 | UnhandledStatement !Word32
                   deriving Show



-- | Array dimensions. GameMaker only supports 1D and 2D arrays.
data Dimensions = OneDim !(At RValue)
                | TwoDim !(At RValue) !(At RValue)
                  deriving Show

-- | Constant values pushed onto stack.
data Constant = CDouble !Double
              | CInt32 !Int32
              | CInt16 !Int16
              | CString !ShortByteString
                deriving Show

-- | Reference to any particular 'Instance', either at compilation time or at runtime.
data Reference = Static !Instance
               | Dynamic !(At RValue)
                 deriving Show

data Variable = Var !Reference !ShortByteString
              | VArray !Dimensions !Reference !ShortByteString
                deriving Show

-- | Anything pushable on the stack.
data Value = Constant !Constant
           | Varying !Variable
             deriving Show

data Binary = Mul
            | Div
            | Add
            | Sub
            | And
            | Or
            | Xor
            | Shl
            | Shr
            | Rem
            | Mod
              deriving Show

-- | Any expression that yields anything assignable.
data RValue = Value      !Value
            | RCompare   !Asm.Comparison !DataType !DataType !RValue !(At RValue)
            | Cast       !DataType !DataType !RValue
            | Negate     !DataType !RValue
            | Complement !DataType !RValue
            | Binary     !Binary !DataType !DataType !RValue !(At RValue)
            | RCall      !ShortByteString ![At RValue]
              deriving Show



data Stage1 = Normal1 [At Asm.Instruction]

              -- | Holds the offset of the start of the first condition,
              --   all comparisons after the first one,
              --   the offset of the end of the switch, and the body
            | Switch1 !Word32 [At Asm.Instruction] !Word32 [At Stage1]

               -- | Degenerate default-only switch
            | Default1 !Word32 !Word32 [At Stage1]
              deriving Show

-- | Traverses the instruction list front to back and cuts out the case statements.
stage1 :: [At Asm.Instruction] -> Identity [At Stage1]
stage1 xs =
  case xs of
    At n _ : _ -> inner n id xs
    []         -> Identity []
  where
    inner loc acc as =
      case as of
        At n a : bs ->
          case a of
            Asm.Bt (Int23 j) | j > 0 ->
              let to = fromIntegral $ fromIntegral n + (j - 1) * 4
                  (css, mv, middle) = limit to bs

              in case mv of
                   Just (At m (Asm.B (Int23 k))) ->
                     let to' = fromIntegral $ fromIntegral m + k * 4
                         (raw, mx, cs) = limit to' middle

                     in case mx of
                          Just (At o (Asm.Popz _)) -> do
                            defs <- stage1 raw
                            rest <- stage1 cs
                            let new = At (n + 4) (Switch1 (m + 4) css o defs)
                            Identity $ case acc [] of
                                         [] -> new:rest
                                         ls -> At loc (Normal1 ls) : new : rest

                          _ -> inner loc (acc . (:) (At n a)) bs

                   _               ->
                     inner loc (acc . (:) (At n a)) bs

            Asm.B (Int23 2) ->
              case bs of
                b@(At m (Asm.B (Int23 j))) : cs ->
                  let to = fromIntegral $ fromIntegral m + j * 4
                      (raw, mv, ds) = limit to cs

                  in case mv of
                       Just (At o (Asm.Popz Variable)) -> do
                         defs <- stage1 raw
                         rest <- stage1 ds
                         let new = At (n + 4) (Default1 (m + 4) o defs)
                         Identity $ case acc [] of
                                      [] -> new:rest
                                      ls -> At loc (Normal1 ls) : new : rest

                       _                      ->
                         inner loc (acc . (:) (At n a) . (:) b) cs

                _ -> inner loc (acc . (:) (At n a)) bs

            _ -> inner loc (acc . (:) (At n a)) bs

        [] -> Identity $ case acc [] of
                          [] -> []
                          ls -> [At loc (Normal1 ls)]

    limit
      :: Word32 -> [At Asm.Instruction]
      -> ([At Asm.Instruction], Maybe (At Asm.Instruction), [At Asm.Instruction])
    limit j as =
      case as of
        At n a : bs ->
          case compare n j of
            LT -> let (cs, mv, ds) = limit j bs
                  in (At n a : cs, mv, ds)

            EQ -> ([], Just (At n a), bs)

            GT -> ([], Nothing, At n a : bs)

        [] -> ([], Nothing, [])



backlimit :: Word32 -> Word32 -> [At a] -> Either (Word32, Stage2Error) ([At a], [At a])
backlimit pos j = go
  where
    go as =
      case as of
        At n a : bs ->
          case compare n j of
            GT -> do (cs, ds) <- go bs
                     Right (At n a : cs, ds)

            EQ -> Right ([At n a], bs)

            LT -> Left (pos, BackjumpMissInbound n)

        [] -> Left (pos, BackjumpOutOfBounds)



-- | Writing a variable to memory.
data Assignment = Assignment !DataType !DataType !(At Variable) !RValue
                  deriving Show

data Jump = J | Jt | Jf
            deriving Show

data Case2 = Case2 !(At RValue) !Word32 [At Stage2]
             deriving Show

data Default2 = NoDefault2
              | Default2 Word32 [At Stage2]
                deriving Show

data EnvOut2 = NoEnvOut2
             | EnvOut2
               deriving Show

data Stage2 = Assign2   !Assignment
            | Reassign2 !Binary !DataType !DataType !Assignment
            | Call2     !ShortByteString [At RValue]
            | Return2   !RValue
            | Exit2
            | Continue2
            | Break2    !Jump

            | Loop2     !Jump !Word32 [At Stage2]
            | Repeat2   !RValue !Word32 [At Stage2]
            | Switch2   !RValue [Case2] !Default2
            | Env2      !Reference !Word32 [At Stage2]

            | Cast2     !DataType !DataType !RValue
            | Compare2  !Asm.Comparison !DataType !DataType !RValue !(At RValue)
            | Jump2     !Jump !Word32
            | Not2      !RValue

              -- These form &&, || and ! statements with 2+ elements
            | Allow2    -- ^ B 2; Push 1
            | Deny2     -- ^ B 2; Push 0
            | AllowNot2 -- ^ B 2; Push 1; Not
            | DenyNot2  -- ^ B 2; Push 0; Not
              deriving Show

data Context2 =
       Context2
         { places :: [Inside2]
         , lower  :: !Word32   -- ^ Lower area boundary
         , upper  :: !Word32   -- ^ Upper area boundary
         , break  :: !Word32   -- ^ Break point, if applicable
         }
       deriving Show

data Inside2 = Back2    -- ^ In a loop
             | Def2     -- ^ In a case definition
             | With2    -- ^ In a with() statement
             | Without2 -- ^ In a with() statement with a 'PopEnvAny' escape
               deriving Show

-- | Traverses the instruction list back to front, converting simple operations
--   into 'RValue's, cutting out backwards jumps and categorizing forwards ones.
stage2
  :: Word32 -> Word32 -> Strg -> Objt -> [At Stage1]
  -> Either (Word32, Stage2Error) [At Stage2]
stage2 offset size strgs objts xs = do
  At _ r <- process2 strgs objts (Context2 [] offset (offset + size) (offset + size)) (invert2 xs)
  Right r

invert2 :: [At Stage1] -> [At Stage1]
invert2 = go []
  where
    go rs as =
      case as of
        At n a : bs ->
          let a' = case a of
                     Normal1 cs        -> Normal1 (reverse cs)
                     Switch1 m cs o ds -> Switch1 m (reverse cs) o (invert2 ds)
                     Default1 m o ds   -> Default1 m o (invert2 ds)

          in go (At n a' : rs) bs

        [] -> rs



backlimit2
  :: Word32 -> Word32 -> [At Stage1]
  -> Either (Word32, Stage2Error) ([At Stage1], [At Stage1])
backlimit2 pos j = go
  where
    go as =
      case as of
        At n a : bs ->
          case compare n j of
            GT -> do
              (cs, ds) <- go bs
              Right (At n a : cs, ds)

            EQ ->
              case a of
                Normal1 _ -> Right ([At n a], bs)
                _ -> Left (pos, BackjumpMissIntoCase n)

            LT ->
              case a of
                Normal1 os -> do
                  (ls', rs') <- backlimit pos j os

                  let ls = case ls' of
                             _:_ -> At j (Normal1 ls') : []
                             []  -> []

                      rs = case rs' of
                             _:_ -> At n (Normal1 rs') : bs
                             []  -> bs

                  Right (ls, rs)

                _ -> Left (pos, BackjumpMissIntoCase n)

        [] -> Left (pos, BackjumpOutOfBounds)



process2
  :: Strg -> Objt -> Context2 -> [At Stage1]
  -> Either (Word32, Stage2Error) (At [At Stage2])
process2 strgs objts ctx = go (ctx.upper)
  where
    go loc0 xs =
      case xs of
        At loc (Normal1 as) : ys          ->
          case as of
            [] -> go loc0 ys
            _  -> do
              (At loc2 bs, more) <- normal2 strgs objts loc ctx ys loc loc0 as
              At loc3 cs <- go loc2 more
              Right (At loc3 (bs <> cs))

        At loc (Switch1 lower css upper dfs) : ys -> do
          case ys of
            At loc1 (Normal1 as) : zs -> do
              case as of
                At _ (Asm.Cmp Eq _ _) : bs -> do
                  (r0@(At loc2 _), cs) <- rvalue2 strgs objts loc loc1 bs

                  case cs of
                    At _ (Asm.Dup _ _) : ds -> do
                      (At loc3 ra, es) <- rvalue2 strgs objts loc loc2 ds

                      let cases is =
                            case is of
                              _:_ -> do
                                (n, r, js) <- case2 strgs objts loc loc1 is
                                ls <- cases js
                                Right ((r, n) : ls)

                              []  -> Right [(r0, lower)]

                          slicer edge ns is =
                            case ns of
                              (r, n) : os@(_:_) ->
                                if n == edge
                                  then do
                                    rest <- slicer edge os is
                                    Right $ Case2 r edge [] : rest

                                  else do
                                    (js', ks) <- backlimit2 loc n is

                                    let ctx' = Context2 (Def2 : ctx.places) n edge upper
                                    At _ js <- process2 strgs objts ctx' js'

                                    rest <- slicer n os ks
                                    Right $ Case2 r edge js : rest

                              [(r, _)]  -> do
                                let ctx' = Context2 (Def2 : ctx.places) loc edge upper
                                At _ defs <- process2 strgs objts ctx' is

                                Right [Case2 r edge defs]

                              [] -> Left (loc, EmptyCaseSlice edge)

                      (upper', css', dfs', defz) <-
                        case css of
                          At n (Asm.B (Int23 j)) : css' -> do
                            let to = fromIntegral $ fromIntegral n + j * 4

                            (dfz, dfp) <- backlimit2 loc to dfs

                            let ctx' = Context2 (Def2 : ctx.places) to upper upper
                            At _ defz <- process2 strgs objts ctx' dfz

                            Right (to, css', dfp, Default2 upper defz)

                          _ -> Right (upper, css, dfs, NoDefault2)

                      ns <- cases css'
                      defs <- slicer upper' ns dfs'

                      At loc4 b <- go loc3 (At loc1 (Normal1 es) : zs)

                      let this = At loc3 (Switch2 ra defs defz)
                      Right $ At loc4 (this : b)

                    _ -> Left (loc, MalformedSwitch loc)

                _ -> Left (loc, MalformedSwitch loc)

            _ -> Left (loc, NoRValueBeforeCase loc)

        At loc (Default1 lower upper ds) : ys -> do
          case ys of
            At origin (Normal1 as) : zs -> do
              (At loc2 r, bs) <- rvalue2 strgs objts loc loc as

              let ctx' = Context2 (Def2 : ctx.places) lower upper upper
              At _    defs <- process2 strgs objts ctx' ds

              At loc4 rest <- process2 strgs objts ctx $ At origin (Normal1 bs) : zs

              Right . At loc4 $ At loc2 (Switch2 r [] (Default2 loc4 defs)) : rest

            _ -> Left (loc, NoRValueBeforeCase loc)

        [] -> pure (At 0 [])



case2
  :: Strg -> Objt -> Word32 -> Word32 -> [At Asm.Instruction]
  -> Either (Word32, Stage2Error) (Word32, At RValue, [At Asm.Instruction])
case2 strgs objts pos loc xs =
  case xs of
    At n (Asm.Bt (Int23 j)) : At _ (Asm.Cmp Eq _ _)
                            : bs -> do
      (At loc1 r, cs) <- rvalue2 strgs objts pos loc bs
      case cs of
        At _ (Asm.Dup 0 _) : ds ->
          let to = fromIntegral $ fromIntegral n + j * 4
          in Right (to, At loc1 r, ds)

        _ -> Left (pos, IncorrectSwitch loc)

    _ -> Left (pos, ExpectedSwitch loc)



normal2
  :: Strg -> Objt -> Word32 -> Context2 -> [At Stage1] -> Word32 -> Word32
  -> [At Asm.Instruction]
  -> Either (Word32, Stage2Error) (At [At Stage2], [At Stage1])
normal2 strgs objts pos ctx rs origin = go
  where
    go loc xs =
      case xs of
        At loc1 i : ys ->
          case i of
            Asm.Cmp cmp dt1 dt2 -> do
              (r@(At loc2 _), as) <- rvalue2 strgs objts pos loc1 ys
              (   At loc3 l , bs) <- rvalue2 strgs objts pos loc2 as
              let this = At loc3 (Compare2 cmp dt1 dt2 l r)
              Right (At loc3 [this], At origin (Normal1 bs) : rs)

            Asm.Pop to from inst name Asm.Normal -> do
              (At loc2 r, as) <- rvalue2 strgs objts pos loc1 ys
              let this = At loc2 (Assign2 (Assignment to from (At loc1 (Var (Static inst) name)) r))
              Right (At loc2 [this], At origin (Normal1 as) : rs)

            Asm.Pop to from _inst name Asm.StackTop ->
              case ys of
                At loc2 j : zs@(At _ nonBreak : _) | Just (bin, dt1, dt2) <- binary2 j
                                                   , not $ break2 nonBreak -> do
                  (At loc3 r  , as) <- rvalue2 strgs objts pos loc2 zs
                  case as of
                    At _ (Asm.PushCst (PushVari _ _ Asm.StackTop)) : At _ (Asm.Dup 0 _) : as' -> do
                      (At loc4 inst, bs) <- instance2 strgs objts pos loc3 as' name
                      let this = At loc4 (Reassign2 bin dt1 dt2 (Assignment to from (At loc2 inst) r))
                      Right (At loc4 [this], At origin (Normal1 bs) : rs)

                    _ -> Left (pos, ExpectedDupOnInstanceReassign loc3)

                _ -> do
                  (At loc2 j, as) <- instance2 strgs objts pos loc1 ys name
                  (At loc3 r, bs) <- rvalue2 strgs objts pos loc2 as
                  let this = At loc3 (Assign2 (Assignment to from (At loc2 j) r))
                  Right (At loc3 [this], At origin (Normal1 bs) : rs)

            Asm.Pop to from _inst name Asm.Array ->
              case ys of
                At loc2 j : zs@(At _ nonBreak : _) | Just (bin, dt1, dt2) <- binary2 j
                                                   , not $ break2 nonBreak -> do
                  (At loc3 r  , as) <- rvalue2 strgs objts pos loc2 zs
                  case as of
                    At _ (Asm.PushCst (PushVari _ _ Asm.Array)) : At _ (Asm.Dup 1 _) : as' -> do
                      (At loc4 arr, bs) <- array2 strgs objts pos loc3 as' name
                      let this = At loc4 (Reassign2 bin dt1 dt2 (Assignment to from (At loc2 arr) r))
                      Right (At loc4 [this], At origin (Normal1 bs) : rs)

                    _ -> Left (pos, ExpectedDupOnArrayReassign loc3)

                _ -> do
                  (At loc2 j, as) <- array2 strgs objts pos loc1 ys name
                  (At loc3 r, bs) <- rvalue2 strgs objts pos loc2 as
                  let this = At loc3 (Assign2 (Assignment to from (At loc2 j) r))
                  Right (At loc3 [this], At origin (Normal1 bs) : rs)

            Asm.Exit Int32 -> do
              At loc2 zs <- cull2 pos loc1 ctx.places ys
              Right (At loc2 [At loc1 Exit2], At origin (Normal1 zs) : rs)

            Asm.PopEnvAny ->
              case ys of
                At _ (Asm.B (Int23 2)) : At loc2 (Asm.PopEnv (Int23 j)) : zs ->
                  env2 loc2 EnvOut2 zs j

                _ -> Left (pos, ExpectedPopEnvAnyEscape loc1)

            Asm.Popz Int32 ->
              case ys of
                At _ (Asm.Bt (Int23 j)) : At _    (Asm.Conv Int32 Boolean)
                                        : At _    (Asm.Dup _ Int32)
                                        : At _    (Asm.Sub Int32 Int32)
                                        : At locr (Asm.PushCst (PushInt32 1))
                                        : zs -> do

                  let to = fromIntegral $ fromIntegral loc1 + 4 * (j - 1)
                  (as, bs) <- backlimit2 pos to (At origin (Normal1 zs) : rs)

                  case bs of
                    At origin2 (Normal1 (At _ (Asm.Bt _) : At _ (Asm.Cmp Le Int32 Int32)
                                                         : At _ (Asm.PushCst (PushInt32 0))
                                                         : At m (Asm.Dup _ Int32)
                                                         : cs)) : ns -> do

                      (At loc2 r, ds) <- rvalue2 strgs objts pos m cs

                      let ctx' = Context2 (Back2 : ctx.places) to locr loc1
                      At _    defs <- process2 strgs objts ctx' as
                      At loc4 rest <- process2 strgs objts ctx (At origin2 (Normal1 ds) : ns)
                      Right (At loc4 (At loc2 (Repeat2 r locr defs) : rest), [])

                    _   -> Left (pos, MalformedRepeatPop loc)

                _   -> Left (pos, MalformedRepeatPop loc)

            Asm.Popz Variable -> do
              (At loc2 r, as) <- rvalue2 strgs objts pos loc1 ys
              case r of
                RCall name args -> do
                  let this = At loc2 (Call2 name args)
                  Right (At loc2 [this], At origin (Normal1 as) : rs)

                _   -> Left (pos, InconvertibleCallReturn loc)

            Asm.Ret Variable -> do
              (At loc2 r, as) <- rvalue2 strgs objts pos loc1 ys

              At loc3 bs <- cull2 pos loc2 ctx.places as
              Right (At loc3 [At loc2 (Return2 r)], At origin (Normal1 bs) : rs)

            Asm.Conv from to -> do
              (At loc2 r, as) <- rvalue2 strgs objts pos loc1 ys
              Right (At loc2 [At loc2 (Cast2 from to r)], At origin (Normal1 as) : rs)

            Asm.Not Boolean ->
              case ys of
                At _ (Asm.PushCst (PushInt16 0)) : At loc2 (Asm.B (Int23 2)) : zs ->
                  Right (At loc2 [At loc2 DenyNot2], At origin (Normal1 zs) : rs)

                At _ (Asm.PushCst (PushInt16 1)) : At loc2 (Asm.B (Int23 2)) : zs ->
                  Right (At loc2 [At loc2 AllowNot2], At origin (Normal1 zs) : rs)

                _   -> do
                  (At loc2 r, as) <- rvalue2 strgs objts pos loc1 ys
                  Right (At loc2 [At loc2 (Not2 r)], At origin (Normal1 as) : rs)

            Asm.B  (Int23 j) -> jump2 loc1 ys J  j
            Asm.Bt (Int23 j) -> jump2 loc1 ys Jt j
            Asm.Bf (Int23 j) -> jump2 loc1 ys Jf j

            Asm.PopEnv (Int23 j) -> env2 loc1 NoEnvOut2 ys j

            Asm.PushCst (PushInt16 0) ->
              case ys of
                At loc2 (Asm.B (Int23 2)) : zs ->
                  Right (At loc2 [At loc2 Deny2], At origin (Normal1 zs) : rs)

                _   -> Left (pos, MalformedOr loc)

            Asm.PushCst (PushInt16 1) ->
              case ys of
                At loc2 (Asm.B (Int23 2)) : zs ->
                  Right (At loc2 [At loc2 Allow2], At origin (Normal1 zs) : rs)

                _   -> Left (pos, MalformedAnd loc)

            _   -> Left (pos, UnknownStatement loc)

        [] -> Right (At loc [], rs)


    jump2
      :: Word32 -> [At Asm.Instruction] -> Jump -> Int32
      -> Either (Word32, Stage2Error) (At [At Stage2], [At Stage1])
    jump2 loc xs jmp j =
      if j < 0
        then backjump
        else case ctx.places of
               Without2 : _
                 | J <- jmp, to == ctx.upper + 8 ->
                     Right (At loc [At loc (Break2 J)], At origin (Normal1 xs) : rs)

                 | otherwise -> do
                     new <- frontjump
                     Right (new, At origin (Normal1 xs) : rs)

               _            -> do
                 new <- frontjump
                 Right (new, At origin (Normal1 xs) : rs)
      where
        to :: Word32
        to = fromIntegral $ fromIntegral loc + 4 * j

        backjump :: Either (Word32, Stage2Error) (At [At Stage2], [At Stage1])
        backjump =
          case compare to ctx.lower of
            GT -> backslice
            EQ -> let loopsOnly p =
                        case p of
                          Back2    -> True
                          Def2     -> False
                          With2    -> True
                          Without2 -> True

                  in case filter loopsOnly ctx.places of
                       [] -> backslice
                       _  -> Right (At loc [At loc Continue2], At origin (Normal1 xs) : rs)

            LT -> Left (pos, BackjumpMissInbound loc)

        backslice :: Either (Word32, Stage2Error) (At [At Stage2], [At Stage1])
        backslice = do
          (as, bs) <- backlimit2 pos to (At origin (Normal1 xs) : rs)

          let ctx' = Context2 (Back2 : ctx.places) to loc (loc + 4)
          At _    cs   <- process2 strgs objts ctx' as
          At loc2 rest <- process2 strgs objts ctx bs
          Right (At loc2 (At to (Loop2 jmp loc cs) : rest), [])

        frontjump :: Either (Word32, Stage2Error) (At [At Stage2])
        frontjump =
          if to == ctx.break
            then Right (At loc [At loc (Break2 jmp)])
            else if to > ctx.upper
                   then Left (pos, FrontjumpBeyondContext loc)
                   else Right (At loc [At loc (Jump2 jmp to)])


    env2
      :: Word32 -> EnvOut2 -> [At Asm.Instruction] -> Int32
      -> Either (Word32, Stage2Error) (At [At Stage2], [At Stage1])
    env2 loc out xs j = do
      let to = fromIntegral $ fromIntegral loc + 4 * j
      (as, bs) <- backlimit2 pos to (At origin (Normal1 xs) : rs)

      case bs of
        At origin2 (Normal1 (At _ (Asm.PushEnv _) : cs)) : ns -> do
          (r@(At loc1 _), ds) <- rvalue2 strgs objts pos loc cs

          let place = case out of
                        NoEnvOut2 -> With2
                        EnvOut2   -> Without2

              ctx' = Context2 (place : ctx.places) to loc (loc + 4)

          At _    defs <- process2 strgs objts ctx' as
          At loc4 rest <- process2 strgs objts ctx  (At origin2 (Normal1 ds) : ns)
          Right (At loc4 (At loc1 (Env2 (reference2 strgs objts r) loc defs) : rest), [])

        _   -> Left (pos, ExpectedRepeatPopEnv loc)



-- | Removes context Pops on Ret and Exit. They're processed in arbitrary order because
--   the bytecode doesn't seem to have any consistency.
cull2
  :: Word32 -> Word32 -> [Inside2] -> [At Asm.Instruction]
  -> Either (Word32, Stage2Error) (At [At Asm.Instruction])
cull2 pos loc is0 =
  let (n, m) = mush 0 0 is0
  in go n m loc
  where
    mush :: Int -> Int -> [Inside2] -> (Int, Int)
    mush !n !m is =
      case is of
        i : js ->
          case i of
            Back2    -> mush n m js
            Def2     -> let (n', m') = mush n m js
                        in (n' + 1, m')

            With2    -> let (n', m') = mush n m js
                        in (n', m' + 1)

            Without2 -> let (n', m') = mush n m js
                        in (n', m' + 1)

        []     -> (0, 0)

    go
      :: Int -> Int -> Word32 -> [At Asm.Instruction]
      -> Either (Word32, Stage2Error) (At [At Asm.Instruction])
    go n m loc0 xs
      | n <= 0, m <= 0 = Right (At loc0 xs)
      | otherwise      =
          case xs of
            At loc1 (Asm.Popz Variable) : ys ->
              if n > 0
                then go (n - 1) m loc1 ys
                else Left (pos, SurplusPopz loc)

            At loc1 Asm.PopEnvAny : ys ->
              if m > 0
                then go n (m - 1) loc1 ys
                else Left (pos, SurplusPopEnvAny loc)

            _ -> Left (pos, ExpectedPops loc)



rvalue2
  :: Strg -> Objt -> Word32 -> Word32 -> [At Asm.Instruction]
  -> Either (Word32, Stage2Error) (At RValue, [At Asm.Instruction])
rvalue2 strgs objts pos loc xs =
  case xs of
    At loc1 i : ys ->
      let double as f = do
            (r@(At loc2 _), bs) <- rvalue2 strgs objts pos loc1 as
            (   At loc3 l , cs) <- rvalue2 strgs objts pos loc2 bs
            Right (At loc3 (f l r), cs)

      in case i of
           Asm.Conv from to -> do
             (At loc2 r, zs) <- rvalue2 strgs objts pos loc ys
             Right (At loc2 (Cast from to r), zs)

           Asm.Mul dt1 dt2 -> double ys $ Binary Mul dt1 dt2
           Asm.Div dt1 dt2 -> double ys $ Binary Div dt1 dt2
           Asm.Rem dt1 dt2 -> double ys $ Binary Rem dt1 dt2
           Asm.Mod dt1 dt2 -> double ys $ Binary Mod dt1 dt2
           Asm.Add dt1 dt2 -> double ys $ Binary Add dt1 dt2
           Asm.Sub dt1 dt2 -> double ys $ Binary Sub dt1 dt2
           Asm.And dt1 dt2 -> double ys $ Binary And dt1 dt2
           Asm.Or  dt1 dt2 -> double ys $ Binary Or  dt1 dt2
           Asm.Xor dt1 dt2 -> double ys $ Binary Xor dt1 dt2

           Asm.Neg dt      -> do (At loc2 v, zs) <- rvalue2 strgs objts pos loc1 ys
                                 Right (At loc2 (Negate dt v), zs)

           Asm.Not dt      -> do (At loc2 v, zs) <- rvalue2 strgs objts pos loc1 ys
                                 Right (At loc2 (Complement dt v), zs)

           Asm.Shl dt1 dt2 -> double ys $ Binary Shl dt1 dt2
           Asm.Shr dt1 dt2 -> double ys $ Binary Shr dt1 dt2

           Asm.Cmp cmp dt1 dt2 -> double ys $ RCompare cmp dt1 dt2

           Asm.PushCst (PushStrg n) ->
             Right (At loc1 (Value (Constant (CString n))), ys)

           Asm.PushCst (PushInt16 n) ->
             Right (At loc1 (Value (Constant (CInt16 n))), ys)

           Asm.PushCst (PushInt32 n) ->
             Right (At loc1 (Value (Constant (CInt32 n))), ys)

           Asm.PushCst (PushDouble n) ->
             Right (At loc1 (Value (Constant (CDouble n))), ys)

           Asm.PushCst (PushVari inst name Asm.Normal) -> do
             Right (At loc1 (Value (Varying (Var (Static inst) name))), ys)

           Asm.PushCst (PushVari _ name Asm.StackTop) -> do
             (At loc2 arr, zs) <- instance2 strgs objts pos loc1 ys name
             Right (At loc2 (Value (Varying arr)), zs)

           Asm.PushCst (PushVari _ name Asm.Array) -> do
             (At loc2 arr, zs) <- array2 strgs objts pos loc1 ys name
             Right (At loc2 (Value (Varying arr)), zs)

           Asm.PushLoc (PushVari Local name Asm.Normal) ->
             Right (At loc1 (Value (Varying (Var (Static Local) name))), ys)

           Asm.PushGlb (PushVari Global name Asm.Normal) ->
             Right (At loc1 (Value (Varying (Var (Static Global) name))), ys)

           Asm.PushVar Self Variable name Asm.Normal ->
             Right (At loc1 (Value (Varying (Var (Static Self) name))), ys)

           Asm.PushI16 v Int16 ->
             Right (At loc1 (Value (Constant (CInt16 v))), ys)

           Asm.Call count Int32 name Asm.Array -> do
             let get loc0 as n
                   | n <= 0    = Right (loc0, [], as)
                   | otherwise = do
                       (At loc2 r, bs) <- rvalue2 strgs objts pos loc1 as
                       (loc3, rs, cs) <- get loc2 bs (n - 1)
                       Right (loc3, At loc2 r : rs, cs)

             (loc', args, zs) <- get loc1 ys count
             Right (At loc' (RCall name args), zs)

           _ -> Left (pos, NotAnRValue loc1)

    [] -> Left (pos, RValueOutOfBounds)



break2 :: Asm.Instruction -> Bool
break2 (Asm.Break _ _) = True
break2 _               = False

binary2 :: Asm.Instruction -> Maybe (Binary, DataType, DataType)
binary2 i =
  case i of
    Asm.Mul dt1 dt2 -> Just (Mul, dt1, dt2)
    Asm.Div dt1 dt2 -> Just (Div, dt1, dt2)
    Asm.Rem dt1 dt2 -> Just (Rem, dt1, dt2)
    Asm.Mod dt1 dt2 -> Just (Mod, dt1, dt2)
    Asm.Add dt1 dt2 -> Just (Add, dt1, dt2)
    Asm.Sub dt1 dt2 -> Just (Sub, dt1, dt2)
    Asm.And dt1 dt2 -> Just (And, dt1, dt2)
    Asm.Or  dt1 dt2 -> Just (Or , dt1, dt2)
    Asm.Xor dt1 dt2 -> Just (Xor, dt1, dt2)
    Asm.Shl dt1 dt2 -> Just (Shl, dt1, dt2)
    Asm.Shr dt1 dt2 -> Just (Shr, dt1, dt2)
    _               -> Nothing



reference2 :: Strg -> Objt -> At RValue -> Reference
reference2 strgs objts (At loc r) =
  case r of
    Value (Constant (CInt16 i)) | Right inst <- convertInstance strgs objts i ->
      Static inst

    _ -> Dynamic (At loc r)

instance2
  :: Strg -> Objt -> Word32 -> Word32 -> [At Asm.Instruction] -> ShortByteString
  -> Either (Word32, Stage2Error) (At Variable, [At Asm.Instruction])
instance2 strgs objts pos loc xs name = do
  (r@(At loc1 _), ys) <- rvalue2 strgs objts pos loc xs
  Right (At loc1 (Var (reference2 strgs objts r) name), ys)

array2
  :: Strg -> Objt -> Word32 -> Word32 -> [At Asm.Instruction] -> ShortByteString
  -> Either (Word32, Stage2Error) (At Variable, [At Asm.Instruction])
array2 strgs objts pos loc xs name =
  case xs of
    At _ (Asm.Add Int32 Int32) : At _ (Asm.Break (-1) Int16)
                               : as -> do

      (y@(At loc1 _), bs) <- rvalue2 strgs objts pos loc as

      case bs of
        At _ (Asm.Mul Int32 Int32) : At _ (Asm.PushCst (PushInt32 32000))
                                   : At _ (Asm.Break (-1) Int16)
                                   : cs -> do

          (x@(At loc2 _), ds) <- rvalue2 strgs objts pos loc1 cs
          (j@(At loc3 _), es) <- rvalue2 strgs objts pos loc2 ds
          Right (At loc3 (VArray (TwoDim x y) (reference2 strgs objts j) name), es)

        _ -> Left (pos, InvalidSecondDimAssignment loc)

    as -> do
      (x@(At loc1 _), bs) <- rvalue2 strgs objts pos loc  as
      (j@(At loc2 _), cs) <- rvalue2 strgs objts pos loc1 bs
      Right (At loc2 (VArray (OneDim x) (reference2 strgs objts j) name), cs)



data Condition = Const   !DataType !DataType !RValue
               | Compare !Asm.Comparison !DataType !DataType !RValue !(At RValue)
               | Every   [At Condition]
               | Any     [At Condition]
               | Not     !Condition
                 deriving Show

data Else = NoElse
          | Else [At Expr]
            deriving Show

data Case = Case !(At RValue) [At Expr]
            deriving Show

data Default = NoDefault
             | Default [At Expr]
               deriving Show

data Expr = Declare  !Assignment
          | Assign   !Assignment
          | Reassign !Binary !DataType !DataType !Assignment
          | Call     !ShortByteString [At RValue]
          | Return   !RValue
          | Exit
          | Continue
          | Break
          | If       !Condition [At Expr] !Else
          | While    !Condition [At Expr]
          | Do       !(At Condition) [At Expr]
          | For      !Assignment !(At Condition) !Assignment [At Expr]
          | Repeat   !RValue [At Expr]
          | Switch   !RValue [Case] !Default
          | With     !Reference [At Expr]
            deriving Show



newtype Context3 =
          Context3
            { upper :: Word32 -- ^ Upper area boundary
            }
          deriving Show

newtype Locals = Locals (RadixTree ())
                 deriving Show

makeLocals :: SmallArray ShortByteString -> Locals
makeLocals args = go 1 Radix.empty
  where
    go n !rad
      | n >= sizeofSmallArray args = Locals rad
      | otherwise                  =
          go (n + 1) $ addLocal_ (indexSmallArray args n) rad

addLocal :: ShortByteString -> Locals -> Locals
addLocal bs (Locals ls) = Locals $ addLocal_ bs ls

addLocal_ :: ShortByteString -> RadixTree () -> RadixTree ()
addLocal_ bs ls = Radix.insert (Radix.feedShortByteString bs) () ls

isDeclared :: Locals -> ShortByteString -> Bool
isDeclared (Locals ls) bs = Radix.member (Radix.feedShortByteString bs) ls



-- | Traverses the instruction list front to back, cuts out conditionals and
--   figures out while/do/for loops.
stage3 :: Word32 -> SmallArray ShortByteString -> [At Stage2] -> Either (Word32, Stage3Error) [At Expr]
stage3 size args xs = do
  (ys, mayTrail, _) <- process3 (Context3 size) (makeLocals args) $ invert3 xs
  case mayTrail of
    Nothing -> Right ys
    Just _  -> Left (0, TrailingStatements)

invert3 :: [At Stage2] -> [At Stage2]
invert3 = go []
  where
    go rs as =
      case as of
        At n a : bs ->
          let a' = case a of
                     Loop2   j l xs  -> Loop2 j l (invert3 xs)
                     Repeat2 r l xs  -> Repeat2 r l (invert3 xs)
                     Switch2 r cs ds -> Switch2 r (incase cs []) (indef ds)
                     Env2    r l xs  -> Env2 r l (invert3 xs)
                     _               -> a

          in go (At n a' : rs) bs

        [] -> rs

    incase ((Case2 r l xs):ys) zs = incase ys (Case2 r l (invert3 xs):zs)
    incase []                  zs = zs

    indef NoDefault2      = NoDefault2
    indef (Default2 l xs) = Default2 l (invert3 xs)



newtype Trailing3 = Condition3 Condition

process3
  :: Context3 -> Locals -> [At Stage2]
  -> Either (Word32, Stage3Error) ([At Expr], Maybe (At Trailing3), Locals)
process3 ctx locals xs =
  case xs of
    _:_ -> do
      (as, locals1, eiYs) <- advance3 ctx locals xs
      case eiYs of
        Right ys -> do
          (bs, mayTrail, locals2) <- process3 ctx locals1 ys
          Right (as <> bs, mayTrail, locals2)

        Left trail -> Right (as, Just trail, locals1)

    []  -> Right ([], Nothing, locals)

advance3
  :: Context3 -> Locals -> [At Stage2]
  -> Either (Word32, Stage3Error) ([At Expr], Locals, Either (At Trailing3) [At Stage2])
advance3 ctx locals xs =
  case xs of
    At n a : ys ->
      case a of
        Assign2 x@(Assignment _ _ (At _ v) _) -> do
          let tryFor inst name = do
                (as, locals', zs) <- advance3 ctx locals ys
                Right $ 
                  case as of
                    [At loc (While cond thn)]
                      | conditionHas3 inst name cond
                      , For3 inc thn' <- for3 inst name thn ->
                          let this = For x (At loc cond) inc thn'
                          in ([At n this], locals', zs)

                    _ -> ([At n (Assign x)], locals, Right ys)

          case v of
            Var (Static Local) name
              | not $ isDeclared locals name -> do
                  let !locals' = addLocal name locals

                  Right $ ([At n (Declare x)], locals', Right ys)

              | otherwise -> tryFor Local name

            Var (Static Self) name -> tryFor Self name

            _ -> Right ([At n (Assign x)], locals, Right ys)

        Reassign2 bin to from x -> Right ([At n (Reassign bin to from x)], locals, Right ys)
        Call2     name args     -> Right ([At n (Call name args)], locals, Right ys)
        Return2   r             -> Right ([At n (Return r)], locals, Right ys)
        Exit2                   -> Right ([At n Exit], locals, Right ys)
        Continue2               -> Right ([At n Continue], locals, Right ys)
        Break2 J                -> Right ([At n Break], locals, Right ys)

        Loop2 jmp upper ds      -> do
          (ds', mayTrail, _) <- process3 (Context3 upper) locals ds
          case jmp of
            J  -> case mayTrail of
                    Just _ ->
                      Left (n, TrailingInLoop n)

                    Nothing ->
                      case ds' of
                        [At loc (If cond thn (Else [At _ Break]))] ->
                          Right ([At loc (While cond thn)], locals, Right ys)

                        _ -> Left (n, UnknownLoopType n)

            Jf -> case mayTrail of
                    Nothing ->
                      Left (n, TrailingInLoop n)

                    Just (At loc (Condition3 cond)) ->
                      Right ([At n (Do (At loc cond) ds')], locals, Right ys)

            _ -> Left (n, UnknownLoopType n)

        Repeat2 i upper ds      -> do
          (ds', mayTrail, _) <- process3 (Context3 upper) locals ds
          case mayTrail of
            Nothing -> Right ([At n (Repeat i ds')], locals, Right ys)
            Just _  -> Left (n, TrailingInRepeat n)

        Switch2 r css defs      -> do
          css' <- flip traverse css $ \(Case2 o upper cs) -> do
                    (cs', mayTrail, _) <- process3 (Context3 upper) locals cs
                    case mayTrail of
                      Nothing -> Right $ Case o cs'
                      Just _  -> Left (n, TrailingInCase upper)

          defs' <- case defs of
                     NoDefault2        -> Right NoDefault
                     Default2 upper ds -> do
                       (ds', mayTrail, _) <- process3 (Context3 upper) locals ds
                       case mayTrail of
                         Nothing -> Right $ Default ds'
                         Just _  -> Left (n, TrailingInDefault upper)

          Right ([At n (Switch r css' defs')], locals, Right ys)

        Env2 r upper ds         -> do
          (ds', mayTrail, _) <- process3 (Context3 upper) locals ds
          case mayTrail of
            Nothing -> Right ([At n (With r ds')], locals, Right ys)
            Just _  -> Left (n, TrailingInWith n)

        Cast2 dt Boolean r -> do
          (b, zs) <- if3 n ctx locals (At n (Const dt Boolean r)) ys
          Right (b, locals, zs)

        Compare2 cmp dt1 dt2 l r -> do
          (b, zs) <- if3 n ctx locals (At n (Compare cmp dt1 dt2 l r)) ys
          Right (b, locals, zs)

        Not2 x ->
          case x of
            Cast dt Boolean r -> do
              (b, zs) <- if3 n ctx locals (At n (Not (Const dt Boolean r))) ys
              Right (b, locals, zs)

            RCompare cmp dt1 dt2 l r -> do
              (b, zs) <- if3 n ctx locals (At n (Not (Compare cmp dt1 dt2 l r))) ys
              Right (b, locals, zs)

            _ -> Left (n, MalformedConditional n)

        _ -> Left (n, UnhandledStatement n)

    [] -> Right ([], locals, Right [])



conditionHas3 :: Instance -> ShortByteString -> Condition -> Bool
conditionHas3 inst name = go
  where
    go cond =
      case cond of
        Const _ _ v              -> rvalueHas3 inst name v
        Compare _ _ _ l (At _ r) -> rvalueHas3 inst name l || rvalueHas3 inst name r
        Every cs                 -> any (\(At _ v) -> go v) cs
        Any cs                   -> any (\(At _ v) -> go v) cs
        Not c                    -> go c

rvalueHas3 :: Instance -> ShortByteString -> RValue -> Bool
rvalueHas3 inst name = go
  where
    go x =
      case x of
        Value dyn ->
          case dyn of
            Varying (Var (Static inst') name') -> inst == inst' && name == name'
            _                                  -> False

        RCompare _ _ _ l (At _ r) -> go l || go r
        Cast _ _ v                -> go v
        Negate _ v                -> go v
        Complement _ v            -> go v
        Binary _ _ _ l (At _ r)   -> go l || go r
        RCall _ vs                -> any (\(At _ v) -> go v) vs



data For3 = NoFor3
          | For3 !Assignment [At Expr]

for3 :: Instance -> ShortByteString -> [At Expr] -> For3
for3 inst name = go
  where
    go xs =
      case xs of
        a : bs@(_:_) ->
          case go bs of
            NoFor3    -> NoFor3
            For3 v cs -> For3 v (a:cs)

        At _ a : [] ->
          case a of
            Assign v@(Assignment _ _ (At _ (Var (Static inst') name')) r)
              | inst == inst', name == name', rvalueHas3 inst name r ->
                  For3 v []

            _ -> NoFor3

        [] -> NoFor3



condition3
  :: Word32 -> Word32 -> [At Stage2]
  -> Either (Word32, Stage3Error) (At Condition, [At Stage2])
condition3 pos loc xs =
  case xs of
    At n a : ys ->
      case a of
        Cast2 dt Boolean r ->
          Right (At n (Const dt dt r), ys)

        Compare2 cmp dt1 dt2 l r ->
          Right (At n (Compare cmp dt1 dt2 l r), ys)

        Not2 x ->
          case x of
            Cast dt1 dt2 r ->
              Right (At n (Not (Const dt1 dt2 r)), ys)

            RCompare cmp dt1 dt2 l r ->
              Right (At n (Not (Compare cmp dt1 dt2 l r)), ys)

            _ -> Left (pos, MalformedConditional loc)

        _ -> Left (pos, MalformedConditional loc)

    _ -> Left (pos, ExpectedConditional loc)



if3
  :: Word32 -> Context3 -> Locals -> At Condition -> [At Stage2]
  -> Either (Word32, Stage3Error) ([At Expr], Either (At Trailing3) [At Stage2])
if3 pos ctx locals (At loc cond) xs =
  case xs of
    At n a : ys ->
      case a of
        Break2 Jf  -> do
          (as, mayTrail, _) <- process3 ctx locals ys
          case mayTrail of
            Nothing -> Right ([At loc (If cond as (Else [At n Break]))], Right [])
            Just _  -> Left (pos, TrailingInIf n)

        Jump2 Jf j | j == ctx.upper -> do
          let (as, r) = frontcheck3 j ys
          case r of
            None3 -> do
              (as', mayTrail, _) <- process3 ctx locals as
              case mayTrail of
                Nothing -> Right ([At loc (If cond as' NoElse)], Right [])
                Just _  -> Left (pos, TrailingInIf n)

            Jump3 k | k == ctx.upper -> do
              (as', mayTrail, _) <- process3 ctx locals as
              case mayTrail of
                Nothing -> Right ([At loc (If cond as' (Else []))], Right [])
                Just _  -> Left (pos, TrailingInIf n)

            _ -> Left (pos, MalformedConditional n)

        Jump2 jmp j ->
          case jmp of
            J -> Left (pos, MalformedConditional n)
            _ -> do
              (as, r, bs) <- frontlimit3 pos j ys
              case r of
                None3 -> do
                  (as', mayTrail, _) <- process3 (Context3 j) locals as
                  case mayTrail of
                    Nothing -> Right ([At loc (If cond as' NoElse)], Right bs)
                    Just _  -> Left (pos, TrailingInIf n)

                Jump3 k -> do
                  (as', mayTrail, _) <- process3 (Context3 (j - 4)) locals as
                  case mayTrail of
                    Just _  -> Left (pos, TrailingInIf n)
                    Nothing ->
                      if k == ctx.upper
                        then do
                          (bs', mayTrail', _) <- process3 (Context3 k) locals bs
                          case mayTrail' of
                            Nothing -> Right ([At loc (If cond as' (Else bs'))], Right [])
                            Just _  -> Left (pos, TrailingInElse k)


                        else do
                          (bs1, cs) <- frontlimit pos k bs
                          (bs2, mayTrail', _) <- process3 (Context3 k) locals bs1
                          case mayTrail' of
                            Nothing -> Right ([At loc (If cond as' (Else bs2))], Right cs)
                            Just _  -> Left (pos, TrailingInElse k)

                One3 ->
                  case jmp of
                    Jf -> Left (pos, WrongConditionalJumpType loc)
                    Jt -> do
                      (v, vs) <- condition3 pos loc as
                      cs <- boolean3 pos True [] v j vs
                      if3 pos ctx locals (At loc (Any (At loc cond : cs))) bs

                Zero3 ->
                  case jmp of
                    Jt -> Left (pos, WrongConditionalJumpType loc)
                    Jf -> do
                      (v, vs) <- condition3 pos loc as
                      cs <- boolean3 pos False [] v j vs
                      if3 pos ctx locals (At loc (Every (At loc cond : cs))) bs

                OneNot3 ->
                  case jmp of
                    Jf -> Left (pos, WrongConditionalJumpType loc)
                    Jt -> do
                      (v, vs) <- condition3 pos loc as
                      cs <- boolean3 pos True [] v j vs
                      if3 pos ctx locals (At loc (Not (Every (At loc cond : cs)))) bs

                ZeroNot3 ->
                  case jmp of
                    Jt -> Left (pos, WrongConditionalJumpType loc)
                    Jf -> do
                      (v, vs) <- condition3 pos loc as
                      cs <- boolean3 pos False [] v j vs
                      if3 pos ctx locals (At loc (Not (Any (At loc cond : cs)))) bs

        _ -> Left (pos, MalformedConditional n)

    _ -> Right ([], Left (At loc (Condition3 cond)))


boolean3
  :: Word32 -> Bool -> [At Condition] -> At Condition -> Word32 -> [At Stage2]
  -> Either (Word32, Stage3Error) [At Condition]
boolean3 pos t cond (At loc r) j xs =
  case xs of
    []          -> Right (cond <> [At loc r])
    At n a : ys ->
      case a of
        Jump2 jmp k ->
          if k == j
            then case t of
                   True  | Jt <- jmp -> do
                     (v, vs) <- condition3 pos loc ys
                     boolean3 pos t (cond <> [At loc r]) v j vs

                   False | Jf <- jmp -> do
                     (v, vs) <- condition3 pos loc ys
                     boolean3 pos t (cond <> [At loc r]) v j vs

                   _   -> Left (pos, WrongConditionalJumpType n)

            else
              case jmp of
                J -> Left (pos, MalformedConditional n)
                _ -> do
                  (as, l, bs) <- frontlimit3 pos k ys
                  case l of
                    One3 ->
                      case jmp of
                        Jf -> Left (pos, WrongConditionalJumpType loc)
                        Jt -> do
                          (v, vs) <- condition3 pos loc as
                          cs <- boolean3 pos True [] v k vs
                          boolean3 pos t cond (At loc (Any (At loc r : cs))) j bs

                    Zero3 ->
                      case jmp of
                        Jt -> Left (pos, WrongConditionalJumpType loc)
                        Jf -> do
                          (v, vs) <- condition3 pos loc as
                          cs <- boolean3 pos False [] v k vs
                          boolean3 pos t cond (At loc (Every (At loc r : cs))) j bs

                    OneNot3 ->
                      case jmp of
                        Jf -> Left (pos, WrongConditionalJumpType loc)
                        Jt -> do
                          (v, vs) <- condition3 pos loc as
                          cs <- boolean3 pos True [] v k vs
                          boolean3 pos t cond (At loc (Not (Every (At loc r : cs)))) j bs

                    ZeroNot3 ->
                      case jmp of
                        Jt -> Left (pos, WrongConditionalJumpType loc)
                        Jf -> do
                          (v, vs) <- condition3 pos loc as
                          cs <- boolean3 pos False [] v k vs
                          boolean3 pos t cond (At loc (Not (Any (At loc r : cs)))) j bs

                    _ -> Left (pos, MalformedConditional n)

        _ -> Left (pos, MalformedConditional n)



frontlimit :: Word32 -> Word32 -> [At a] -> Either (Word32, Stage3Error) ([At a], [At a])
frontlimit pos j = go
  where
    go as =
      case as of
        At n a : bs ->
          case compare n j of
            LT -> do (cs, ds) <- go bs
                     Right (At n a : cs, ds)

            EQ -> Right ([], as)

            GT -> Left (pos, FrontjumpMissInbound n)

        [] -> Left (pos, FrontjumpOutOfBounds)




data Limit3 = None3
            | Jump3 !Word32
            | One3
            | Zero3
            | OneNot3
            | ZeroNot3
              deriving Show

frontlimit3
  :: Word32 -> Word32 -> [At Stage2]
  -> Either (Word32, Stage3Error) ([At Stage2], Limit3, [At Stage2])
frontlimit3 pos j = go
  where
    go as =
      case as of
        At n a : bs ->
          case compare n (j - 4) of
            LT -> do (cs, r, ds) <- go bs
                     Right (At n a : cs, r, ds)

            EQ ->
              case a of
                AllowNot2 -> Right ([], OneNot3 , bs)
                DenyNot2  -> Right ([], ZeroNot3, bs)
                Allow2    -> Right ([], One3    , bs)
                Deny2     -> Right ([], Zero3   , bs)
                Jump2 J w -> Right ([], Jump3 w , bs)
                _         ->
                  case bs of
                    At m _          : _
                      | m == j -> Right ([At n a], None3, bs)

                    _ -> Left (pos, FrontjumpMissInbound n)

            GT ->
              if n == j
                then Right ([], None3, as)
                else Left (pos, FrontjumpMissInbound n)

        [] -> Left (pos, FrontjumpOutOfBounds)


frontcheck3 :: Word32 -> [At Stage2] -> ([At Stage2], Limit3)
frontcheck3 j = go0
  where
    go0 as =
      case as of
        a : bs -> go1 a bs
        []     -> ([], None3)

    go1 (At n a) as =
      case as of
        b : cs ->
          let (ds, r) = go1 b cs
          in (At n a : ds, r)

        []     ->
          case a of
            Jump2 J w | n == j - 4 -> ([], Jump3 w)
            _         -> ([At n a], None3)



data Source =
       Source
         { name        :: !ShortByteString
         , arguments   :: !(SmallArray ShortByteString)
         , expressions :: [At Expr]
         }
       deriving Show

decompile :: Strg -> Objt -> Assembly -> Either Error Source
decompile strgs objts asm =
  case stage1 asm.instructions of
    Identity s1 ->
      case stage2 asm.offset asm.size strgs objts s1 of
        Left (pos, e) -> Left $ Error asm.name pos (Stage2Error e)
        Right s2      ->
          case stage3 asm.size asm.arguments s2 of
            Left (pos, e) -> Left $ Error asm.name pos (Stage3Error e)
            Right s3      ->
              Right $! Source asm.name asm.arguments s3
