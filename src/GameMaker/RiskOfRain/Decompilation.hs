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
           , DataKinds
           , DuplicateRecordFields
           , FlexibleInstances
           , MultiParamTypeClasses
           , NoFieldSelectors
           , OverloadedRecordDot
           , OverloadedStrings
           , TemplateHaskell
           , TypeOperators
           , TypeFamilies
           , UndecidableInstances #-}

module GameMaker.RiskOfRain.Decompilation
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
  , decompile
  ) where

import           GameMaker.RiskOfRain.Disassembly hiding (Instruction (..), Variable (..))
import qualified GameMaker.RiskOfRain.Disassembly as Asm
import           GameMaker.RiskOfRain.Unpacking

import           Data.ByteString.Short (ShortByteString)
import           Data.Int
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Optics.TH



-- | Array dimensions. GameMaker only supports 1D and 2D arrays.
data Dimensions = OneDim (At RValue)
                | TwoDim (At RValue) (At RValue)
                  deriving Show

-- | Constant values pushed onto stack.
data Constant = CDouble Double
              | CInt32 Int32
              | CInt16 Int16
              | CString ShortByteString
                deriving Show

-- | Reference to any particular 'Instance', either at compilation time or at runtime.
data Reference = Static Instance
               | Dynamic (At RValue)
                 deriving Show

data Variable = Var Reference ShortByteString
              | VArray Dimensions Reference ShortByteString
                deriving Show

-- | Anything pushable on the stack.
data Value = Constant Constant
           | Varying Variable
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
data RValue = Value      Value
            | RCompare   Asm.Comparison DataType DataType RValue (At RValue)
            | Cast       DataType DataType RValue
            | Negate     DataType RValue
            | Complement DataType RValue
            | Binary     Binary DataType DataType RValue (At RValue)
            | RCall      ShortByteString [At RValue]
              deriving Show



data Stage1 = Normal1 [At Asm.Instruction]

              -- | Holds the offset of the start of the first condition,
              --   all comparisons after the first one,
              --   the offset of the end of the switch, and the body
            | Switch1 Word32 [At Asm.Instruction] Word32 [At Stage1]

               -- | Degenerate default-only switch
            | Default1 Word32 Word32 [At Stage1]
              deriving Show

-- | Traverses the instruction list front to back and cuts out the case statements.
stage1 :: [At Asm.Instruction] -> Result [At Stage1]
stage1 xs =
  case xs of
    At n _ : _ -> inner n id xs
    []         -> Success []
  where
    inner loc acc as =
      case as of
        At n a : bs ->
          case a of
            Asm.Bt (Int23 j) | j > 0 ->
              let to = fromIntegral $ fromIntegral n + (j - 1) * 4
                  ~(css, mv, middle) = limit to bs

              in case mv of
                   Just (At m (Asm.B (Int23 k))) ->
                     let to' = fromIntegral $ fromIntegral m + k * 4
                         ~(raw, mx, cs) = limit to' middle

                     in case mx of
                          Just (At o (Asm.Popz _)) -> do
                            defs <- stage1 raw
                            rest <- stage1 cs
                            let new = At (n + 4) (Switch1 (m + 4) css o defs)
                            Success $ case acc [] of
                                        [] -> new:rest
                                        ls -> At loc (Normal1 ls) : new : rest

                          _ -> inner loc (acc . (:) (At n a)) bs

                   _               ->
                     inner loc (acc . (:) (At n a)) bs

            Asm.B (Int23 2) ->
              case bs of
                b@(At m (Asm.B (Int23 j))) : cs ->
                  let to = fromIntegral $ fromIntegral m + j * 4
                      ~(raw, mv, ds) = limit to cs

                  in case mv of
                       Just (At o (Asm.Popz Variable)) -> do
                         defs <- stage1 raw
                         rest <- stage1 ds
                         let new = At (n + 4) (Default1 (m + 4) o defs)
                         Success $ case acc [] of
                                     [] -> new:rest
                                     ls -> At loc (Normal1 ls) : new : rest

                       _                      ->
                         inner loc (acc . (:) (At n a) . (:) b) cs

                _ -> inner loc (acc . (:) (At n a)) bs

            _ -> inner loc (acc . (:) (At n a)) bs

        [] -> Success $ case acc [] of
                          [] -> []
                          ls -> [At loc (Normal1 ls)]

    limit
      :: Word32 -> [At Asm.Instruction]
      -> ([At Asm.Instruction], Maybe (At Asm.Instruction), [At Asm.Instruction])
    limit j as =
      case as of
        At n a : bs ->
          case compare n j of
            LT -> let ~(cs, mv, ds) = limit j bs
                  in (At n a : cs, mv, ds)

            EQ -> ([], Just (At n a), bs)

            GT -> ([], Nothing, At n a : bs)

        [] -> ([], Nothing, [])



backlimit :: Word32 -> [At a] -> Result ([At a], [At a])
backlimit j = go
  where
    go as =
      case as of
        At n a : bs ->
          case compare n j of
            GT -> do ~(cs, ds) <- go bs
                     Success (At n a : cs, ds)

            EQ -> Success ([At n a], bs)

            LT -> Failure (fromIntegral n) "Overshot backlimit"

        [] -> Failure (-1) "Overshot backlimit"



-- | Writing a variable to memory.
data Assignment = Assignment DataType DataType (At Variable) RValue
                  deriving Show

data Jump = J | Jt | Jf
            deriving Show

data Case2 = Case2 (At RValue) Word32 [At Stage2]
             deriving Show

data Default2 = NoDefault2
              | Default2 Word32 [At Stage2]
                deriving Show

data EnvOut2 = NoEnvOut2
             | EnvOut2
               deriving Show

data Stage2 = Assign2   Assignment
            | Reassign2 Binary DataType DataType Assignment
            | Call2     ShortByteString [At RValue]
            | Return2   RValue
            | Exit2
            | Continue2
            | Break2    Jump

            | Loop2     Jump Word32 [At Stage2]
            | Repeat2   RValue Word32 [At Stage2]
            | Switch2   RValue [Case2] (Default2)
            | Env2      Reference Word32 [At Stage2]

            | Cast2     DataType DataType RValue
            | Compare2  Asm.Comparison DataType DataType RValue (At RValue)
            | Jump2     Jump Word32
            | Not2      RValue

              -- These form &&, || and ! statements with 2+ elements
            | Allow2    -- ^ B 2; Push 1
            | Deny2     -- ^ B 2; Push 0
            | AllowNot2 -- ^ B 2; Push 1; Not
            | DenyNot2  -- ^ B 2; Push 0; Not
              deriving Show

data Context2 =
       Context2
         { places :: [Inside2]
         , lower  :: Word32    -- ^ Lower area boundary
         , upper  :: Word32    -- ^ Upper area boundary
         , break  :: Word32    -- ^ Break point, if applicable
         }
       deriving Show

data Inside2 = Back2    -- ^ In a loop
             | Def2     -- ^ In a case definition
             | With2    -- ^ In a with() statement
             | Without2 -- ^ In a with() statement with a 'PopEnvAny' escape
               deriving Show

-- | Traverses the instruction list back to front, converting simple operations
--   into 'RValue's, cutting out backwards jumps and categorizing forwards ones.
stage2 :: Word32 -> Objects -> [At Stage1] -> Result [At Stage2]
stage2 size objs xs = do
  At _ r <- process2 objs (Context2 [] 0 size 0xFFFFFFFF) $ invert2 xs
  Success r

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



backlimit2 :: Word32 -> [At Stage1] -> Result ([At Stage1], [At Stage1])
backlimit2 j = go
  where
    go as =
      case as of
        At n a : bs ->
          case compare n j of
            GT -> do
              ~(cs, ds) <- go bs
              Success (At n a : cs, ds)

            EQ ->
              case a of
                Normal1 _ -> Success ([At n a], bs)
                _ -> Failure (fromIntegral n) "Backjump points into a case"

            LT ->
              case a of
                Normal1 os -> do
                  ~(ls', rs') <- backlimit j os

                  let ls = case ls' of
                             _:_ -> At j (Normal1 ls') : []
                             []  -> []

                      rs = case rs' of
                             _:_ -> At n (Normal1 rs') : bs
                             []  -> bs

                  Success (ls, rs)

                _ -> Failure (fromIntegral n) "Backjump points into a case"

        [] -> Failure (-1) "Overshot backlimit2"



process2 :: Objects -> Context2 -> [At Stage1] -> Result (At [At Stage2])
process2 objs ctx = go (ctx.upper)
  where
    go loc0 xs =
      case xs of
        At loc (Normal1 as) : ys          ->
          case as of
            [] -> go loc0 ys
            _  -> do
              ~(At loc2 bs, more) <- normal2 objs ctx ys loc loc0 as
              At loc3 cs <- go loc2 more
              Success (At loc3 (bs <> cs))

        At loc (Switch1 lower css upper dfs) : ys -> do
          case ys of
            At loc1 (Normal1 as) : zs -> do
              case as of
                At _ (Asm.Cmp Eq _ _) : bs -> do
                  ~(r0@(At loc2 _), cs) <- rvalue2 objs loc1 bs

                  case cs of
                    At _ (Asm.Dup _ _) : ds -> do
                      ~(At loc3 ra, es) <- rvalue2 objs loc2 ds

                      let cases is =
                            case is of
                              _:_ -> do
                                ~(n, r, js) <- case2 objs loc1 is
                                ls <- cases js
                                Success ((r, n) : ls)

                              []  -> Success [(r0, lower)]

                          slicer edge ns is =
                            case ns of
                              (r, n) : os@(_:_) ->
                                if n == edge
                                  then do
                                    rest <- slicer edge os is
                                    Success $ Case2 r edge [] : rest

                                  else do
                                    ~(js', ks) <- backlimit2 n is

                                    let ctx' = Context2 (Def2 : ctx.places) n edge upper
                                    At _ js <- process2 objs ctx' js'

                                    rest <- slicer n os ks
                                    Success $ Case2 r edge js : rest

                              [(r, _)]  -> do
                                let ctx' = Context2 (Def2 : ctx.places) loc edge upper
                                At _ defs <- process2 objs ctx' is

                                Success [Case2 r edge defs]

                              [] -> Failure (fromIntegral edge) "Empty slice"

                      ~(upper', css', dfs', defz) <-
                        case css of
                          At n (Asm.B (Int23 j)) : css' -> do
                            let to = fromIntegral $ fromIntegral n + j * 4

                            ~(dfz, dfp) <- backlimit2 to dfs

                            let ctx' = Context2 (Def2 : ctx.places) to upper upper
                            At _ defz <- process2 objs ctx' dfz

                            Success (to, css', dfp, Default2 upper defz)

                          _ -> Success (upper, css, dfs, NoDefault2)

                      ns <- cases css'
                      defs <- slicer upper' ns dfs'

                      At loc4 b <- go loc3 (At loc1 (Normal1 es) : zs)

                      let this = At loc3 (Switch2 ra defs defz)
                      Success $ At loc4 (this : b)

                    _ -> Failure (fromIntegral loc) "Malformed switch"

                _ -> Failure (fromIntegral loc) "Malformed switch"

            _ -> Failure (fromIntegral loc) "No r-value before case"

        At loc (Default1 lower upper ds) : ys -> do
          case ys of
            At origin (Normal1 as) : zs -> do
              ~(At loc2 r, bs) <- rvalue2 objs loc as

              let ctx' = Context2 (Def2 : ctx.places) lower upper upper
              At _    defs <- process2 objs ctx' ds

              At loc4 rest <- process2 objs ctx $ At origin (Normal1 bs) : zs

              Success . At loc4 $ At loc2 (Switch2 r [] (Default2 loc4 defs)) : rest

            _ -> Failure (fromIntegral loc) "No r-value before case"

        [] -> pure (At 0 [])



case2 :: Objects -> Word32 -> [At Asm.Instruction] -> Result (Word32, At RValue, [At Asm.Instruction])
case2 objs loc xs =
  case xs of
    At n (Asm.Bt (Int23 j)) : At _ (Asm.Cmp Eq _ _)
                            : bs -> do
      ~(At loc1 r, cs) <- rvalue2 objs loc bs
      case cs of
        At _ (Asm.Dup 0 _) : ds ->
          let to = fromIntegral $ fromIntegral n + j * 4
          in Success (to, At loc1 r, ds)

        _ -> Failure (fromIntegral loc) "Incorrect switch case statement continuation"

    _ -> Failure (fromIntegral loc) "Not a switch case statement"



normal2
  :: Objects -> Context2 -> [At Stage1] -> Word32 -> Word32 -> [At Asm.Instruction]
  -> Result (At [At Stage2], [At Stage1])
normal2 objs ctx rs origin = go
  where
    go loc xs =
      case xs of
        At loc1 i : ys ->
          case i of
            Asm.Cmp cmp dt1 dt2 -> do
              ~(r@(At loc2 _), as) <- rvalue2 objs loc1 ys
              ~(   At loc3 l , bs) <- rvalue2 objs loc2 as
              let this = At loc3 (Compare2 cmp dt1 dt2 l r)
              Success (At loc3 [this], At origin (Normal1 bs) : rs)

            Asm.Pop to from inst name Asm.Normal -> do
              ~(At loc2 r, as) <- rvalue2 objs loc1 ys
              let this = At loc2 (Assign2 (Assignment to from (At loc1 (Var (Static inst) name)) r))
              Success (At loc2 [this], At origin (Normal1 as) : rs)

            Asm.Pop to from _inst name Asm.StackTop ->
              case ys of
                At loc2 j : zs@(At _ nonBreak : _) | Just (bin, dt1, dt2) <- binary2 j
                                                   , not $ break2 nonBreak -> do
                  ~(At loc3 r  , as) <- rvalue2 objs loc2 zs
                  case as of
                    At _ (Asm.PushCst (PushVari _ _ Asm.StackTop)) : At _ (Asm.Dup 0 _) : as' -> do
                      ~(At loc4 inst, bs) <- instance2 objs loc3 as' name
                      let this = At loc4 (Reassign2 bin dt1 dt2 (Assignment to from (At loc2 inst) r))
                      Success (At loc4 [this], At origin (Normal1 bs) : rs)

                    _ -> Failure (fromIntegral loc3) "No Dup on instance reassignment"

                _ -> do
                  ~(At loc2 j, as) <- instance2 objs loc1 ys name
                  ~(At loc3 r, bs) <- rvalue2 objs loc2 as
                  let this = At loc3 (Assign2 (Assignment to from (At loc2 j) r))
                  Success (At loc3 [this], At origin (Normal1 bs) : rs)

            Asm.Pop to from _inst name Asm.Array ->
              case ys of
                At loc2 j : zs@(At _ nonBreak : _) | Just (bin, dt1, dt2) <- binary2 j
                                                   , not $ break2 nonBreak -> do
                  ~(At loc3 r  , as) <- rvalue2 objs loc2 zs
                  case as of
                    At _ (Asm.PushCst (PushVari _ _ Asm.Array)) : At _ (Asm.Dup 1 _) : as' -> do
                      ~(At loc4 arr, bs) <- array2 objs loc3 as' name
                      let this = At loc4 (Reassign2 bin dt1 dt2 (Assignment to from (At loc2 arr) r))
                      Success (At loc4 [this], At origin (Normal1 bs) : rs)

                    _ -> Failure (fromIntegral loc3) "No Dup on array reassignment"

                _ -> do
                  ~(At loc2 j, as) <- array2 objs loc1 ys name
                  ~(At loc3 r, bs) <- rvalue2 objs loc2 as
                  let this = At loc3 (Assign2 (Assignment to from (At loc2 j) r))
                  Success (At loc3 [this], At origin (Normal1 bs) : rs)

            Asm.Exit Int32 -> do
              At loc2 zs <- cull2 loc1 ctx.places ys
              Success (At loc2 [At loc1 Exit2], At origin (Normal1 zs) : rs)

            Asm.PopEnvAny ->
              case ys of
                At _ (Asm.B (Int23 2)) : At loc2 (Asm.PopEnv (Int23 j)) : zs ->
                  env2 loc2 EnvOut2 zs j

                _ -> Failure (fromIntegral loc1) "Not a PopEnvAny env escape"

            Asm.Popz Int32 ->
              case ys of
                At _ (Asm.Bt (Int23 j)) : At _    (Asm.Conv Int32 Boolean)
                                        : At _    (Asm.Dup _ Int32)
                                        : At _    (Asm.Sub Int32 Int32)
                                        : At locr (Asm.PushCst (PushInt32 1))
                                        : zs -> do

                  let to = fromIntegral $ fromIntegral loc1 + 4 * (j - 1)
                  ~(as, bs) <- backlimit2 to (At origin (Normal1 zs) : rs)

                  case bs of
                    At origin2 (Normal1 (At _ (Asm.Bt _) : At _ (Asm.Cmp Le Int32 Int32)
                                                         : At _ (Asm.PushCst (PushInt32 0))
                                                         : At m (Asm.Dup _ Int32)
                                                         : cs)) : ns -> do

                      ~(At loc2 r, ds) <- rvalue2 objs m cs

                      let ctx' = Context2 (Back2 : ctx.places) to locr loc1
                      At _    defs <- process2 objs ctx' as
                      At loc4 rest <- process2 objs ctx (At origin2 (Normal1 ds) : ns)
                      Success (At loc4 (At loc2 (Repeat2 r locr defs) : rest), [])

                    _   -> Failure (fromIntegral loc) $ "Not a repeat pop continuation"

                _   -> Failure (fromIntegral loc) $ "Not a repeat pop"

            Asm.Popz Variable -> do
              ~(At loc2 r, as) <- rvalue2 objs loc1 ys
              case r of
                RCall name args -> do
                  let this = At loc2 (Call2 name args)
                  Success (At loc2 [this], At origin (Normal1 as) : rs)

                _   -> Failure (fromIntegral loc) $ "Inconvertible " <> showsPrec 11 r []

            Asm.Ret Variable -> do
              ~(At loc2 r, as) <- rvalue2 objs loc1 ys

              At loc3 bs <- cull2 loc2 ctx.places as
              Success (At loc3 [At loc2 (Return2 r)], At origin (Normal1 bs) : rs)

            Asm.Conv from to -> do
              ~(At loc2 r, as) <- rvalue2 objs loc1 ys
              Success (At loc2 [At loc2 (Cast2 from to r)], At origin (Normal1 as) : rs)

            Asm.Not Boolean ->
              case ys of
                At _ (Asm.PushCst (PushInt16 0)) : At loc2 (Asm.B (Int23 2)) : zs ->
                  Success (At loc2 [At loc2 DenyNot2], At origin (Normal1 zs) : rs)

                At _ (Asm.PushCst (PushInt16 1)) : At loc2 (Asm.B (Int23 2)) : zs ->
                  Success (At loc2 [At loc2 AllowNot2], At origin (Normal1 zs) : rs)

                _   -> do
                  ~(At loc2 r, as) <- rvalue2 objs loc1 ys
                  Success (At loc2 [At loc2 (Not2 r)], At origin (Normal1 as) : rs)

            Asm.B  (Int23 j) -> jump2 loc1 ys J  j
            Asm.Bt (Int23 j) -> jump2 loc1 ys Jt j
            Asm.Bf (Int23 j) -> jump2 loc1 ys Jf j

            Asm.PopEnv (Int23 j) -> env2 loc1 NoEnvOut2 ys j

            Asm.PushCst (PushInt16 0) ->
              case ys of
                At loc2 (Asm.B (Int23 2)) : zs ->
                  Success (At loc2 [At loc2 Deny2], At origin (Normal1 zs) : rs)

                _   -> Failure (fromIntegral loc) "Not a Deny"

            Asm.PushCst (PushInt16 1) ->
              case ys of
                At loc2 (Asm.B (Int23 2)) : zs ->
                  Success (At loc2 [At loc2 Allow2], At origin (Normal1 zs) : rs)

                _   -> Failure (fromIntegral loc) "Not an Allow"

            _   -> Failure (fromIntegral loc) $ "Unknown normal " <> showsPrec 11 i []

        [] -> Success (At loc [], rs)


    jump2 :: Word32 -> [At Asm.Instruction] -> Jump -> Int32 -> Result (At [At Stage2], [At Stage1])
    jump2 loc xs jmp j =
      if j < 0
        then backjump
        else case ctx.places of
               Without2 : _
                 | J <- jmp, to == ctx.upper + 8 ->
                     Success (At loc [At loc (Break2 J)], At origin (Normal1 xs) : rs)

                 | otherwise -> do
                     new <- frontjump
                     Success (new, At origin (Normal1 xs) : rs)

               _            -> do
                 new <- frontjump
                 Success (new, At origin (Normal1 xs) : rs)
      where
        to :: Word32
        to = fromIntegral $ fromIntegral loc + 4 * j

        backjump :: Result (At [At Stage2], [At Stage1])
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
                       _  -> Success (At loc [At loc Continue2], At origin (Normal1 xs) : rs)

            LT -> Failure (fromIntegral loc) "Backjump under loop limits"

        backslice :: Result (At [At Stage2], [At Stage1])
        backslice = do
          ~(as, bs) <- backlimit2 to (At origin (Normal1 xs) : rs)

          let ctx' = Context2 (Back2 : ctx.places) to loc (loc + 4)
          At _    cs   <- process2 objs ctx' as
          At loc2 rest <- process2 objs ctx bs
          Success (At loc2 (At to (Loop2 jmp loc cs) : rest), [])

        frontjump :: Result (At [At Stage2])
        frontjump =
          if to == ctx.break
            then Success (At loc [At loc (Break2 jmp)])
            else if to > ctx.upper
                   then Failure (fromIntegral loc) "Jump beyond upper boundary"
                   else Success (At loc [At loc (Jump2 jmp to)])


    env2 :: Word32 -> EnvOut2 -> [At Asm.Instruction] -> Int32 -> Result (At [At Stage2], [At Stage1])
    env2 loc out xs j = do
      let to = fromIntegral $ fromIntegral loc + 4 * j
      ~(as, bs) <- backlimit2 to (At origin (Normal1 xs) : rs)

      case bs of
        At origin2 (Normal1 (At _ (Asm.PushEnv _) : cs)) : ns -> do
          ~(r@(At loc1 _), ds) <- rvalue2 objs loc cs

          let place = case out of
                        NoEnvOut2 -> With2
                        EnvOut2   -> Without2

              ctx' = Context2 (place : ctx.places) to loc (loc + 4)

          At _    defs <- process2 objs ctx' as
          At loc4 rest <- process2 objs ctx  (At origin2 (Normal1 ds) : ns)
          Success (At loc4 (At loc1 (Env2 (reference2 objs r) loc defs) : rest), [])

        _   -> Failure (fromIntegral loc) $ "Not a repeat pop env"



-- | Removes context Pops on Ret and Exit. They're processed in arbitrary order because
--   the bytecode doesn't seem to have any consistency.
cull2 :: Word32 -> [Inside2] -> [At Asm.Instruction] -> Result (At [At Asm.Instruction])
cull2 loc is0 =
  let ~(n, m) = mush 0 0 is0
  in go n m loc
  where
    mush :: Int -> Int -> [Inside2] -> (Int, Int)
    mush !n !m is =
      case is of
        i : js ->
          case i of
            Back2    -> mush n m js
            Def2     -> let ~(n', m') = mush n m js
                        in (n' + 1, m')

            With2    -> let ~(n', m') = mush n m js
                        in (n', m' + 1)

            Without2 -> let ~(n', m') = mush n m js
                        in (n', m' + 1)

        []     -> (0, 0)

    go :: Int -> Int -> Word32 -> [At Asm.Instruction] -> Result (At [At Asm.Instruction])
    go n m loc0 xs
      | n <= 0, m <= 0 = Success (At loc0 xs)
      | otherwise      =
          case xs of
            At loc1 (Asm.Popz Variable) : ys ->
              if n > 0
                then go (n - 1) m loc1 ys
                else Failure (fromIntegral loc) "Unwarranted Popz on cull"

            At loc1 Asm.PopEnvAny : ys ->
              if m > 0
                then go n (m - 1) loc1 ys
                else Failure (fromIntegral loc) "Unwarranted Popz on cull"

            _ -> Failure (fromIntegral loc) $
                   showString "Cannot cull (" . shows n . showString " Popz and "
                                              $ shows m " PopEnvAny left"



rvalue2 :: Objects -> Word32 -> [At Asm.Instruction] -> Result (At RValue, [At Asm.Instruction])
rvalue2 objs loc xs =
  case xs of
    At loc1 i : ys ->
      let double as f = do
            ~(r@(At loc2 _), bs) <- rvalue2 objs loc1 as
            ~(   At loc3 l , cs) <- rvalue2 objs loc2 bs
            Success (At loc3 (f l r), cs)

      in case i of
           Asm.Conv from to -> do
             ~(At loc2 r, zs) <- rvalue2 objs loc ys
             Success (At loc2 (Cast from to r), zs)

           Asm.Mul dt1 dt2 -> double ys $ Binary Mul dt1 dt2
           Asm.Div dt1 dt2 -> double ys $ Binary Div dt1 dt2
           Asm.Rem dt1 dt2 -> double ys $ Binary Rem dt1 dt2
           Asm.Mod dt1 dt2 -> double ys $ Binary Mod dt1 dt2
           Asm.Add dt1 dt2 -> double ys $ Binary Add dt1 dt2
           Asm.Sub dt1 dt2 -> double ys $ Binary Sub dt1 dt2
           Asm.And dt1 dt2 -> double ys $ Binary And dt1 dt2
           Asm.Or  dt1 dt2 -> double ys $ Binary Or  dt1 dt2
           Asm.Xor dt1 dt2 -> double ys $ Binary Xor dt1 dt2

           Asm.Neg dt      -> do ~(At loc2 v, zs) <- rvalue2 objs loc1 ys
                                 Success (At loc2 (Negate dt v), zs)

           Asm.Not dt      -> do ~(At loc2 v, zs) <- rvalue2 objs loc1 ys
                                 Success (At loc2 (Complement dt v), zs)

           Asm.Shl dt1 dt2 -> double ys $ Binary Shl dt1 dt2
           Asm.Shr dt1 dt2 -> double ys $ Binary Shr dt1 dt2

           Asm.Cmp cmp dt1 dt2 -> double ys $ RCompare cmp dt1 dt2

           Asm.PushCst (PushStrg n) ->
             Success (At loc1 (Value (Constant (CString n))), ys)

           Asm.PushCst (PushInt16 n) ->
             Success (At loc1 (Value (Constant (CInt16 n))), ys)

           Asm.PushCst (PushInt32 n) ->
             Success (At loc1 (Value (Constant (CInt32 n))), ys)

           Asm.PushCst (PushDouble n) ->
             Success (At loc1 (Value (Constant (CDouble n))), ys)

           Asm.PushCst (PushVari inst name Asm.Normal) -> do
             Success (At loc1 (Value (Varying (Var (Static inst) name))), ys)

           Asm.PushCst (PushVari _ name Asm.StackTop) -> do
             ~(At loc2 arr, zs) <- instance2 objs loc1 ys name
             Success (At loc2 (Value (Varying arr)), zs)

           Asm.PushCst (PushVari _ name Asm.Array) -> do
             ~(At loc2 arr, zs) <- array2 objs loc1 ys name
             Success (At loc2 (Value (Varying arr)), zs)

           Asm.PushLoc (PushVari Local name Asm.Normal) ->
             Success (At loc1 (Value (Varying (Var (Static Local) name))), ys)

           Asm.PushGlb (PushVari Global name Asm.Normal) ->
             Success (At loc1 (Value (Varying (Var (Static Global) name))), ys)

           Asm.PushVar Self Variable name Asm.Normal ->
             Success (At loc1 (Value (Varying (Var (Static Self) name))), ys)

           Asm.PushI16 v Int16 ->
             Success (At loc1 (Value (Constant (CInt16 v))), ys)

           Asm.Call count Int32 name Asm.Array -> do
             let get loc0 as n
                   | n <= 0    = Success (loc0, [], as)
                   | otherwise = do
                       ~(At loc2 r, bs) <- rvalue2 objs loc1 as
                       ~(loc3, rs, cs) <- get loc2 bs (n - 1)
                       Success (loc3, At loc2 r : rs, cs)

             ~(loc', args, zs) <- get loc1 ys count
             Success (At loc' (RCall name args), zs)

           _ -> Failure (fromIntegral loc1) $ "No r-value " <> showsPrec 11 i []

    [] -> Failure (fromIntegral loc) "R-value bottom"



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



reference2 :: Objects -> At RValue -> Reference
reference2 objs (At loc r) =
  case r of
    Value (Constant (CInt16 i)) | Right inst <- convertInstance objs i ->
      Static inst

    _ -> Dynamic (At loc r)

instance2 :: Objects -> Word32 -> [At Asm.Instruction] -> ShortByteString -> Result (At Variable, [At Asm.Instruction])
instance2 objs loc xs name = do
  ~(r@(At loc1 _), ys) <- rvalue2 objs loc xs
  Success (At loc1 (Var (reference2 objs r) name), ys)

array2 :: Objects -> Word32 -> [At Asm.Instruction] -> ShortByteString -> Result (At Variable, [At Asm.Instruction])
array2 objs loc xs name =
  case xs of
    At _ (Asm.Add Int32 Int32) : At _ (Asm.Break (-1) Int16)
                               : as -> do

      ~(y@(At loc1 _), bs) <- rvalue2 objs loc as

      case bs of
        At _ (Asm.Mul Int32 Int32) : At _ (Asm.PushCst (PushInt32 32000))
                                   : At _ (Asm.Break (-1) Int16)
                                   : cs -> do

          ~(x@(At loc2 _), ds) <- rvalue2 objs loc1 cs
          ~(j@(At loc3 _), es) <- rvalue2 objs loc2 ds
          Success (At loc3 (VArray (TwoDim x y) (reference2 objs j) name), es)

        _ -> Failure (fromIntegral loc) "Invalid array second dimension assignment"

    as -> do
      ~(x@(At loc1 _), bs) <- rvalue2 objs loc  as
      ~(j@(At loc2 _), cs) <- rvalue2 objs loc1 bs
      Success (At loc2 (VArray (OneDim x) (reference2 objs j) name), cs)




data Condition = Const   DataType DataType RValue
               | Compare Asm.Comparison DataType DataType RValue (At RValue)
               | Every   [At Condition]
               | Any     [At Condition]
               | Not     Condition
                 deriving Show

data Else = NoElse
          | Else [At Expr]
            deriving Show

data Case = Case (At RValue) [At Expr]
            deriving Show

data Default = NoDefault
             | Default [At Expr]
               deriving Show

data Expr = Declare  Assignment
          | Assign   Assignment
          | Reassign Binary DataType DataType Assignment
          | Call     ShortByteString [At RValue]
          | Return   RValue
          | Exit
          | Continue
          | Break
          | If       Condition [At Expr] Else
          | While    Condition [At Expr]
          | Do       (At Condition) [At Expr]
          | For      Assignment (At Condition) Assignment [At Expr]
          | Repeat   RValue [At Expr]
          | Switch   RValue [Case] Default
          | With     Reference [At Expr]
            deriving Show



newtype Context3 =
          Context3
            { upper :: Word32 -- ^ Upper area boundary
            }
          deriving Show

newtype Locals = Locals (Set ShortByteString)
                 deriving Show

addLocal :: ShortByteString -> Locals -> Locals
addLocal bs (Locals ls) = Locals $ Set.insert bs ls

isDeclared :: ShortByteString -> Locals -> Bool
isDeclared bs (Locals ls) = Set.member bs ls



-- | Traverses the instruction list front to back, cuts out conditionals and
--   figures out while/do/for loops.
stage3 :: Word32 -> [ShortByteString] -> [At Stage2] -> Result [At Expr]
stage3 size args xs = do
  ~(ys, mayTrail, _) <- process3 (Context3 size) (Locals $ Set.fromList args) $ invert3 xs
  case mayTrail of
    Nothing -> Success ys
    Just _  -> Failure 0 "Trailing statement at stage end"

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

process3 :: Context3 -> Locals -> [At Stage2] -> Result ([At Expr], Maybe (At Trailing3), Locals)
process3 ctx locals xs =
  case xs of
    _:_ -> do
      ~(as, locals1, eiYs) <- advance3 ctx locals xs
      case eiYs of
        Right ys -> do
          ~(bs, mayTrail, locals2) <- process3 ctx locals1 ys
          Success (as <> bs, mayTrail, locals2)

        Left trail -> Success (as, Just trail, locals1)

    []  -> Success ([], Nothing, locals)

advance3 :: Context3 -> Locals -> [At Stage2] -> Result ([At Expr], Locals, Either (At Trailing3) [At Stage2])
advance3 ctx locals xs =
  case xs of
    At n a : ys ->
      case a of
        Assign2 x@(Assignment _ _ (At _ v) _) ->
          case v of
            Var (Static Local) name
              | not $ isDeclared name locals -> do
                  let locals' = addLocal name locals

                  ~(as, locals'', zs) <- locals' `seq` advance3 ctx locals' ys
                  Success $
                    case as of
                      [At loc (While cond thn)]
                        | conditionHas3 name cond, For3 inc thn' <- for3 name thn ->
                            let this = For x (At loc cond) inc thn'
                            in ([At n this], locals', zs)

                      _ -> (At n (Declare x) : as, locals'', zs)

            _ -> Success ([At n (Assign x)], locals, Right ys)

        Reassign2 bin to from x -> Success ([At n (Reassign bin to from x)], locals, Right ys)
        Call2     name args     -> Success ([At n (Call name args)], locals, Right ys)
        Return2   r             -> Success ([At n (Return r)], locals, Right ys)
        Exit2                   -> Success ([At n Exit], locals, Right ys)
        Continue2               -> Success ([At n Continue], locals, Right ys)
        Break2 J                -> Success ([At n Break], locals, Right ys)

        Loop2 jmp upper ds      -> do
          (ds', mayTrail, _) <- process3 (Context3 upper) locals ds
          case jmp of
            J  -> case mayTrail of
                    Just _ ->
                      Failure (fromIntegral n) "Trailing statement in Loop J"

                    Nothing ->
                      case ds' of
                        [At loc (If cond thn (Else [At _ Break]))] ->
                          Success ([At loc (While cond thn)], locals, Right ys)

                        _ -> Failure (fromIntegral n) "Unaccounted loop"

            Jf -> case mayTrail of
                    Nothing ->
                      Failure (fromIntegral n) "No trailing conditional in Loop Jf"

                    Just (At loc (Condition3 cond)) ->
                      Success ([At n (Do (At loc cond) ds')], locals, Right ys)

            _  -> Failure (fromIntegral n) "Unaccounted loop"

        Repeat2 i upper ds      -> do
          (ds', mayTrail, _) <- process3 (Context3 upper) locals ds
          case mayTrail of
            Nothing -> Success ([At n (Repeat i ds')], locals, Right ys)
            Just _  -> Failure (fromIntegral n) "Trailing statement in Repeat"

        Switch2 r css defs      -> do
          css' <- flip traverse css $ \(Case2 o upper cs) -> do
                    ~(cs', mayTrail, _) <- process3 (Context3 upper) locals cs
                    case mayTrail of
                      Nothing -> Success $ Case o cs'
                      Just _  -> Failure (fromIntegral upper) "Trailing statement in Case"

          defs' <- case defs of
                     NoDefault2        -> Success NoDefault
                     Default2 upper ds -> do
                       ~(ds', mayTrail, _) <- process3 (Context3 upper) locals ds
                       case mayTrail of
                         Nothing -> Success $ Default ds'
                         Just _  -> Failure (fromIntegral upper) "Trailing statement in Default"

          Success ([At n (Switch r css' defs')], locals, Right ys)

        Env2 r upper ds         -> do
          (ds', mayTrail, _) <- process3 (Context3 upper) locals ds
          case mayTrail of
            Nothing -> Success ([At n (With r ds')], locals, Right ys)
            Just _  -> Failure (fromIntegral n) "Trailing statement in With"

        Cast2 dt Boolean r -> do
          ~(b, zs) <- if3 ctx locals (At n (Const dt Boolean r)) ys
          Success (b, locals, zs)

        Compare2 cmp dt1 dt2 l r -> do
          ~(b, zs) <- if3 ctx locals (At n (Compare cmp dt1 dt2 l r)) ys
          Success (b, locals, zs)

        Not2 x ->
          case x of
            Cast dt Boolean r -> do
              ~(b, zs) <- if3 ctx locals (At n (Not (Const dt Boolean r))) ys
              Success (b, locals, zs)

            RCompare cmp dt1 dt2 l r -> do
              ~(b, zs) <- if3 ctx locals (At n (Not (Compare cmp dt1 dt2 l r))) ys
              Success (b, locals, zs)

            _ -> Failure (fromIntegral n) $ "Unknown condition " <> showsPrec 11 x []

        _ -> Failure (fromIntegral n) "Unaccounted"

    [] -> Success ([], locals, Right [])



conditionHas3 :: ShortByteString -> Condition -> Bool
conditionHas3 name = go
  where
    go cond =
      case cond of
        Const _ _ v              -> rvalueHas3 name v
        Compare _ _ _ l (At _ r) -> rvalueHas3 name l || rvalueHas3 name r
        Every cs                 -> any (\(At _ v) -> go v) cs
        Any cs                   -> any (\(At _ v) -> go v) cs
        Not c                    -> go c

rvalueHas3 :: ShortByteString -> RValue -> Bool
rvalueHas3 name = go
  where
    go x =
      case x of
        Value dyn ->
          case dyn of
            Varying (Var (Static Local) name')      -> name == name'
            Varying (VArray _ (Static Local) name') -> name == name'
            _                                       -> False

        RCompare _ _ _ l (At _ r) -> go l || go r
        Cast _ _ v                -> go v
        Negate _ v                -> go v
        Complement _ v            -> go v
        Binary _ _ _ l (At _ r)   -> go l || go r
        RCall _ vs                -> any (\(At _ v) -> go v) vs



data For3 = NoFor3
          | For3 Assignment [At Expr]

for3 :: ShortByteString -> [At Expr] -> For3
for3 name = go
  where
    go xs =
      case xs of
        a : bs@(_:_) ->
          case go bs of
            NoFor3    -> NoFor3
            For3 v cs -> For3 v (a:cs)

        At _ a : [] ->
          case a of
            Assign v@(Assignment _ _ (At _ (Var (Static Local) name')) r)
              | name == name', rvalueHas3 name r ->
                  For3 v []

            _ -> NoFor3

        [] -> NoFor3



condition3 :: Word32 -> [At Stage2] -> Result (At Condition, [At Stage2])
condition3 loc xs =
  case xs of
    At n a : ys ->
      case a of
        Cast2 dt Boolean r ->
          Success (At n (Const dt dt r), ys)

        Compare2 cmp dt1 dt2 l r ->
          Success (At n (Compare cmp dt1 dt2 l r), ys)

        Not2 x ->
          case x of
            Cast dt1 dt2 r ->
              Success (At n (Not (Const dt1 dt2 r)), ys)

            RCompare cmp dt1 dt2 l r ->
              Success (At n (Not (Compare cmp dt1 dt2 l r)), ys)

            _ -> Failure (fromIntegral n) $ "Unknown condition " <> showsPrec 11 x []

        _ -> Failure (fromIntegral n) $ "Unknown condition " <> showsPrec 11 a []

    _ -> Failure (fromIntegral loc) "Expecting a condition at local end"



if3 :: Context3 -> Locals -> At Condition -> [At Stage2] -> Result ([At Expr], Either (At Trailing3) [At Stage2])
if3 ctx locals (At loc cond) xs =
  case xs of
    At n a : ys ->
      case a of
        Break2 Jf  -> do
          ~(as, mayTrail, _) <- process3 ctx locals ys
          case mayTrail of
            Nothing -> Success ([At loc (If cond as (Else [At n Break]))], Right [])
            Just _  -> Failure (fromIntegral n) "Trailing statement in breaking If"

        Jump2 Jf j | j == ctx.upper -> do
          let ~(as, r) = frontcheck3 j ys
          case r of
            None3 -> do
              ~(as', mayTrail, _) <- process3 ctx locals as
              case mayTrail of
                Nothing -> Success ([At loc (If cond as' NoElse)], Right [])
                Just _  -> Failure (fromIntegral n) "Trailing statement in no-else If"

            Jump3 k | k == ctx.upper -> do
              ~(as', mayTrail, _) <- process3 ctx locals as
              case mayTrail of
                Nothing -> Success ([At loc (If cond as' (Else []))], Right [])
                Just _  -> Failure (fromIntegral n) "Trailing statement in no-else If"

            _     -> Failure (fromIntegral n) "Malformed context-end jump"

        Jump2 jmp j ->
          case jmp of
            J -> Failure (fromIntegral n) "Unconditional jump after comparison"
            _ -> do
              ~(as, r, bs) <- frontlimit3 j ys
              case r of
                None3 -> do
                  ~(as', mayTrail, _) <- process3 (Context3 j) locals as
                  case mayTrail of
                    Nothing -> Success ([At loc (If cond as' NoElse)], Right bs)
                    Just _  -> Failure (fromIntegral n) "Trailing statement in simple If"

                Jump3 k -> do
                  ~(as', mayTrail, _) <- process3 (Context3 (j - 4)) locals as
                  case mayTrail of
                    Just _  -> Failure (fromIntegral n) "Trailing statement in an If"
                    Nothing ->
                      if k == ctx.upper
                        then do
                          ~(bs', mayTrail', _) <- process3 (Context3 k) locals bs
                          case mayTrail' of
                            Nothing -> Success ([At loc (If cond as' (Else bs'))], Right [])
                            Just _  -> Failure (fromIntegral k) "Trailing statement in escaping Else"

                        else do
                          ~(bs1, cs) <- frontlimit k bs
                          ~(bs2, mayTrail', _) <- process3 (Context3 k) locals bs1
                          case mayTrail' of
                            Nothing -> Success ([At loc (If cond as' (Else bs2))], Right cs)
                            Just _  -> Failure (fromIntegral k) "Trailing statement in an Else"

                One3 ->
                  case jmp of
                    Jf -> Failure (fromIntegral loc) "Jf points to an Any joint"
                    Jt -> do
                      ~(v, vs) <- condition3 loc as
                      cs <- boolean3 True [] v j vs
                      if3 ctx locals (At loc (Any (At loc cond : cs))) bs

                Zero3 ->
                  case jmp of
                    Jt -> Failure (fromIntegral loc) "Jt points to an Every joint"
                    Jf -> do
                      ~(v, vs) <- condition3 loc as
                      cs <- boolean3 False [] v j vs
                      if3 ctx locals (At loc (Every (At loc cond : cs))) bs

                OneNot3 ->
                  case jmp of
                    Jf -> Failure (fromIntegral loc) "Not Jf points to an Every joint"
                    Jt -> do
                      ~(v, vs) <- condition3 loc as
                      cs <- boolean3 True [] v j vs
                      if3 ctx locals (At loc (Not (Every (At loc cond : cs)))) bs

                ZeroNot3 ->
                  case jmp of
                    Jt -> Failure (fromIntegral loc) "Not Jt points to an Any joint"
                    Jf -> do
                      ~(v, vs) <- condition3 loc as
                      cs <- boolean3 False [] v j vs
                      if3 ctx locals (At loc (Not (Any (At loc cond : cs)))) bs

        _ -> Failure (fromIntegral n) "Cannot form an If"

    _ -> Success ([], Left (At loc (Condition3 cond)))


boolean3 :: Bool -> [At Condition] -> At Condition -> Word32 -> [At Stage2] -> Result [At Condition]
boolean3 t cond (At loc r) j xs =
  case xs of
    []          -> Success (cond <> [At loc r])
    At n a : ys ->
      case a of
        Jump2 jmp k ->
          if k == j
            then case t of
                   True  | Jt <- jmp -> do
                     ~(v, vs) <- condition3 loc ys
                     boolean3 t (cond <> [At loc r]) v j vs

                   False | Jf <- jmp -> do
                     ~(v, vs) <- condition3 loc ys
                     boolean3 t (cond <> [At loc r]) v j vs

                   _     -> Failure (fromIntegral n) "Incorrect comparison continuation"

            else
              case jmp of
                J -> Failure (fromIntegral n) "Unconditional jump after comparison"
                _ -> do
                  ~(as, l, bs) <- frontlimit3 k ys
                  case l of
                    One3 ->
                      case jmp of
                        Jf -> Failure (fromIntegral loc) "Jf points to an Any joint"
                        Jt -> do
                          ~(v, vs) <- condition3 loc as
                          cs <- boolean3 True [] v k vs
                          boolean3 t cond (At loc (Any (At loc r : cs))) j bs

                    Zero3 ->
                      case jmp of
                        Jt -> Failure (fromIntegral loc) "Jt points to an Every joint"
                        Jf -> do
                          ~(v, vs) <- condition3 loc as
                          cs <- boolean3 False [] v k vs
                          boolean3 t cond (At loc (Every (At loc r : cs))) j bs

                    OneNot3 ->
                      case jmp of
                        Jf -> Failure (fromIntegral loc) "Not Jf points to an Every joint"
                        Jt -> do
                          ~(v, vs) <- condition3 loc as
                          cs <- boolean3 True [] v k vs
                          boolean3 t cond (At loc (Not (Every (At loc r : cs)))) j bs

                    ZeroNot3 ->
                      case jmp of
                        Jt -> Failure (fromIntegral loc) "Not Jt points to an Any joint"
                        Jf -> do
                          ~(v, vs) <- condition3 loc as
                          cs <- boolean3 False [] v k vs
                          boolean3 t cond (At loc (Not (Any (At loc r : cs)))) j bs

                    _ -> Failure (fromIntegral n) "Malformed condition combination"

        _ -> Failure (fromIntegral n) "Malformed condition combination"



frontlimit :: Word32 -> [At a] -> Result ([At a], [At a])
frontlimit j = go
  where
    go as =
      case as of
        At n a : bs ->
          case compare n j of
            LT -> do ~(cs, ds) <- go bs
                     Success (At n a : cs, ds)

            EQ -> Success ([], as)

            GT -> Failure (fromIntegral n) "Overshot frontlimit"

        [] -> Failure (-1) "Overshot frontlimit"




data Limit3 = None3
            | Jump3 Word32
            | One3
            | Zero3
            | OneNot3
            | ZeroNot3
              deriving Show

frontlimit3 :: Word32 -> [At Stage2] -> Result ([At Stage2], Limit3, [At Stage2])
frontlimit3 j = go
  where
    go as =
      case as of
        At n a : bs ->
          case compare n (j - 4) of
            LT -> do ~(cs, r, ds) <- go bs
                     Success (At n a : cs, r, ds)

            EQ ->
              case a of
                AllowNot2 -> Success ([], OneNot3 , bs)
                DenyNot2  -> Success ([], ZeroNot3, bs)
                Allow2    -> Success ([], One3    , bs)
                Deny2     -> Success ([], Zero3   , bs)
                Jump2 J w -> Success ([], Jump3 w , bs)
                _         ->
                  case bs of
                    At m _          : _
                      | m == j -> Success ([At n a], None3, bs)

                    _ -> Failure (fromIntegral n) "Overshot frontlimit3"

            GT ->
              if n == j
                then Success ([], None3, as)
                else Failure (fromIntegral n) "Overshot frontlimit3"

        [] -> Failure (-1) "Overshot frontlimit3"


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
          let ~(ds, r) = go1 b cs
          in (At n a : ds, r)

        []     ->
          case a of
            Jump2 J w | n == j - 4 -> ([], Jump3 w)
            _         -> ([At n a], None3)



data Source =
       Source
         { name        :: ShortByteString
         , arguments   :: [ShortByteString]
         , expressions :: [At Expr]
         }
       deriving Show

decompile :: Objects -> Assembly -> Result Source
decompile objs asm = do
  s1 <- stage1 asm.instructions
  s2 <- stage2 asm.size objs s1
  s3 <- stage3 asm.size (drop 1 asm.arguments) s2
  Success (Source asm.name asm.arguments s3)

makeFieldLabelsNoPrefix ''Source
