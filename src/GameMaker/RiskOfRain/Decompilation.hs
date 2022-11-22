{-|
    This module describes operations that turn the entire CODE chunk of the
    Risk of Rain GameMaker data file into readable C-like.

    The bytecode is proper 0xF as described in that little Undertale
    [guide](https://pcy.ulyssis.be/undertale/decompilation-corrected). Also there's the
    0xE [guide](https://github.com/donkeybonks/acolyte/wiki/Bytecode) if you wanna
    figure out more general things.

    Important notes:

      * The interpreter is indeed stack-based, but elements are applied in reverse,
        so when it is said that @0x08 : Add // Push(Pop() + Pop())@,
        first goes left @Pop ()@, then right one, then @Push ()@.

      * We don't need to simulate the stack to decode because the code is
        absolutely unoptimized, so decompilation is just straightforward parsing.
 -}

{-# LANGUAGE BangPatterns
           , DataKinds
           , DeriveAnyClass
           , DeriveFunctor
           , DeriveGeneric
           , DerivingStrategies
           , DerivingVia
           , DuplicateRecordFields
           , FlexibleInstances
           , FunctionalDependencies
           , GeneralizedNewtypeDeriving
           , LambdaCase
           , OverloadedLabels
           , OverloadedStrings
           , PatternSynonyms
           , StandaloneDeriving
           , TemplateHaskell
           , TypeApplications
           , TypeOperators
           , ViewPatterns #-}

-- For any code readers: this module doesn't use any parsers, it's just
-- 'Either' over a 'Seq' and a lot of backwards pattern matching,
-- so while I tried to document this module in a nice fashion,
-- the code still most certainly looks like garbage.

module GameMaker.RiskOfRain.Decompilation
  ( -- * Datatypes
    Constant (..)
  , Dimensions (..)
  , Varied (..)
  , Value (..)
  , Comparable (..)
  , Comparison (..)
  , Assignable (..)
  , Assignment (..)
  , Case' (..)
  , Case
  , DefaultCase
  , Expression (..)
    -- * Conversions
  , expressions
  , simpleExpressions
    -- * Helper functions
  , unparsed
  , pprint
  ) where

import qualified GameMaker.RiskOfRain.Decompilation.Raw as Raw
import           GameMaker.RiskOfRain.Decompilation.Raw hiding (Comparison, Call, Normal, Break, Exit)
import           GameMaker.RiskOfRain.Unpacking

import           Control.Applicative
import           Control.DeepSeq
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Coerce
import           Data.Foldable (asum, fold)
import           Data.List (intersperse)
import           Data.Int
import           Data.Maybe (catMaybes, fromMaybe, isJust)
import           Data.Semigroup (stimes)
import qualified Data.Sequence as S
import           Data.Sequence ( Seq (..) )
import           Data.String (IsString)
import           GHC.Generics (Generic)
import           Lens.Micro
import           Lens.Micro.Labels.TH
import           Numeric (showFFloat)
import           Prelude



-- | Wrapper for @[Char]@ so we don't orphan @'MonadFail' ('Either' '[Char]')@.
newtype Stringlike = Stringlike [Char]
                     deriving newtype (Semigroup, Monoid, IsString)

instance {-# OVERLAPS #-} Alternative (Either Stringlike) where
    empty = Left ""

    Left _ <|> n = n
    m      <|> _ = m

instance MonadFail (Either Stringlike) where
  fail = Left . Stringlike

-- | Internal helper that solves type conversions.
unwrapStringlike :: Either Stringlike a -> Either [Char] a
unwrapStringlike = coerce



-- | Tells where the end of the function is.
codeEnd :: CodeFunction -> Int
codeEnd el = el ^. #offset . to fromIntegral + el ^. #size . to fromIntegral



data CPrinter = CPrinter
                  { isOuter   :: Bool -- ^ Determines whether round brackets are needed around subexpressions
                  , numOffset :: Int  -- ^ Size of the number field
                  , indent    :: Int  -- ^ Total line indentation
                  }
                deriving Show

makeLabels ''CPrinter

-- | Simple text printer for outputting 'Expression's.
class CPrint a where
  cprint :: CPrinter -> a -> Builder

-- | Initializes 'CPrinter'.
cprinter :: CodeFunction -> CPrinter
cprinter el = CPrinter True (length . show $ codeEnd el) 0

-- | 'mtimesDefault' 'error's out on values less than zero.
mtimes :: Monoid a => Int -> a -> a
mtimes n a
  | n < 1     = mempty
  | otherwise = stimes n a

-- | Outputs a line number and an expression at an indent.
numbered :: CPrinter -> Int -> Builder -> Builder
numbered pp n a =
  mconcat
    [ intDec n
    , mtimes (pp ^. #numOffset . to fromIntegral - length (show n) + 1) " "
    , mtimes (pp ^. #indent) " "
    , a
    ]

-- | Same as 'numbered' but the number is blank space.
indented :: CPrinter -> Int -> Builder -> Builder
indented pp n a =
  mconcat
    [ mtimes (pp ^. #numOffset . to fromIntegral + 1) " "
    , mtimes (pp ^. #indent + n) " "
    , a
    ]

-- | Outputs nested 'CPrint'able elements with curly brackets around
--   if there's more than one of them.
wrapped :: (CPrint a, Foldable f) => CPrinter -> Int -> f a -> Builder
wrapped pp n els
  | length els == 1 = "\n" <> subindent pp n els
  | otherwise       = mconcat
                        [ " {\n"
                        , subindent pp n els
                        , indented pp 0 "}\n"
                        ]

-- | Outputs nested 'CPrint'able elements at an additional indent.
subindent :: (CPrint a, Foldable f) => CPrinter -> Int -> f a -> Builder
subindent pp n = foldMap $ cprint pp { indent = pp ^. #indent + n }



-- | Array dimensions. GameMaker only supports 1D and 2D arrays.
data Dimensions = OneDim (Marked Assignable)
                | TwoDim (Marked Assignable) (Marked Assignable)
                  deriving (Show, Eq, Generic, NFData)

instance CPrint Dimensions where
  cprint pp =
    \case
       OneDim (_ :@ x)          -> "[" <> cprint pp x <> "]"
       TwoDim (_ :@ x) (_ :@ y) -> "[" <> cprint pp x <> "][" <> cprint pp y <> "]"



-- | Constant values pushed onto stack.
data Constant = ConstDouble Double
              | ConstInt32 Int32
              | ConstInt16 Int16
              | ConstString BS.ByteString
                deriving (Show, Eq, Generic, NFData)

instance CPrint Constant where
  cprint _ =
    \case
       ConstDouble d -> lazyByteString . BSL.pack $ showFFloat Nothing d ""
       ConstInt32 i  -> int32Dec i
       ConstInt16 i  -> int16Dec i
       ConstString n -> lazyByteString . BSL.pack $ show n



-- | Variables. Pushes in this case mean reading said variable, pops mean writing to it.
data Varied = VarNormal Instance Variable BS.ByteString
            | VarInstanced (Marked Assignable) BS.ByteString
            | VarArray (Marked Dimensions) (Marked Assignable) BS.ByteString
              deriving (Show, Eq, Generic, NFData)

instance CPrint Varied where
  cprint pp =
    \case
       VarNormal    Other   _ n -> "other." <> byteString n
       VarNormal    _       _ n -> byteString n
       VarInstanced (_:@ i)   n -> cprint pp i <> "." <> byteString n
       VarArray     (_:@ d) _ n -> byteString n <> cprint pp d



-- | Anything pushable on the stack.
data Value = Constant Constant
           | Varied Varied
             deriving (Show, Eq, Generic, NFData)

instance CPrint Value where
  cprint pp =
    \case
       Constant con -> cprint pp con
       Varied var   -> cprint pp var



-- | Binary operation used in 'Assignable'. Excludes 'Remainder' and 'Modulo' because
--   those do not have operator counterparts in the language.
data Binary = (:*)
            | (:/)
            | (:+)
            | (:-)
            | (:&)
            | (:|)
            | (:^)
            | (:<<)
            | (:>>)
              deriving (Show, Eq, Generic, NFData)

-- | Any expression that yields anything assignable. This is basically r-value.
data Assignable = Value      (Marked Value)
                | Compared   TypePair Comparable
                | Cast       TypePair (Marked Assignable)
                | Negate     DataType (Marked Assignable)
                | Complement DataType (Marked Assignable)
                | Remainder  TypePair (Marked Assignable) (Marked Assignable)
                | Modulo     TypePair (Marked Assignable) (Marked Assignable)
                | Binary     Binary TypePair (Marked Assignable) (Marked Assignable)
                | Callable   BS.ByteString [Marked Assignable]
                  deriving (Show, Eq, Generic, NFData)

instance CPrint Assignable where
  cprint pp =
    let roundBr pp' f =
          if pp' ^. #isOuter
            then        f pp' { isOuter = False }       
            else "(" <> f pp'                     <> ")"

        convert =
          \case
             (:*)  -> " * "
             (:/)  -> " / "
             (:+)  -> " + "
             (:-)  -> " - "
             (:&)  -> " & "
             (:|)  -> " | "
             (:^)  -> " ^ "
             (:<<) -> " << "
             (:>>) -> " >> "

    in \case
          Value        (_ :@ v)          -> cprint pp v
          Compared   _ v                 -> roundBr pp $ \pp' -> cprint pp' v
          Cast       _ (_ :@ a)          -> cprint pp a
          Remainder  _ (_ :@ a) (_ :@ b) -> "rem (" <> cprint pp a <> ", " <> cprint pp b <> ")"
          Modulo     _ (_ :@ a) (_ :@ b) -> "mod (" <> cprint pp a <> ", " <> cprint pp b <> ")"
          Negate     _ (_ :@ a)          -> roundBr pp $ \pp' -> "-" <> cprint pp' a
          Complement _ (_ :@ a)          -> roundBr pp $ \pp' -> "~" <> cprint pp' a
          Binary  op _ (_ :@ a) (_ :@ b) -> roundBr pp $ \pp' ->
                                              cprint pp' a <> convert op <> cprint pp' b
          Callable f l ->
            mconcat
              [ byteString f
              , " ("
              , mconcat (intersperse ", " $ cprint pp . unkey <$> l)
              , ")"
              ]



-- | Simple comparisons over one or two elements.
data Comparable = Comparable Raw.Comparison (Marked Assignable) (Marked Assignable)
                | Boolean (Marked Assignable)
                | (:!)    (Marked Assignable)
                  deriving (Show, Eq, Generic, NFData)

instance CPrint Comparable where
  cprint pp =
    \case
       Comparable LowerThan   (_ :@ a) (_ :@ b) -> cprint pp a <> " < " <> cprint pp b
       Comparable LTOrEqual   (_ :@ a) (_ :@ b) -> cprint pp a <> " <= " <> cprint pp b
       Comparable Equal       (_ :@ a) (_ :@ b) -> cprint pp a <> " == " <> cprint pp b
       Comparable Inequal     (_ :@ a) (_ :@ b) -> cprint pp a <> " != " <> cprint pp b
       Comparable GTOrEqual   (_ :@ a) (_ :@ b) -> cprint pp a <> " >= " <> cprint pp b
       Comparable GreaterThan (_ :@ a) (_ :@ b) -> cprint pp a <> " > " <> cprint pp b
       Boolean (_ :@ a)                       -> cprint pp a
       (:!)    (_ :@ a)                       ->
         let printed = cprint pp a
         in if BSL.elem ' ' $ toLazyByteString printed
              then "!(" <> printed <> ")"
              else "!" <> printed



-- | Combinations of 'Comparable's.
data Comparison = Comparison TypePair Comparable
                | (:&&) [Marked Comparison]
                | (:||) [Marked Comparison]
                   deriving (Show, Eq, Generic, NFData)

isAnd :: Comparison -> Bool
isAnd ((:&&) _) = True
isAnd _         = False

isOr :: Comparison -> Bool
isOr ((:||) _) = True
isOr _         = False

instance CPrint Comparison where
  cprint pp =
    let ppOuter = pp { isOuter = True }
        ppInner = pp { isOuter = False }
        
        mintercalate _ _             []  = mempty
        mintercalate f _   ((_ :@ x):[]) = if f x
                                             then cprint ppInner x
                                             else cprint ppOuter x
        mintercalate f val ((_ :@ x):xs) =
          if f x
            then cprint ppInner x <> val <> mintercalate f val xs
            else cprint ppOuter x <> val <> mintercalate f val xs

    in do \a -> if pp ^. #isOuter
                  then a
                  else "(" <> a <> ")"
        . \case
             Comparison _ c -> cprint ppOuter c
             (:&&) cs       -> mintercalate isOr  " && " cs
             (:||) cs       -> mintercalate isAnd " || " cs



-- | Writing a variable to memory.
data Assignment = Assignment TypePair (Marked Varied) (Marked Assignable)
                  deriving (Show, Eq, Generic, NFData)

instance CPrint Assignment where
  cprint pp (Assignment _ (_ :@ lvalue) (_ :@ rvalue)) =
    let normal = cprint pp lvalue <> " = " <> cprint pp rvalue

        VarNormal _ v name =? VarNormal Self w name' = v == w && name == name'
        l                  =? r                      = l == r

        convert =
           \case
             (:*)  -> " *= "
             (:/)  -> " /= "
             (:+)  -> " += "
             (:-)  -> " -= "
             (:&)  -> " &= "
             (:|)  -> " |= "
             (:^)  -> " ^= "
             (:<<) -> " <<= "
             (:>>) -> " >>= "

    in case rvalue of
         Binary op _ (_ :@ Value (_ :@ Varied rv)) (_ :@ res) ->
           if lvalue =? rv
             then cprint pp lvalue <> convert op <> cprint pp res
             else normal

         _ -> normal



-- | A single entry of a 'Switch' case.
data Case' a = Case a Expr Bool
               deriving (Show, Eq, Functor, Generic, NFData)

type Case = Case' (Marked Assignable)

instance CPrint Case where
  cprint pp (Case (_ :@ new) code hasBreak) =
    mconcat
      [ indented pp 2 $ "case (" <> cprint pp new <> "):\n"
      , subindent pp 4 code
      , if hasBreak
          then indented pp 4 "break;\n"
          else mempty
      ]

type DefaultCase = Case' ()

{-# COMPLETE DefaultCase #-}
pattern DefaultCase :: Expr -> Bool -> DefaultCase
pattern DefaultCase e b = Case () e b

instance CPrint DefaultCase where
  cprint pp (DefaultCase code hasBreak) =
    mconcat
      [ indented pp 2 $ "default:\n"
      , subindent pp 4 code
      , if hasBreak
          then indented pp 4 "break;\n"
          else mempty
      ]

-- | Decompiled GameMaker instructions.
data Expression = Assign  Assignment
                | Call    BS.ByteString [Marked Assignable]
                | Return  (Marked Assignable)
                | Compare TypePair Comparable
                | For     Assignment (Marked Comparison) Assignment Expr
                | ForDec  (Marked Assignable) Expr
                | Repeat  Int16 Expr
                | If      (Marked Comparison) Expr (Maybe Expr)
                | While   (Marked Comparison) Expr
                | Do      (Marked Comparison) Expr
                | Switch  (Marked Assignable) [Case] (Maybe DefaultCase)
                | With    (Marked Assignable) Expr
                | Continue
                | Break
                | Exit
                | Unparsed (Cooked Instruction)
                | UnparsedLoop Expr
                  deriving (Show, Eq, Generic, NFData)

type Expr = Seq (Marked Expression)

instance CPrint (Marked Expression) where
  cprint pp (n :@ expr) =
    case expr of
      If (_ :@ comp) thn mayElse ->
        mconcat
          [ numbered pp n $ "if (" <> cprint pp comp <> ")"
          , wrapped pp 2 thn
          , case mayElse of
              Nothing  -> mempty
              Just els ->
                mconcat
                  [ indented pp 0 "else"
                  , wrapped pp 2 els
                  ]
          ]

      Switch (_ :@ val) cases def ->
        mconcat
          [ numbered pp n $ "switch (" <> cprint pp val <> ")\n"
          , foldMap (cprint pp) cases
          , foldMap (cprint pp) def
          ]

      With (_ :@ obj) code ->
        mconcat
          [ numbered pp n $ "with (" <> cprint pp obj <> ")"
          , wrapped pp 2 code
          ]

      Repeat times code ->
        mconcat
          [ numbered pp n $ "repeat (" <> int16Dec times <> ")"
          , wrapped pp 2 code
          ]

      For assign (_ :@ comp) modify code ->
        mconcat
          [ numbered pp n $ "for (" <> cprint pp assign <> "; "
                                    <> cprint pp comp   <> "; "
                                    <> cprint pp modify <> ")"
          , wrapped pp 2 code
          ]

      ForDec (_ :@ honk) code ->
        mconcat
          [ numbered pp n $ "for (i = " <> cprint pp honk <> "; i < 0; i--)"
          , wrapped pp 2 code
          ]

      While (_ :@ comp) code ->
        mconcat
          [ numbered pp n $ "while (" <> cprint pp comp <> ")"
          , wrapped pp 2 code
          ]

      Do (_ :@ comp) code ->
        mconcat
          [ numbered pp n "do"
          , wrapped pp 2 code
          , indented pp 0 $ " while (" <> cprint pp comp <> ")\n"
          ]

      Compare _ comp ->
        mconcat
          [ numbered pp n $ cprint pp comp
          , "\n"
          ]

      Return (_ :@ val) ->
        mconcat
          [ numbered pp n $ "return (" <> cprint pp val <> ");"
          , "\n"
          ]

      Assign val -> numbered pp n (cprint pp val) <> ";\n"

      Call fun args ->
        mconcat
          [ numbered pp n $
              mconcat
                [ byteString fun
                , " ("
                , mconcat (intersperse ", " $ cprint pp . unkey <$> args)
                , ");"
                ]
          , "\n"
          ]
 
      Break -> numbered pp n $ byteString "break;\n"

      Continue -> numbered pp n $ byteString "continue;\n"

      Exit -> numbered pp n $ byteString "exit;\n"

      Unparsed v ->
        mconcat
          [ numbered pp n . stringUtf8 $ show v
          , "\n"
          ]

      UnparsedLoop v -> subindent pp 0 v



pattern J :: Bool -> Marked Int -> Marked (Cooked Instruction)
pattern J isPositive jumpTo <-
  ( (\(n :@ i) -> case () of
                    () | B b <- i  -> Just (b > 0, n :@ (n + 4 * fromIntegral b))
                       | otherwise -> Nothing
    ) -> Just (isPositive, jumpTo)
  )

pattern Jt :: Bool -> Marked Int -> Marked (Cooked Instruction)
pattern Jt isPositive jumpTo <-
  ( (\(n :@ i) -> case () of
                    () | Bt b <- i -> Just (b > 0, n :@ (n + 4 * fromIntegral b))
                       | otherwise -> Nothing
    ) -> Just (isPositive, jumpTo)
  )

pattern Jf :: Bool -> Marked Int -> Marked (Cooked Instruction)
pattern Jf isPositive jumpTo <-
  ( (\(n :@ i) -> case () of
                    () | Bf b <- i -> Just (b > 0, n :@ (n + 4 * fromIntegral b))
                       | otherwise -> Nothing
    ) -> Just (isPositive, jumpTo)
  )

pattern Jenv :: Marked Int -> Marked (Cooked Instruction)
pattern Jenv jumpTo <-
  ( (\(n :@ i) -> case () of
                    () | PopEnv b <- i -> Just $ n :@ (n + 4 * fromIntegral b)
                       | otherwise -> Nothing
    ) -> Just jumpTo
  )



--pattern Jmp :: Bool -> Marked Int -> Marked (Cooked Instruction)

-- | Subset of 'Instruction's that jump forwards.
data Jump = Jump       Int23
          | JumpWhen   Int23
          | JumpUnless Int23
            deriving (Show, Eq)

narrow :: Cooked Instruction -> Maybe Jump
narrow i
  | B  b <- i, b > 0 = Just $ Jump b
  | Bt b <- i, b > 0 = Just $ JumpWhen   b
  | Bf b <- i, b > 0 = Just $ JumpUnless b
  | otherwise        = Nothing

widen :: Jump -> Cooked Instruction
widen j =
  case j of
    Jump b       -> B b
    JumpWhen b   -> Bt b
    JumpUnless b -> Bf b

points :: Marked Jump -> Int
points (n :@ j) =
  let b = case j of
            Jump b'       -> b'
            JumpWhen b'   -> b'
            JumpUnless b' -> b'
  in n + 4 * fromIntegral b



-- | Subset of 'Instruction's that jump backwards.
--   Includes 'PopEnv' instructions for 'Deloop' purposes.
data Backjump = Backjump       Int23
              | BackjumpWhen   Int23
              | BackjumpUnless Int23
              | BackjumpEnv    Int23
                deriving (Show, Eq)

backnarrow :: Cooked Instruction -> Maybe Backjump
backnarrow i
  | B  b     <- i, b < 0 = Just $ Backjump       b
  | Bt b     <- i, b < 0 = Just $ BackjumpWhen   b
  | Bf b     <- i, b < 0 = Just $ BackjumpUnless b
  | PopEnv b <- i, b < 0 = Just $ BackjumpEnv b
  | otherwise            = Nothing

backwiden :: Backjump -> Cooked Instruction
backwiden j =
  case j of
    Backjump b       -> B b
    BackjumpWhen b   -> Bt            b
    BackjumpUnless b -> Bf b
    BackjumpEnv b    -> PopEnv b

backpoints :: Marked Backjump -> Int
backpoints (n :@ j) =
  let b = case j of
            Backjump b'       -> b'
            BackjumpWhen b'   -> b'
            BackjumpUnless b' -> b'
            BackjumpEnv b'    -> b' - 1
  in n + 4 * fromIntegral b



-- | Decompilation state.
data DecompS =
       DecompS
         { function   :: CodeFunction     -- ^ Function that is being decompiled
         , inCases    :: Int              -- ^ Number of nested cases we're inside
         , with       :: Bool             -- ^ Whether we are inside a with statement
         , withBreak  :: Maybe Int        -- ^ With statement breakpoint
         , loopPoints :: Maybe (Int, Int) -- ^ Loop statement continue- and breakpoints
         , caseBreak  :: Maybe Int        -- ^ Case statement breakpoint
         , blockEdge  :: Maybe Int        -- ^ The edge of a nested statement (e.g. if)
         }

makeLabels ''DecompS

-- | Initializes 'DecompS'.
decompS :: CodeFunction -> DecompS
decompS el = DecompS el 0 False Nothing Nothing Nothing Nothing

-- | Tells where the function ends locally.
edge :: DecompS -> Int
edge = fromMaybe <$> (^. #function . to codeEnd) <*> (^. #blockEdge)

-- | Sets the case break.
inCase :: Int -> DecompS -> DecompS
inCase brek = do #inCases %~ (+) 1
            . do #caseBreak .~ Just brek

-- | Sets the with statement flag.
inWith :: DecompS -> DecompS
inWith = #with .~ True

-- | Sets the with break.
brokenWith :: Int -> DecompS -> DecompS
brokenWith brek = #withBreak .~ Just brek

-- | Sets the loop statement continue and breakpoints.
withLoop :: Int -> Int -> DecompS -> DecompS
withLoop continue brek = #loopPoints .~ Just (continue, brek)

-- | Sets the local edge.
withEdge :: Int -> DecompS -> DecompS
withEdge edg = #blockEdge .~ Just edg



-- | This stands for @Instructions@, but if I use @inst@ as a variable name I mean @instance@,
--   take care.
type Inst = Seq (Marked (Cooked Instruction))

-- | A generic jump split.
data JumpCut =
       JumpCut
         Inst                           -- ^ Before
         (Marked (Cooked Instruction))  -- ^ The jump itself
         Inst                           -- ^ Part between the jump and where it points
         Inst                           -- ^ After
       deriving (Show, Eq)



-- | Finds the __last__ jump backwards in the instruction sequence going from the back
--   and splits it accordingly.
backjump :: Inst -> Either Stringlike JumpCut
backjump s =
  let (aft, s') = S.breakr (isJust . backnarrow . unkey) s
  in case s' of
       bef :|> n :@ i -> case backnarrow i of
                           Just j  -> slice bef (n :@ j) aft
                           Nothing -> Left "Unreachable state"
       _              -> Left "Not a backjump"
  where
    slice bef j aft =
           -- If address pointed to does not exist
      case S.findIndexR ((== backpoints j) . getKey) bef of
                   -- Fail
        Nothing -> Left $ "Invalid backjump position: " <> Stringlike (show $ backpoints j)
                   -- Split at address
        Just m  -> let (befor, loop) = S.splitAt m bef
                   in Right $ JumpCut befor (backwiden <$> j) loop aft



-- | Finds the __first__ jump forwards in the instruction sequence going from the front
--   and splits it accordingly.
jump :: DecompS -> Inst -> Either Stringlike JumpCut
jump ds s =
  let (bef, s') = S.breakl (isJust . narrow . unkey) s
  in case s' of
       n :@ i :<| aft -> case narrow i of
                           Just j  -> slice bef (n :@ j) aft
                           Nothing -> Left "Unreachable state"
       _              -> Left "Not a jump"
  where
    slice bef j aft =
          -- Checks whether jump points to the end of the block
          -- (since forward jumps can point one beyond the last instruction)
      if points j == edge ds
        then return $ JumpCut bef (widen <$> j) aft empty
        else
               -- If address pointed to does not exist
          case S.findIndexL ((== points j) . getKey) aft of
                       -- Fail
            Nothing -> Left $ "Invalid jump position: " <> Stringlike (show $ points j)
                       -- Split at address
            Just m  -> let (loop, after) = S.splitAt m aft
                       in Right $ JumpCut bef (widen <$> j) loop after



-- | Parses an 'Assignable'.
assignableS :: Inst -> Either Stringlike (Inst, Marked Assignable)
assignableS =
  \case
     s :|> _ :@ Conv (Boolean_, Variable_) -> do
       (s', n :@ Compare t c) <- compareS s
       Right (s', n :@ Compared t c)
     s :|> _ :@ Conv t -> singleC s t Cast
     s :|> n :@ PushCst v -> do
       (s', m :@ res) <- subpush s n v
       Right (s', m :@ Value (m :@ res))
     s :|> n :@ PushLoc v -> do
       (s', m :@ res) <- subpush s n v
       Right (s', m :@ Value (m :@ res))
     s :|> n :@ PushGlb v -> do
       (s', m :@ res) <- subpush s n v
       Right (s', m :@ Value (m :@ res))
     s :|> _ :@ PushVar StackTopOrGlobal _ (Reference name Array) -> do
       (s', n :@ res) <- dimensionsS s name
       Right (s', n :@ Value (n :@ Varied res))
     s :|> _ :@ PushVar StackTopOrGlobal _ (Reference name StackTop) -> do
       (s', n :@ res) <- instanceS s name
       Right (s', n :@ Value (n :@ Varied res))
     s :|> n :@ PushVar i _ (Reference name v) ->
       Right (s, n :@ Value (n :@ Varied (VarNormal i v name)))
     s :|> n :@ PushI16 i _ ->
       Right (s, n :@ Value (n :@ Constant (ConstInt16 i)))
     s :|> _ :@ Neg t -> singleC s t Negate
     s :|> _ :@ Not t -> singleC s t Complement
     s :|> _ :@ Rem t -> doubleC s t Remainder
     s :|> _ :@ Mod t -> doubleC s t Modulo
     s :|> _ :@ Mul t -> doubleC s t $ Binary (:*)
     s :|> _ :@ Div t -> doubleC s t $ Binary (:/)
     s :|> _ :@ And t -> doubleC s t $ Binary (:&)
     s :|> _ :@ Or t  -> doubleC s t $ Binary (:|)
     s :|> _ :@ Xor t -> doubleC s t $ Binary (:^)
     s :|> _ :@ Add t -> doubleC s t $ Binary (:+)
     s :|> _ :@ Sub t -> doubleC s t $ Binary (:-)
     s :|> _ :@ Shl t -> doubleC s t $ Binary (:<<)
     s :|> _ :@ Shr t -> doubleC s t $ Binary (:>>)
     s :|> n :@ Raw.Call i _ (Reference name _) -> repeatN s n i $ Callable name
     q@(_ :|> _ :@ Cmp _ (_, Variable_)) -> do
       (s', n :@ Compare t c) <- compareS q
       Right (s', n :@ Compared t c)
     _ -> Left "Not a assignableS"
  where
    singleC s t f = do
      (s', n :@ res) <- assignableS s
      Right (s', n :@ f t (n :@ res))

    doubleC s t f = do
      (s' , res)       <- assignableS s
      (s'', n :@ res') <- assignableS s'
      Right (s'', n :@ f t (n :@ res') res)

    subpush s n =
      \case
         PushDouble v -> Right (s, n :@ Constant (ConstDouble v))
         PushInt32 v  -> Right (s, n :@ Constant (ConstInt32 v))
         PushInt16 v  -> Right (s, n :@ Constant (ConstInt16 v))
         PushStrg v   -> Right (s, n :@ Constant (ConstString v))
         PushVari StackTopOrGlobal (Reference name Array)    -> do
           (s', m :@ res) <- dimensionsS s name
           Right (s', m :@ Varied res)
         PushVari StackTopOrGlobal (Reference name StackTop) -> do
           (s', m :@ res) <- instanceS s name
           Right (s', m :@ Varied res)
         PushVari i                (Reference name v) ->
           Right (s , n :@ Varied (VarNormal i v name))

-- | Parses a 'VarInstanced'.
instanceS :: Inst -> BS.ByteString -> Either Stringlike (Inst, Marked Varied)
instanceS s name = do
  (s', n :@ inst) <- assignableS s
  Right (s', n :@ VarInstanced (n :@ inst) name)

-- | Parses a 'VarArray'.
--
--   Note: GameMaker only supports 1D and 2D arrays with a limit of 32000 on each dimension
--         (therefore indices go from 0 to 31999). One-dimensional array access works as
--         expected, while two-dimensional case uses @dim1 + 32000 * dim2@ as the index
--         and inserts two 'Raw.Break's (which I think you can turn off in GameMaker
--         settings for sick performance gains, but Hopoo never did this).
dimensionsS :: Inst -> BS.ByteString -> Either Stringlike (Inst, Marked Varied)
dimensionsS s name = do
  (s', dims) <- asum
                  [ do s' :|> _ :@ Raw.Break (-1) Int16_
                          :|> _ :@ Add (Int32_,Int32_) <- return s
                       (s'', dim2) <- assignableS s'
                       s''' :|> _ :@ Raw.Break (-1) Int16_
                            :|> _ :@ PushCst (PushInt32 32000)
                            :|> _ :@ Mul (Int32_,Int32_)       <- return s''
                       (s'''', dim1@(n :@ _)) <- assignableS s'''
                       Right (s'''', n :@ TwoDim dim1 dim2)

                  , do (s', dim1@(n :@ _)) <- assignableS s
                       Right (s', n :@ OneDim dim1)
                  ]
  (s'', arr@(m :@ _)) <- assignableS s'
  Right (s'', m :@ VarArray dims arr name)

-- | Helper function that repeats a parsing action multiple times.
repeatN
  :: Inst                               -- ^ Instruction sequence
  -> Int                                -- ^ Start position
  -> Int16                              -- ^ Number of repetitions
  -> ([Marked Assignable] -> b)         -- ^ Conversion
  -> Either Stringlike (Inst, Marked b)
repeatN s n counter convert = do
  (s', n', res) <- loop s n (fromIntegral counter :: Int)
  Right (s', n' :@ convert res)
  where
    loop sI nI counterI
      | counterI <= 0 = Right (sI, nI, [])
      | otherwise     = do (s', n' :@ assigned) <- assignableS sI
                           (s'', n'', assigned') <- loop s' n' (counterI - 1)
                           Right (s'', n'', (n' :@ assigned) : assigned')

-- | Parses a 'Return' statement.
returnS :: Inst -> Either Stringlike (Inst, Marked Expression)
returnS =
  \case
     s :|> _ :@ Ret Variable_ -> do
       (s', n :@ assigned) <- assignableS s
       Right (s', n :@ Return (n :@ assigned))

     _ -> Left "Not a return statement"

-- | Parses an 'Assignment'.
assignmentS :: Inst -> Either Stringlike (Inst, Marked Assignment)
assignmentS =
  \case
     s :|> _ :@ Pop t _ (Reference name Array) -> do
       (s', dims) <- dimensionsS s name
       (s'', assigned@(n :@ _)) <- assignableS s'
       Right (s'', n :@ Assignment t dims assigned)
     
     s :|> _ :@ Pop t _ (Reference name StackTop) -> do
       (s', inst) <- instanceS s name
       (s'', assigned@(n :@ _)) <- assignableS s'
       Right (s'', n :@ Assignment t inst assigned)
     
     s :|> p :@ Pop t i (Reference name v) -> do
       (s', assigned@(n :@ _)) <- assignableS s
       Right (s', n :@ Assignment t (p :@ VarNormal i v name) assigned)

     _ -> Left "Not an assignment"

-- | A bastardized version of 'assignmentS' for operations where array indices or
--   instance references are cloned with a 'Dup'. This never happens to normal variables,
--   so there is no separate self assignment datatype.
selfassignmentS :: Inst -> Either Stringlike (Inst, Marked Assignment)
selfassignmentS =
  let convert =
        \case
           s :|> _ :@ Mul t -> Right (s, Binary (:*)  t)
           s :|> _ :@ Div t -> Right (s, Binary (:/)  t)
           s :|> _ :@ Rem t -> Right (s, Remainder    t)
           s :|> _ :@ Mod t -> Right (s, Modulo       t)
           s :|> _ :@ And t -> Right (s, Binary (:&)  t)
           s :|> _ :@ Or  t -> Right (s, Binary (:|)  t)
           s :|> _ :@ Xor t -> Right (s, Binary (:^)  t)
           s :|> _ :@ Add t -> Right (s, Binary (:+)  t)
           s :|> _ :@ Sub t -> Right (s, Binary (:-)  t)
           s :|> _ :@ Shl t -> Right (s, Binary (:<<) t)
           s :|> _ :@ Shr t -> Right (s, Binary (:>>) t)
           _                -> Left "Not a double operation"

  in \case
        s :|> _ :@ Pop t _ (Reference name Array) -> do
          (s', op) <- convert s
          ( s'' :|> _ :@ Dup Int32_
                :|> _ :@ PushCst (PushVari StackTopOrGlobal (Reference name' Array))
           , assigned
           ) <- assignableS s'
          (s''', dims@(n :@ res')) <- dimensionsS s'' name
          if name == name'
            then Right (s''', n :@ Assignment t dims (n :@ op (n :@ Value (n :@ Varied res')) assigned))
            else Left "Not a self assignment"

        s :|> _ :@ Pop t _ (Reference name StackTop) -> do
          (s', op) <- convert s
          ( s'' :|> _ :@ Dup Int32_
                :|> _ :@ PushCst (PushVari StackTopOrGlobal (Reference name' StackTop))
           , assigned
           ) <- assignableS s'
          (s''', inst@(n :@ res')) <- instanceS s'' name
          if name == name'
            then Right (s''', n :@ Assignment t inst (n :@ op (n :@ Value (n :@ Varied res')) assigned))
            else Left "Not a self assignment"

        _ -> Left "Not a self assignment"

-- | Parses a 'Call' statement.
callS :: Inst -> Either Stringlike (Inst, Marked Expression)
callS =
  \case
     s :|> n :@ Raw.Call i Int32_ (Reference name Array)
       :|> _ :@ Popz Variable_ -> do
       (s', m :@ res) <- repeatN s n i $ Call name
       Right (s', m :@ res)

     _  -> Left "Not a call statement"



-- | Parses 'Break' and 'Continue' statements based on 'DecompS'.
breakS :: DecompS -> Inst -> Either Stringlike (Inst, Marked Expression)
breakS ds (s :|> J isPositive (n :@ b))
  | Just (_, breakPoint)    <- ds ^. #loopPoints
  , isPositive
  , b == breakPoint    = Right (s, n :@ Break   )

  | Just (continuePoint, _) <- ds ^. #loopPoints
  , not isPositive
  , b == continuePoint = Right (s, n :@ Continue)

  | Just breakPoint         <- ds ^. #caseBreak
  , isPositive
  , b == breakPoint    = Right (s, n :@ Break   )

  | Just breakPoint         <- ds ^. #withBreak
  , isPositive
  , b == breakPoint    = Right (s, n :@ Break   )

  | otherwise = Left "Not a break statement"

breakS _  _ = Left "Not a break statement"



-- | Parses 'Expression's excluding branches.
simpleX :: DecompS -> Inst -> Expr
simpleX ds i
  | Right (s, n :@ assigned) <- assignmentS i     = simpleX ds s           :|> n :@ Assign assigned
  | Right (s, called)        <- callS i           = simpleX ds s           :|> called
  | Right (s, returned)      <- returnS i         = simpleX ds (cleanup s) :|> returned
  | Right (s, broken)        <- breakS ds i       = simpleX ds s           :|> broken
  | Right (s, compared)      <- compareS i        = simpleX ds s           :|> compared
  | Right (s, n :@ assigned) <- selfassignmentS i = simpleX ds s           :|> n :@ Assign assigned
  | otherwise                                 =
      case i of
        S.Empty                    -> S.Empty
        s :|> n :@ Raw.Exit Int32_ -> simpleX ds (cleanup s) :|> n :@ Exit
        s :|> n :@ a               -> simpleX ds s           :|> n :@ Unparsed a

  where
    -- Consumes @Popz Variable_@s and @PopEnvAny@s that happen before returns and exits in
    -- case and with statements. While this is supposed to be failable and properly ordered I
    -- didn't bother, so this trusts GameMaker to have put them properly.
    -- (And at least in Risk of Rain 1.3.0 CODE chunk they definitely do)
    cleanup = consume (ds ^. #inCases) (ds ^. #with)
    
    consume n b s =
      case s of
        s' :|> _ :@ Popz Variable_ -> if n > 0
                                        then consume (n - 1) b s'
                                        else s
        s' :|> _ :@ PopEnvAny      -> if b
                                        then consume n False s'
                                        else s
        _                          -> s



-- | Helper data structure to parse branches. 'Loop' means specifically 'Backjump' loops.
data Deloop = Loop (Marked (Cooked Instruction)) Inst
            | Normal Inst
              deriving (Show, Eq)

-- | Uncons for @['Deloop']@. Fails if 'head' of the list is not 'Normal'.
unconsD :: [Deloop] -> Either Stringlike ([Deloop], Marked (Cooked Instruction))
unconsD []                            = Left "No more instructions left"
unconsD (Normal (a :<| S.Empty) : xs) = Right (           xs, a)
unconsD (Normal (a :<| x      ) : xs) = Right (Normal x : xs, a)
unconsD _                             = Left "Function starts on a loop"

-- | Unsnoc for @['Deloop']@. Fails if 'last' of the list is not 'Normal'.
unsnocD :: [Deloop] -> Either Stringlike ([Deloop], Marked (Cooked Instruction))
unsnocD                           []  = Left "Function ends on a loop"
unsnocD (Normal (S.Empty :|> z) : []) = Right ([]        , z)
unsnocD (Normal (x       :|> z) : []) = Right ([Normal x], z)
unsnocD (_                      : []) = Left "Function ends on a loop"
unsnocD (x                      : xs) = do (l, i) <- unsnocD xs
                                           Right (x : l, i)


-- | Recursively splices 'Inst' into @['Deloop']@.
deloop :: Inst -> [Deloop]
deloop s =
  case backjump s of
    Right (JumpCut bef j loop aft) ->
      deloop bef <> [Loop j loop, Normal aft]
    
    Left _ -> [Normal s]

-- | Splits @['Deloop']@ at specified point.
--
--   Fails if no such point exists or it's inside a 'Loop' section.
splitD :: DecompS -> Int -> [Deloop] -> Either Stringlike ([Deloop], [Deloop])
splitD ds jmpto deloopI
    -- If we're at the end and split points here, return end.
  | [] <- deloopI =
      if jmpto == edge ds
        then Right ([], [])
        else Left "Not a proper split"
    -- If split points to the start of the Loop section, return starting from it.
  | Loop j x@(e :@ _ :<| _) : xs <- deloopI, e >= jmpto, jmpto == edge ds =
      Right ([], Loop j x : xs)
    
  | Normal x : xs <- deloopI
    -- If address points to exists within a Normal block, split.
  , Just m <- S.findIndexL ((== jmpto) . getKey) x =
      let (loop, after) = S.splitAt m x
      in Right $ case loop of
                   S.Empty -> ([]           , Normal after : xs)
                   _       -> ([Normal loop], Normal after : xs)
    -- Otherwise go one block further and tack this one on after.
  | x : xs <- deloopI = do
      (ys, zs) <- splitD ds jmpto xs
      Right (x : ys, zs)



-- | Parses half a 'Comparable'.
halfCompareS :: Inst -> Either Stringlike (Inst, Raw.Comparison, TypePair, Marked Assignable)
halfCompareS s = do
  s' :|> _ :@ Cmp eq t <- Right s
  (s'', res) <- assignableS s'
  return (s'', eq, t, res)



-- | Parses a 'Comparable'.
compareS :: Inst -> Either Stringlike (Inst, Marked Expression)
compareS =
  \case
     s :|> _ :@ Cmp eq t             -> do
      (s', res) <- assignableS s
      (s'', n :@ res') <- assignableS s'
      return (s'', n :@ Compare t (Comparable eq (n :@ res') res))

     s :|> _ :@ Conv t@(_, Boolean_) -> compSingleC s t Boolean

     s :|> _ :@ Conv t@(_, Boolean_)
       :|> _ :@ Not Boolean_         -> compSingleC s t (:!)

     s :|> _ :@ Not Boolean_         -> do
       (s', n :@ Compare t f) <- compareS s
       Right (s', n :@ Compare t ((:!) $ n :@ Compared t f))

     _ -> Left "Not a compare statement"
  where
    compSingleC s t f = do
      (s', n :@ res) <- assignableS s
      return (s', n :@ Compare t (f $ n :@ res))



-- | Parses a 'Comparison' going left to right.
comparisonC
  :: DecompS
  -> Maybe (Inst, Marked Comparison) -- ^ A previous comparison to tie into if such exists.
  -> Inst
  -> Either Stringlike (Inst, Marked Comparison)
comparisonC ds previous s =
  asum
    [ -- Either consume a single conditional
      do (S.Empty, res) <- segment previous ds s
         Right (S.Empty, res)
      -- Or consume multiple in a row
    , do (s', res) <- segment previous ds s
         comparisonC ds (Just (s', res)) s'
    , -- Or parse a single comparison
      case previous of
        Nothing   -> single s
        Just comp -> Right comp
    ]
  where
    -- Parses a jump to 0 or 1 preceded by 'B' 2.
    -- Previous stacks of comparisons will point to said 0s or 1s to simulate the behavior of
    -- your usual (&&) and (||) cascades.
    segment
      :: Maybe (Inst, Marked Comparison)
      -> DecompS
      -> Inst
      -> Either Stringlike (Inst, Marked Comparison)
    segment previousI dsI sI = do
      JumpCut bef j (loop :|> _ :@ B 2) aft <- jump dsI sI
      let consume n b =
            case previousI of
              -- If the before-jump part of comparison was supplied, use it
              Just (_, comp) -> do
                (:) comp <$> comparisonI j (withEdge b dsI) loop
              -- Otherwise parse and combine comparisons before and inside the loop
              Nothing ->
                (<>)
                  <$> comparisonI j (withEdge n dsI) bef
                  <*> comparisonI j (withEdge b dsI) loop

      case (j, aft) of
        (Jt True (n :@ b), _ :@ PushCst (PushInt16 1) :<| after) -> do
          consumed <- consume n b
          Right (after, n :@ (:||) consumed)
        -- I have no idea why "not 0" is a possible value, but it is.
        (Jf True (n :@ b), _ :@ PushCst (PushInt16 0) :<| _ :@ Not Boolean_ :<| after) -> do
          consumed <- consume n b
          Right (after, n :@ (:||) consumed)
        (Jf True (n :@ b), _ :@ PushCst (PushInt16 0) :<| after) -> do
          consumed <- consume n b
          Right (after, n :@ (:&&) consumed)
        _ -> Left "Not a comparison segment"

    -- Internal counterpart of 'comparisonC'
    comparisonI
      :: Marked (Cooked Instruction) -> DecompS -> Inst -> Either Stringlike [Marked Comparison]
    comparisonI j dsI sI =
      asum
        [ do (S.Empty, res) <- single sI
             Right [res]
        
        , multiple j dsI sI

        , do (S.Empty, res) <- segment Nothing dsI sI
             Right [res]

        , do (s', res) <- segment Nothing ds sI
             (:[]) . snd <$> comparisonC ds (Just (s', res)) s'
        ]

    -- Parses blocks of comparisons pointing to the same 0 or 1. While this looks
    -- binary it is recursively locked with 'comparisonI' so it will parse arrays.
    multiple
      :: Marked (Cooked Instruction) -> DecompS -> Inst -> Either Stringlike [Marked Comparison]
    multiple i dsI sI = do
      JumpCut bef j loop S.Empty <- jump dsI sI
      case () of
        () | Jt _ _ <- i, Jt _ (n :@ b) <- j, edge dsI == b ->
               (<>)
                 <$> comparisonI i (withEdge n dsI) bef
                 <*> comparisonI i dsI              loop

           | Jf _ _ <- i, Jf _ (n :@ b) <- j, edge dsI == b ->
               (<>)
                 <$> comparisonI i (withEdge n dsI) bef
                 <*> comparisonI i dsI              loop

           | otherwise -> Left "Not a proper comparison multiblock"

    -- | Simple single comparison match.
    single :: Inst -> Either Stringlike (Inst, Marked Comparison)
    single sI =
      let (bef, s') = S.breakl (isJust . narrow . unkey) sI
      in do
           n :@ Compare t c :<| S.Empty <- Right $ simpleX ds bef
           Right (s', n :@ Comparison t c)


-- | Parses an 'If'.
ifC :: DecompS -> Inst -> Inst -> [Deloop] -> Either Stringlike Expr
ifC ds pre x xs = do
  -- Find the first jump forward
  (bef, jmp :<| aft) <- Right $ S.breakl (isJust . narrow . unkey) x
  -- Ensure there's a comparison before it
  (bef', n :@ Compare t c) <- compareS bef
  -- Read the rest of the comparison
  (Jf _ (_ :@ thnJ) :<| aft', comp) <- comparisonC ds (Just (jmp :<| aft, n :@ Comparison t c)) $ jmp :<| aft
  -- Split off the @then@ statement
  (thn, post) <- splitD (withEdge thnJ ds) thnJ (Normal aft' : xs)
  asum
    [ do 
         -- If then ends on a) jump forward
         (thn', J True (ifPos :@ elsJ)) <- unsnocD thn
         -- Split the else
         (els, post') <- splitD (withEdge elsJ ds) elsJ post
         -- Convert everything into expressions and combine the results
         thn''  <- exprX (withEdge thnJ ds) S.Empty thn'
         els'   <- exprX (withEdge elsJ ds) S.Empty els
         post'' <- exprX ds S.Empty post'
         Right $ mconcat
                   [ simpleX ds pre
                   , simpleX ds bef'
                   , S.singleton $ ifPos :@ If comp thn'' (Just els')
                   , post''
                   ]
    , do
         -- If it doesn't, convert and combine just the @then@
         thn'  <- exprX (withEdge thnJ ds) S.Empty thn
         post' <- exprX ds S.Empty post
         Right $ mconcat
                   [ simpleX ds pre
                   , simpleX ds bef'
                   , S.singleton $ n :@ If comp thn' Nothing
                   , post'
                   ]
    ]



-- | Parses a 'Switch'.
switchC :: DecompS -> Inst -> Inst -> [Deloop] -> Either Stringlike Expr
switchC ds pre x xs =
  asum
    [ do
         -- Find the first jump, assume it jumps to the code block of the first case.
         JumpCut bef (Jt True (_ :@ c)) (els :|> J _ (_ :@ caseEnd)) aft <- jump ds x
         -- Parse the switch expression itself and the first case that come before the jump
         (bef'', firstCase, switch@(switchStart :@ _)) <-
           asum
             [ do (bef' :|> _ :@ Dup Variable_, _, _, firstCase) <- halfCompareS bef
                  (bef'', switch) <- assignableS bef'
                  Right (bef'', firstCase, switch)
                  -- The switch expression might be a Boolean, then Dup is over Boolean_.
             , do (bef' :|> _ :@ Dup Boolean_, _, _, firstCase) <- halfCompareS bef
                  (bef'', n :@ Compare t c') <- compareS bef'
                  Right (bef'', firstCase, n :@ Compared t c')
             ]
         -- Split the remaining part into case expressions and everything outside.
         (ans, post) <- splitD ds caseEnd (Normal aft : xs)
         -- Separate the conditionals.
         (vals, defval) <- parseConditions els
         -- Split up and convert case expression blocks and check whether nothing is left.
         (cases, defcase) <-
           parseCasesWithDef (withEdge caseEnd ds) caseEnd ans (vals <> [c :@ Case firstCase]) defval
         -- Remove the trailing 'Popz' 'Variable_'/'Boolean_'.
         (post', _ :@ Popz _) <- unconsD post
         -- Convert everything into expressions and combine the results.
         post'' <- exprX ds S.Empty post'
         Right $ mconcat
                   [ simpleX ds pre
                   , simpleX ds bef''
                   , S.singleton $ switchStart :@ Switch switch cases defcase
                   , post''
                   ]
      -- This is a fallback for a switch with no cases and a default.
      -- Since there is just a default, there is no 'Dup', the structure looks like
      -- @'Assignable' > 'B' 2 > 'B' ? > loop > 'B' 1 > 'Popz' 'Variable_'@.
    , do
         -- Find the 'B' 2.
         JumpCut bef (_ :@ B 2) (caseJ@(_ :@ B _) :<| S.Empty) aft <- jump ds x
         -- Get the 'Assignable' atop the switch.
         (bef', as@(q :@ _)) <- assignableS bef
         -- Get the loop, remove 'B 1' and 'Popz' 'Variable_' at the end of it.
         JumpCut S.Empty _ (loop :|> _ :@ B 1) (_ :@ Popz Variable_ :<| post) <- jump ds $ caseJ :<| aft
         -- Convert everything into expressions and combine the results.
         post' <- exprX ds S.Empty $ [Normal post]
         other <- exprX ds S.Empty xs
         Right $ mconcat
                   [ simpleX ds bef'
                   , S.singleton $ q :@ Switch as [] (Just $ DefaultCase (simpleX ds loop) True)
                   , post'
                   , other
                   ]
    ]
  where
    -- Parses conditions from the back. It's easier to do from the back since
    -- there is an offchance certain conditions are complex and hold jumps in them
    -- (I however never checked if that ever happens).
    parseConditions
      :: Inst
      -> Either
           Stringlike
           ( [Marked (Expr -> Bool -> Case)]
           , Maybe (Marked (Expr -> Bool -> DefaultCase))
           )
    parseConditions =
      \case
         s :|> Jt _ (_ :@ b) -> do (s' :|> _ :@ Dup _, _, _, sub) <- halfCompareS s
                                   (cases, defcase) <- parseConditions s'
                                   Right $ ((b :@ Case sub) : cases, defcase)
         -- Default case is unconditional.
         s :|> J  _ (_ :@ b) -> do (cases, _) <- parseConditions s
                                   Right (cases, Just $ b :@ DefaultCase)
         S.Empty             -> Right ([], Nothing)
         _ -> Left "Malformed switch conditions"

    -- | Same as 'parseCases' but accounting for the default case.
    parseCasesWithDef
      :: DecompS
      -> Int
      -> [Deloop]
      -> [Marked (Expr -> Bool -> Case)]
      -> Maybe (Marked (Expr -> Bool -> DefaultCase))
      -> Either Stringlike ([Case], Maybe DefaultCase)
    parseCasesWithDef dsI caseEnd deloopI cases mayDefCase
        -- When there's a default case
      | Just (n :@ toDefCase) <- mayDefCase = do
          -- Same block of code as 'parseCases'
          (bef, bit) <- splitD dsI n deloopI
          let (bit', q, hasBreak)
                | Right (bitI, qI :@ B 1) <- unsnocD bit = (bitI, qI, True )
                | otherwise                              = (bit , n , False)
          ([], casesRes) <- parseCases dsI caseEnd bef cases
          res' <- exprX (inCase caseEnd $ withEdge q dsI) S.Empty bit'
          -- But we store the result as the default case.
          -- We also reverse cases because we parsed them from the back to the front.
          Right (reverse casesRes, Just $ toDefCase res' hasBreak)

      | _ : _ <- cases = do
          ([], casesRes) <- parseCases dsI caseEnd deloopI cases
          Right (reverse $ casesRes, Nothing)

      | otherwise = Left "Malformed switch expression blocks"

    -- | Recursively breaks up expression blocks.
    parseCases
      :: DecompS
      -> Int
      -> [Deloop]
      -> [Marked (Expr -> Bool -> Case)]
      -> Either Stringlike ([Deloop], [Case])
    parseCases _   _       deloopI                  []   = Right (deloopI, [])
    parseCases dsI caseEnd deloopI ((n :@ toCase) : other) = do
      -- Cut off the end part we're parsing
      (bef, bit) <- splitD dsI n deloopI
      -- If it has an unconditional jump at the end, assume it's a break statement.
      let (bit', q, hasBreak)
            | Right (bitI, qI :@ B _) <- unsnocD bit = (bitI, qI, True )
            | otherwise                              = (bit , n , False)
      -- Parse every other case
      (rest, cases) <- parseCases dsI caseEnd bef other
      -- Parse and combine results
      res' <- exprX (inCase caseEnd $ withEdge q dsI) S.Empty bit'
      Right (rest, toCase res' hasBreak : cases)
 


-- | Common type for backward loop parsers in 'exprX'.
type Backloop a = DecompS
               -> Inst                        -- ^ Instructions before the loop
               -> Marked (Cooked Instruction) -- ^ Jump instruction itself
               -> Inst                        -- ^ Instructions inside the loop
               -> [Deloop]                    -- ^ Everything after the loop
               -> Either Stringlike a



-- | Parses a 'Repeat' statement.
repeatC :: Backloop Expr
repeatC ds bef j loop xs
  | Jt False _ <- j = do
      -- Break off the counter value push with the first comparison before the loop
      bef' :|> repeatStart :@ PushI16 n Int16_
           :|> _ :@ Dup Int32_
           :|> _ :@ PushCst (PushInt32 0)
           :|> _ :@ Cmp LTOrEqual (Int32_,Int32_)
           :|> _ :@ Bt _                          <- Right bef
      -- Break off the decrement with the check at the end of the loop
      loop' :|> _ :@ PushCst (PushInt32 1)
            :|> _ :@ Sub (Int32_,Int32_)
            :|> _ :@ Dup Int32_
            :|> _ :@ Conv (Int32_,Boolean_) <- Right loop
      -- Remove the 'Popz' after the loop
      Normal (repeatEnd :@ Popz Int32_ :<| rest) : rs <- Right xs
      -- Parse and combine the results
      loop'' <- finaleI (withEdge repeatEnd ds) loop'
      post <- exprX ds S.empty (Normal rest : rs)
      Right $ mconcat
                [ simpleX ds bef'
                , S.singleton $ repeatStart :@ Repeat n loop''
                , post
                ]

  | otherwise = Left "Not a repeat statement"

-- | Parses a 'For' statement.
forC :: Backloop Expr
forC ds bef j loop xs
  | J False (_ :@ loopStart) <- j = do
     -- Break off the 'Assignable' before the loop.
     (bef', forStart :@ a) <- assignmentS bef
     -- Break off the comparison at the start of the loop.
     (Jf _ (_ :@ loopEnd) :<| loop', c) <- comparisonC ds Nothing loop
     -- And the assignment at the end of it.
     (loop'', assignStart :@ k) <- assignmentS loop'
     -- Parse and combine the results.
     loop''' <- finaleI ( withEdge assignStart $ withLoop loopStart loopEnd ds) loop''
     post <- exprX ds S.empty xs
     Right $ mconcat
               [ simpleX ds bef'
               , S.singleton $ forStart :@ For a c k loop'''
               , post
               ]

  | otherwise = Left "Not a for statement"

-- | Parses a 'ForDec' statement.
--
--   This is a quirky variation of For that holds @i@ on the stack. Seems to only happen
--   in the @for (i = ...; i < 0; i--)@ case.
forDecC :: Backloop Expr
forDecC ds bef j loop xs
  | Jt False (n :@ loopStart) <- j = do
      -- Remove the 'LtOrEqual' 0 comparison.
      bef' :|> _ :@ Bt _ <- Right bef
      (bef'' :|> _ :@ Dup _, LTOrEqual, _, _ :@ Value (_ :@ Constant (ConstInt32 0))) <- halfCompareS bef'
      -- And break off 'Assignable' before that.
      (bef''', forStart :@ a) <- assignableS bef''
      -- Remove the decrement and the comparison to zero at the end of the loop.
      loop' :|> q :@ PushCst (PushInt32 1)
            :|> _ :@ Sub (_, _)
            :|> _ :@ Dup _
            :|> _ :@ Conv (_, Boolean_) <- Right loop
      -- And the 'Popz' after it.
      (xs', _ :@ Popz Int32_) <- unconsD xs
      -- Parse and combine the results.
      loop'' <- finaleI ( withEdge q $ withLoop loopStart (n + 4) ds) loop'
      post <- exprX ds S.empty xs'
      Right $ mconcat
                [ simpleX ds bef'''
                , S.singleton $ forStart :@ ForDec (q :@ a) loop''
                , post
                ]

  | otherwise = Left "Not a for (decrement variety) statement"

-- | Parses a 'While' statement.
whileC :: Backloop Expr
whileC ds bef j loop xs
  | J False (_ :@ loopStart) <- j = do
      -- Parse a comparison at the start of the loop.
      (Jf _ (_ :@ whileEnd) :<| loop', c) <- comparisonC ds Nothing loop
      -- Parse and combine the results.
      loop'' <- finaleI (withLoop loopStart whileEnd ds) loop'
      post <- exprX ds S.empty xs
      Right $ mconcat
                [ simpleX ds bef
                , S.singleton $ loopStart :@ While c loop''
                , post
                ]

  | otherwise = Left "Not a while statement"

-- | Parses a 'Do' statement.
--   
--   NOTE: The only 'Do' statement I could find in Risk of Rain 1 is in
--         3416/"gml_Script__string_rpos" and that one's just a single basic
--         'Comparable', so instead of making an untestable 'comparisonS' counterpart to
--         'comparisonC' I stuck with the simple comparison.
doC :: Backloop Expr
doC ds bef j loop xs
  | Jf False (_ :@ doStart) <- j = do
      -- Parse a comparison at the end of the loop.
      (loop', _ :@ Compare t c) <- compareS loop
      -- Parse and combine the results.
      loop'' <- finaleI ds loop'
      post <- exprX ds S.empty xs
      Right $ mconcat
                [ simpleX ds bef
                , S.singleton $ doStart :@ Do (doStart :@ Comparison t c) loop''
                , post
                ]

  | otherwise = Left "Not a do statement"

-- | Parses a 'With' statement.
withC :: Backloop Expr
withC ds bef j loop xs
  | Jenv (withEnd :@ _) <- j = do
      -- Consume the corresponding 'Assignable' before the loop.
      (bef', a@(withStart :@ _)) <- assignableS bef
      -- If there's 'B' 2 >> PopEnvAny right after the loop
      (xs', ds') <- asum
                      [ do (xs', _ :@ B 2) <- unconsD xs
                           (xs'', m :@ PopEnvAny) <- unconsD xs'
                           -- Specify there's a loop break.
                           Right (xs'', brokenWith m ds)
                        -- Otherwise just do nothing
                      , Right (xs, ds)
                      ]
      -- Remove redundant 'PushEnv' at the start of the loop
      _ :@ PushEnv _ :<| loop' <- Right loop
      -- Parse and combine the results.
      loop'' <- finaleI (withEdge withEnd $ inWith ds') loop'
      post <- exprX ds S.empty xs'
      Right $ mconcat
                [ simpleX ds bef'
                , S.singleton $ withStart :@ With a loop''
                , post
                ]

  | otherwise = Left "Not a with statement"



-- | Parses 'Expression's.
exprX :: DecompS -> Inst -> [Deloop] -> Either Stringlike Expr
exprX ds bef                   []  = Right $ simpleX ds bef
exprX ds bef (Normal S.Empty : []) = Right $ simpleX ds bef
exprX ds bef (Normal x       : xs) =
  asum
    [ ifC     ds bef x xs
    , switchC ds bef x xs
      -- If there are no 'If's or 'Switch'es
    , case xs of
                        -- we either tack it on as the @before the loop bit@,
        Loop _ _ : _ -> exprX ds x xs
                        -- or just parse it all the simple way.
        _            -> (simpleX ds x <>) <$> exprX ds bef xs

    ]
exprX ds bef (Loop j loop    : xs) =
  asum
    [ repeatC ds bef j loop xs
    , forC    ds bef j loop xs
    , forDecC ds bef j loop xs
    , whileC  ds bef j loop xs
    , doC     ds bef j loop xs
    , withC   ds bef j loop xs
    , do rest <- exprX ds S.empty xs
         Right $ mconcat
                   [ simpleX ds bef
                     -- There is no point in marking this properly since we won't print
                     -- this loop shell
                   , S.singleton $ 0 :@ UnparsedLoop (simpleX ds $ loop :|> j)
                   , rest
                   ]
    ]



-- | Parses a 'CodeFunction' and a set of 'Instruction's corresponding to it.
finaleX :: CodeFunction -> Inst -> Either Stringlike Expr
finaleX = finaleI . decompS

-- | Internal version of 'finaleX' that operates over parts of functions with a
--   supplied 'DecompS'.
finaleI :: DecompS -> Inst -> Either Stringlike Expr
finaleI ds = exprX ds S.empty . deloop



-- | Decompiles the 'Code' chunk of the 'Form' into readable 'CodeFunction's together
--   with matching 'Expr's.
--
--   'extort' should be used to extract the result in a lazy fashion; all the caveats of
--   'instructions' apply.
expressions :: Form -> Code -> Vari -> Func -> [(CodeFunction, Either [Char] Expr)]
expressions form cod var fun =
  flip fmap (instructions form cod var fun) $ \(cf, ei) ->
         (,) cf $ do
           i <- ei
           unwrapStringlike $ finaleX cf (S.fromList i)



-- | A version of 'expressions' that doesn't split the branches up so all of the jumps are left
--   'Unparsed'.
simpleExpressions :: Form -> Code -> Vari -> Func -> [(CodeFunction, Either [Char] Expr)]
simpleExpressions form cod var fun =
  flip fmap (instructions form cod var fun) $ \(cf, ei) ->
         (,) cf $ do
           i <- ei
           Right $ simpleX (decompS cf) (S.fromList i)



-- | Helper function that outputs identifiers of functions with unparsed elements in the
--   decompiled form.
unparsed :: [(CodeFunction, Expr)] -> [Int]
unparsed = catMaybes . fmap search . zip [0..]
  where
    search (n, (_, expr)) =
      if any matches expr
        then Just n
        else Nothing

    matches (_ :@ q) =
      case q of
        For _ _ _ ex      -> any matches ex
        Repeat _ ex       -> any matches ex
        If _ ex Nothing   -> any matches ex
        If _ ex mayDex    -> any matches ex || any (any matches) mayDex
        While _ ex        -> any matches ex
        Do _ ex           -> any matches ex
        Switch _ ex defex -> any (\(Case _ e _) -> any matches e) ex
                              || any (\(Case _ e _) -> any matches e) defex
        With _ ex         -> any matches ex
        Unparsed _        -> True
        UnparsedLoop _    -> True
        _                 -> False

-- | Prints an 'Expr'ession to stdout.
pprint :: (CodeFunction, Expr) -> IO ()
pprint (el, expr) =
  BSL.putStr .
    toLazyByteString .
      fold .
        flip fmap expr $ \ex ->
          cprint (cprinter el) ex
