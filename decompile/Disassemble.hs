{-|
    This module disassembles the CODE chunk into raw bytecode.

    Bytecode is version 16, similar to the one Undertale used.
 -}

{-# LANGUAGE BangPatterns
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , NoFieldSelectors
           , OverloadedRecordDot
           , RecordWildCards #-}

module Disassemble
  ( convertInstance

  , Strings
  , makeStrings

  , Functions
  , makeFunctions

  , Variables
  , makeVariables

  , Arguments
  , makeArguments

  , Error (..)
  , Reason (..)
  , BadPushable (..)

  , Int23 (..)
  , Word23 (..)
  , DataType (..)
  , Instance (..)
  , Variable (..)
  , Comparison (..)
  , Push (..)
  , At (..)
  , Instruction (..)
  , Assembly (..)
  , disassemble
  ) where

import           Control.Monad.ST
import           Data.Bits hiding (Xor (..), And (..))
import           Data.ByteString.Short (ShortByteString)
import           Data.Int
import           Data.Patricia.Word.Strict (Patricia)
import qualified Data.Patricia.Word.Strict as Patricia
import           Data.RadixTree.Word8.Strict (RadixTree)
import qualified Data.RadixTree.Word8.Strict as Radix
import           Data.Primitive.PrimArray
import           Data.Primitive.SmallArray
import           Data.Word
import           GameMaker.RiskOfRain.Decoder hiding (Error (..))
import           Parser.Lathe
import           Parser.Lathe.Binary



data Error = NoFunctionName !StrgRef
           | NoArguments !ShortByteString
           | Error !ShortByteString !ByteOffset !Reason
             deriving Show

data Reason = BadDataType !Word8
            | BadInstance !Int16
            | BadVariableType !Word8
            | BadComparison !Word8
            | ObjectMiss !Int16
            | VariableMiss !Word32
            | FunctionMiss !Word32
            | StringMiss !StrgRef
            | StringIndexMiss !Word32
            | BadPushable !BadPushable
            | IllegalOp !Word8 !Word8 !Word8 !Word8
            | AbruptEnd
              deriving Show

data BadPushable = BadPushFloat
                 | BadPushInt64
                 | BadPushBoolean
                 | BadPushInstance
                   deriving Show



int16le :: Word8 -> Word8 -> Int16
int16le a b = (fromIntegral b `unsafeShiftL` 8)
            + fromIntegral a



-- | While every single place refers to these as 24-bit integers, the 24th
--   bit is __always__ zero and the 23rd bit serves as the sign.
--   @(-1)@ is thus @FF FF 7F@ (little-endian format).
newtype Int23 = Int23 Int32
                deriving newtype Show

int23le :: Word8 -> Word8 -> Word8 -> Int23
int23le a b c =
  Int23 . (`unsafeShiftR` 9) $ fromIntegral c `unsafeShiftL` 25
                             + fromIntegral b `unsafeShiftL` 17
                             + fromIntegral a `unsafeShiftL` 9



-- | Assumed to have the same restrictions as 'Int23', though this is impossible
--   to confirm based on the file alone.
newtype Word23 = Word23 Word32
                 deriving newtype Show

word23le :: Word8 -> Word8 -> Word8 -> Word23
word23le a b c =
  Word23 $ fromIntegral c `shiftL` 16
         + fromIntegral b `shiftL` 8
         + fromIntegral a



-- | GameMaker data types.
data DataType = Double
              | Float
              | Int32
              | Int64
              | Boolean
              | Variable
              | String
              | Instance
              | Int16
                deriving Show

dataType :: Word8 -> Parser Reason DataType
dataType w =
  case dataType' w of
    Right dt -> pure dt
    Left e   -> err e

dataType' :: Word8 -> Either Reason DataType
dataType' w =
  case w of
    0x0 -> Right Double
    0x1 -> Right Float
    0x2 -> Right Int32
    0x3 -> Right Int64
    0x4 -> Right Boolean
    0x5 -> Right Variable
    0x6 -> Right String
    0x7 -> Right Instance
    0xf -> Right Int16
    _   -> Left $ BadDataType w


typePair :: Word8 -> Parser Reason (DataType, DataType)
typePair w =
  case typePair' w of
    Right ab -> pure ab
    Left e   -> err e

typePair' :: Word8 -> Either Reason (DataType, DataType)
typePair' w = do
  l <- dataType' $ w .&. 0xF
  r <- dataType' $ w `unsafeShiftR` 4
  Right (l, r)



data Instance = Object !ShortByteString
              | Self
              | Other
              | All
              | Noone
              | Global
          -- \| Builtin
              | Local
                deriving (Eq, Show)

instance_ :: Strg -> Objt -> Word8 -> Word8 -> Parser Reason Instance
instance_ strgs objts a b =
  case convertInstance strgs objts (int16le a b) of
    Right i -> pure i
    Left e  -> err e

convertInstance :: Strg -> Objt -> Int16 -> Either Reason Instance
convertInstance strgs objts w =
  if w >= 0
    then if fromIntegral w < sizeofSmallArray objts.elements
           then let object = indexSmallArray objts.elements (fromIntegral w)
                in case lookupStrgRef strgs object.name of
                     Nothing   -> Left $ StringMiss object.name
                     Just name -> Right $ Object name

           else Left $ ObjectMiss w

    else case w of
           -1 -> Right Self
           -2 -> Right Other
           -3 -> Right All
           -4 -> Right Noone
           -5 -> Right Global
        -- -6 -> Right Builtin
           -7 -> Right Local
           _  -> Left $ BadInstance w



data Variable = Array
              | StackTop
              | Normal
          -- \| Unknown
                deriving Show

variable :: Word8 -> Parser Reason Variable
variable w =
  case variable' w of
    Right v -> pure v
    Left e  -> err e

variable' :: Word8 -> Either Reason Variable
variable' w = do
  case w of
    0x00 -> Right Array
    0x80 -> Right StackTop
    0xA0 -> Right Normal
 -- 0xE0 -> Right Unknown
    _    -> Left $ BadVariableType w

getReference :: Parser Reason (Word23, Variable)
getReference = do
  a <- word8 AbruptEnd
  b <- word8 AbruptEnd
  c <- word8 AbruptEnd
  d <- word8 AbruptEnd

  v <- variable d
  pure (word23le a b c, v)



data Comparison = Lt
                | Le
                | Eq
                | Ne
                | Ge
                | Gt
                  deriving Show

comparison :: Word8 -> Parser Reason Comparison
comparison w =
  case comparison' w of
    Right c -> pure c
    Left e  -> err e

comparison' :: Word8 -> Either Reason Comparison
comparison' w = do
  case w of
    1 -> Right Lt
    2 -> Right Le
    3 -> Right Eq
    4 -> Right Ne
    5 -> Right Ge
    6 -> Right Gt
    _ -> Left $ BadComparison w



newtype Strings = Strings (SmallArray ShortByteString)

makeStrings :: Strg -> Strings
makeStrings strgs =
  Strings $
    smallArrayFromListN (Patricia.size strgs.strings) $
      Patricia.foldr (\a -> (:) a) [] strgs.strings

findString :: Strings -> Word32 -> Maybe ShortByteString
findString (Strings elements) pos =
  let pos_ = fromIntegral pos
  in if pos_ < 0 || pos_ >= sizeofSmallArray elements
       then Nothing
       else Just $! indexSmallArray elements pos_



data VariRef = VariRef !Int32 !ShortByteString
               deriving Show

newtype Variables = Variables (Patricia VariRef)
                    deriving Show

makeVariables :: Strg -> Vari -> Either StrgRef Variables
makeVariables strgs varis = go Patricia.empty 0
  where
    go !pat n
      | n >= sizeofSmallArray varis.elements = Right (Variables pat)
      | otherwise                            = do
          let el = indexSmallArray varis.elements n
          case lookupStrgRef strgs el.name of
            Nothing     -> Left el.name
            Just string ->
              let !ref = VariRef el.occurrences string

              in go (Patricia.insert (fromIntegral el.address) ref pat) (n + 1)

findVariable :: Word32 -> Word23 -> Variables -> Parser Reason (ShortByteString, Variables)
findVariable pos ref varis =
  case findVariable' pos ref varis of
    Right vs -> pure vs
    Left e   -> err e

findVariable' :: Word32 -> Word23 -> Variables -> Either Reason (ShortByteString, Variables)
findVariable' pos (Word23 ref) (Variables pat) =
  case Patricia.lookup (fromIntegral pos) pat of
    Just (VariRef occurrences name) ->
      Right
        ( name
        , Variables $
            let !pat' = Patricia.delete (fromIntegral pos) pat
            in if occurrences - 1 > 0
                 then let !ref' = VariRef (occurrences - 1) name
                      in Patricia.insert (fromIntegral pos + fromIntegral ref) ref' pat'
                 else pat'
        )

    Nothing -> Left $ VariableMiss pos



data FuncRef = FuncRef !Int32 !ShortByteString
               deriving Show

newtype Functions = Functions (Patricia FuncRef)
                    deriving Show

makeFunctions :: Strg -> Func -> Either StrgRef Functions
makeFunctions strgs funcs = go Patricia.empty 0
  where
    go !pat n
      | n >= sizeofSmallArray funcs.positions = Right (Functions pat)
      | otherwise                             = do
          let el = indexSmallArray funcs.positions n
          case lookupStrgRef strgs el.name of
            Nothing     -> Left el.name
            Just string ->
              let !ref = FuncRef el.occurrences string

              in go (Patricia.insert (fromIntegral el.address) ref pat) (n + 1)



findFunction :: Word32 -> Word23 -> Functions -> Parser Reason (ShortByteString, Functions)
findFunction pos ref funs =
  case findFunction' pos ref funs of
    Right fs -> pure fs
    Left e   -> err e

findFunction' :: Word32 -> Word23 -> Functions -> Either Reason (ShortByteString, Functions)
findFunction' pos (Word23 ref) (Functions pat) =
  case Patricia.lookup (fromIntegral pos) pat of
    Just (FuncRef occurrences name) ->
      Right
        ( name
        , Functions $
            let !pat' = Patricia.delete (fromIntegral pos) pat
            in if occurrences - 1 > 0
                 then let !ref' = FuncRef (occurrences - 1) name
                      in Patricia.insert (fromIntegral pos + fromIntegral ref) ref' pat'
                 else pat'
        )

    Nothing -> Left $ FunctionMiss pos



newtype Arguments = Arguments (RadixTree (SmallArray ShortByteString))

instance Show Arguments where
  showsPrec _ (Arguments rad) =
    showList $
      Radix.foldrWithKey (\k a -> (:) (Radix.buildShortByteString k, a)) [] rad

makeArguments :: Strg -> Func -> Either StrgRef Arguments
makeArguments strgs funcs = go (sizeofSmallArray funcs.elements - 1) Radix.empty
  where
    go :: Int -> RadixTree (SmallArray ShortByteString) -> Either StrgRef Arguments
    go n !rad
      | n < 0     = Right $ Arguments rad
      | otherwise = do
          let fun = indexSmallArray funcs.elements n
          name <- findRef fun.name fun.name
          args <- traversePrimSmall (findRef fun.name) fun.arguments
          let !rad' = Radix.insert (Radix.feedShortByteString name) args rad
          go (n - 1) rad'

    traversePrimSmall
      :: (StrgRef -> Either StrgRef ShortByteString)
      -> PrimArray StrgRef
      -> Either StrgRef (SmallArray ShortByteString)
    traversePrimSmall f arr =
      runST $ do
        mbrr <- newSmallArray (sizeofPrimArray arr) $
                  errorWithoutStackTrace
                    "GameMaker.RiskOfRain.Decompiler.Stage1.makeArguments: empty cell"

        let inner n
              | n < 0     = do
                  !brr <- unsafeFreezeSmallArray mbrr
                  pure $ Right brr

              | otherwise =
                  case f (indexPrimArray arr n) of
                    Left e  -> pure $ Left e
                    Right a -> do
                      writeSmallArray mbrr n a
                      inner (n - 1)

        inner (sizeofPrimArray arr - 1)

    findRef :: StrgRef -> StrgRef -> Either StrgRef ShortByteString
    findRef funcName refName =
      case lookupStrgRef strgs refName of
        Nothing     -> Left funcName
        Just string -> Right string



findArguments :: Arguments -> ShortByteString -> Maybe (SmallArray ShortByteString)
findArguments (Arguments rad) name =
  Radix.lookup (Radix.feedShortByteString name) rad



data Push = PushDouble !Double
          | PushInt32 !Int32
          | PushInt16 !Int16
          | PushStrg !ShortByteString
          | PushVari !Instance !ShortByteString !Variable
            deriving Show

getPush
  :: Word32 -> Strg -> Objt -> Variables -> Word8 -> Word8 -> Word8
  -> Parser Reason (Push, Variables)
getPush offset strgs objts varis a b c = do
  dt <- dataType c
  case dt of
    Double   -> do
      f <- doubleLE AbruptEnd
      let !r = PushDouble f
      pure (r, varis)

    Int16    -> do
      let !r = PushInt16 (int16le a b)
      pure (r, varis)

    Int32    -> do
      i <- int32LE AbruptEnd
      let !r = PushInt32 i
      pure (r, varis)

    String   -> do
      ref <- word32LE AbruptEnd
      case findString (makeStrings strgs) ref of
        Nothing     -> err $ StringIndexMiss ref
        Just string -> do
          let !r = PushStrg string
          pure (r, varis)

    Variable -> do
      i <- instance_ strgs objts a b
      (w, v) <- getReference
      (var, varis') <- findVariable offset w varis
      let !r = PushVari i var v
      pure (r, varis')

    _         ->
      err $
        BadPushable $
          case dt of
            Float    -> BadPushFloat
            Int64    -> BadPushInt64
            Boolean  -> BadPushBoolean
            Instance -> BadPushInstance



data Instruction = Conv !DataType !DataType
                 | Mul !DataType !DataType
                 | Div !DataType !DataType
                 | Rem !DataType !DataType
                 | Mod !DataType !DataType
                 | Add !DataType !DataType
                 | Sub !DataType !DataType
                 | And !DataType !DataType
                 | Or !DataType !DataType
                 | Xor !DataType !DataType
                 | Neg !DataType
                 | Not !DataType
                 | Shl !DataType !DataType
                 | Shr !DataType !DataType
                 | Cmp !Comparison !DataType !DataType
                 | Pop !DataType !DataType !Instance !ShortByteString !Variable
                 | Dup !Int16 !DataType
                 | Ret !DataType
                 | Exit !DataType
                 | Popz !DataType
                 | B !Int23
                 | Bt !Int23
                 | Bf !Int23
                 | PushEnv !Int23
                 | PopEnv !Int23
                 | PopEnvAny
                 | PushCst !Push
                 | PushLoc !Push
                 | PushGlb !Push
                 | PushVar !Instance !DataType !ShortByteString !Variable
                 | PushI16 !Int16 !DataType
                 | Call !Int16 !DataType !ShortByteString !Variable
                 | Break !Int16 !DataType
                   deriving Show



data At a = At !Word32 a
            deriving Show



data Assembly =
       Assembly
         { name         :: !ShortByteString
         , arguments    :: !(SmallArray ShortByteString)
         , offset       :: !Word32
         , size         :: !Word32
         , instructions :: [At Instruction]
         }
       deriving Show



disassemble
  :: Strg -> Objt -> Arguments -> Variables -> Functions -> CodeFunction
  -> Either Error (Assembly, Variables, Functions)
disassemble strgs objts args varis funcs this =
  case lookupStrgRef strgs this.name of
    Nothing   -> Left $ NoFunctionName this.name
    Just name ->
      case findArguments args name of
        Nothing        -> Left $ NoArguments name
        Just arguments ->
          let (Scrap point _ _, res) =
                           parse (instructions this.offset strgs objts varis funcs)
                             this.bytecode
          in case res of
               Left e                        -> Left $ Error name point e
               Right (insts, varis', funcs') -> 
                 let !r = Assembly
                            { offset       = fromIntegral $ this.offset
                            , size         = this.size
                            , instructions = insts
                            , ..
                            }

                 in Right (r, varis', funcs')



instructions
  :: ByteOffset -> Strg -> Objt -> Variables -> Functions
  -> Parser Reason ([At Instruction], Variables, Functions)
instructions global strgs objts = go
  where
    go varis funcs = do
      end <- atEnd
      if end
        then pure ([], varis, funcs)
        else do
          local <- bytesRead
          let !offset = fromIntegral (global + local)
          (i, varis', funcs') <- instruction offset strgs objts varis funcs
          (xs, varis'', funcs'') <- go varis' funcs'
          let !r = At offset i : xs
          pure (r, varis'', funcs'')



instruction
  :: Word32 -> Strg -> Objt -> Variables -> Functions
  -> Parser Reason (Instruction, Variables, Functions)
instruction offset strgs objts varis funcs = do
  a <- word8 AbruptEnd
  b <- word8 AbruptEnd
  c <- word8 AbruptEnd
  op <- word8 AbruptEnd

  let single
        :: (DataType -> Instruction)
        -> Parser Reason (Instruction, Variables, Functions)
      single f = do
        dt <- dataType c
        let !r = f dt
        pure (r, varis, funcs)

      double
        :: (DataType -> DataType -> Instruction)
        -> Parser Reason (Instruction, Variables, Functions)
      double f = do
        (dt1, dt2) <- typePair c
        let !r = f dt1 dt2
        pure (r, varis, funcs)

      goto
        :: (Int23 -> Instruction)
        -> Parser Reason (Instruction, Variables, Functions)
      goto f =
        let !r = f $ int23le a b c
        in pure (r, varis, funcs)

  case op of
    0x07 -> double Conv
    0x08 -> double Mul
    0x09 -> double Div
    0x0A -> double Rem
    0x0B -> double Mod
    0x0C -> double Add
    0x0D -> double Sub
    0x0E -> double And
    0x0F -> double Or
    0x10 -> double Xor
    0x11 -> single Neg
    0x12 -> single Not
    0x13 -> double Shl
    0x14 -> double Shr

    0x15 -> do
      cmp <- comparison b
      (x, y) <- typePair c
      let !r = Cmp cmp x y
      pure (r, varis, funcs)

    0x45 -> do
      i <- instance_ strgs objts a b
      (x, y) <- typePair c
      (w, v) <- getReference
      (var, varis') <- findVariable offset w varis
      let !r = Pop x y i var v
      pure (r, varis', funcs)

    0x84 -> do
      dt <- dataType c
      let !r = PushI16 (int16le a b) dt
      pure (r, varis, funcs)

    0x86 -> do
      dt <- dataType c
      let !r = Dup (int16le a b) dt
      pure (r, varis, funcs)

    0x9C -> single Ret
    0x9D -> single Exit
    0x9E -> single Popz

    0xB6 -> goto B
    0xB7 -> goto Bt
    0xB8 -> goto Bf
    0xBA -> goto PushEnv

    0xBB -> let Int23 i = int23le a b c
            in case i of
                 -0x100000 -> pure (PopEnvAny, varis, funcs)
                 _         -> do
                   let !r = PopEnv (Int23 i)
                   pure (r, varis, funcs)

    0xC0 -> do
      (push, varis') <- getPush offset strgs objts varis a b c
      pure (PushCst push, varis', funcs)

    0xC1 -> do
      (push, varis') <- getPush offset strgs objts varis a b c
      pure (PushLoc push, varis', funcs)

    0xC2 -> do
      (push, varis') <- getPush offset strgs objts varis a b c
      pure (PushGlb push, varis', funcs)

    0xC3 -> do
      i <- instance_ strgs objts a b
      (w, v) <- getReference
      (var, varis') <- findVariable offset w varis
      dt <- dataType c
      let !r = PushVar i dt var v
      pure (r, varis', funcs)

    0xD9 -> do
      (w, v) <- getReference
      (fun, funcs') <- findFunction offset w funcs
      dt <- dataType c
      let !r = Call (int16le a b) dt fun v
      pure (r, varis, funcs')

    0xFF -> do
      dt <- dataType c
      let !r = Break (int16le a b) dt
      pure (r, varis, funcs)

    _    -> err $ IllegalOp a b c op
