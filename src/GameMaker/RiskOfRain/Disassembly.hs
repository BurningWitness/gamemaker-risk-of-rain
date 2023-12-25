{-|
    This module disassembles the CODE chunk into raw bytecode.

    Bytecode is version 16, similar to the one Undertale used.
 -}

{-# LANGUAGE BangPatterns
           , DataKinds
           , DerivingStrategies
           , DuplicateRecordFields
           , FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , NoFieldSelectors
           , OverloadedRecordDot
           , TemplateHaskell
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

module GameMaker.RiskOfRain.Disassembly
  ( Int23 (..)
  , DataType (..)
  , Instance (..)
  , convertInstance
  , Variable (..)
  , Comparison (..)
  , Push (..)
  , At (..)
  , Instruction (..)

  , Strings
  , makeStrings

  , Objects
  , makeObjects
  , findObject

  , Variables
  , makeVariables

  , Functions
  , makeFunctions

  , Arguments
  , makeArguments

  , Assembly (..)
  , disassemble
  ) where

import           GameMaker.RiskOfRain.Unpacking

import           Data.Binary.Get
import           Data.Bits hiding (Xor (..), And (..))
import qualified Data.ByteString.Char8 as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString, fromShort)
import           Data.Char (chr)
import           Data.Int
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Primitive.Array (Array)
import           Data.Primitive.Array hiding (Array)
import           Data.Word
import           Optics.TH



hexa :: Word8 -> ShowS
hexa w =
  let (q, r) = quotRem w 16

      c x = chr $ fromIntegral x + if x < 10
                                     then 48
                                     else 55
  in (:) (c q) . (:) (c r)

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

dataType :: Word8 -> Get DataType
dataType w =
  case dataType' w of
    Right dt -> pure dt
    Left err -> fail err

dataType' :: Word8 -> Either String DataType
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
    _   -> Left $ "Not a datatype identifier: " <> show w


typePair :: Word8 -> Get (DataType, DataType)
typePair w =
  case typePair' w of
    Right ab -> pure ab
    Left err -> fail err

typePair' :: Word8 -> Either String (DataType, DataType)
typePair' w = do
  l <- dataType' $ w .&. 0xF
  r <- dataType' $ w `unsafeShiftR` 4
  Right (l, r)



data Instance = Object ShortByteString
              | Self
              | Other
              | All
              | Noone
              | Global
          -- \| Builtin
              | Local
                deriving Show

instance_ :: Objects -> Word8 -> Word8 -> Get Instance
instance_ objts a b =
  case convertInstance objts (int16le a b) of
    Right i  -> pure i
    Left err -> fail err

convertInstance :: Objects -> Int16 -> Either String Instance
convertInstance objts w =
  if w >= 0
    then Object <$> findObject w objts
    else case w of
           -1 -> Right Self
           -2 -> Right Other
           -3 -> Right All
           -4 -> Right Noone
           -5 -> Right Global
        -- -6 -> Right Builtin
           -7 -> Right Local
           _  -> Left $ "Not an instance identifier: " <> show w



data Variable = Array
              | StackTop
              | Normal
          -- \| Unknown
                deriving Show

variable :: Word8 -> Get Variable
variable w =
  case variable' w of
    Right v  -> pure v
    Left err -> fail err

variable' :: Word8 -> Either String Variable
variable' w = do
  case w of
    0x00 -> Right Array
    0x80 -> Right StackTop
    0xA0 -> Right Normal
 -- 0xE0 -> Right Unknown
    _    -> Left $ "Not a variable type: " <> show w

getReference :: Get (Word23, Variable)
getReference = do
  a <- getWord8
  b <- getWord8
  c <- getWord8
  d <- getWord8

  v <- variable d
  pure (word23le a b c, v)



data Comparison = Lt
                | Le
                | Eq
                | Ne
                | Ge
                | Gt
                  deriving Show

comparison :: Word8 -> Get Comparison
comparison w =
  case comparison' w of
    Right c  -> pure c
    Left err -> fail err

comparison' :: Word8 -> Either String Comparison
comparison' w = do
  case w of
    1 -> Right Lt
    2 -> Right Le
    3 -> Right Eq
    4 -> Right Ne
    5 -> Right Ge
    6 -> Right Gt
    _ -> Left $ "Not a comparison: " <> show w



fold' :: (a -> b -> b) -> b -> Stream a Result r -> Result (b, r)
fold' f = go
  where
    go z s =
      case s of
        Yield a s' -> let z' = f a z
                      in z' `seq` go z' s'

        Effect m   -> go z =<< m

        End r      -> Success (z, r)



data Strings = Strings {-# UNPACK #-} !Int {-# UNPACK #-} !(Array ShortByteString)

makeStrings :: Strg -> Result Strings
makeStrings (Strg count elements) =
  Strings (fromIntegral count) . arrayFromListN (fromIntegral count) . fst <$>
    fold' (\s -> (<> [s])) [] elements

findString :: Word32 -> Strings -> Get ShortByteString
findString pos strs =
  case findString' pos strs of
    Right bs -> pure bs
    Left err -> fail err

findString' :: Word32 -> Strings -> Either String ShortByteString
findString' pos (Strings count elements) =
  let pos_ = fromIntegral pos
  in if pos_ < 0 || pos_ >= count
       then Left . showString "Out of bounds string lookup ("
                     . shows pos_ $ showChar ')' []

       else Right $ indexArray elements pos_



data Objects = Objects {-# UNPACK #-} !Int {-# UNPACK #-} !(Array ShortByteString)

makeObjects :: Objt -> Result Objects
makeObjects (Objt count elements) =
  Objects (fromIntegral count) . arrayFromListN (fromIntegral count) . fst <$>
    fold' (\o -> (<> [o.name])) [] elements

findObject :: Int16 -> Objects -> Either String ShortByteString
findObject n (Objects count elements) =
  let n_ = fromIntegral n
  in if n_ < 0 || n_ >= count
       then Left . showString "Out of bounds object name lookup ("
                     . shows n_ $ showChar ')' []

       else Right $ indexArray elements n_



data VariRef = VariRef {-# UNPACK #-} !Int32 !ShortByteString
               deriving Show

newtype Variables = Variables (IntMap VariRef)
                    deriving Show

makeVariables :: Vari -> Result Variables
makeVariables (Vari _ _ _ elements) =
  let f element acc =
        let address_ = fromIntegral $ element.address
        in if address_ > 0
             then IntMap.insert address_ (VariRef (element.occurrences) (element.name))
                    acc
             else acc

  in (\ ~(m, _) -> Variables m) <$> fold' f IntMap.empty elements

findVariable :: Int -> Word23 -> Variables -> Get (ShortByteString, Variables)
findVariable pos ref varis =
  case findVariable' pos ref varis of
    Right vs -> pure vs
    Left err -> fail err

findVariable' :: Int -> Word23 -> Variables -> Either String (ShortByteString, Variables)
findVariable' pos (Word23 ref) (Variables varis) =
  case IntMap.lookup (fromIntegral pos) varis of
    Just (VariRef occurrences name) ->
      Right
        ( name
        , Variables $
            let varis' = IntMap.delete (fromIntegral pos) varis
            in if occurrences - 1 > 0
                 then IntMap.insert (fromIntegral pos + fromIntegral ref)
                                    (VariRef (occurrences - 1) name)
                                    varis'
                 else varis'
        )

    Nothing -> Left $ "Could not find variable at position " <> show pos



data FuncRef = FuncRef {-# UNPACK #-} !Int32 !ShortByteString
               deriving Show

newtype Functions = Functions (IntMap FuncRef)
                    deriving Show

makeFunctions :: Func -> Result (Functions, Func2)
makeFunctions (Func _count positions) =
  let f element acc =
        let address_ = fromIntegral $ element.address
        in if address_ > 0
             then IntMap.insert address_ (FuncRef (element.occurrences) (element.name))
                    acc
             else acc

  in (\ ~(m, r) -> (Functions m, r)) <$> fold' f IntMap.empty positions

findFunction :: Int -> Word23 -> Functions -> Get (ShortByteString, Functions)
findFunction pos ref funs =
  case findFunction' pos ref funs of
    Right fs -> pure fs
    Left err -> fail err

findFunction' :: Int -> Word23 -> Functions -> Either String (ShortByteString, Functions)
findFunction' pos (Word23 ref) (Functions funcs) =
  case IntMap.lookup (fromIntegral pos) funcs of
    Just (FuncRef occurrences name) ->
      Right
        ( name
        , Functions $
            let funcs' = IntMap.delete (fromIntegral pos) funcs
            in if occurrences - 1 > 0
                 then IntMap.insert (fromIntegral pos + fromIntegral ref)
                                    (FuncRef (occurrences - 1) name)
                                    funcs'
                 else funcs'
        )

    Nothing -> Left $ "Could not find function at position " <> show pos



newtype Arguments = Arguments (Map ShortByteString [ShortByteString])
                    deriving Show

makeArguments :: Func2 -> Result Arguments
makeArguments (Func2 _count functions) =
  let seqList (x:xs) = x `seq` seqList xs
      seqList    []  = ()

      f function acc =
        seqList function.arguments `seq` Map.insert function.name function.arguments acc

  in (\ ~(m, _) -> Arguments m) <$> fold' f Map.empty functions

findArguments :: ShortByteString -> Arguments -> Either String [ShortByteString]
findArguments name (Arguments funcs) =
  case Map.lookup name funcs of
    Just args -> Right args
    Nothing   ->
      Left $ "Could not find arguments for function " <> Strict.unpack (fromShort name)



data Push = PushDouble Double
          | PushInt32 Int32
          | PushInt16 Int16
          | PushStrg ShortByteString
          | PushVari Instance ShortByteString Variable
            deriving Show

getPush :: Int -> Strings -> Objects -> Variables -> Word8 -> Word8 -> Word8 -> Get (Push, Variables)
getPush offset strgs objts varis a b c = do
  dt <- dataType c
  case dt of
    Double   -> do
      f <- getDoublele
      pure (PushDouble f, varis)

    Int16    -> pure (PushInt16 (int16le a b), varis)

    Int32    -> do
      i <- getInt32le
      pure (PushInt32 i, varis)

    String   -> do
      ref <- getWord32le
      string <- findString ref strgs
      pure (PushStrg string, varis)

    Variable -> do
      i <- instance_ objts a b
      bytes <- bytesRead
      ~(w, v) <- getReference
      ~(var, varis') <- findVariable (offset + fromIntegral bytes) w varis
      pure (PushVari i var v, varis')

    _         -> fail $ "Not a pushable datatype: " <> show dt



data Instruction = Conv DataType DataType
                 | Mul DataType DataType
                 | Div DataType DataType
                 | Rem DataType DataType
                 | Mod DataType DataType
                 | Add DataType DataType
                 | Sub DataType DataType
                 | And DataType DataType
                 | Or DataType DataType
                 | Xor DataType DataType
                 | Neg DataType
                 | Not DataType
                 | Shl DataType DataType
                 | Shr DataType DataType
                 | Cmp Comparison DataType DataType
                 | Pop DataType DataType Instance ShortByteString Variable
                 | Dup Int16 DataType
                 | Ret DataType
                 | Exit DataType
                 | Popz DataType
                 | B Int23
                 | Bt Int23
                 | Bf Int23
                 | PushEnv Int23
                 | PopEnv Int23
                 | PopEnvAny
                 | PushCst Push
                 | PushLoc Push
                 | PushGlb Push
                 | PushVar Instance DataType ShortByteString Variable
                 | PushI16 Int16 DataType
                 | Call Int16 DataType ShortByteString Variable
                 | Break Int16 DataType
                   deriving Show



data At a = At Word32 a
            deriving Show



data Assembly =
       Assembly
         { name         :: ShortByteString
         , arguments    :: [ShortByteString]
         , size         :: Word32
         , instructions :: [At Instruction]
         }
       deriving Show

disassemble
  :: Chunk Code -> Strings -> Objects -> Variables -> Functions -> Arguments -> Code -> Stream Assembly Result ()
disassemble (Chunk start_ _) strgs objts vars funs args (Code count functions) =
  go start vars funs functions
  where
    start :: Int
    start = fromIntegral start_ + 8 + fromIntegral count * 4

    go offset varis funcs s =
      case s of
        Yield fun s' ->
          case findArguments fun.name args of
            Right arguments ->
              let get = getInstructions offset strgs objts varis funcs
              in case runGetOrFail get (fun.bytecode) of
                   Right (_, _, ~(ins, varis', funcs')) ->
                     let len = Lazy.length (fun.bytecode)
                         off' = offset + fromIntegral len
                     in Yield (Assembly (fun.name) arguments (fromIntegral len) ins) $
                          go off' varis' funcs' s'

                   Left (_, o, err)  -> Effect $ Failure o err

            Left err        -> Effect $ Failure (fromIntegral offset) err

        Effect m     -> case m of
                          Success s'  -> go offset varis funcs s'
                          Failure o e -> Effect $ Failure o e

        End r        -> End r

getInstructions
  :: Int -> Strings -> Objects -> Variables -> Functions -> Get ([At Instruction], Variables, Functions)
getInstructions offset strgs objts = go
  where
    go varis funcs = do
      end <- isEmpty
      if end
        then pure ([], varis, funcs)
        else do
          bytes <- bytesRead
          ~(i, varis', funcs') <- getInstruction offset strgs objts varis funcs
          ~(xs, varis'', funcs'') <- go varis' funcs'
          pure (At (fromIntegral bytes) i : xs, varis'', funcs'')

getInstruction
  :: Int -> Strings -> Objects -> Variables -> Functions -> Get (Instruction, Variables, Functions)
getInstruction offset strgs objts varis funcs = do
  a <- getWord8
  b <- getWord8
  c <- getWord8
  op <- getWord8

  let single :: (DataType -> Instruction) -> Get (Instruction, Variables, Functions)
      single f = do
        dt <- dataType c
        pure (f dt, varis, funcs)

      double :: (DataType -> DataType -> Instruction) -> Get (Instruction, Variables, Functions)
      double f = do
        ~(dt1, dt2) <- typePair c
        pure (f dt1 dt2, varis, funcs)

      goto :: (Int23 -> Instruction) -> Get (Instruction, Variables, Functions)
      goto f = pure (f $ int23le a b c, varis, funcs)

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
      ~(x, y) <- typePair c
      pure (Cmp cmp x y, varis, funcs)

    0x45 -> do
      i <- instance_ objts a b
      ~(x, y) <- typePair c
      bytes <- bytesRead
      ~(w, v) <- getReference
      ~(var, varis') <- findVariable (offset + fromIntegral bytes) w varis
      pure (Pop x y i var v, varis', funcs)

    0x84 -> do
      dt <- dataType c
      pure (PushI16 (int16le a b) dt, varis, funcs)

    0x86 -> do
      dt <- dataType c
      pure (Dup (int16le a b) dt, varis, funcs)

    0x9C -> single Ret
    0x9D -> single Exit
    0x9E -> single Popz

    0xB6 -> goto B
    0xB7 -> goto Bt
    0xB8 -> goto Bf
    0xBA -> goto PushEnv

    0xBB -> pure $ let Int23 i = int23le a b c
                   in case i of
                        -0x100000 -> (PopEnvAny       , varis, funcs)
                        _         -> (PopEnv (Int23 i), varis, funcs)

    0xC0 -> do
      ~(push, varis') <- getPush offset strgs objts varis a b c
      pure (PushCst push, varis', funcs)

    0xC1 -> do
      ~(push, varis') <- getPush offset strgs objts varis a b c
      pure (PushLoc push, varis', funcs)

    0xC2 -> do
      ~(push, varis') <- getPush offset strgs objts varis a b c
      pure (PushGlb push, varis', funcs)

    0xC3 -> do
      i <- instance_ objts a b
      bytes <- bytesRead
      ~(w, v) <- getReference
      ~(var, varis') <- findVariable (offset + fromIntegral bytes) w varis
      dt <- dataType c
      pure (PushVar i dt var v, varis', funcs)

    0xD9 -> do
      bytes <- bytesRead
      ~(w, v) <- getReference
      ~(fun, funcs') <- findFunction (offset + fromIntegral bytes) w funcs
      dt <- dataType c
      pure (Call (int16le a b) dt fun v, varis, funcs')

    0xFF -> do
      dt <- dataType c
      pure (Break (int16le a b) dt, varis, funcs)

    _    -> fail . showString "Illegal operation " . hexa a . showChar ' '
                                                   . hexa b . showChar ' '
                                                   . hexa c . showChar ' '
                                                   $ hexa op []



makeFieldLabelsNoPrefix ''Assembly
