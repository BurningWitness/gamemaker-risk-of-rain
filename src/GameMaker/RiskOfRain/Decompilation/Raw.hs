{-# LANGUAGE BangPatterns
           , DeriveAnyClass
           , DeriveFunctor
           , DeriveGeneric
           , DerivingStrategies
           , FlexibleContexts
           , GeneralizedNewtypeDeriving
           , PatternSynonyms #-}

module GameMaker.RiskOfRain.Decompilation.Raw
  ( -- * Key
    Key (.., (:@))
  , getKey
  , unkey
  , -- * Extra types
    Int23 (..)
  , Word23 (..)
    -- * GameMaker types
  , DataType (..)
  , TypePair
  , Instance (..)
  , Variable (..)
  , Comparison (..)
  , Reference (..)
  , Push (..)
  , Instruction (..)
  , Raw
  , Cooked
  , Marked
    -- * Decompilation functions
  , rawInstructions
  , instructions
  , extort
  , totalFunctions
  ) where

import           GameMaker.RiskOfRain.Lens
import           GameMaker.RiskOfRain.Unpacking

import           Control.Applicative
import           Control.DeepSeq
import           Data.Bifunctor
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import           Data.Function (on)
import           Data.Int
import qualified Data.IntMap.Strict as IMS
import           Data.List (mapAccumL)
import           Data.Word
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           GHC.Generics (Generic)
import           Lens.Micro
import           Lens.Micro.Internal
import           Prelude



-- | A datatype describing a key-value pair, with 'Eq', 'Ord' and 'Functor' only
--   working over the value part.
data Key k i = Key k i
               deriving (Show, Functor)

instance Eq i => Eq (Key k i) where
  (==) = (==) `on` unkey

instance Ord i => Ord (Key k i) where
  compare = compare `on` unkey

instance (NFData k, NFData i) => NFData (Key k i) where
  rnf (Key k i) = rnf (k, i)

instance Bifunctor Key where
  bimap f g (Key k i) = Key (f k) (g i)



{-# COMPLETE (:@) #-}
infixr 7 :@
pattern (:@) :: k -> i -> Key k i
pattern (:@) k i = Key k i

getKey :: Key k i -> k
getKey (k :@ _) = k

unkey :: Key k i -> i
unkey (_ :@ i) = i



-- | A 23-bit signed integer.
--
--   Yes the Undertale community says they're 24-bit, but guess what, they're bound
--   from 0 to 0x7FFFFF (if reading unsigned) and the 24th bit is 0x800000.
newtype Int23 = Int23 { unInt23 :: Int32 }
                deriving newtype (Show, Eq, Ord, Enum, Num, Integral, Real, NFData)

-- | Read a 'Int23' in little-endian format. Consumes three bytes.
getInt23le :: Get Int23
getInt23le = do
  val1 <- getWord8
  val2 <- getWord8
  val3 <- getWord8
  -- Int24 can be converted to Int32 through sign extension by putting three bits
  -- on the top of Int32 and shifting it eight to the right, but as the 24th bit is
  -- always empty we need to shift one further in both directions.
  return . Int23 $ (`shiftR` 9) $ fromIntegral val3 `shiftL` 25
                                + fromIntegral val2 `shiftL` 17
                                + fromIntegral val1 `shiftL` 9



-- | A 23-bit unsigned integer.
--
--   It's only 23-bit for consistency with 'Int24', actual bitsize doesn't matter
--   since you'd need 0x7FFFFF (8388607) references to start using the 24th bit.
newtype Word23 = Word23 { unWord23 :: Word32 }
                 deriving newtype (Show, Eq, Ord, Enum, Num, Integral, Real, NFData)

-- | Read a 'Word23' in little-endian format. Consumes three bytes.
getWord23le :: Get Word23
getWord23le = do
  val1 <- getWord8
  val2 <- getWord8
  val3 <- getWord8
  return . Word23 $ fromIntegral val3 `shiftL` 16
                  + fromIntegral val2 `shiftL` 8
                  + fromIntegral val1



-- | GameMaker data types.
data DataType = Double_
              | Float_
              | Int32_
              | Int64_
              | Boolean_
              | Variable_
              | String_
              | Instance_
              | Int16_
              | DataTypeOther Int8
                deriving (Show, Eq, Ord)

instance NFData DataType where
  rnf (DataTypeOther a) = rnf a
  rnf !_                = ()

toDataType :: Word8 -> DataType
toDataType val =
  case val of
    0x0 -> Double_
    0x1 -> Float_
    0x2 -> Int32_
    0x3 -> Int64_
    0x4 -> Boolean_
    0x5 -> Variable_
    0x6 -> String_
    0x7 -> Instance_
    0xf -> Int16_
    _   -> DataTypeOther $ fromIntegral val

-- | Takes up 1 byte.
dataTypeI :: Get DataType
dataTypeI = toDataType <$> getWord8



-- | A pair of datatypes.
type TypePair = (DataType, DataType)

-- | Takes up 1 byte.
typePairI :: Get TypePair
typePairI = do
  val <- getWord8
  return (toDataType $ val .&. 0xF, toDataType $ val `shiftR` 4)



-- | Instance types. 'ObjectSpecific' ones store the object identifier;
--   'Self', 'Other', 'All' and 'Noone' are even used as keywords in code.
data Instance = ObjectSpecific Int16
              | StackTopOrGlobal
              | Self
              | Other
              | All
              | Noone
              | Global
              | InstanceUnknown
              | Local
                deriving (Show, Eq)

instance NFData Instance where
  rnf (ObjectSpecific a) = rnf a
  rnf !_                 = ()

-- | Takes up 2 bytes.
instanceI :: Get Instance
instanceI = do
  val <- getInt16le
  case () of
    () | val  >  0 -> return $ ObjectSpecific val
       | val ==  0 -> return StackTopOrGlobal
       | val == -1 -> return Self
       | val == -2 -> return Other
       | val == -3 -> return All
       | val == -4 -> return Noone
       | val == -5 -> return Global
       | val == -6 -> return InstanceUnknown
       | val == -7 -> return Local
       | otherwise -> fail $ "Not an instance identifier: " <> show val



-- | GameMaker variable types. 'VariableUnknown' never happens in Risk of Rain 1.
data Variable = Array
              | StackTop
              | Normal
              | VariableUnknown
                deriving (Show, Eq, Ord)

instance NFData Variable where
  rnf !_ = ()

-- | Takes up 1 byte.
variableI :: Get Variable
variableI = do
  val <- getWord8
  case val of
    0x00 -> return Array
    0x80 -> return StackTop
    0xA0 -> return Normal
    0xE0 -> return VariableUnknown
    _    -> fail $ "Not a variable type: " <> show val



data Comparison = LowerThan
                | LTOrEqual
                | Equal
                | Inequal
                | GTOrEqual
                | GreaterThan
                  deriving (Show, Eq)

instance NFData Comparison where
  rnf !_ = ()

-- | Takes up 1 byte.
comparisonI :: Get Comparison
comparisonI = do
  val <- getWord8
  case val of
    1 -> return LowerThan
    2 -> return LTOrEqual
    3 -> return Equal
    4 -> return Inequal
    5 -> return GTOrEqual
    6 -> return GreaterThan
    _ -> fail $ "Not a comparison: " <> show val



-- | References to variable names, these are always complementary to certain instructions.
data Reference r = Reference r Variable
                   deriving (Show, Eq, Ord)

instance NFData r => NFData (Reference r) where
  rnf (Reference r v) = rnf (r, v)

referenceI :: Get (Reference Word23)
referenceI =
  Reference
    <$> getWord23le
    <*> variableI



-- | Types of things different 'Instruction''s will push onto the stack.
--
--   Parametrized over the string identifier (later converted via the STRG chunk)
--   and variable identifier (later converted via the VARI chunk).
data Push s r = PushDouble Double
              | PushInt32 Int32
              | PushInt16 Int16
              | PushStrg s
              | PushVari Instance (Reference r)
                deriving (Show, Eq, Generic, NFData)

pushI :: Get (Push Word32 Word23)
pushI = do
  dtype <- lookAhead $ skip 2 >> dataTypeI
  case dtype of
    Double_   -> do
      _ <- skip 4
      PushDouble <$> getDoublele
    Int16_    -> do
      val <- getInt16le
      _ <- skip 2
      return $ PushInt16 val
    Int32_    -> do
      _ <- skip 4
      PushInt32 <$> getInt32le
    String_   -> do
      _ <- skip 4
      PushStrg <$> getWord32le
    Variable_ -> do
      inst <- instanceI
      _ <- skip 2
      PushVari inst <$> referenceI
    _ -> fail $ "Not a pushable datatype: " <> show dtype



-- | The instruction type.
--
--   Parametrized over the string identifier (in 'Push') and variable/function identifier
--   (later converted via VARI and FUNC (in case of 'Call') chunks).
data Instruction s r = Conv TypePair
                     | Mul TypePair
                     | Div TypePair
                     | Rem TypePair
                     | Mod TypePair
                     | Add TypePair
                     | Sub TypePair
                     | And TypePair
                     | Or TypePair
                     | Xor TypePair
                     | Neg DataType
                     | Not DataType
                     | Shl TypePair
                     | Shr TypePair
                     | Cmp Comparison TypePair
                     | Pop TypePair Instance (Reference r)
                     | Dup DataType
                     | Ret DataType
                     | Exit DataType
                     | Popz DataType
                     | B Int23
                     | Bt Int23
                     | Bf Int23
                     | PushEnv Int23
                     | PopEnv Int23
                     | PopEnvAny
                     | PushCst (Push s r)
                     | PushLoc (Push s r)
                     | PushGlb (Push s r)
                     | PushVar Instance DataType (Reference r)
                     | PushI16 Int16 DataType
                     | Call Int16 DataType (Reference r)
                     | Break Int16 DataType
                       deriving (Show, Eq, Generic, NFData)

type Raw f = f Word32 Word23

type Cooked f = f BS.ByteString BS.ByteString

type Marked = Key Int



getRawInstructions :: Int -> Int -> Get [Marked (Raw Instruction)]
getRawInstructions boff siz = do
  off <- fromIntegral <$> bytesRead
  loop (off + siz)
  where
    loop til = do
      off <- fromIntegral <$> bytesRead
      case compare off til of
        GT -> fail "Overstepped"
        EQ -> return []
        LT -> do
          op <- lookAhead $ skip 3 >> getWord8
          key <- case op of
                   0x07 -> doubleI Conv
                   0x08 -> doubleI Mul
                   0x09 -> doubleI Div
                   0x0A -> doubleI Rem
                   0x0B -> doubleI Mod
                   0x0C -> doubleI Add
                   0x0D -> doubleI Sub
                   0x0E -> doubleI And
                   0x0F -> doubleI Or
                   0x10 -> doubleI Xor
                   0x11 -> singleI Neg
                   0x12 -> singleI Not
                   0x13 -> doubleI Shl
                   0x14 -> doubleI Shr
                   0x15 -> skipOp $
                             Cmp
                               <$> do _padding <- skip 1
                                      comparisonI
                               <*> typePairI
                   0x45 -> Pop
                             <$> typePairI
                             <*> instanceI
                             <*> do _opCode <- skip 1
                                    referenceI
                   0x86 -> singleI Dup
                   0x9C -> singleI Ret
                   0x9D -> singleI Exit
                   0x9E -> singleI Popz
                   0xB6 -> gotoI B
                   0xB7 -> gotoI Bt
                   0xB8 -> gotoI Bf
                   0xBA -> gotoI PushEnv
                   0xBB -> skipOp $ do
                     val <- getInt23le
                     return $
                       case val of
                         -0x100000 -> PopEnvAny
                         _         -> PopEnv val
                   0xC0 -> PushCst <$> pushI
                   0xC1 -> PushLoc <$> pushI
                   0xC2 -> PushGlb <$> pushI
                   0xC3 -> PushVar
                             <$> instanceI
                             <*> dataTypeI
                             <*> do _opCode <- skip 1
                                    referenceI
                   0x84 -> skipOp $
                             PushI16
                               <$> getInt16le
                               <*> dataTypeI
                   0xD9 -> Call
                             <$> getInt16le
                             <*> dataTypeI
                             <*> do _opCode <- skip 1
                                    referenceI
                   0xFF -> skipOp $
                             Break
                               <$> getInt16le
                               <*> dataTypeI
                   _    -> do
                             val <- (,,,)
                                      <$> getWord8
                                      <*> getWord8
                                      <*> getWord8
                                      <*> getWord8
                             fail $ "Illegal operation: " <> show val
          (:) ((boff + off) :@ key) <$> loop til

    skipOp :: Get a -> Get a
    skipOp action = do
      res <- action
      _opCode <- skip 1
      return res

    singleI :: (DataType -> f) -> Get f
    singleI f =
      skipOp $
        f <$> do _padding <- skip 2
                 dataTypeI

    doubleI :: (TypePair -> f) -> Get f
    doubleI f =
      skipOp $
        f <$> do _padding <- skip 2
                 typePairI

    gotoI :: (Int23 -> f) -> Get f
    gotoI f =
      skipOp $
        f <$> getInt23le



coherentHead :: Vector CodeFunction -> (CodeFunction, Either String ())
coherentHead vec =
  let cf = vec Vec.! 0
  in ( cf
     , case vec Vec.!? 1 of
         Nothing   -> Right ()
         Just next
           | fromIntegral (cfSize cf) + cfOffset cf == cfOffset next -> Right ()
           | otherwise -> Left $ mconcat
                                   [ "Boundary mismatch (", show $ cfSize cf, " + "
                                   , show $ cfOffset cf, " /= ", show $ cfOffset next, ")"
                                   ]
     )



-- | Parses 'Code' into a set of instructions.
--
--   Fails if:
--
--     * Bytecode is not 0xF;
--
--     * Functions overlap or there are any spare instructions left.
rawInstructions
  :: Code
  -> [(CodeFunction, Either [Char] [Marked (Raw Instruction)])]
rawInstructions (Code baseoff (CodeOps bs) els) = row (Vec.length els) els bs 0
  where
    row n elems input off
      | n <= 0    = []
      | otherwise =
          let (cf, matching) = coherentHead elems
          in case matching of
               Left err -> [(cf, Left err)]
               Right () ->
                 case runGetOrFail (getRawInstructions (baseoff + off) . fromIntegral $ cf^.size) input of
                   Left  (_   , _   , err) -> [(cf, Left err)]
                   Right (rest, off', val) -> (cf, Right val) : row (n - 1) (Vec.tail elems) rest (off + fromIntegral off')


-- | Evaluates a list of 'CodeFunction's paired with 'Either' values
--   while lazily returning their names and positions.
--
--   Used to modify 'instructions' and "GameMaker.RiskOfRain.Decompilation.expressions" to behave
--   the same way "GameMaker.RiskOfRain.Unpacking.decodeForm" and 'rawInstructions' do.
extort :: [(CodeFunction, Either a c)] -> ([(Int, BS.ByteString)], Either a [(CodeFunction, c)])
extort [] = ([], Right [])
extort as = loop 1 as
  where
    loop n ((a, r):rest) =
      first ((:) (n, a^.name)) $
        case r of
          Right val -> second (fmap $ (:) (a, val)) $ loop (n + 1) rest
          Left err -> ([], Left err)

    loop _ [] = ([], Right [])



-- | Polymorphic helper function that dumps addresses and their occurrences into a 'BSL.Map'.
nameMap
  :: ( Each s s a a
     , HasAddress a b
     , Integral b
     , HasOccurences a c
     , HasName a BS.ByteString
     )
  => s -> IMS.IntMap (c, BS.ByteString)
nameMap container =
  IMS.fromList
    . fmap (\(n, v) -> (n - 16, v))
    . filter (\(n, _) -> n > 0)
    $ container^..each.to
        (\a -> (fromIntegral $ a^.address,(a^.occurences, a^.name)))



-- | Polymorphic helper function that attempts to cycle an occurrence of an address in a map.
cycleMap
  :: ( Ord a
     , Num a
     , Show a
     )
  => String
  -> IMS.IntMap (a, BS.ByteString)
  -> Int
  -> Reference Word23
  -> (IMS.IntMap (a, BS.ByteString), Either [Char] (Reference BS.ByteString))
cycleMap qual nmap pos (Reference ref val) =
  case IMS.lookup (fromIntegral pos) nmap of
    Nothing ->
      (nmap, Left $ "Could not find " <> qual <> " occurring at position " <> show pos)

    Just (occs, nam) ->
      ( nmap & IMS.delete pos . if occs > 1
                                  then IMS.insert (pos + fromIntegral ref) (occs - 1, nam)
                                  else id
      , Right $ Reference nam val
      )



-- | Helper type for coherence.
type TransformS = (IMS.IntMap (Int32, BS.ByteString), IMS.IntMap (Word32, BS.ByteString))

instruction
  :: Form
  -> TransformS
  -> [Marked (Raw Instruction)]
  -> Either [Char] (TransformS, [Marked (Cooked Instruction)])
instruction form transformS raw =
  traverse sequence $ mapAccumL (\s (a :@ b) -> fmap ((:@) a) <$> transform s (a :@ b)) transformS raw
  where
    transform
      :: TransformS
      -> Marked (Raw Instruction)
      -> (TransformS, Either [Char] (Cooked Instruction))
    transform s (n :@ i) =
      case i of
        Conv t        -> (s, Right $ Conv t)
        Mul t         -> (s, Right $ Mul t)
        Div t         -> (s, Right $ Div t)
        Rem t         -> (s, Right $ Rem t)
        Mod t         -> (s, Right $ Mod t)
        Add t         -> (s, Right $ Add t)
        Sub t         -> (s, Right $ Sub t)
        And t         -> (s, Right $ And t)
        Or t          -> (s, Right $ Or t)
        Xor t         -> (s, Right $ Xor t)
        Neg d         -> (s, Right $ Neg d)
        Not d         -> (s, Right $ Not d)
        Shl t         -> (s, Right $ Shl t)
        Shr t         -> (s, Right $ Shr t)
        Cmp c t       -> (s, Right $ Cmp c t)
        Pop t j r     -> let (s', r') = cookVari s n r
                         in (s', Pop t j <$> r')
        Dup d         -> (s, Right $ Dup d)
        Ret d         -> (s, Right $ Ret d)
        Exit d        -> (s, Right $ Exit d)
        Popz d        -> (s, Right $ Popz d)
        B g           -> (s, Right $ B g)
        Bt g          -> (s, Right $ Bt g)
        Bf g          -> (s, Right $ Bf g)
        PushEnv g     -> (s, Right $ PushEnv g)
        PopEnv g      -> (s, Right $ PopEnv g)
        PopEnvAny     -> (s, Right   PopEnvAny)
        PushCst p     -> let (s', r') = toPush s n p
                         in (s', PushCst <$> r')
        PushLoc p     -> let (s', r') = toPush s n p
                         in (s', PushLoc <$> r')
        PushGlb p     -> let (s', r') = toPush s n p
                         in (s', PushGlb <$> r')
        PushVar j d r -> let (s', r') = cookVari s n r
                         in (s', PushVar j d <$> r')
        PushI16 j d   -> (s, Right $ PushI16 j d)
        Call j d r    -> let (s', r') = cookFunc s n r
                         in (s', Call j d <$> r')
        Break j d     -> (s, Right $ Break j d)

    toPush
      :: TransformS
      -> Int
      -> Push Word32 Word23
      -> (TransformS, Either [Char] (Push BS.ByteString BS.ByteString))
    toPush s n p =
      case p of
        PushDouble d -> (s, Right $ PushDouble d)
        PushInt32 i  -> (s, Right $ PushInt32 i)
        PushInt16 i  -> (s, Right $ PushInt16 i)
        PushStrg t   -> (s, PushStrg <$> cookStrg t)
        PushVari i r -> let (s', r') = cookVari s n r
                        in (s', PushVari i <$> r')

    cookStrg :: Word32 -> Either [Char] BS.ByteString
    cookStrg s =
      case form^?strg.ix (fromIntegral s) of
        Nothing     -> Left $ "Could not find string under index" <> show s
        Just string -> Right string

    cookVari
      :: TransformS
      -> Int
      -> Reference Word23
      -> (TransformS, Either [Char] (Reference BS.ByteString))
    cookVari (varis, funcs) pos ref =
      let (varis', res) = cycleMap "variable" varis pos ref
      in (,) (varis', funcs) res

    cookFunc
      :: TransformS
      -> Int
      -> Reference Word23
      -> (TransformS, Either [Char] (Reference BS.ByteString))
    cookFunc (varis, funcs) pos ref =
      let (funcs', res) = cycleMap "function" funcs pos ref
      in (,) (varis, funcs') res


-- | Parses 'Code' into a set of instructions while also using 'Vari', 'Func' and 'Strg'
--   chunks to supply variable, function and string names respectively.
--
--   Fails if:
--
--     * Bytecode is not 0xF;
--
--     * Functions overlap or there are any spare instructions left;
--
--     * Any reference cannot be found in its matching chunk.
instructions
  :: Form
  -> Code
  -> Vari
  -> Func
  -> [(CodeFunction, Either [Char] [Marked (Cooked Instruction)])]
instructions form cod var fun =
  let transformS :: TransformS
      transformS = (nameMap $ var^.elements, nameMap $ fun^.positions)

      f s (cf, ei) = case ei of
                       Left err  -> (s, (cf, Left err))
                       Right val ->
                         case instruction form s val of
                           Left err           -> (s , (cf, Left err    ))
                           Right (s', cooked) -> (s', (cf, Right cooked))

  in snd $ mapAccumL f transformS $ rawInstructions cod



-- | Total number of functions within the 'Code' chunk.
totalFunctions :: Code -> Int
totalFunctions = Vec.length . flip (^.) elements
