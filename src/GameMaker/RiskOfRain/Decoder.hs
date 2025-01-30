{-|
    This module is a set of datatypes the Risk of Rain GameMaker data file parses into.

    The structure is very similar to the Undertale build, yet there are minor differences.
 -}

{-# LANGUAGE BangPatterns
           , DerivingVia
           , DuplicateRecordFields
           , GADTs
           , GeneralizedNewtypeDeriving
           , NoFieldSelectors
           , OverloadedStrings
           , PatternSynonyms
           , PolyKinds
           , RankNTypes
           , RecordWildCards
           , RoleAnnotations
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-call-arity #-}

module GameMaker.RiskOfRain.Decoder
  ( -- * Itself
    Decoder

    -- * Run
  , ChunkName (..)
  , Error (..)
  , decode

    -- * Decode
    -- ** FORM
  , form
  , Form (..)

    -- ** Chunks
  , discard
  , GameMaker.RiskOfRain.Decoder.maybe

    -- *** GEN8
  , gen8
  , Gen8 (..)

    -- *** OPTN
  , optn
  , Optn

    -- *** EXTN
  , extn
  , Extn (..)
  , ExtnTriplet (..)
  , ExtnSegment (..)
  , ExtnOperation (..)

    -- *** SOND
  , sond
  , Sond (..)
  , SondElement (..)

    -- *** AGRP
  , Agrp

    -- *** SPRT
  , sprt
  , Sprt (..)
  , SprtElement (..)

    -- *** BGND
  , bgnd
  , Bgnd (..)
  , BgndElement (..)

    -- *** PATH
  , Path

    -- *** SCPT
  , scpt
  , Scpt (..)
  , ScptBinding (..)

    -- *** SHDR
  , Shdr

    -- *** FONT
  , font
  , Font (..)
  , FontElement (..)
  , FontBit (..)

    -- *** TMLN
  , Tmln

    -- *** OBJT
  , objt
  , Objt (..)
  , ObjtElement (..)
  , ShapePoint (..)
  , ObjtEvent (..)
  , ObjtAction (..)

    -- *** ROOM
  , room
  , Room (..)
  , RoomElement (..)
  , RGBA (..)
  , RoomBackground (..)
  , RoomView (..)
  , RoomObject (..)
  , RoomTile (..)

    -- *** DAFL
  , Dafl

    -- *** TPAG
  , tpag
  , Tpag (..)
  , TpagElement (..)

    -- **** Reference
  , TpagRef (..)
  , lookupTpagRef

    -- *** CODE
  , code
  , Code (..)
  , CodeFunction (..)

    -- *** VARI
  , vari
  , Vari (..)
  , VariElement (..)

    -- *** FUNC
  , func
  , Func (..)
  , FuncPosition (..)
  , FuncArguments (..)

    -- *** STRG
  , strg
  , Strg (..)

    -- **** Reference
  , StrgRef (..)
  , lookupStrgRef

    -- *** TXTR
  , txtr
  , Txtr (..)
  , TxtrElement (..)

    -- *** AUDO
  , audo
  , Audo (..)
  ) where


import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.ByteString.Short (ShortByteString)
import           Data.Int
import           Data.Patricia.Word.Strict (Patricia)
import qualified Data.Patricia.Word.Strict as Patricia
import           Data.Primitive.PrimArray
import           Data.Primitive.SmallArray
import           Data.Primitive.Types
import           Data.Word
import           GHC.Base (unsafeChr)
import           Parser.Lathe as Lathe
import           Parser.Lathe.Binary



replicateSmallArrayParser :: Int -> Parser e a -> Parser e (SmallArray a)
replicateSmallArrayParser len f
  | len <= 0  = pure emptySmallArray
  | otherwise = do
      list <- replicateM len f
      pure $! smallArrayFromListN len list



-- | Chunk name as four ASCII bytes.
newtype ChunkName = ChunkName Word32
                    deriving Eq

instance Show ChunkName where
  showsPrec _ (ChunkName d) =
      showChar (unsafeChr $ fromIntegral (d `unsafeShiftR` 24))
    . showChar (unsafeChr $ fromIntegral (d `unsafeShiftR` 16) .&. 0xFF)
    . showChar (unsafeChr $ fromIntegral (d `unsafeShiftR` 8)  .&. 0xFF)
    . showChar (unsafeChr $ fromIntegral (d                    .&. 0xFF))



pattern FORM :: ChunkName
pattern FORM = ChunkName 0x464F524D

pattern GEN8, OPTN, EXTN, SOND, AGRP, SPRT, BGND, PATH, SCPT, SHDR, FONT
      , TMLN, OBJT, ROOM, DAFL, TPAG, CODE, VARI, FUNC, STRG, TXTR, AUDO :: ChunkName
pattern GEN8 = ChunkName 0x47454E38
pattern OPTN = ChunkName 0x4F50544E
pattern EXTN = ChunkName 0x4558544E
pattern SOND = ChunkName 0x534F4E44
pattern AGRP = ChunkName 0x41475250
pattern SPRT = ChunkName 0x53505254
pattern BGND = ChunkName 0x42474E44
pattern PATH = ChunkName 0x50415448
pattern SCPT = ChunkName 0x53435054
pattern SHDR = ChunkName 0x53484452
pattern FONT = ChunkName 0x464F4E54
pattern TMLN = ChunkName 0x544D4C4E
pattern OBJT = ChunkName 0x4F424A54
pattern ROOM = ChunkName 0x524F4F4D
pattern DAFL = ChunkName 0x4441464C
pattern TPAG = ChunkName 0x54504147
pattern CODE = ChunkName 0x434F4445
pattern VARI = ChunkName 0x56415249
pattern FUNC = ChunkName 0x46554E43
pattern STRG = ChunkName 0x53545247
pattern TXTR = ChunkName 0x54585452
pattern AUDO = ChunkName 0x4155444F



-- | Types of errors that may occur during decoding.
data Error = -- | Reached end of input.
             AbruptEnd

             -- | Next chunk name is not the one expected.
           | Mismatch
               ChunkName -- ^ Expected chunk name.
               ChunkName -- ^ Found chunk name.

             -- | Parsing within a chunk either overstepped or left trailing data.
           | Misaligned
               ChunkName
               Word32     -- ^ Expected chunk size.
               ByteOffset -- ^ Number of chunk bytes parsed.

             -- | Found extra data after the FORM chunk.
           | TrailingData
             deriving Show



type Length = Word32

type role Decoder nominal representational
-- | Decoder for a chunk of type @tag@ that returns @a@.
newtype Decoder tag a = Decoder (ChunkName -> Length -> Parser Error a)

runDecoder :: Decoder tag a -> ChunkName -> Length -> Parser Error a
runDecoder (Decoder f) = f

chunkP :: ChunkName -> Decoder flag a -> Parser Error a
chunkP (ChunkName ref) (Decoder consume) = do
  name <- word32BE AbruptEnd
  if name == ref
    then do
      len <- word32LE AbruptEnd
      consume (ChunkName ref) len

    else err $ Mismatch (ChunkName ref) (ChunkName name)



data Strictness = Strict | Lenient

strict, lenient :: Parser Error a -> Decoder tag a
strict  = consuming Strict
lenient = consuming Lenient

consuming :: Strictness -> Parser Error a -> Decoder tag a
consuming strictness = \parser ->
  Decoder $ \name len -> do
    o <- bytesRead
    r <- parser
    o' <- bytesRead
    let delta = o' - o
        diff = fromIntegral len - delta

    if diff >= 0
      then do
        if diff > 0
          then case strictness of
                 Strict  -> err $ Misaligned name len delta
                 Lenient -> skip diff AbruptEnd

          else pure ()

        pure r

      else err $ Misaligned name len delta


-- | Skip any chunk.
discard :: Decoder tag ()
discard =
  Decoder $ \_ len -> do
    skip (fromIntegral len) AbruptEnd



-- | Skip any chunk if it's empty, otherwise decode as normal.
maybe :: Decoder tag a -> Decoder tag (Maybe a)
maybe (Decoder decoder) =
  Decoder $ \name len ->
    if len == 0
      then pure Nothing
      else Just <$> decoder name len



decode :: Decoder Form a -> LB.ByteString -> (Scrap, Either Error a)
decode formD = parse $ chunkP FORM formD



data Form gen8 optn extn sond agrp sprt bgnd path scpt shdr font
          tmln objt room dafl tpag code vari func strg txtr audo =
       Form
         { gen8 :: gen8
         , optn :: optn
         , extn :: extn
         , sond :: sond
         , agrp :: agrp
         , sprt :: sprt
         , bgnd :: bgnd
         , path :: path
         , scpt :: scpt
         , shdr :: shdr
         , font :: font
         , tmln :: tmln
         , objt :: objt
         , room :: room
         , dafl :: dafl
         , tpag :: tpag
         , code :: code
         , vari :: vari
         , func :: func
         , strg :: strg
         , txtr :: txtr
         , audo :: audo
         }
       deriving Show

form
  :: Decoder Gen8 gen8
  -> Decoder Optn optn
  -> Decoder Extn extn
  -> Decoder Sond sond
  -> Decoder Agrp agrp
  -> Decoder Sprt sprt
  -> Decoder Bgnd bgnd
  -> Decoder Path path
  -> Decoder Scpt scpt
  -> Decoder Shdr shdr
  -> Decoder Font font
  -> Decoder Tmln tmln
  -> Decoder Objt objt
  -> Decoder Room room
  -> Decoder Dafl dafl
  -> Decoder Tpag tpag
  -> Decoder Code code
  -> Decoder Vari vari
  -> Decoder Func func
  -> Decoder Strg strg
  -> Decoder Txtr txtr
  -> Decoder Audo audo
  -> Decoder Form (Form gen8 optn extn sond agrp sprt bgnd path scpt shdr font
                        tmln objt room dafl tpag code vari func strg txtr audo)
form gen8D optnD extnD sondD agrpD sprtD bgndD pathD scptD shdrD fontD
     tmlnD objtD roomD daflD tpagD codeD variD funcD strgD txtrD audoD =
  strict $ do
    gen8_ <- chunkP GEN8 gen8D
    optn_ <- chunkP OPTN optnD
    extn_ <- chunkP EXTN extnD
    sond_ <- chunkP SOND sondD
    agrp_ <- chunkP AGRP agrpD
    sprt_ <- chunkP SPRT sprtD
    bgnd_ <- chunkP BGND bgndD
    path_ <- chunkP PATH pathD
    scpt_ <- chunkP SCPT scptD
    shdr_ <- chunkP SHDR shdrD
    font_ <- chunkP FONT fontD
    tmln_ <- chunkP TMLN tmlnD
    objt_ <- chunkP OBJT objtD
    room_ <- chunkP ROOM roomD
    dafl_ <- chunkP DAFL daflD
    tpag_ <- chunkP TPAG tpagD
    code_ <- chunkP CODE codeD
    vari_ <- chunkP VARI variD
    func_ <- chunkP FUNC funcD
    strg_ <- chunkP STRG strgD
    txtr_ <- chunkP TXTR txtrD
    audo_ <- chunkP AUDO audoD
    end <- atEnd
    if end
      then pure $! Form
                     gen8_ optn_ extn_ sond_ agrp_ sprt_ bgnd_ path_ scpt_ shdr_ font_
                     tmln_ objt_ room_ dafl_ tpag_ code_ vari_ func_ strg_ txtr_ audo_
  
      else err TrailingData



-- | Curious information about the videogame.
data Gen8 =
       Gen8
         { unknown1       :: !Word32
         , name           :: !StrgRef
         , filename       :: !StrgRef
         , unknown2       :: !Word32
         , unknown3       :: !Word32
         , unknown4       :: !Word32
         , unknown5       :: !Word32
         , unknown6       :: !Word32
         , unknown7       :: !Word32
         , unknown8       :: !Word32
         , name2          :: !StrgRef
         , major          :: !Word32
         , minor          :: !Word32
         , release        :: !Word32
         , build          :: !Word32
         , defaultHeight  :: !Word32
         , defaultWidth   :: !Word32
         , info           :: !Word32
         , licenseMD5     :: !(PrimArray Word32)
         , licenseCRC32   :: !Word32
         , timestamp      :: !Word32
         , unknown9       :: !Word32
         , displayName    :: !StrgRef
         , unknown10      :: !Word32
         , unknown11      :: !Word32
         , unknown12      :: !Word32
         , unknown13      :: !Word32
         , unknown14      :: !Word32
         , unknown15      :: !Word32
         , rooms          :: !(PrimArray Word32)
         }
       deriving Show



-- | Global pointer to a string in the @STRG@ chunk.
newtype StrgRef = StrgRef Word32
                    deriving stock Show
                    deriving newtype Prim

stringRef :: Parser Error StrgRef
stringRef = StrgRef <$> word32LE AbruptEnd



-- | Global pointer to an entry in the @TPAG@ chunk.
newtype TpagRef = TpagRef Word32
                     deriving stock Show
                     deriving newtype Prim

textureRef :: Parser Error TpagRef
textureRef = TpagRef <$> word32LE AbruptEnd



-- | Distance between given offset and the next multiple of 128.
align :: ByteOffset -> ByteOffset -> ByteOffset
align ment offset =
  let a = offset .&. (ment - 1)
  in case a of
       0 -> 0
       _ -> ment - a



gen8 :: Decoder Gen8 Gen8
gen8 =
  strict $ do
    unknown1      <- word32LE AbruptEnd
    name          <- stringRef
    filename      <- stringRef
    unknown2      <- word32LE AbruptEnd
    unknown3      <- word32LE AbruptEnd
    unknown4      <- word32LE AbruptEnd
    unknown5      <- word32LE AbruptEnd
    unknown6      <- word32LE AbruptEnd
    unknown7      <- word32LE AbruptEnd
    unknown8      <- word32LE AbruptEnd
    name2         <- stringRef
    major         <- word32LE AbruptEnd
    minor         <- word32LE AbruptEnd
    release       <- word32LE AbruptEnd
    build         <- word32LE AbruptEnd
    defaultHeight <- word32LE AbruptEnd
    defaultWidth  <- word32LE AbruptEnd
    info          <- word32LE AbruptEnd
    licenseMD5    <- replicatePrimArrayA 4 $ word32BE AbruptEnd
    licenseCRC32  <- word32LE AbruptEnd
    timestamp     <- word32LE AbruptEnd
    unknown9      <- word32LE AbruptEnd
    displayName   <- stringRef
    unknown10     <- word32LE AbruptEnd
    unknown11     <- word32LE AbruptEnd
    unknown12     <- word32LE AbruptEnd
    unknown13     <- word32LE AbruptEnd
    unknown14     <- word32LE AbruptEnd
    unknown15     <- word32LE AbruptEnd
    roomCount     <- word32LE AbruptEnd
    rooms         <- replicatePrimArrayA (fromIntegral roomCount) $ word32LE AbruptEnd
    pure $! Gen8 {..}



-- | Too little information to meaningfully structure.
data Optn =
       Optn
         { unknown1  :: !Word32
         , unknown2  :: !Word32
         , info      :: !Word32
         , unknown3  :: !Word32
         , unknown4  :: !Word32
         , unknown5  :: !Word32
         , unknown6  :: !Word32
         , unknown7  :: !Word32
         , unknown8  :: !Word32
         , unknown9  :: !Word32
         , unknown10 :: !Word32
         , unknown11 :: !Word32
         , unknown12 :: !Word32
         , unknown13 :: !Word32
         , unknown14 :: !Word32
         , unknown15 :: !Word32
         }
       deriving Show

optn :: Decoder Optn Optn
optn =
  strict $ do
    unknown1  <- word32LE AbruptEnd
    unknown2  <- word32LE AbruptEnd
    info      <- word32LE AbruptEnd
    unknown3  <- word32LE AbruptEnd
    unknown4  <- word32LE AbruptEnd
    unknown5  <- word32LE AbruptEnd
    unknown6  <- word32LE AbruptEnd
    unknown7  <- word32LE AbruptEnd
    unknown8  <- word32LE AbruptEnd
    unknown9  <- word32LE AbruptEnd
    unknown10 <- word32LE AbruptEnd
    unknown11 <- word32LE AbruptEnd
    unknown12 <- word32LE AbruptEnd
    unknown13 <- word32LE AbruptEnd
    unknown14 <- word32LE AbruptEnd
    unknown15 <- word32LE AbruptEnd
    pure Optn {..}



-- | Seemingly bindings between GameMaker IO operations and system ones
--   (just based on the names pointed to), however most of this data
--   is just ones and twos scattered around.
data Extn =
       Extn
         { unknown1      :: !(SmallArray ExtnTriplet)
         , unknown2      :: !(SmallArray ExtnSegment)
         , unknown3      :: !(PrimArray Word32)       -- ^ May be a hash.
         }
       deriving Show

data ExtnTriplet =
       ExtnTriplet
         { unknown1 :: !StrgRef
         , unknown2 :: !StrgRef
         , unknown3 :: !StrgRef
         }
       deriving Show

data ExtnSegment =
       ExtnSegment
         { name           :: !ExtnTriplet
         , unknown1       :: !Word32
         , operations     :: !(SmallArray ExtnOperation)
         }
       deriving Show

data ExtnOperation =
       ExtnOperation
         { operationFs   :: !StrgRef
         , operationId   :: !Word32
         , unknown1      :: !Word32
         , unknown2      :: !Word32
         , operation     :: !StrgRef
         , unknown3      :: !(PrimArray Word32)
         }
       deriving Show

extnTripletP :: Parser Error ExtnTriplet
extnTripletP = do
  unknown1 <- stringRef
  unknown2 <- stringRef
  unknown3 <- stringRef
  pure $! ExtnTriplet {..}

extnSegmentP :: Parser Error ExtnSegment
extnSegmentP = do
  name           <- extnTripletP
  unknown1       <- word32LE AbruptEnd

  operationCount <- word32LE AbruptEnd
  _ptrs <- skip (fromIntegral operationCount * 4) AbruptEnd
  operations     <- replicateSmallArrayParser (fromIntegral operationCount) extnOperationP

  pure $! ExtnSegment {..}

extnOperationP :: Parser Error ExtnOperation
extnOperationP = do
  operationFs   <- stringRef
  operationId   <- word32LE AbruptEnd
  unknown1      <- word32LE AbruptEnd
  unknown2      <- word32LE AbruptEnd
  operation     <- stringRef
  unknown3Count <- word32LE AbruptEnd
  unknown3      <- replicatePrimArrayA (fromIntegral unknown3Count) (word32LE AbruptEnd)
  pure $! ExtnOperation {..}

extn :: Decoder Extn Extn
extn =
  strict $ do
    unknown1Count <- word32LE AbruptEnd
    _ptrs <- skip (fromIntegral unknown1Count * 4) AbruptEnd
    unknown1 <- replicateSmallArrayParser (fromIntegral unknown1Count) extnTripletP

    unknown2Count <- word32LE AbruptEnd
    _ptrs <- skip (fromIntegral unknown2Count * 4) AbruptEnd
    unknown2 <- replicateSmallArrayParser (fromIntegral unknown2Count) extnSegmentP

    unknown3 <- replicatePrimArrayA 4 $ word32BE AbruptEnd

    pure $! Extn {..}



-- | Sound files and information.
newtype Sond =
          Sond
            { elements :: SmallArray SondElement
            }
          deriving Show

data SondElement =
       SondElement
         { name       :: !StrgRef
         , flags      :: !Word32
         , extension  :: !StrgRef
         , filename   :: !StrgRef
         , unknown1   :: !Word32    -- ^ Always zero
         , volume     :: !Float
         , pitch      :: !Float
         , groupId    :: !Int32
         , identifier :: !Int32
         }
       deriving Show

sondElementP :: Parser Error SondElement
sondElementP = do
  name       <- stringRef
  flags      <- word32LE AbruptEnd
  extension  <- stringRef
  filename   <- stringRef
  unknown1   <- word32LE AbruptEnd
  volume     <- floatLE AbruptEnd
  pitch      <- floatLE AbruptEnd
  groupId    <- int32LE AbruptEnd
  identifier <- int32LE AbruptEnd
  pure $! SondElement {..}

sond :: Decoder Sond Sond
sond =
  strict $ do
    count    <- word32LE AbruptEnd
    _ptrs    <- skip (fromIntegral count * 4) AbruptEnd
    elements <- replicateSmallArrayParser (fromIntegral count) sondElementP
    pure $! Sond {..}



-- | Empty dictionary in both files
data Agrp



-- | Foreground sprites with all the masks and stuff.
newtype Sprt =
          Sprt
            { elements :: SmallArray SprtElement
            }
          deriving Show

data SprtElement =
       SprtElement
         { name         :: !StrgRef
         , width        :: !Int32
         , height       :: !Int32
         , marginLeft   :: !Int32
         , marginRight  :: !Int32
         , marginTop    :: !Int32
         , marginBottom :: !Int32
         , unknown1     :: !Int32             -- Always zero
         , unknown2     :: !Int32             -- Always zero
         , unknown3     :: !Int32             -- Always zero
         , bBoxMode     :: !Int32
         , sepMasks     :: !Int32
         , originX      :: !Int32
         , originY      :: !Int32
         , textures     :: !(PrimArray TpagRef)
         , masks        :: !(SmallArray ShortByteString)
         }
       deriving Show

sprtElementP :: Parser Error SprtElement
sprtElementP = do
  name         <- stringRef
  width        <- int32LE AbruptEnd
  height       <- int32LE AbruptEnd
  marginLeft   <- int32LE AbruptEnd
  marginRight  <- int32LE AbruptEnd
  marginTop    <- int32LE AbruptEnd
  marginBottom <- int32LE AbruptEnd
  unknown1     <- int32LE AbruptEnd
  unknown2     <- int32LE AbruptEnd
  unknown3     <- int32LE AbruptEnd
  bBoxMode     <- int32LE AbruptEnd
  sepMasks     <- int32LE AbruptEnd
  originX      <- int32LE AbruptEnd
  originY      <- int32LE AbruptEnd

  textureCount <- word32LE AbruptEnd
  textures     <- replicatePrimArrayA (fromIntegral textureCount) textureRef

  maskCount    <- word32LE AbruptEnd
  masks        <- replicateSmallArrayParser (fromIntegral maskCount) $
                    let ~(q, r) = quotRem (fromIntegral width :: Int) 8

                        width8 | r == 0    = q
                               | otherwise = q + 1

                    in shortByteString (fromIntegral width8 * fromIntegral height) AbruptEnd

  bytes <- bytesRead
  skip ( case fromIntegral bytes `rem` 4 of
           0 -> 0
           n -> 4 - n
       )
       AbruptEnd

  pure $! SprtElement {..}

sprt :: Decoder Sprt Sprt
sprt =
  strict $ do
    count <- word32LE AbruptEnd
    skip (fromIntegral count * 4) AbruptEnd
    elements <- replicateSmallArrayParser (fromIntegral count) sprtElementP

    pure $! Sprt {..}



-- | Background sprites.
newtype Bgnd =
          Bgnd
            { elements :: SmallArray BgndElement
            }
          deriving Show

data BgndElement =
       BgndElement
         { name     :: !StrgRef
         , unknown1 :: !Word32    -- ^ Always zero
         , unknown2 :: !Word32    -- ^ Always zero
         , unknown3 :: !Word32    -- ^ Always zero
         , texture  :: !TpagRef
         }
       deriving Show

bgndElementP :: Parser Error BgndElement
bgndElementP = do
  name     <- stringRef
  unknown1 <- word32LE AbruptEnd
  unknown2 <- word32LE AbruptEnd
  unknown3 <- word32LE AbruptEnd
  texture  <- textureRef
  pure $! BgndElement {..}

bgnd :: Decoder Bgnd Bgnd
bgnd =
  strict $ do
    count <- word32LE AbruptEnd
    _ptrs <- skip (fromIntegral count * 4) AbruptEnd
    elements <- replicateSmallArrayParser (fromIntegral count) bgndElementP
    pure $! Bgnd {..}



-- | Empty dictionary in both files
data Path



-- | Bindings of script functions to identifiers.
--
--   I suppose this is an extremely elaborate strategy to bind t'CodeFunction's to
--   t'FuncPosition's or something, however the difference between the two is just a
--   prefix.
newtype Scpt =
          Scpt
            { bindings :: SmallArray ScptBinding
            }
          deriving Show

data ScptBinding =
       ScptBinding
         { pointer    :: !StrgRef
         , identifier :: !Word32
         }
       deriving Show

scptBindingP :: Parser Error ScptBinding
scptBindingP = do
  pointer      <- stringRef
  identifier   <- word32LE AbruptEnd
  pure $! ScptBinding {..}

scpt :: Decoder Scpt Scpt
scpt =
  strict $ do
    count <- word32LE AbruptEnd
    _ptrs <- skip (fromIntegral count * 4) AbruptEnd
    bindings <- replicateSmallArrayParser (fromIntegral count) scptBindingP
    pure $! Scpt {..}



-- | Shader descriptions. No useful information here other than pointers to files in STRG.
data Shdr



-- | Font descriptions because GameMaker can't tug around font files, so they're shoved
--   in textures.
data Font =
       Font
         { elements :: !(SmallArray FontElement)
         , charset  :: !(PrimArray Word16)
         }
       deriving Show

data FontElement =
       FontElement
         { kind           :: !StrgRef
         , name           :: !StrgRef
         , emSize         :: !Word32
         , bold           :: !Word32
         , italic         :: !Word32
         , rangeStart     :: !Word16
         , charset        :: !Word8
         , antialiasing   :: !Word8
         , rangeEnd       :: !Word32
         , texture        :: !TpagRef
         , scaleX         :: !Float
         , scaleY         :: !Float
         , characters     :: !(SmallArray FontBit)
         }
       deriving Show

data FontBit =
       FontBit
         { character :: !Char
         , offsetX   :: !Int16
         , offsetY   :: !Int16
         , width     :: !Int16
         , height    :: !Int16
         , advance   :: !Int16
         , bearingX  :: !Int16
         , bearingY  :: !Int16
         }
       deriving Show

fontBitP :: Parser Error FontBit
fontBitP = do
  character <- unsafeChr . fromIntegral <$> word16LE AbruptEnd
  offsetX   <- int16LE AbruptEnd
  offsetY   <- int16LE AbruptEnd
  width     <- int16LE AbruptEnd
  height    <- int16LE AbruptEnd
  advance   <- int16LE AbruptEnd
  bearingX  <- int16LE AbruptEnd
  bearingY  <- int16LE AbruptEnd
  pure $! FontBit {..}

fontElementP :: Parser Error FontElement
fontElementP = do
  kind           <- stringRef
  name           <- stringRef
  emSize         <- word32LE AbruptEnd
  bold           <- word32LE AbruptEnd
  italic         <- word32LE AbruptEnd
  rangeStart     <- word16LE AbruptEnd
  charset        <- word8 AbruptEnd
  antialiasing   <- word8 AbruptEnd
  rangeEnd       <- word32LE AbruptEnd
  texture        <- textureRef
  scaleX         <- floatLE AbruptEnd
  scaleY         <- floatLE AbruptEnd

  characterCount <- word32LE AbruptEnd
  _ptrs2         <- skip (fromIntegral characterCount * 4) AbruptEnd
  characters     <- replicateSmallArrayParser (fromIntegral characterCount) fontBitP

  pure $! FontElement {..}

font :: Decoder Font Font
font =
  strict $ do
    count <- word32LE AbruptEnd
    _ptrs <- skip (fromIntegral count * 4) AbruptEnd
    elements <- replicateSmallArrayParser (fromIntegral count) fontElementP
    charset <- replicatePrimArrayA 256 (word16LE AbruptEnd)
    pure $! Font {..}



-- | Empty dictionary in both files
data Tmln



-- | Pretty much garbage. Extremely bulky, confusing and utterly undecryptable since
--   Risk of Rain barely uses physics at all.
newtype Objt =
          Objt
            { elements :: SmallArray ObjtElement
            }
          deriving Show

-- | The only thing from here we know for sure are name and sprite index
data ObjtElement =
       ObjtElement
         { name            :: !StrgRef
         , spriteIndex     :: !Int32
         , visible         :: !Int32
         , solid           :: !Int32
         , depth           :: !Int32
         , persistent      :: !Int32
         , parentId        :: !Int32
         , textureMaskId   :: !Int32
         , unknown1        :: !Int32
         , unknown2        :: !Int32
         , unknown3        :: !Int32
         , unknown4        :: !Float
         , unknown5        :: !Float
         , unknown6        :: !Float
         , unknown7        :: !Float
         , unknown8        :: !Float
         , unknown9        :: !Float
         , unknown10       :: !Int32
         , unknown11       :: !Int32
         , shapePoints     :: !(SmallArray ShapePoint)
         , unknown12       :: !(SmallArray ObjtEvent) -- ^ Always 12
         }
       deriving Show

data ShapePoint =
       ShapePoint
         { x :: !Float
         , y :: !Float
         }
       deriving Show

newtype ObjtEvent =
          ObjtEvent
            { elements :: SmallArray ObjtAction
            }
          deriving Show

data ObjtAction =
       ObjtAction
         { unknown1   :: !Int32
         , unknown2   :: !Int32
         , unknown3   :: !Int32   -- Points eight bytes ahead
         , unknown4   :: !Int32
         , unknown5   :: !Int32
         , unknown6   :: !Int32
         , unknown7   :: !Int32
         , unknown8   :: !Int32
         , unknown9   :: !Int32
         , unknown10  :: !Int32
         , unknown11  :: !StrgRef -- Points to an empty string
         , identifier :: !Int32
         , unknown12  :: !Int32
         , unknown13  :: !Int32
         , unknown14  :: !Int32
         , unknown15  :: !Int32
         , unknown16  :: !Int32
         }
       deriving Show

objtActionP :: Parser Error ObjtAction
objtActionP = do
  unknown1   <- int32LE AbruptEnd
  unknown2   <- int32LE AbruptEnd
  unknown3   <- int32LE AbruptEnd
  unknown4   <- int32LE AbruptEnd
  unknown5   <- int32LE AbruptEnd
  unknown6   <- int32LE AbruptEnd
  unknown7   <- int32LE AbruptEnd
  unknown8   <- int32LE AbruptEnd
  unknown9   <- int32LE AbruptEnd
  unknown10  <- int32LE AbruptEnd
  unknown11  <- stringRef
  identifier <- int32LE AbruptEnd
  unknown12  <- int32LE AbruptEnd
  unknown13  <- int32LE AbruptEnd
  unknown14  <- int32LE AbruptEnd
  unknown15  <- int32LE AbruptEnd
  unknown16  <- int32LE AbruptEnd
  pure $! ObjtAction {..}

objtEventP :: Parser Error ObjtEvent
objtEventP = do
  count    <- word32LE AbruptEnd
  _ptrs <- skip (fromIntegral count * 4) AbruptEnd
  elements <- replicateSmallArrayParser (fromIntegral count) objtActionP
  pure $! ObjtEvent {..}

shapePointP :: Parser Error ShapePoint
shapePointP = do
  x <- floatLE AbruptEnd
  y <- floatLE AbruptEnd
  pure $! ShapePoint {..}

objtElementP :: Parser Error ObjtElement
objtElementP = do
  name            <- stringRef
  spriteIndex     <- int32LE AbruptEnd
  visible         <- int32LE AbruptEnd
  solid           <- int32LE AbruptEnd
  depth           <- int32LE AbruptEnd
  persistent      <- int32LE AbruptEnd
  parentId        <- int32LE AbruptEnd
  textureMaskId   <- int32LE AbruptEnd
  unknown1        <- int32LE AbruptEnd
  unknown2        <- int32LE AbruptEnd
  unknown3        <- int32LE AbruptEnd
  unknown4        <- floatLE AbruptEnd
  unknown5        <- floatLE AbruptEnd
  unknown6        <- floatLE AbruptEnd
  unknown7        <- floatLE AbruptEnd
  unknown8        <- floatLE AbruptEnd
  shapePointCount <- int32LE AbruptEnd
  unknown9        <- floatLE AbruptEnd
  unknown10       <- int32LE AbruptEnd
  unknown11       <- int32LE AbruptEnd
  shapePoints     <- replicateSmallArrayParser (fromIntegral shapePointCount) shapePointP

  unknown12Count  <- word32LE AbruptEnd
  _ptrs <- skip (fromIntegral unknown12Count * 4) AbruptEnd
  unknown12       <- replicateSmallArrayParser (fromIntegral unknown12Count) objtEventP

  pure $! ObjtElement {..}

objt :: Decoder Objt Objt
objt =
  strict $ do
    count <- word32LE AbruptEnd
    _ptrs <- skip (fromIntegral count * 4) AbruptEnd
    elements <- replicateSmallArrayParser (fromIntegral count) objtElementP
    pure $! Objt {..}



-- | Room data.
newtype Room =
          Room
            { elements :: SmallArray RoomElement
            }
          deriving Show

data RoomElement =
       RoomElement
         { name            :: !StrgRef
         , caption         :: !StrgRef
         , width           :: !Word32
         , height          :: !Word32
         , speed           :: !Word32
         , persistent      :: !Int32
         , rgba            :: !RGBA
         , drawBGColor     :: !Int32
         , unknown1        :: !Word32
         , flags           :: !Word32
         , bgOffset        :: !Word32
         , viewOffset      :: !Word32
         , objOffset       :: !Word32
         , tileOffset      :: !Word32
         , world           :: !Word32
         , top             :: !Word32
         , left            :: !Word32
         , right           :: !Word32
         , bottom          :: !Word32
         , gravityX        :: !Float
         , gravityY        :: !Float
         , metersPerPixel  :: !Float
         , backgrounds     :: !(SmallArray RoomBackground)
         , views           :: !(SmallArray RoomView)
         , objects         :: !(SmallArray RoomObject)
         , tiles           :: !(SmallArray RoomTile)
         }
       deriving Show

data RGBA =
       RGBA
         { r :: !Word8
         , g :: !Word8
         , b :: !Word8
         , a :: !Word8
         }
       deriving Show

data RoomBackground =
       RoomBackground
         { enabled    :: !Int32
         , foreground :: !Int32
         , bgDefIndex :: !Int32
         , x          :: !Int32
         , y          :: !Int32
         , tileX      :: !Int32
         , tileY      :: !Int32
         , speedX     :: !Int32
         , speedY     :: !Int32
         , identifier :: !Int32
         }
       deriving Show

data RoomView =
       RoomView
         { enabled    :: !Int32
         , viewX      :: !Int32
         , viewY      :: !Int32
         , viewWidth  :: !Int32
         , viewHeight :: !Int32
         , portX      :: !Int32
         , portY      :: !Int32
         , portWidth  :: !Int32
         , portHeight :: !Int32
         , borderX    :: !Int32
         , borderY    :: !Int32
         , speedX     :: !Int32
         , speedY     :: !Int32
         , identifier :: !Int32
         }
       deriving Show

data RoomObject =
       RoomObject
         { x          :: !Int32
         , y          :: !Int32
         , identifier :: !Int32
         , initCode   :: !Int32
         , unknown5   :: !Int32
         , scaleX     :: !Float
         , scaleY     :: !Float
         , unknown8   :: !Int32
         , rotation   :: !Float
         }
       deriving Show

data RoomTile =
       RoomTile
         { x          :: !Int32
         , y          :: !Int32
         , bgDefIndex :: !Int32
         , sourceX    :: !Int32
         , sourceY    :: !Int32
         , width      :: !Int32
         , height     :: !Int32
         , tileDepth  :: !Int32
         , identifier :: !Int32
         , scaleX     :: !Float
         , scaleY     :: !Float
         , unknown12  :: !Int32
         }
       deriving Show

roomTileP :: Parser Error RoomTile
roomTileP = do
  x          <- int32LE AbruptEnd
  y          <- int32LE AbruptEnd
  bgDefIndex <- int32LE AbruptEnd
  sourceX    <- int32LE AbruptEnd
  sourceY    <- int32LE AbruptEnd
  width      <- int32LE AbruptEnd
  height     <- int32LE AbruptEnd
  tileDepth  <- int32LE AbruptEnd
  identifier <- int32LE AbruptEnd
  scaleX     <- floatLE AbruptEnd
  scaleY     <- floatLE AbruptEnd
  unknown12  <- int32LE AbruptEnd
  pure $! RoomTile {..}

roomObjectP :: Parser Error RoomObject
roomObjectP = do
  x          <- int32LE AbruptEnd
  y          <- int32LE AbruptEnd
  identifier <- int32LE AbruptEnd
  initCode   <- int32LE AbruptEnd
  unknown5   <- int32LE AbruptEnd
  scaleX     <- floatLE AbruptEnd
  scaleY     <- floatLE AbruptEnd
  unknown8   <- int32LE AbruptEnd
  rotation   <- floatLE AbruptEnd
  pure $! RoomObject {..}

roomViewP :: Parser Error RoomView
roomViewP = do
  enabled    <- int32LE AbruptEnd
  viewX      <- int32LE AbruptEnd
  viewY      <- int32LE AbruptEnd
  viewWidth  <- int32LE AbruptEnd
  viewHeight <- int32LE AbruptEnd
  portX      <- int32LE AbruptEnd
  portY      <- int32LE AbruptEnd
  portWidth  <- int32LE AbruptEnd
  portHeight <- int32LE AbruptEnd
  borderX    <- int32LE AbruptEnd
  borderY    <- int32LE AbruptEnd
  speedX     <- int32LE AbruptEnd
  speedY     <- int32LE AbruptEnd
  identifier <- int32LE AbruptEnd
  pure $! RoomView {..}

roomBackgroundP :: Parser Error RoomBackground
roomBackgroundP = do
  enabled    <- int32LE AbruptEnd
  foreground <- int32LE AbruptEnd
  bgDefIndex <- int32LE AbruptEnd
  x          <- int32LE AbruptEnd
  y          <- int32LE AbruptEnd
  tileX      <- int32LE AbruptEnd
  tileY      <- int32LE AbruptEnd
  speedX     <- int32LE AbruptEnd
  speedY     <- int32LE AbruptEnd
  identifier <- int32LE AbruptEnd
  pure $! RoomBackground {..}

rgbaP :: Parser Error RGBA
rgbaP = do
  r <- word8 AbruptEnd
  g <- word8 AbruptEnd
  b <- word8 AbruptEnd
  a <- word8 AbruptEnd
  pure $! RGBA {..}

roomElementP :: Parser Error RoomElement
roomElementP = do
  name            <- stringRef
  caption         <- stringRef
  width           <- word32LE AbruptEnd
  height          <- word32LE AbruptEnd
  speed           <- word32LE AbruptEnd
  persistent      <- int32LE AbruptEnd
  rgba            <- rgbaP
  drawBGColor     <- int32LE AbruptEnd
  unknown1        <- word32LE AbruptEnd
  flags           <- word32LE AbruptEnd
  bgOffset        <- word32LE AbruptEnd
  viewOffset      <- word32LE AbruptEnd
  objOffset       <- word32LE AbruptEnd
  tileOffset      <- word32LE AbruptEnd
  world           <- word32LE AbruptEnd
  top             <- word32LE AbruptEnd
  left            <- word32LE AbruptEnd
  right           <- word32LE AbruptEnd
  bottom          <- word32LE AbruptEnd
  gravityX        <- floatLE AbruptEnd
  gravityY        <- floatLE AbruptEnd
  metersPerPixel  <- floatLE AbruptEnd

  backgroundCount <- word32LE AbruptEnd
  _ptrs <- skip (fromIntegral backgroundCount * 4) AbruptEnd
  backgrounds     <- replicateSmallArrayParser (fromIntegral backgroundCount) roomBackgroundP

  viewCount       <- word32LE AbruptEnd
  _ptrs <- skip (fromIntegral viewCount * 4) AbruptEnd
  views           <- replicateSmallArrayParser (fromIntegral viewCount) roomViewP

  objectCount     <- word32LE AbruptEnd
  _ptrs <- skip (fromIntegral objectCount * 4) AbruptEnd
  objects         <- replicateSmallArrayParser (fromIntegral objectCount) roomObjectP

  tileCount       <- word32LE AbruptEnd
  _ptrs <- skip (fromIntegral tileCount * 4) AbruptEnd
  tiles           <- replicateSmallArrayParser (fromIntegral tileCount) roomTileP

  pure $! RoomElement {..}

room :: Decoder Room Room
room =
  strict $ do
    count <- word32LE AbruptEnd
    _ptrs <- skip (fromIntegral count * 4) AbruptEnd
    elements <- replicateSmallArrayParser (fromIntegral count) roomElementP
    pure $! Room {..}



-- | Empty
data Dafl



-- | Sprite information as related to textures they reside in.
newtype Tpag =
          Tpag
            { elements :: Patricia TpagElement
            }
          deriving Show

data TpagElement =
       TpagElement
         { offsetX        :: !Word16
         , offsetY        :: !Word16
         , width          :: !Word16
         , height         :: !Word16
         , renderX        :: !Word16
         , renderY        :: !Word16
         , boundingX      :: !Word16
         , boundingY      :: !Word16
         , boundingWidth  :: !Word16
         , boundingHeight :: !Word16
         , imageId        :: !Word16
         }
       deriving Show

tpagElementP :: Parser Error TpagElement
tpagElementP = do
  offsetX        <- word16LE AbruptEnd
  offsetY        <- word16LE AbruptEnd
  width          <- word16LE AbruptEnd
  height         <- word16LE AbruptEnd
  renderX        <- word16LE AbruptEnd
  renderY        <- word16LE AbruptEnd
  boundingX      <- word16LE AbruptEnd
  boundingY      <- word16LE AbruptEnd
  boundingWidth  <- word16LE AbruptEnd
  boundingHeight <- word16LE AbruptEnd
  imageId        <- word16LE AbruptEnd
  pure $! TpagElement {..}

tpag :: Decoder Tpag Tpag
tpag =
  strict $ do
    count <- word32LE AbruptEnd
    _ptrs <- skip (fromIntegral count * 4) AbruptEnd

    let go pat n
          | n <= 0    = pure pat
          | otherwise = do
              offset  <- bytesRead
              element <- tpagElementP
              let !pat' = Patricia.insert (fromIntegral offset) element pat
              go pat' (n - 1)

    elements <- go Patricia.empty count
    pure $! Tpag {..}

lookupTpagRef :: Tpag -> TpagRef -> Maybe TpagElement
lookupTpagRef (Tpag pat) (TpagRef ref) = Patricia.dirtyLookup (fromIntegral ref) pat



-- | Interpreted GameMaker code chunk.
newtype Code =
          Code
            { functions :: SmallArray CodeFunction
            }
          deriving Show

data CodeEntry =
       CodeEntry
         { name     :: !StrgRef
         , size     :: !Word32
         , unknown1 :: !Word32
         , offset   :: !Int32
         , unknown2 :: !Word32
         }
       deriving Show

data CodeFunction =
       CodeFunction
         { name     :: !StrgRef
         , size     :: !Word32
         , unknown1 :: !Word32
         , offset   :: !ByteOffset    -- ^ Global offset of this chunk of bytecode.
         , bytecode :: !LB.ByteString
         , unknown2 :: !Word32
         }
       deriving Show

codeEntryP :: Parser Error CodeEntry
codeEntryP = do
  name     <- stringRef
  size     <- word32LE AbruptEnd
  unknown1 <- word32LE AbruptEnd
  offset   <- int32LE AbruptEnd
  unknown2 <- word32LE AbruptEnd
  pure $! CodeEntry {..}

code :: Decoder Code Code
code =
  strict $ do
    count <- word32LE AbruptEnd
    if count <= 0
      then pure $! Code emptySmallArray
      else do
        ptr <- word32LE AbruptEnd
        _ <- skip ((fromIntegral count - 1) * 4) AbruptEnd
        global <- bytesRead
        blob <- lazyByteString (fromIntegral ptr - fromIntegral global) AbruptEnd
        entries <- replicateM (fromIntegral count) codeEntryP
    
        let go acc                   []  = do
              end <- atEnd
              if end
                then pure $! acc []
                else err AbruptEnd
    
            go acc (CodeEntry {..} : es) = do
              local <- bytesRead
              let offset_ = global + local
              bytecode <- lazyByteString (fromIntegral size) AbruptEnd
              let !fun = CodeFunction {offset = offset_, ..}
              go (acc . (:) fun) es
    
        functions <- let (_, ei) = parse (go id entries) blob
                     in case ei of
                          Right fs -> pure $! smallArrayFromListN (fromIntegral count) fs
                          Left e   -> err e
    
        pure $! Code {..}



-- | Variable names and where they occur in code.
data Vari =
       Vari
         { unknown1 :: !Word32
         , unknown2 :: !Word32
         , unknown3 :: !Word32
         , elements :: !(SmallArray VariElement)
         }
       deriving Show

-- | Every address points to the next address and it repeats @occurences@ times.
data VariElement =
       VariElement
         { name        :: !StrgRef
         , unknown1    :: !Int32
         , unknown2    :: !Int32
         , occurrences :: !Int32
         , address     :: !Int32
         }
       deriving Show

variElementP :: Parser Error VariElement
variElementP = do
  name        <- stringRef
  unknown1    <- int32LE AbruptEnd
  unknown2    <- int32LE AbruptEnd
  occurrences <- int32LE AbruptEnd
  address     <- int32LE AbruptEnd
  pure $! VariElement {..}

vari :: Decoder Vari Vari
vari =
  Decoder $ \name len ->
    (\parser -> runDecoder (strict parser) name len) $ do
      unknown1 <- word32LE AbruptEnd
      unknown2 <- word32LE AbruptEnd
      unknown3 <- word32LE AbruptEnd

      -- Elements are a blob accessed by pointer, count has to be inferred instead
      let count = (len - 12) `quot` 20

      elements <- replicateSmallArrayParser (fromIntegral count) variElementP
      pure $! Vari {..}



-- | Function information.
data Func =
       Func
         { positions     :: !(SmallArray FuncPosition)
         , elements      :: !(SmallArray FuncArguments)
         }
       deriving Show

-- | Same reasoning as with t'VariElement'.
data FuncPosition =
       FuncPosition
         { name        :: !StrgRef
         , occurrences :: !Int32
         , address     :: !Int32
         }
       deriving Show

-- | Arguments each function consumes.
--
--   Note: first argument is always @"arguments"@, then optionally other ones.
data FuncArguments =
       FuncArguments
         { name          :: !StrgRef
         , arguments     :: !(PrimArray StrgRef)
         }
       deriving Show

funcArgumentsP :: Parser Error FuncArguments
funcArgumentsP = do
  argumentCount <- word32LE AbruptEnd
  name          <- stringRef
  arguments     <- replicatePrimArrayA (fromIntegral argumentCount) $ do
                     _argPositionId <- word32LE AbruptEnd
                     stringRef

  pure $! FuncArguments {..}

funcPositionP :: Parser Error FuncPosition
funcPositionP = do
  name        <- stringRef
  occurrences <- int32LE AbruptEnd
  address     <- int32LE AbruptEnd
  pure $! FuncPosition {..}

func :: Decoder Func Func
func =
  strict $ do
    positionCount <- word32LE AbruptEnd
    positions <- replicateSmallArrayParser (fromIntegral positionCount) funcPositionP
    elementCount <- word32LE AbruptEnd
    elements <- replicateSmallArrayParser (fromIntegral elementCount) funcArgumentsP
    pure $! Func {..}



-- | Strings.
newtype Strg =
          Strg
            { strings :: Patricia ShortByteString
            }
          deriving Show

stringP :: Patricia ShortByteString -> Parser Error (Patricia ShortByteString)
stringP pat = do
  _size <- word32LE AbruptEnd
  offset <- bytesRead
  string <- shortByteStringNul AbruptEnd
  pure $! Patricia.insert (fromIntegral offset) string pat

strg :: Decoder Strg Strg
strg = do
  strict $ do
    count <- word32LE AbruptEnd
    _ptrs <- skip (fromIntegral count * 4) AbruptEnd

    let go pat n
          | n <= 0    = pure pat
          | otherwise = do
              pat' <- stringP pat
              go pat' (n - 1)

    strings <- go Patricia.empty count
    offset <- bytesRead
    skip (align 0x80 offset) AbruptEnd
    pure $! Strg {..}

lookupStrgRef :: Strg -> StrgRef -> Maybe ShortByteString
lookupStrgRef (Strg pat) (StrgRef ref) = Patricia.dirtyLookup (fromIntegral ref) pat



-- | Raw textures.
newtype Txtr =
          Txtr
            { elements :: SmallArray TxtrElement
            }
          deriving Show

data TxtrElement =
       TxtrElement
         { unknown1 :: !Word32
         , image    :: !ShortByteString
         }
       deriving Show

data TxtrEntry =
       TxtrEntry
         { unknown1 :: !Word32
         , size     :: !Int
         }
       deriving Show

txtr :: Decoder Txtr Txtr
txtr =
  Decoder $ \name len -> do
    (\parser -> runDecoder (lenient parser) name len) $ do
      global <- bytesRead

      count <- word32LE AbruptEnd
      _ptrs <- skip (fromIntegral count * 4) AbruptEnd
      let delta = fromIntegral count * 12 + 4
          offset = global + delta

          enter acc current unknown1 n
            | n <= 0    = do
                let !el = TxtrEntry unknown1
                            (fromIntegral $ global - fromIntegral current + fromIntegral len)
                pure $! acc [el]

            | otherwise = do
                unknown1' <- word32LE AbruptEnd
                local     <- word32LE AbruptEnd
                let size = fromIntegral $ local - current
                    !el = TxtrEntry {..}

                enter (acc . (:) el) local unknown1' (n - 1)

      entries <- if count <= 0
                   then pure []
                   else do
                     unknown1 <- word32LE AbruptEnd
                     local    <- word32LE AbruptEnd
                     enter id local unknown1 (fromIntegral count - 1 :: Int)

      skip (align 0x80 offset) AbruptEnd

      let slice acc                   []  = pure $! acc []
          slice acc (TxtrEntry {..} : ts) = do
            image <- shortByteString size AbruptEnd
            let !el = TxtrElement {..}
            slice (acc . (:) el) ts

      elementList <- slice id entries
      let !elements = smallArrayFromListN (fromIntegral count) elementList

      pure $! Txtr {..}



-- | Raw audio files.
newtype Audo =
          Audo
            { tracks :: SmallArray ShortByteString
            }
          deriving Show

audo :: Decoder Audo Audo
audo =
  Decoder $ \name len -> do
    (\parser -> runDecoder (strict parser) name len) $ do
      count <- word32LE AbruptEnd
      _ptrs <- skip (fromIntegral count * 4) AbruptEnd

      let go acc i = do
            size <- word32LE AbruptEnd
            track <- shortByteString (fromIntegral size) AbruptEnd

            unless (i == 0) $
              skip (align 0x04 $ fromIntegral size) AbruptEnd `catch` \_ -> pure ()

            if i <= 0
              then pure $! acc [track]
              else go (acc . (:) track) (i - 1)

      trackList <- go id (fromIntegral count - 1 :: Int)
      let !tracks = smallArrayFromListN (fromIntegral count) trackList

      pure $! Audo {..}
