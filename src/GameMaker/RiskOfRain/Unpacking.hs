{-|
    This module is a set of datatypes the Risk of Rain GameMaker data file parses into.

    The structure is very similar to the Undertale build, yet there are minor differences.
 -}

{-# LANGUAGE BangPatterns
           , DataKinds
           , DuplicateRecordFields
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , NoFieldSelectors
           , OverloadedRecordDot
           , OverloadedStrings
           , QuantifiedConstraints
           , RecordWildCards
           , StandaloneDeriving
           , TemplateHaskell
           , TypeApplications
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

module GameMaker.RiskOfRain.Unpacking
  ( Result (..)
  , Stream (..)

  , Chunk (..)
  , Chunks (..)
  , parseChunks

  , Gen8 (..)
  , parseGen8

  , Optn

  , Extn (..)
  , Extn2 (..)
  , ExtnTriplet (..)
  , ExtnSegment (..)
  , ExtnOperation (..)
  , parseExtn

  , Sond (..)
  , SondElement (..)
  , parseSond

  , Agrp

  , Sprt (..)
  , SprtElement (..)
  , parseSprt

  , Bgnd (..)
  , BgndElement (..)
  , parseBgnd

  , Path

  , Scpt (..)
  , ScptBinding (..)
  , parseScpt

  , Shdr

  , Font (..)
  , FontElement (..)
  , FontBit (..)
  , parseFont

  , Tmln

  , Objt (..)
  , ObjtElement (..)
  , ShapePoint (..)
  , ObjtEvent (..)
  , ObjtAction (..)
  , parseObjt

  , Room (..)
  , RoomElement (..)
  , RGBA (..)
  , RoomBackground (..)
  , RoomView (..)
  , RoomObject (..)
  , RoomTile (..)
  , parseRoom

  , Dafl

  , Tpag (..)
  , TpagElement (..)
  , parseTpag

  , Code (..)
  , CodeFunction (..)
  , parseCode

  , Vari (..)
  , VariElement (..)
  , parseVari

  , Func (..)
  , Func2 (..)
  , FuncPosition (..)
  , FuncArguments (..)
  , parseFunc

  , Strg (..)
  , parseStrg

  , Txtr (..)
  , TxtrElement (..)
  , parseTxtr

  , Audo (..)
  , parseAudo
  ) where

import           Control.Monad
import           Data.Binary.Get hiding (remaining)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Strict
import           Data.ByteString.Lazy (LazyByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import           Data.ByteString.Short (ShortByteString, toShort)
import           Data.Char (chr)
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Int
import           Data.Word
import           Optics.TH



data Result a = Success a
              | Failure ByteOffset String
                deriving Show

instance Functor Result where
  fmap f (Success a)   = Success (f a)
  fmap _ (Failure o e) = Failure o e

instance Applicative Result where
  pure = Success

  Success f   <*> Success a   = Success (f a)
  Failure o e <*> _           = Failure o e
  _           <*> Failure o e = Failure o e

instance Monad Result where
  Success a   >>= m = m a
  Failure o e >>= _ = Failure o e

parse :: Get a -> LazyByteString -> Result a
parse get datum =
  case runGetOrFail get datum of
    Right (_, _, a) -> Success a
    Left  (_, o, e) -> Failure o e



data Stream a m r = Yield a (Stream a m r)
                  | Effect (m (Stream a m r))
                  | End r

deriving instance (Show a, forall x. Show x => Show (m x), Show r) => Show (Stream a m r)

streamDict
  :: Get a
  -> (LazyByteString -> ByteOffset -> Result r)
  -> Int
  -> LazyByteString
  -> ByteOffset
  -> Stream a Result r
streamDict f r = go
  where
    go n bs !o
      | n <= 0    = Effect $ End <$> r bs o
      | otherwise =
          case runGetOrFail f bs of
            Right (cs, d, a) -> Yield a $ go (n - 1) cs (o + d)
            Left (_, d, err) -> Effect $ Failure (o + d) err



data Chunk a =
       Chunk
         { offset :: Int
         , datum  :: LazyByteString
         }

instance Show (Chunk a) where
  showsPrec d (Chunk l _) =
    showParen (d > 10) $ showString "Chunk " . shows l . showString " _"

data Chunks =
       Chunks
         { gen8 :: Chunk Gen8
         , optn :: Chunk Optn
         , extn :: Chunk Extn
         , sond :: Chunk Sond
         , agrp :: Chunk Agrp
         , sprt :: Chunk Sprt
         , bgnd :: Chunk Bgnd
         , path :: Chunk Path
         , scpt :: Chunk Scpt
         , shdr :: Chunk Shdr
         , font :: Chunk Font
         , tmln :: Chunk Tmln
         , objt :: Chunk Objt
         , room :: Chunk Room
         , dafl :: Chunk Dafl
         , tpag :: Chunk Tpag
         , code :: Chunk Code
         , vari :: Chunk Vari
         , func :: Chunk Func
         , strg :: Chunk Strg
         , txtr :: Chunk Txtr
         , audo :: Chunk Audo
         }
       deriving Show



parseChunks :: Lazy.ByteString -> Result Chunks
parseChunks = parse getChunks

chunk :: ByteString -> Get (Chunk a)
chunk ref = do
  offset <- bytesRead
  name <- getByteString 4
  if name == ref
    then do
      len <- getWord32le
      datum <- getLazyByteString $ fromIntegral len
      pure $ Chunk (fromIntegral offset) datum

    else fail $ "Not a " <> Strict.unpack ref

getChunks :: Get Chunks
getChunks = do
  name <- getByteString 4
  if name == "FORM"
    then do
      _len <- getWord32le
      gen8 <- chunk "GEN8"
      optn <- chunk "OPTN"
      extn <- chunk "EXTN"
      sond <- chunk "SOND"
      agrp <- chunk "AGRP"
      sprt <- chunk "SPRT"
      bgnd <- chunk "BGND"
      path <- chunk "PATH"
      scpt <- chunk "SCPT"
      shdr <- chunk "SHDR"
      font <- chunk "FONT"
      tmln <- chunk "TMLN"
      objt <- chunk "OBJT"
      room <- chunk "ROOM"
      dafl <- chunk "DAFL"
      tpag <- chunk "TPAG"
      code <- chunk "CODE"
      vari <- chunk "VARI"
      func <- chunk "FUNC"
      strg <- chunk "STRG"
      txtr <- chunk "TXTR"
      audo <- chunk "AUDO"

      end <- isEmpty
      if end
        then pure Chunks {..}
        else fail "Trailing data"

    else fail "Not a FORM"



-- | Curious information about the videogame.
data Gen8 =
       Gen8
         { unknown1       :: Word32
         , name           :: ShortByteString
         , filename       :: ShortByteString
         , unknown2       :: Word32
         , unknown3       :: Word32
         , unknown4       :: Word32
         , unknown5       :: Word32
         , unknown6       :: Word32
         , unknown7       :: Word32
         , unknown8       :: Word32
         , name2          :: ShortByteString
         , major          :: Word32
         , minor          :: Word32
         , release        :: Word32
         , build          :: Word32
         , defaultHeight  :: Word32
         , defaultWidth   :: Word32
         , info           :: Word32
         , licenseMD5     :: ShortByteString
         , licenseCRC32   :: Word32
         , timestamp      :: UTCTime
         , unknown9       :: Word32
         , displayName    :: ShortByteString
         , unknown10      :: Word32
         , unknown11      :: Word32
         , unknown12      :: Word32
         , unknown13      :: Word32
         , unknown14      :: Word32
         , unknown15      :: Word32
         , roomCount      :: Word32
         , rooms          :: [Word32]
         }
       deriving Show

parseGen8 :: Chunk Strg -> Chunk Gen8 -> Result Gen8
parseGen8 strg (Chunk _offset datum) =
  case runGetOrFail get datum of
    Right (_, _, a) -> Success a
    Left  (_, o, e) -> Failure o e
  where
    get = do
      unknown1      <- getWord32le
      name          <- getString strg
      filename      <- getString strg
      unknown2      <- getWord32le
      unknown3      <- getWord32le
      unknown4      <- getWord32le
      unknown5      <- getWord32le
      unknown6      <- getWord32le
      unknown7      <- getWord32le
      unknown8      <- getWord32le
      name2         <- getString strg
      major         <- getWord32le
      minor         <- getWord32le
      release       <- getWord32le
      build         <- getWord32le
      defaultHeight <- getWord32le
      defaultWidth  <- getWord32le
      info          <- getWord32le
      licenseMD5    <- toShort <$> getByteString 16
      licenseCRC32  <- getWord32le
      timestamp     <- systemToUTCTime . flip MkSystemTime 0 . fromIntegral <$> getWord32le
      unknown9      <- getWord32le
      displayName   <- getString strg
      unknown10     <- getWord32le
      unknown11     <- getWord32le
      unknown12     <- getWord32le
      unknown13     <- getWord32le
      unknown14     <- getWord32le
      unknown15     <- getWord32le
      roomCount     <- getWord32le
      rooms         <- replicateM (fromIntegral roomCount) getWord32le

      end <- isEmpty
      if end
         then pure Gen8 {..}
         else fail "Trailing GEN8 data"



-- | To little information to meaningfully structure.
--   Identical between Linux and Windows versions.
data Optn



-- | Seemingly bindings between GameMaker IO operations and system ones
--   (just based on the names pointed to), however most of this data
--   is just ones and twos scattered around.
data Extn =
       Extn
         { count    :: Word32
         , triplets :: Stream ExtnTriplet Result Extn2
         }
       deriving Show

data Extn2 =
       Extn2
         { count    :: Word32
         , segments :: Stream ExtnSegment Result LazyByteString
         }
       deriving Show

data ExtnTriplet =
       ExtnTriplet
         { unknown1 :: ShortByteString
         , unknown2 :: ShortByteString
         , unknown3 :: ShortByteString
         }
       deriving Show

data ExtnSegment =
       ExtnSegment
         { name           :: ExtnTriplet
         , unknown1       :: Word32
         , operationCount :: Word32
         , operations     :: [ExtnOperation]
         }
       deriving Show

data ExtnOperation =
       ExtnOperation
         { operationFs   :: ShortByteString
         , operationId   :: Word32
         , unknown1      :: Word32
         , unknown2      :: Word32
         , operation     :: ShortByteString
         , unknown3Count :: Word32
         , unknown3      :: [Word32]
         }
       deriving Show

parseExtn :: Chunk Strg -> Chunk Extn -> Result Extn
parseExtn strg (Chunk _offset datum) =
  case runGetOrFail measure1 datum of
    Right (bs, o, extn) -> pure $ extn bs o
    Left (_, o, err)    -> Failure o err
  where
    measure1 = do
      count <- getWord32le
      _ptrs <- skip $ fromIntegral count * 4
      pure $ \bs o ->
        Extn
          { count    = count
          , triplets = streamDict (getExtnTriplet strg) more (fromIntegral count) bs o
          }

    measure2 = do
      count <- getWord32le
      _ptrs <- skip $ fromIntegral count * 4
      pure $ \bs o ->
        Extn2
          { count    = count
          , segments = streamDict (getExtnSegment strg) (\cs _ -> pure cs) (fromIntegral count) bs o
          }

    more bs o =
      case runGetOrFail measure2 bs of
        Right (cs, p, extn) -> pure $ extn cs (o + p)
        Left (_, p, err)    -> Failure (o + p) err

getExtnTriplet :: Chunk Strg -> Get ExtnTriplet
getExtnTriplet strg = do
  unknown1 <- getString strg
  unknown2 <- getString strg
  unknown3 <- getString strg
  pure ExtnTriplet {..}

getExtnSegment :: Chunk Strg -> Get ExtnSegment
getExtnSegment strg = do
  name           <- getExtnTriplet strg
  unknown1       <- getWord32le

  operationCount <- getWord32le
  _ptrs <- skip $ fromIntegral operationCount * 4
  operations     <- replicateM (fromIntegral operationCount) (getExtnOperation strg)

  pure ExtnSegment {..}

getExtnOperation :: Chunk Strg -> Get ExtnOperation
getExtnOperation strg = do
  operationFs   <- getString strg
  operationId   <- getWord32le
  unknown1      <- getWord32le
  unknown2      <- getWord32le
  operation     <- getString strg
  unknown3Count <- getWord32le
  unknown3      <- replicateM (fromIntegral unknown3Count) getWord32le
  pure ExtnOperation {..}



-- | Sound files and information.
data Sond =
       Sond
         { count    :: Word32
         , elements :: Stream SondElement Result ()
         }
       deriving Show

data SondElement =
       SondElement
         { name       :: ShortByteString
         , flags      :: Word32
         , extension  :: ShortByteString
         , filename   :: ShortByteString
         , unknown1   :: Word32          -- ^ Always zero
         , volume     :: Float
         , pitch      :: Float
         , groupId    :: Int32
         , identifier :: Int32
         }
       deriving Show

parseSond :: Chunk Strg -> Chunk Sond -> Result Sond
parseSond strg (Chunk _offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, sond) -> pure $ sond bs o
    Left (_, o, err)     -> Failure o err
  where
    measure = do
      count <- getWord32le
      _ptrs <- skip $ fromIntegral count * 4
      pure $ \bs o ->
        Sond
          { count    = count
          , elements = streamDict (getSondElement strg) edge (fromIntegral count) bs o
          }

    edge bs o
      | Lazy.null bs = Success ()
      | otherwise    = Failure o "Trailing SOND data"

getSondElement :: Chunk Strg -> Get SondElement
getSondElement strg = do
  name       <- getString strg
  flags      <- getWord32le
  extension  <- getString strg
  filename   <- getString strg
  unknown1   <- getWord32le
  volume     <- getFloatle
  pitch      <- getFloatle
  groupId    <- getInt32le
  identifier <- getInt32le
  pure SondElement {..}



-- | Empty dictionary in both files
data Agrp



-- | Foreground sprites with all the masks and stuff.
data Sprt =
       Sprt
         { count    :: Word32
         , elements :: Stream SprtElement Result ()
         }
       deriving Show

data SprtElement =
       SprtElement
         { name         :: ShortByteString
         , width        :: Int32
         , height       :: Int32
         , marginLeft   :: Int32
         , marginRight  :: Int32
         , marginTop    :: Int32
         , marginBottom :: Int32
         , unknown1     :: Int32             -- Always zero
         , unknown2     :: Int32             -- Always zero
         , unknown3     :: Int32             -- Always zero
         , bBoxMode     :: Int32
         , sepMasks     :: Int32
         , originX      :: Int32
         , originY      :: Int32
         , textureCount :: Word32
         , textures     :: [TpagElement]
         , maskCount    :: Word32
         , masks        :: [Lazy.ByteString]
         }
       deriving Show

parseSprt :: Chunk Strg -> Chunk Tpag -> Chunk Sprt -> Result Sprt
parseSprt strg tpag (Chunk _offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, sprt) -> pure $ sprt bs o
    Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      skip $ fromIntegral count * 4
      pure $ \bs o ->
        Sprt
          { count    = count
          , elements = streamDict (getSprtElement strg tpag) edge (fromIntegral count) bs o
          }

    edge bs o
      | Lazy.null bs = Success ()
      | otherwise    = Failure o "Trailing SOND data"

getSprtElement :: Chunk Strg -> Chunk Tpag -> Get SprtElement
getSprtElement strg tpag = do
  name         <- getString strg
  width        <- getInt32le
  height       <- getInt32le
  marginLeft   <- getInt32le
  marginRight  <- getInt32le
  marginTop    <- getInt32le
  marginBottom <- getInt32le
  unknown1     <- getInt32le
  unknown2     <- getInt32le
  unknown3     <- getInt32le
  bBoxMode     <- getInt32le
  sepMasks     <- getInt32le
  originX      <- getInt32le
  originY      <- getInt32le

  textureCount <- getWord32le
  textures     <- replicateM (fromIntegral textureCount) (getTpagElement tpag)

  maskCount    <- getWord32le
  masks        <- replicateM (fromIntegral maskCount) $
                    let ~(q, r) = quotRem (fromIntegral width :: Int) 8

                        width8 | r == 0    = q
                               | otherwise = q + 1

                    in getLazyByteString $ fromIntegral width8 * fromIntegral height

  bytes <- bytesRead
  skip $ case fromIntegral bytes `rem` 4 of
            0 -> 0
            n -> 4 - n

  pure SprtElement {..}



-- | Background sprites.
data Bgnd =
       Bgnd
         { count    :: Word32
         , elements :: Stream BgndElement Result ()
         }
       deriving Show

data BgndElement =
       BgndElement
         { name     :: ShortByteString
         , unknown1 :: Word32           -- ^ Always zero
         , unknown2 :: Word32           -- ^ Always zero
         , unknown3 :: Word32           -- ^ Always zero
         , texture  :: TpagElement
         }
       deriving Show

parseBgnd :: Chunk Strg -> Chunk Tpag -> Chunk Bgnd -> Result Bgnd
parseBgnd strg tpag (Chunk _offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, bgnd) -> pure $ bgnd bs o
    Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      _ptrs <- skip $ fromIntegral count * 4
      pure $ \bs o ->
        Bgnd
          { count    = count
          , elements = streamDict (getBgndElement strg tpag) edge (fromIntegral count) bs o
          }

    edge bs o
      | Lazy.null bs = Success ()
      | otherwise    = Failure o "Trailing BGND data"

getBgndElement :: Chunk Strg -> Chunk Tpag -> Get BgndElement
getBgndElement strg tpag = do
  name         <- getString strg
  unknown1     <- getWord32le
  unknown2     <- getWord32le
  unknown3     <- getWord32le
  texture      <- getTpagElement tpag
  pure BgndElement {..}



-- | Empty dictionary in both files
data Path



-- | Bindings of script functions to identifiers.
--
--   I suppose this is an extremely elaborate strategy to bind 'CodeFunction's to
--   'FuncPosition's or something, however the difference between the two is just a prefix.
data Scpt =
       Scpt
         { count    :: Word32
         , bindings :: Stream ScptBinding Result ()
         }
       deriving Show

data ScptBinding =
       ScptBinding
         { pointer    :: ShortByteString
         , identifier :: Word32
         }
       deriving Show

parseScpt :: Chunk Strg -> Chunk Scpt -> Result Scpt
parseScpt strg (Chunk _offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, scpt) -> pure $ scpt bs o
    Left (_, o, err)     -> Failure o err
  where
    measure = do
      count <- getWord32le
      _ptrs <- skip $ fromIntegral count * 4
      pure $ \bs o ->
        Scpt
          { count    = count
          , bindings = streamDict (getScptBinding strg) edge (fromIntegral count) bs o
          }

    edge bs o
      | Lazy.null bs = Success ()
      | otherwise    = Failure o "Trailing SCPT data"

getScptBinding :: Chunk Strg -> Get ScptBinding
getScptBinding strg = do
  pointer      <- getString strg
  identifier   <- getWord32le
  pure ScptBinding {..}




-- | Shader descriptions. No useful information here other than pointers to files in STRG.
data Shdr



-- | Font descriptions because GameMaker can't tug around font files, so they're shoved
--   in textures.
data Font =
       Font
         { count    :: Word32
         , elements :: Stream FontElement Result LazyByteString
         }
       deriving Show

data FontElement =
       FontElement
         { kind           :: ShortByteString
         , name           :: ShortByteString
         , emSize         :: Word32
         , bold           :: Word32
         , italic         :: Word32
         , rangeStart     :: Word16
         , charset        :: Word8
         , antialiasing   :: Word8
         , rangeEnd       :: Word32
         , texture        :: TpagElement
         , scaleX         :: Float
         , scaleY         :: Float
         , characterCount :: Word32
         , characters     :: [FontBit]
         }
       deriving Show

data FontBit =
       FontBit
         { character :: Char
         , offsetX   :: Int16
         , offsetY   :: Int16
         , width     :: Int16
         , height    :: Int16
         , advance   :: Int16
         , bearingX  :: Int16
         , bearingY  :: Int16
         }
       deriving Show

parseFont :: Chunk Strg -> Chunk Tpag -> Chunk Font -> Result Font
parseFont strg tpag (Chunk _offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, font) -> pure $ font bs o
    Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      _ptrs <- skip $ fromIntegral count * 4
      pure $ \bs o ->
        Font
          { count    = count
          , elements = streamDict (getFontElement strg tpag) (\cs _ -> pure cs) (fromIntegral count) bs o
          }

getFontElement :: Chunk Strg -> Chunk Tpag -> Get FontElement
getFontElement strg tpag = do
  kind           <- getString strg
  name           <- getString strg
  emSize         <- getWord32le
  bold           <- getWord32le
  italic         <- getWord32le
  rangeStart     <- getWord16le
  charset        <- getWord8
  antialiasing   <- getWord8
  rangeEnd       <- getWord32le
  texture        <- getTpagElement tpag
  scaleX         <- getFloatle
  scaleY         <- getFloatle

  characterCount <- getWord32le
  _ptrs2         <- skip $ fromIntegral characterCount * 4
  characters     <- replicateM (fromIntegral characterCount) getFontBit

  pure FontElement {..}

getFontBit :: Get FontBit
getFontBit = do
  character <- chr . fromIntegral <$> getWord16le
  offsetX   <- getInt16le
  offsetY   <- getInt16le
  width     <- getInt16le
  height    <- getInt16le
  advance   <- getInt16le
  bearingX  <- getInt16le
  bearingY  <- getInt16le
  pure FontBit {..}



-- | Empty dictionary in both files
data Tmln



-- | Pretty much garbage. Extremely bulky, confusing and utterly undecryptable since
--   Risk of Rain barely uses physics at all.
data Objt =
       Objt
         { count    :: Word32
         , elements :: Stream ObjtElement Result ()
         }
       deriving Show

-- | The only thing from here we know for sure are name and sprite index
data ObjtElement =
       ObjtElement
         { name            :: ShortByteString
         , spriteIndex     :: Int32
         , visible         :: Int32
         , solid           :: Int32
         , depth           :: Int32
         , persistent      :: Int32
         , parentId        :: Int32
         , textureMaskId   :: Int32
         , unknown1        :: Int32
         , unknown2        :: Int32
         , unknown3        :: Int32
         , unknown4        :: Float
         , unknown5        :: Float
         , unknown6        :: Float
         , unknown7        :: Float
         , unknown8        :: Float
         , shapePointCount :: Int32
         , unknown9        :: Float
         , unknown10       :: Int32
         , unknown11       :: Int32
         , shapePoints     :: [ShapePoint]
         , unknown12Count  :: Word32          -- ^ Always 12
         , unknown12       :: [ObjtEvent]
         }
       deriving Show

data ShapePoint =
       ShapePoint
         { x :: Float
         , y :: Float
         }
       deriving Show

data ObjtEvent =
       ObjtEvent
         { count    :: Word32
         , elements :: [ObjtAction]
         }
       deriving Show

data ObjtAction =
       ObjtAction
         { unknown1   :: Int32
         , unknown2   :: Int32
         , unknown3   :: Int32           -- Points eight bytes ahead
         , unknown4   :: Int32
         , unknown5   :: Int32
         , unknown6   :: Int32
         , unknown7   :: Int32
         , unknown8   :: Int32
         , unknown9   :: Int32
         , unknown10  :: Int32
         , unknown11  :: ShortByteString -- Points to an empty string
         , identifier :: Int32
         , unknown12  :: Int32
         , unknown13  :: Int32
         , unknown14  :: Int32
         , unknown15  :: Int32
         , unknown16  :: Int32
         }
       deriving Show

parseObjt :: Chunk Strg -> Chunk Objt -> Result Objt
parseObjt strg (Chunk _offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, objt) -> Success $ objt bs o
    Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      _ptrs <- skip $ fromIntegral count * 4
      pure $ \bs o ->
        Objt
          { count    = count
          , elements = streamDict (getObjtElement strg) edge (fromIntegral count) bs o
          }

    edge bs o
      | Lazy.null bs = Success ()
      | otherwise    = Failure o "Trailing OBJT data"

getObjtElement :: Chunk Strg -> Get ObjtElement
getObjtElement strg = do
  name            <- getString strg
  spriteIndex     <- getInt32le
  visible         <- getInt32le
  solid           <- getInt32le
  depth           <- getInt32le
  persistent      <- getInt32le
  parentId        <- getInt32le
  textureMaskId   <- getInt32le
  unknown1        <- getInt32le
  unknown2        <- getInt32le
  unknown3        <- getInt32le
  unknown4        <- getFloatle
  unknown5        <- getFloatle
  unknown6        <- getFloatle
  unknown7        <- getFloatle
  unknown8        <- getFloatle
  shapePointCount <- getInt32le
  unknown9        <- getFloatle
  unknown10       <- getInt32le
  unknown11       <- getInt32le
  shapePoints     <- replicateM (fromIntegral shapePointCount) getShapePoint

  unknown12Count  <- getWord32le
  _ptrs <- skip $ fromIntegral unknown12Count * 4
  unknown12       <- replicateM (fromIntegral unknown12Count) (getObjtEvent strg)

  pure ObjtElement {..}

getShapePoint :: Get ShapePoint
getShapePoint = do
  x <- getFloatle
  y <- getFloatle
  pure ShapePoint {..}

getObjtEvent :: Chunk Strg -> Get ObjtEvent
getObjtEvent strg = do
  count    <- getWord32le
  _ptrs <- skip $ fromIntegral count * 4
  elements <- replicateM (fromIntegral count) (getObjtAction strg)
  pure ObjtEvent {..}

getObjtAction :: Chunk Strg -> Get ObjtAction
getObjtAction strg = do
  unknown1   <- getInt32le
  unknown2   <- getInt32le
  unknown3   <- getInt32le
  unknown4   <- getInt32le
  unknown5   <- getInt32le
  unknown6   <- getInt32le
  unknown7   <- getInt32le
  unknown8   <- getInt32le
  unknown9   <- getInt32le
  unknown10  <- getInt32le
  unknown11  <- getString strg
  identifier <- getInt32le
  unknown12  <- getInt32le
  unknown13  <- getInt32le
  unknown14  <- getInt32le
  unknown15  <- getInt32le
  unknown16  <- getInt32le
  pure ObjtAction {..}



-- | Room data.
data Room =
       Room
         { count    :: Word32
         , elements :: Stream RoomElement Result ()
         }
       deriving Show

data RoomElement =
       RoomElement
         { name            :: ShortByteString
         , caption         :: ShortByteString
         , width           :: Word32
         , height          :: Word32
         , speed           :: Word32
         , persistent      :: Int32
         , rgba            :: RGBA
         , drawBGColor     :: Int32
         , unknown1        :: Word32
         , flags           :: Word32
         , bgOffset        :: Word32
         , viewOffset      :: Word32
         , objOffset       :: Word32
         , tileOffset      :: Word32
         , world           :: Word32
         , top             :: Word32
         , left            :: Word32
         , right           :: Word32
         , bottom          :: Word32
         , gravityX        :: Float
         , gravityY        :: Float
         , metersPerPixel  :: Float
         , backgroundCount :: Word32
         , backgrounds     :: [RoomBackground]
         , viewCount       :: Word32
         , views           :: [RoomView]
         , objectCount     :: Word32
         , objects         :: [RoomObject]
         , tileCount       :: Word32
         , tiles           :: [RoomTile]
         }
       deriving Show

data RGBA = RGBA Word8 Word8 Word8 Word8
            deriving Show

data RoomBackground =
       RoomBackground
         { enabled    :: Int32
         , foreground :: Int32
         , bgDefIndex :: Int32
         , x          :: Int32
         , y          :: Int32
         , tileX      :: Int32
         , tileY      :: Int32
         , speedX     :: Int32
         , speedY     :: Int32
         , identifier :: Int32
         }
       deriving Show

data RoomView =
       RoomView
         { enabled    :: Int32
         , viewX      :: Int32
         , viewY      :: Int32
         , viewWidth  :: Int32
         , viewHeight :: Int32
         , portX      :: Int32
         , portY      :: Int32
         , portWidth  :: Int32
         , portHeight :: Int32
         , borderX    :: Int32
         , borderY    :: Int32
         , speedX     :: Int32
         , speedY     :: Int32
         , identifier :: Int32
         }
       deriving Show

data RoomObject =
       RoomObject
         { x          :: Int32
         , y          :: Int32
         , identifier :: Int32
         , initCode   :: Int32
         , unknown5   :: Int32
         , scaleX     :: Float
         , scaleY     :: Float
         , unknown8   :: Int32
         , rotation   :: Float
         }
       deriving Show

data RoomTile =
       RoomTile
         { x          :: Int32
         , y          :: Int32
         , bgDefIndex :: Int32
         , sourceX    :: Int32
         , sourceY    :: Int32
         , width      :: Int32
         , height     :: Int32
         , tileDepth  :: Int32
         , identifier :: Int32
         , scaleX     :: Float
         , scaleY     :: Float
         , unknown12  :: Int32
         }
       deriving Show

parseRoom :: Chunk Strg -> Chunk Room -> Result Room
parseRoom strg (Chunk _offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, room) -> Success $ room bs o
    Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      _ptrs <- skip $ fromIntegral count * 4
      pure $ \bs o ->
        Room
          { count    = count
          , elements = streamDict (getRoomElement strg) edge (fromIntegral count) bs o
          }

    edge bs o
      | Lazy.null bs = Success ()
      | otherwise    = Failure o "Trailing ROOM data"

getRoomElement :: Chunk Strg -> Get RoomElement
getRoomElement strg = do
  name            <- getString strg
  caption         <- getString strg
  width           <- getWord32le
  height          <- getWord32le
  speed           <- getWord32le
  persistent      <- getInt32le

  -- All other integers are little-endian, so this could be interpreted as ABGR
  r               <- getWord8
  g               <- getWord8
  b               <- getWord8
  a               <- getWord8
  let rgba = RGBA r g b a

  drawBGColor     <- getInt32le
  unknown1        <- getWord32le
  flags           <- getWord32le
  bgOffset        <- getWord32le
  viewOffset      <- getWord32le
  objOffset       <- getWord32le
  tileOffset      <- getWord32le
  world           <- getWord32le
  top             <- getWord32le
  left            <- getWord32le
  right           <- getWord32le
  bottom          <- getWord32le
  gravityX        <- getFloatle
  gravityY        <- getFloatle
  metersPerPixel  <- getFloatle

  backgroundCount <- getWord32le
  _ptrs <- skip $ fromIntegral backgroundCount * 4
  backgrounds     <- replicateM (fromIntegral backgroundCount) getRoomBackground

  viewCount       <- getWord32le
  _ptrs <- skip $ fromIntegral viewCount * 4
  views           <- replicateM (fromIntegral viewCount) getRoomView

  objectCount     <- getWord32le
  _ptrs <- skip $ fromIntegral objectCount * 4
  objects         <- replicateM (fromIntegral objectCount) getRoomObject

  tileCount       <- getWord32le
  _ptrs <- skip $ fromIntegral tileCount * 4
  tiles           <- replicateM (fromIntegral tileCount) getRoomTile

  pure RoomElement {..}

getRoomBackground :: Get RoomBackground
getRoomBackground = do
  enabled    <- getInt32le
  foreground <- getInt32le
  bgDefIndex <- getInt32le
  x          <- getInt32le
  y          <- getInt32le
  tileX      <- getInt32le
  tileY      <- getInt32le
  speedX     <- getInt32le
  speedY     <- getInt32le
  identifier <- getInt32le
  pure RoomBackground {..}

getRoomView :: Get RoomView
getRoomView = do
  enabled    <- getInt32le
  viewX      <- getInt32le
  viewY      <- getInt32le
  viewWidth  <- getInt32le
  viewHeight <- getInt32le
  portX      <- getInt32le
  portY      <- getInt32le
  portWidth  <- getInt32le
  portHeight <- getInt32le
  borderX    <- getInt32le
  borderY    <- getInt32le
  speedX     <- getInt32le
  speedY     <- getInt32le
  identifier <- getInt32le
  pure RoomView {..}

getRoomObject :: Get RoomObject
getRoomObject = do
  x          <- getInt32le
  y          <- getInt32le
  identifier <- getInt32le
  initCode   <- getInt32le
  unknown5   <- getInt32le
  scaleX     <- getFloatle
  scaleY     <- getFloatle
  unknown8   <- getInt32le
  rotation   <- getFloatle
  pure RoomObject {..}

getRoomTile :: Get RoomTile
getRoomTile = do
  x          <- getInt32le
  y          <- getInt32le
  bgDefIndex <- getInt32le
  sourceX    <- getInt32le
  sourceY    <- getInt32le
  width      <- getInt32le
  height     <- getInt32le
  tileDepth  <- getInt32le
  identifier <- getInt32le
  scaleX     <- getFloatle
  scaleY     <- getFloatle
  unknown12  <- getInt32le
  pure RoomTile {..}



-- | Empty
data Dafl



-- | Sprite information as related to textures they reside in.
data Tpag =
       Tpag
         { count    :: Word32
         , elements :: Stream TpagElement Result ()
         }
       deriving Show

data TpagElement =
       TpagElement
         { offsetX        :: Word16
         , offsetY        :: Word16
         , width          :: Word16
         , height         :: Word16
         , renderX        :: Word16
         , renderY        :: Word16
         , boundingX      :: Word16
         , boundingY      :: Word16
         , boundingWidth  :: Word16
         , boundingHeight :: Word16
         , imageId        :: Word16
         }
       deriving Show

parseTpag :: Chunk Tpag -> Result Tpag
parseTpag (Chunk _offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, tpag) -> Success $ tpag bs o
    Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      _ptrs <- skip $ fromIntegral count * 4
      pure $ \bs o ->
        Tpag
          { count    = count
          , elements = streamDict getTpagElement_ edge (fromIntegral count) bs o
          }

    edge bs o
      | Lazy.null bs = Success ()
      | otherwise    = Failure o "Trailing TPAG data"

getTpagElement :: Chunk Tpag -> Get TpagElement
getTpagElement (Chunk start tpag) = do
  offset <- getWord32le

  let {-# NOINLINE body #-}
      body = toStrict tpag

  case runGetOrFail getTpagElement_
         . fromStrict $ Strict.drop (fromIntegral offset - start - 8) body of
    Right (_, _, el) -> pure el
    Left (_, _, err) -> fail err

getTpagElement_ :: Get TpagElement
getTpagElement_ = do
  offsetX        <- getWord16le
  offsetY        <- getWord16le
  width          <- getWord16le
  height         <- getWord16le
  renderX        <- getWord16le
  renderY        <- getWord16le
  boundingX      <- getWord16le
  boundingY      <- getWord16le
  boundingWidth  <- getWord16le
  boundingHeight <- getWord16le
  imageId        <- getWord16le
  pure TpagElement {..}



-- | Interpreted GameMaker code chunk. Will be empty if the game was compiled.
data Code =
       Code
         { count     :: Word32
         , functions :: Stream CodeFunction Result ()
         }
       deriving Show

data CodeFunction =
       CodeFunction
         { name     :: ShortByteString
         , unknown1 :: Word32
         , bytecode :: Lazy.ByteString
         , unknown2 :: Word32
         }
       deriving Show

parseCode :: Chunk Strg -> Chunk Code -> Result (Maybe Code)
parseCode strg (Chunk offset datum)
  | Lazy.null datum = Success Nothing
  | otherwise       =
      case runGetOrFail measure datum of
        Right (bs, o, tpag) -> tpag bs o
        Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      mayPtr <- if fromIntegral count > (0 :: Int)
                  then do
                    w <- getWord32le
                    skip $ (fromIntegral count - 1) * 4
                    pure $ Just w

                  else pure Nothing

      pure $ \bs o ->
        case mayPtr of
          Just ptr ->
            let delta = fromIntegral ptr - fromIntegral offset - fromIntegral o - 8
                ~(cs, ds) = Lazy.splitAt delta bs

            in Success $
                 Just Code
                        { count     = count
                        , functions = get cs (fromIntegral count :: Int) ds (o + delta)
                        }

          Nothing | Lazy.null bs -> Success . Just $ Code count (End ())
                  | otherwise    -> Failure o "Trailing CODE data"

    get ops n bs !o
      | n <= 0    = End ()
      | otherwise =
          case runGetOrFail (getCodeFunction strg ops) bs of
            Right (cs, p, ~(a, ops')) -> Yield a $ get ops' (n - 1) cs (o + p)
            Left (_, p, err)          -> Effect $ Failure p err

getCodeFunction :: Chunk Strg -> LazyByteString -> Get (CodeFunction, LazyByteString)
getCodeFunction strg ops = do
  name     <- getString strg
  size     <- getWord32le
  unknown1 <- getWord32le
  _offset  <- getInt32le
  let ~(bytecode, ops') = Lazy.splitAt (fromIntegral size) ops
  unknown2 <- getWord32le
  pure (CodeFunction {..}, ops')



-- | Variable names and where they occur in code.
data Vari =
       Vari
         { unknown1 :: Word32
         , unknown2 :: Word32
         , unknown3 :: Word32
         , elements :: Stream VariElement Result ()
         }
       deriving Show

-- | Every address points to the next address and it repeats @occurences@ times.
data VariElement =
       VariElement
         { name        :: ShortByteString
         , unknown1    :: Int32
         , unknown2    :: Int32
         , occurrences :: Int32
         , address     :: Int32
         }
       deriving Show

parseVari :: Chunk Strg -> Chunk Vari -> Result (Maybe Vari)
parseVari strg (Chunk _offset datum)
  | Lazy.null datum = Success Nothing
  | otherwise       =
      case runGetOrFail measure datum of
        Right (bs, o, vari) -> Success . Just $ vari bs o
        Left (_, o, err)    -> Failure o err
  where
    measure = do
      unknown1 <- getWord32le
      unknown2 <- getWord32le
      unknown3 <- getWord32le
      pure $ \bs o ->
        Vari
          { unknown1 = unknown1
          , unknown2 = unknown2
          , unknown3 = unknown3
          , elements = get bs o
          }

    get bs !o
      | Lazy.null bs = End ()
      | otherwise    =
          case runGetOrFail (getVariElement strg) bs of
            Right (cs, p, a) -> Yield a $ get cs (o + p)
            Left (_, p, err) -> Effect $ Failure (o + p) err

getVariElement :: Chunk Strg -> Get VariElement
getVariElement strg = do
  name        <- getString strg
  unknown1    <- getInt32le
  unknown2    <- getInt32le
  occurrences <- getInt32le
  address     <- getInt32le
  pure VariElement {..}



-- | Function information.
data Func =
       Func
         { count     :: Word32
         , positions :: Stream FuncPosition Result Func2
         }
       deriving Show

data Func2 =
       Func2
         { count    :: Word32
         , elements :: Stream FuncArguments Result ()
         }
       deriving Show

-- | Same reasoning as with 'VariElement'.
data FuncPosition =
       FuncPosition
         { name        :: ShortByteString
         , occurrences :: Int32
         , address     :: Int32
         }
       deriving Show

-- | Arguments each function consumes.
--
--   Note: first argument is always @"arguments"@, then optionally other ones.
data FuncArguments =
       FuncArguments
         { name          :: ShortByteString
         , argumentCount :: Word32
         , arguments     :: [ShortByteString]
         }
       deriving Show

parseFunc :: Chunk Strg -> Chunk Func -> Result (Maybe Func)
parseFunc strg (Chunk _offset datum)
  | Lazy.null datum = Success Nothing
  | otherwise       =
      case runGetOrFail measure1 datum of
        Right (bs, o, func) -> Success . Just $ func bs o
        Left (_, o, err)    -> Failure o err
  where
    measure1 = do
      count <- getWord32le
      pure $ \bs o ->
        Func
          { count     = count
          , positions = streamDict (getFuncPosition strg) more (fromIntegral count) bs o
          }

    measure2 = do
      count <- getWord32le
      pure $ \bs o ->
        Func2
          { count    = count
          , elements = streamDict (getFuncArguments strg) edge (fromIntegral count) bs o
          }

    more bs o =
      case runGetOrFail measure2 bs of
        Right (cs, p, func2) -> pure $ func2 cs (o + p)
        Left (_, p, err)     -> Failure (o + p) err

    edge bs o
      | Lazy.null bs = Success ()
      | otherwise    = Failure o "Trailing TPAG data"

getFuncPosition :: Chunk Strg -> Get FuncPosition
getFuncPosition strg = do
  name        <- getString strg
  occurrences <- getInt32le
  address     <- getInt32le
  pure FuncPosition {..}

getFuncArguments :: Chunk Strg -> Get FuncArguments
getFuncArguments strg = do
  argumentCount <- getWord32le
  name <- getString strg
  arguments     <- replicateM (fromIntegral argumentCount) $ do
                     _argPositionId <- getWord32le
                     getString strg

  pure FuncArguments {..}



-- | Strings.
data Strg =
       Strg
         { count   :: Word32
         , strings :: Stream ShortByteString Result LazyByteString
         }
       deriving Show

parseStrg :: Chunk Strg -> Result Strg
parseStrg (Chunk _offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, strg) -> pure $ strg bs o
    Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      let count_ = fromIntegral count
      _ptrs <- skip $ count_ * 4
      pure $ \bs o ->
        Strg
          { count   = count
          , strings = streamDict getString_ (\cs _ -> pure cs) (fromIntegral count) bs o
          }

getString_ :: Get ShortByteString
getString_ = do
  size <- getWord32le
  string <- getLazyByteString (fromIntegral size)
  _ <- getWord8 -- '\NUL'
  pure . toShort $ toStrict string

getString :: Chunk Strg -> Get ShortByteString
getString (Chunk start strg) = do
  offset <- getWord32le

  let {-# NOINLINE body #-}
      body = toStrict strg

      get = do
        len <- getWord32le
        getLazyByteString (fromIntegral len)

  case runGetOrFail get
         . fromStrict $ Strict.drop (fromIntegral offset - start - 12) body of
    Right (_, _, short) -> pure . toShort $ toStrict short
    Left (_, _, err)    -> fail err



-- | Raw textures.
data Txtr =
       Txtr
         { count    :: Word32
         , elements :: Stream TxtrElement Result ()
         }
       deriving Show

data TxtrElement =
       TxtrElement
         { unknown1 :: Word32
         , image    :: LazyByteString
         }
       deriving Show

parseTxtr :: Chunk Txtr -> Result Txtr
parseTxtr (Chunk offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, txtr) -> Success $ txtr bs o
    Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      if count <= 0
        then pure $ \_ _ -> Txtr 0 (End ())
        else do
          _ptrs <- skip $ fromIntegral count * 4
          (unknown1, ptr) <- getTxtrElement

          pure $ \bs o ->
            let ~(cs, ds) = Lazy.splitAt (fromIntegral ptr - fromIntegral offset - o - 8) bs
            in Txtr
                 { count    = count
                 , elements = get unknown1 ptr ds (fromIntegral count - 1 :: Int) cs o
                 }

    get unknown1 ptr txs n bs !o
      | n <= 0    = Yield (TxtrElement unknown1 txs) $ End ()
      | otherwise = 
          case runGetOrFail getTxtrElement bs of
            Right (cs, p, ~(unknown1', ptr')) ->
              let ~(tex, rest) = Lazy.splitAt (fromIntegral ptr' - fromIntegral ptr) txs
              in Yield (TxtrElement unknown1 tex) $
                   get unknown1' ptr' rest (n - 1) cs (o + p)

            Left (_, p, err) -> Effect $ Failure (o + p) err

getTxtrElement :: Get (Word32, Word32)
getTxtrElement = do
  unknown1 <- getWord32le
  ptr      <- getWord32le
  pure (unknown1, ptr)



-- | Raw audio files.
data Audo =
       Audo
         { count :: Word32
         , files :: Stream LazyByteString Result ()
         }
       deriving Show

parseAudo :: Chunk Audo -> Result Audo
parseAudo (Chunk offset datum) =
  case runGetOrFail measure datum of
    Right (bs, o, audo) -> pure $ audo bs o
    Left (_, o, err)    -> Failure o err
  where
    measure = do
      count <- getWord32le
      if count <= 0
        then pure $ \_ _ -> Audo count (End ())
        else do
          ptr <- getWord32le
          pure $ \bs o ->                                     -- // or was it Length + 4?
            let ~(cs, ds) = Lazy.splitAt (fromIntegral ptr - fromIntegral offset - o - 4) bs
            in Audo
                 { count = count
                 , files = get ptr ds (fromIntegral count - 1 :: Int) cs o
                 }

    get ptr aus n bs !o
      | n <= 0    = Yield aus $ End ()
      | otherwise = 
          case runGetOrFail getWord32le bs of
            Right (cs, p, ptr') ->
              let ~(au, rest) = Lazy.splitAt (fromIntegral ptr' - fromIntegral ptr) aus
              in Yield au $ get ptr' rest (n - 1) cs (o + p)

            Left (_, p, err) -> Effect $ Failure (o + p) err



do fmap concat $
     sequence
       [ makeFieldLabelsNoPrefix ''Chunk
       , makeFieldLabelsNoPrefix ''Chunks

       , makeFieldLabelsNoPrefix ''Gen8

       , makeFieldLabelsNoPrefix ''Extn
       , makeFieldLabelsNoPrefix ''Extn2
       , makeFieldLabelsNoPrefix ''ExtnTriplet
       , makeFieldLabelsNoPrefix ''ExtnSegment
       , makeFieldLabelsNoPrefix ''ExtnOperation

       , makeFieldLabelsNoPrefix ''Sond
       , makeFieldLabelsNoPrefix ''SondElement

       , makeFieldLabelsNoPrefix ''Sprt
       , makeFieldLabelsNoPrefix ''SprtElement

       , makeFieldLabelsNoPrefix ''Bgnd
       , makeFieldLabelsNoPrefix ''BgndElement

       , makeFieldLabelsNoPrefix ''Scpt
       , makeFieldLabelsNoPrefix ''ScptBinding

       , makeFieldLabelsNoPrefix ''Font
       , makeFieldLabelsNoPrefix ''FontElement
       , makeFieldLabelsNoPrefix ''FontBit

       , makeFieldLabelsNoPrefix ''Objt
       , makeFieldLabelsNoPrefix ''ObjtElement
       , makeFieldLabelsNoPrefix ''ShapePoint
       , makeFieldLabelsNoPrefix ''ObjtEvent
       , makeFieldLabelsNoPrefix ''ObjtAction

       , makeFieldLabelsNoPrefix ''Room
       , makeFieldLabelsNoPrefix ''RoomElement
       , makeFieldLabelsNoPrefix ''RGBA
       , makeFieldLabelsNoPrefix ''RoomBackground
       , makeFieldLabelsNoPrefix ''RoomView
       , makeFieldLabelsNoPrefix ''RoomObject
       , makeFieldLabelsNoPrefix ''RoomTile

       , makeFieldLabelsNoPrefix ''Tpag
       , makeFieldLabelsNoPrefix ''TpagElement

       , makeFieldLabelsNoPrefix ''Code
       , makeFieldLabelsNoPrefix ''CodeFunction

       , makeFieldLabelsNoPrefix ''Vari
       , makeFieldLabelsNoPrefix ''VariElement

       , makeFieldLabelsNoPrefix ''Func
       , makeFieldLabelsNoPrefix ''Func2
       , makeFieldLabelsNoPrefix ''FuncPosition
       , makeFieldLabelsNoPrefix ''FuncArguments

       , makeFieldLabelsNoPrefix ''Strg

       , makeFieldLabelsNoPrefix ''Txtr
       , makeFieldLabelsNoPrefix ''TxtrElement

       , makeFieldLabelsNoPrefix ''Audo
       ]
