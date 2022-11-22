{-|
    This module is a set of datatypes the Risk of Rain GameMaker data file parses into,
    right now only tested on Risk of Rain 1.3.0 Windows and Linux versions.

    I spent way too long figuring some of these out, but then it also turned out some
    [gorgeous people](https://pcy.ulyssis.be/undertale/unpacking-corrected) tried to do
    the same with Undertale and succeeded quite a lot, so a big thanks goes to them.

    It's not a 1 to 1 to the structure described on the aforementioned website though,
    certain fields are not really useful (e.g. array lengths) and therefore are omitted,
    plus there are certain incredibly minor differences.

    Well and also some names don't match because there is zero point synching between the two.
 -}

{-# LANGUAGE DataKinds
           , DeriveAnyClass
           , DeriveGeneric
           , DuplicateRecordFields
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , NoFieldSelectors
           , OverloadedStrings
           , OverloadedLabels
           , TemplateHaskell
           , TypeApplications #-}

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module GameMaker.RiskOfRain.Unpacking where

import           GameMaker.RiskOfRain.Stream
import           GameMaker.RiskOfRain.Unpacking.Help

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Int
import           Data.Set (Set)
import           Data.String
import           Data.Time.Clock (UTCTime)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Word
import           GHC.Generics
import           Lens.Micro.Labels.TH
import           Prelude hiding (getChar)



data Form =
       Form
         { gen8 :: Gen8
         , optn :: Optn
         , extn :: Extn
         , sond :: Sond
         , agrp :: Agrp
         , sprt :: Sprt
         , bgnd :: Bgnd
         , path :: Path
         , scpt :: Scpt
         , shdr :: Shdr
         , font :: Font
         , tmln :: Tmln
         , objt :: Objt
         , room :: Room
         , dafl :: Dafl
         , tpag :: Tpag
         , code :: Maybe Code
         , vari :: Maybe Vari
         , func :: Maybe Func
         , strg :: Strg
         , txtr :: Txtr
         , audo :: Audo
         }
       deriving (Show, Generic, NFData)



-- | Curious information about the videogame.
data Gen8 =
       Gen8
         { unknown1       :: Word32
         , name           :: BS.ByteString
         , filename       :: BS.ByteString
         , unknown2       :: Word32
         , unknown3       :: Word32
         , unknown4       :: Word32
         , unknown5       :: Word32
         , unknown6       :: Word32
         , unknown7       :: Word32
         , unknown8       :: Word32
         , name_          :: BS.ByteString
         , major          :: Word32
         , minor          :: Word32
         , release        :: Word32
         , build          :: Word32
         , defaultHeight  :: Word32
         , defaultWidth   :: Word32
         , info           :: Set InfoFlag
         , licenseMD5     :: BS.ByteString
         , licenseCRC32   :: Word32
         , timestamp      :: UTCTime
         , unknown9       :: Word32
         , displayName    :: BS.ByteString
         , unknown10      :: Word32
         , unknown11      :: Word32
         , unknown12      :: Word32
         , unknown13      :: Word32
         , unknown14      :: Word32
         , unknown15      :: Word32
         , rooms          :: Vector Word32
         }
       deriving (Show, Generic, NFData)

getGen8 :: BSL.ByteString -> Get Gen8
getGen8 form =
  chunk "GEN8" $ \_ -> do
    Gen8
      <$> getWord32le
      <*> getPtr form getByteStringNul
      <*> getPtr form getByteStringNul
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getPtr form getByteStringNul
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getInfoFlags
      <*> getByteString 16
      <*> getWord32le
      <*> getTime
      <*> getWord32le
      <*> getPtr form getByteStringNul
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> do size <- fromIntegral <$> getWord32le
             Vec.replicateM size getWord32le



-- | Garbage. Identical between Linux and Windows versions.
type Optn = BSL.ByteString

getOptn :: Get Optn
getOptn =
  chunk "OPTN" $ \_ ->
    getRemainingLazyByteString



-- | Seemingly bindings between GameMaker IO operations and system ones
--   (just based on the names pointed to), however most of this data
--   is just ones and twos scattered around.
data Extn =
       Extn
         { unknown1 :: Vector ExtnTriplet
         , unknown2 :: Vector ExtnSegment
         , unknown3 :: BSL.ByteString
         }
       deriving (Show, Generic, NFData)

getExtn :: BSL.ByteString -> Get Extn
getExtn form =
  chunk "EXTN" $ \_ ->
    Extn
      <$> getDict (getExtnTriplet form)
      <*> getDict (getExtnSegment form)
      <*> getRemainingLazyByteString

data ExtnTriplet =
       ExtnTriplet
         { unknown1 :: BS.ByteString
         , unknown2 :: BS.ByteString
         , unknown3 :: BS.ByteString
         }
       deriving (Show, Generic, NFData)

getExtnTriplet :: BSL.ByteString -> Get ExtnTriplet
getExtnTriplet form =
  ExtnTriplet
    <$> getPtr form getByteStringNul
    <*> getPtr form getByteStringNul
    <*> getPtr form getByteStringNul

data ExtnSegment =
       ExtnSegment
         { name       :: ExtnTriplet
         , unknown1   :: Word32
         , operations :: Vector ExtnOperation
         }
       deriving (Show, Generic, NFData)

getExtnSegment :: BSL.ByteString -> Get ExtnSegment
getExtnSegment form =
  ExtnSegment
    <$> getExtnTriplet form
    <*> getWord32le
    <*> getDict (getExtnOperation form)

data ExtnOperation =
       ExtnOperation
         { operationFs :: BS.ByteString
         , operationId :: Word32
         , unknown1    :: Word32
         , unknown2    :: Word32
         , operation   :: BS.ByteString
         , unknown3    :: Vector Word32
         }
       deriving (Show, Generic, NFData)

getExtnOperation :: BSL.ByteString -> Get ExtnOperation
getExtnOperation form =
  ExtnOperation
    <$> getPtr form getByteStringNul
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getPtr form getByteStringNul
    <*> do size <- fromIntegral <$> getWord32le
           Vec.replicateM size getWord32le



-- | Sound files and information.
type Sond = Vector SondElement

getSond :: BSL.ByteString -> Get Sond
getSond form =
  chunk "SOND" $ \_ ->
    getDict $ getSondElement form

data SondElement =
       SondElement
         { name       :: BS.ByteString
         , flags      :: Set SoundEntryFlag
         , extension  :: BS.ByteString
         , filename   :: BS.ByteString
         , unknown1   :: Word32             -- Always zero
         , volume     :: Float
         , pitch      :: Float
         , groupId    :: Float
         , identifier :: Int32
         }
       deriving (Show, Generic, NFData)

getSondElement :: BSL.ByteString -> Get SondElement
getSondElement form =
  SondElement
    <$> getPtr form getByteStringNul
    <*> getSoundEntryFlags
    <*> getPtr form getByteStringNul
    <*> getPtr form getByteStringNul
    <*> getWord32le
    <*> getFloatle
    <*> getFloatle
    <*> getFloatle
    <*> getInt32le



-- | Empty in both files
type Agrp = Vector ()

getAgrp :: Get Agrp
getAgrp =
  chunk "AGRP" $ \_ ->
    getDict $ pure ()



-- | Foreground sprites with all the masks and stuff.
type Sprt = Vector SprtElement

getSprt :: BSL.ByteString -> Get Sprt
getSprt form =
  chunk "SPRT" $ \_ ->
    getDictSized $ getSprtElement form

data SprtElement =
       SprtElement
         { name         :: BS.ByteString
         , width        :: Int32
         , height       :: Int32
         , marginLeft   :: Int32
         , marginRight  :: Int32
         , marginTop    :: Int32
         , marginBottom :: Int32
         , unknown1     :: Int32                 -- Always zero
         , unknown2     :: Int32                 -- Always zero
         , unknown3     :: Int32                 -- Always zero
         , bBoxMode     :: Int32
         , sepMasks     :: Int32
         , originX      :: Int32
         , originY      :: Int32
         , textures     :: Vector TpagElement
         , remaining    :: BSL.ByteString
         }
       deriving (Show, Generic, NFData)

getSprtElement :: BSL.ByteString -> Get SprtElement
getSprtElement form =
  SprtElement
    <$> getPtr form getByteStringNul
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> do size <- fromIntegral <$> getWord32le
           Vec.replicateM size $ getPtr form getTpagElement
    <*> getRemainingLazyByteString



-- | Background sprites.
type Bgnd = Vector BgndElement

getBgnd :: BSL.ByteString -> Get Bgnd
getBgnd form =
  chunk "BGND" $ \_ ->
    getDict $ getBgndElement form

data BgndElement =
       BgndElement
         { beName     :: BS.ByteString
         , beUnknown1 :: Word32                  -- Always zero
         , beUnknown2 :: Word32                  -- Always zero
         , beUnknown3 :: Word32                  -- Always zero
         , beTexture  :: TpagElement
         }
       deriving (Show, Generic, NFData)

getBgndElement :: BSL.ByteString -> Get BgndElement
getBgndElement form =
  BgndElement
    <$> getPtr form getByteStringNul
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getPtr form getTpagElement



-- | Empty in both files
type Path = Vector ()

getPath :: Get Path
getPath =
  chunk "PATH" $ \_ ->
    getDict $ pure ()



-- | Bindings of script functions to identifiers.
--
--   I suppose this is an extremely elaborate strategy to bind 'CodeFunction's to
--   'FuncPosition's or something, however the difference between the two is just a prefix.
type Scpt = Vector ScptBinding

getScpt :: BSL.ByteString -> Get Scpt
getScpt form =
  chunk "SCPT" $ \_ ->
    getDict $ getScptBinding form

data ScptBinding =
       ScptBinding
         { sbPointer    :: BS.ByteString
         , sbIdentifier :: Word32
         }
       deriving (Show, Generic, NFData)

getScptBinding :: BSL.ByteString -> Get ScptBinding
getScptBinding form =
  ScptBinding
    <$> getPtr form getByteStringNul
    <*> getWord32le



-- | Garbage. For the record shaders are stored in STRG.
type Shdr = BSL.ByteString

getShdr :: Get Shdr
getShdr =
  chunk "SHDR" $ \_ ->
    getRemainingLazyByteString



-- | Font descriptions because GameMaker can't tug around font files, so they're shoved
--   in textures.
type Font = Vector FontElement

getFont :: BSL.ByteString -> Get Font
getFont form =
  chunk "FONT" $ \_ -> do
    font <- getDict $ getFontElement form
    _ <- getRemainingLazyByteString
    return font

data FontElement =
       FontElement
         { kind         :: BS.ByteString
         , name         :: BS.ByteString
         , emSize       :: Word32
         , bold         :: Bool
         , italic       :: Bool
         , rangeStart   :: Int16
         , charset      :: Word8
         , antialiasing :: Word8
         , rangeEnd     :: Word32
         , texture      :: TpagElement
         , scaleX       :: Float
         , scaleY       :: Float
         , characters   :: Vector FontBit
         }
       deriving (Show, Generic, NFData)

getFontElement :: BSL.ByteString -> Get FontElement
getFontElement form =
  FontElement
    <$> getPtr form getByteStringNul
    <*> getPtr form getByteStringNul
    <*> getWord32le
    <*> getBool
    <*> getBool
    <*> getInt16le
    <*> getWord8
    <*> getWord8
    <*> getWord32le
    <*> getPtr form getTpagElement
    <*> getFloatle
    <*> getFloatle
    <*> getDict getFontBit

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
       deriving (Show, Generic, NFData)

getFontBit :: Get FontBit
getFontBit =
  FontBit
    <$> getChar
    <*> getInt16le
    <*> getInt16le
    <*> getInt16le
    <*> getInt16le
    <*> getInt16le
    <*> getInt16le
    <*> getInt16le



-- | Always a single 32bit zero.
type Tmln = BSL.ByteString

getTmln :: Get Tmln
getTmln =
  chunk "TMLN" $ \_ ->
    getRemainingLazyByteString



-- | Pretty much garbage. Extremely bulky, confusing and utterly undecryptable since
--   Risk of Rain barely uses physics at all.
type Objt = Vector ObjtElement

getObjt :: BSL.ByteString -> Get Objt
getObjt form =
  chunk "OBJT" $ \_ -> do
    objt <- getDict $ getObjtElement form
    _ <- getRemainingLazyByteString
    return objt

-- | The only thing from here we know for sure are name and sprite index
data ObjtElement =
       ObjtElement
         { name            :: BS.ByteString
         , spriteIndex     :: Int32
         , visible         :: Bool
         , solid           :: Bool
         , depth           :: Int32
         , persistent      :: Bool
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
         , shapePoints     :: Vector (Float, Float)
         , unknown12       :: Vector ObjtSub        -- Always holds 12 elements
         }
       deriving (Show, Generic, NFData)

getObjtElement :: BSL.ByteString -> Get ObjtElement
getObjtElement form = do
  unknown1  <- getPtr form getByteStringNul
  unknown2  <- getInt32le
  unknown3  <- getBool
  unknown4  <- getBool
  unknown5  <- getInt32le
  unknown6  <- getBool
  unknown7  <- getInt32le
  unknown8  <- getInt32le
  unknown9  <- getInt32le
  unknown10 <- getInt32le
  unknown11 <- getInt32le
  unknown12 <- getFloatle
  unknown13 <- getFloatle
  unknown14 <- getFloatle
  unknown15 <- getFloatle
  unknown16 <- getFloatle
  adNumber  <- getInt32le
  ObjtElement
    unknown1
    unknown2
    unknown3
    unknown4
    unknown5
    unknown6
    unknown7
    unknown8
    unknown9
    unknown10
    unknown11
    unknown12
    unknown13
    unknown14
    unknown15
    unknown16
    adNumber
    <$> getFloatle
    <*> getInt32le
    <*> getInt32le
    <*> Vec.replicateM (fromIntegral adNumber) ((,) <$> getFloatle <*> getFloatle)
    <*> getDict getObjtSub

type ObjtSub = Vector ObjtMeta

getObjtSub :: Get ObjtSub
getObjtSub = getDict getObjtMeta

-- | Only 'objtMetaUnknown1' seems to change meaningfully in this one, the rest is garbage.
data ObjtMeta = ObjtMeta
                  { unknown1   :: Int32
                  , unknown2   :: Int32
                  , unknown3   :: Int32 -- Points to eighth byte of this structure
                  , unknown4   :: Int32
                  , unknown5   :: Int32
                  , unknown6   :: Int32
                  , unknown7   :: Int32
                  , unknown8   :: Int32
                  , unknown9   :: Int32
                  , unknown10  :: Int32
                  , unknown11  :: Int32 -- Points to an empty string
                  , identifier :: Int32
                  , unknown13  :: Int32
                  , unknown14  :: Int32
                  , unknown15  :: Int32
                  , unknown16  :: Int32
                  , unknown17  :: Int32
                  }
                deriving (Show, Generic, NFData)

getObjtMeta :: Get ObjtMeta
getObjtMeta =
  ObjtMeta
    <$> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le



-- | Room data.
type Room = Vector RoomElement

getRoom :: BSL.ByteString -> Get Room
getRoom form =
  chunk "ROOM" $ \_ ->
    getDict (getRoomElement form)

data RoomElement =
       RoomElement
         { name           :: BS.ByteString
         , caption        :: BS.ByteString
         , width          :: Word32
         , height         :: Word32
         , speed          :: Word32
         , persistent     :: Bool
         , rgba           :: RGBA
         , drawBGColor    :: Bool
         , unknown1       :: Word32
         , flags          :: Set RoomEntryFlag
         , bgOffset       :: Word32
         , viewOffset     :: Word32
         , objOffset      :: Word32
         , tileOffset     :: Word32
         , world          :: Word32
         , top            :: Word32
         , left           :: Word32
         , right          :: Word32
         , bottom         :: Word32
         , gravityX       :: Float
         , gravityY       :: Float
         , metersPerPixel :: Float
         , backgrounds    :: Vector RoomBackground
         , views          :: Vector RoomView
         , objects        :: Vector RoomObject
         , tiles          :: Vector RoomTile
         }
       deriving (Show, Generic, NFData)

getRoomElement :: BSL.ByteString -> Get RoomElement
getRoomElement form =
  RoomElement
    <$> getPtr form getByteStringNul
    <*> getPtr form getByteStringNul
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getBool
    <*> getRGBA
    <*> getBool
    <*> getWord32le
    <*> getRoomEntryFlags
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getFloatle
    <*> getFloatle
    <*> getFloatle
    <*> getDict getRoomBackground
    <*> getDict getRoomView
    <*> getDict getRoomObject
    <*> getDict getRoomTile

data RoomBackground =
       RoomBackground
         { enabled    :: Bool
         , foreground :: Bool
         , bgDefIndex :: Int32
         , x          :: Int32
         , y          :: Int32
         , tileX      :: Bool
         , tileY      :: Bool
         , speedX     :: Int32
         , speedY     :: Int32
         , identifier :: Int32
         }
       deriving (Show, Generic, NFData)

getRoomBackground :: Get RoomBackground
getRoomBackground =
  RoomBackground
    <$> getBool
    <*> getBool
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getBool
    <*> getBool
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le

data RoomView =
       RoomView
         { enabled    :: Bool
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
       deriving (Show, Generic, NFData)

getRoomView :: Get RoomView
getRoomView =
  RoomView
    <$> getBool
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le

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
       deriving (Show, Generic, NFData)

getRoomObject :: Get RoomObject
getRoomObject =
  RoomObject
    <$> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getFloatle
    <*> getFloatle
    <*> getInt32le
    <*> getFloatle

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
       deriving (Show, Generic, NFData)

getRoomTile :: Get RoomTile
getRoomTile =
  RoomTile
    <$> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getFloatle
    <*> getFloatle
    <*> getInt32le



-- | Empty.
type Dafl = BSL.ByteString

getDafl :: Get Dafl
getDafl =
  chunk "DAFL" $ \_ ->
    getRemainingLazyByteString



-- | Sprite information as related to textures they reside in.
type Tpag = Vector TpagElement

getTpag :: Get Tpag
getTpag =
  chunk "TPAG" $ \_ ->
    getDict getTpagElement

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
       deriving (Show, Generic, NFData)

getTpagElement :: Get TpagElement
getTpagElement =
  TpagElement
    <$> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le



-- | Interpreted GameMaker code chunk. Will be empty if the game was compiled.
data Code =
       Code
         { offset     :: Int64               -- Not part of CODE chunk but we need this
         , operations :: BSL.ByteString
         , elements   :: Vector CodeFunction
         }
       deriving (Show, Generic, NFData)



getCode :: Int64 -> BSL.ByteString -> Get (Maybe Code)
getCode offglobal form =
  chunk "CODE" $ \cSize ->
    msum [ do True <- isEmpty
              return Nothing
         , do
              -- Size of the pointer list
              size <- getWord32le
              -- The pointer list itself
              _ <- skip $ 4 * fromIntegral size
              Just <$> do
                Code (offglobal + 4 + 4 * fromIntegral size)
                  <$> getLazyByteString (fromIntegral $ cSize - 4 * 6 * size - 4)
                         -- Elements pointed to by the pointer list are at the very end
                  <*> Vec.replicateM (fromIntegral size) (getCodeFunction offglobal form)
           ]

-- | A map of functions over 'CodeOps'. None of them overlap and there is no leftover code.
data CodeFunction =
       CodeFunction
         { name       :: BS.ByteString
         , size       :: Word32
         , unknown1   :: Word32
         , offset     :: Int64         -- Realigned this to global values
         , unknown2   :: Word32
         }
       deriving (Show, Generic, NFData)

getCodeFunction :: Int64 -> BSL.ByteString -> Get CodeFunction
getCodeFunction offglobal form =
  CodeFunction
    <$> getPtr form getByteStringNul
    <*> getWord32le
    <*> getWord32le
    <*> do offlocal <- bytesRead
           offset <- getInt32le
           return $ offglobal + offlocal + fromIntegral offset
    <*> getWord32le



-- | Variable names and where they occur in code.
data Vari =
       Vari
         { unknown1 :: Word32
         , unknown2 :: Word32
         , unknown3 :: Word32
         , elements :: Vector VariElement
         }
       deriving (Show, Generic, NFData)

getVari :: BSL.ByteString -> Get (Maybe Vari)
getVari form =
  chunk "VARI" $ \_ ->
    msum
      [ do True <- isEmpty
           return Nothing
      , Just <$> do
          Vari
            <$> getWord32le
            <*> getWord32le
            <*> getWord32le
            <*> fmap Vec.fromList (many $ getVariElement form)
      ]

-- | Every address points to the next address and it repeats @occurences@ times.
data VariElement =
       VariElement
         { name       :: BS.ByteString
         , unknown1   :: Int32
         , unknown2   :: Int32
         , occurences :: Int32
         , address    :: Int32
         }
       deriving (Show, Generic, NFData)

getVariElement :: BSL.ByteString -> Get VariElement
getVariElement form =
  VariElement
    <$> getPtr form getByteStringNul
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le
    <*> getInt32le



-- | Function information.
data Func =
       Func
         { positions :: Vector FuncPosition
         , arguments :: Vector FuncArguments
         }
       deriving (Show, Generic, NFData)

getFunc :: BSL.ByteString -> Get (Maybe Func)
getFunc form =
  chunk "FUNC" $ \_ ->
    msum
      [ do True <- isEmpty
           return Nothing
      , Just <$> do
          Func
            <$> do tripSize <- getWord32le
                   Vec.replicateM (fromIntegral tripSize) (getFuncPosition form)
            <*> do elemSize <- getWord32le
                   Vec.replicateM (fromIntegral elemSize) (getFuncArguments form)
      ]

-- | Same reasoning as with 'VariElement'.
data FuncPosition =
       FuncPosition
         { name       :: BS.ByteString
         , occurences :: Word32
         , address    :: Word32
         }
       deriving (Show, Generic, NFData)

getFuncPosition :: BSL.ByteString -> Get FuncPosition
getFuncPosition form =
  FuncPosition
    <$> getPtr form getByteStringNul
    <*> getWord32le
    <*> getWord32le

-- | Arguments each function consumes.
--
--   Note: first argument is always @"arguments"@, then optionally other ones.
data FuncArguments =
       FuncArguments
         { name      :: BS.ByteString
         , arguments :: Vector BS.ByteString
         }
       deriving (Show, Generic, NFData)

getFuncArguments :: BSL.ByteString -> Get FuncArguments
getFuncArguments form = do
  size <- fromIntegral <$> getWord32le
  FuncArguments
    <$> getPtr form getByteStringNul
    <*> do Vec.replicateM size $ do
             _argPositionId <- getWord32le
             getPtr form getByteStringNul



-- | Strings.
type Strg = Vector BS.ByteString

getStrg :: Get Strg
getStrg =
  chunk "STRG" $ \_ -> do
    strg <- getDict getStrgString
    _ <- getRemainingLazyByteString
    return strg

type StrgString = BS.ByteString

getStrgString :: Get BS.ByteString
getStrgString = do
  _size <- getWord32le
  getByteStringNul



-- | Raw textures.
type Txtr = Vector TxtrElement

getTxtr :: BSL.ByteString -> Get Txtr
getTxtr form =
  chunk "TXTR" $ \_ -> do
    txtr <- getDictSized (getTxtrElement form)
    _ <- getRemainingLazyByteString
    return txtr

data TxtrElement =
       TxtrElement
         { unknown1 :: Word32
         , image    :: BS.ByteString
         }
       deriving (Show, Generic, NFData)

getTxtrElement :: BSL.ByteString -> Get TxtrElement
getTxtrElement form =
  TxtrElement
    <$> getWord32le
    <*> getPtr form getPNG



-- | Raw audio files.
type Audo = Vector BS.ByteString

getAudo :: Get Audo
getAudo =
  chunk "AUDO" $ \_ -> do
    audo <- getDictSized getAudoFile
    _ <- getRemainingLazyByteString
    return audo

getAudoFile :: Get BS.ByteString
getAudoFile = do
  size <- getWord32le
  audo <- getByteString (fromIntegral size)
  _ <- getRemainingLazyByteString -- the first blob leaves 3 bytes hanging, after that
                                  -- it's mostly 2 or somethimes none.
  return audo



makeLabels ''Form

makeLabels ''Gen8

makeLabels ''Extn
makeLabels ''ExtnTriplet
makeLabels ''ExtnSegment
makeLabels ''ExtnOperation

makeLabels ''SondElement

makeLabels ''SprtElement

makeLabels ''BgndElement

makeLabels ''ScptBinding

makeLabels ''FontElement
makeLabels ''FontBit

makeLabels ''ObjtElement
makeLabels ''ObjtMeta

makeLabels ''RoomElement
makeLabels ''RoomBackground
makeLabels ''RoomView
makeLabels ''RoomObject
makeLabels ''RoomTile

makeLabels ''TpagElement

makeLabels ''Code
makeLabels ''CodeFunction

makeLabels ''Vari
makeLabels ''VariElement

makeLabels ''Func
makeLabels ''FuncPosition
makeLabels ''FuncArguments

makeLabels ''TxtrElement



-- | Decodes the 'Form'.
--
--   First argument of the resulting tuple is a list of parsed chunk names.
--   Each name is produced lazily after the previous chunk is evaluated.
--   Reaching end of the list thus means fully evaluating the decoding result.
decodeForm :: BSL.ByteString -> Stream (Int, String) String Form
decodeForm file =
  case runGetOrFail (extChunk "FORM") file of
    Left  (_   , _, err     ) -> Error err
    Right (rest, _, formsize) ->
      ( segment "GEN8" (\_   -> getGen8)
      . segment "OPTN" (\_ _ -> getOptn)
      . segment "EXTN" (\_   -> getExtn)
      . segment "SOND" (\_   -> getSond)
      . segment "AGRP" (\_ _ -> getAgrp)
      . segment "SPRT" (\_   -> getSprt)
      . segment "BGND" (\_   -> getBgnd)
      . segment "PATH" (\_ _ -> getPath)
      . segment "SCPT" (\_   -> getScpt)
      . segment "SHDR" (\_ _ -> getShdr)
      . segment "FONT" (\_   -> getFont)
      . segment "TMLN" (\_ _ -> getTmln)
      . segment "OBJT" (\_   -> getObjt)
      . segment "ROOM" (\_   -> getRoom)
      . segment "DAFL" (\_ _ -> getDafl)
      . segment "TPAG" (\_ _ -> getTpag)
      . segment "CODE"          getCode
      . segment "VARI" (\_   -> getVari)
      . segment "FUNC" (\_   -> getFunc)
      . segment "STRG" (\_ _ -> getStrg)
      . segment "TXTR" (\_   -> getTxtr)
      . segment "AUDO" (\_ _ -> getAudo)
      ) (\x _ _ _ -> Bottom x) Form 1 (BSL.take (fromIntegral formsize) rest) 0
  where
    segment name get f app n rest off =
      Next (n, name) $
        case runGetOrFail (get off file) rest of
          Right (rest', off', a  ) -> f (app a) (n + 1) rest' (off + off')
          Left  (_    , _   , err) -> Error err



-- | Total number of chunks 'decodeForm' parses.
totalChunks :: Int
totalChunks = 22
