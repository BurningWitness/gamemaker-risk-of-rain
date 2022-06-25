{-|
    This module is a set of datatypes the Risk of Rain GameMaker data file parses into,
    right now only tested on Risk of Rain 1.3.0 Windows and Linux versions.

    I spent way too long figuring some of these out, but then it also turned out some
    [gorgeous people](https://pcy.ulyssis.be/undertale/unpacking-corrected) tried to do
    the same with Undertale and succeeded quite a lot, so a big thanks goes to them.

    It's not a 1 to 1 to the structure described on the aforementioned website though,
    certain fields are not really useful (e.g. list lengths) and therefore are omitted,
    plus there are certain incredibly minor differences.

    Well and also some names don't match because there is zero point synching between the two.
 -}

{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           , FlexibleInstances
           , OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}

module GameMaker.RiskOfRain.Unpacking
  ( InfoFlag (..)
  , CRC32 (..)
  , MD5 (..)
  , RGBA (..)
  , RoomEntryFlag (..)
  , SoundEntryFlag (..)
  , module GameMaker.RiskOfRain.Unpacking
  ) where

import           GameMaker.RiskOfRain.Unpacking.Unpack

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Functor ((<&>))
import           Data.Int
import           Data.String
import           Data.Time.Clock (UTCTime)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Word
import           GHC.Generics
import           Prelude



-- | Decodes the 'Form'.
--
--   First argument of the resulting tuple is a list of parsed chunk names.
--   Each name is produced lazily after the previous chunk is evaluated.
--   Reaching end of the list thus means fully evaluating the decoding result.
decodeForm :: IsString s => BSL.ByteString -> ([(Int, s)], Either [Char] Form)
decodeForm file =
  case runGetOrFail (extChunk "FORM") file of
    Left  (_   , _, err     ) -> ([], Left err)
    Right (rest, _, formsize) ->
      ( segment "GEN8"
      . segment "OPTN"
      . segment "EXTN"
      . segment "SOND"
      . segment "AGRP"
      . segment "SPRT"
      . segment "BGND"
      . segment "PATH"
      . segment "SCPT"
      . segment "SHDR"
      . segment "FONT"
      . segment "TMLN"
      . segment "OBJT"
      . segment "ROOM"
      . segment "DAFL"
      . segment "TPAG"
      . tiltCode (segment "CODE")
      . segment "VARI"
      . segment "FUNC"
      . segment "STRG"
      . segment "TXTR"
      . segment "AUDO"
      ) (\x _ _ _ -> ([], Right x)) Form 1 (BSL.take (fromIntegral formsize) rest) 0
  where
    segment name f app n rest off =
      let (x, y) = case runGetOrFail (unpack file) rest of
                     Right (rest', off', a  ) -> f (app a) (n + 1) rest' (off + off')
                     Left  (_    , _   , err) -> ([], Left err)
      in ((n, name):x, y)

    tiltCode f x n r o p =
      let (a, b) = f x n r o p
      in ( a
         , b <&> \form ->
             form
               { fCode = fCode form <&> \code ->
                           code
                             { cOffset   = fromIntegral p + cOffset code
                             , cElements = cElements code <&> \el ->
                                             el { cfOffset = fromIntegral p + cfOffset el }
                             }
               }
         )



-- | Total number of chunks 'decodeForm' parses.
totalChunks :: Int
totalChunks = 22



data Form =
       Form
         { fGen8 :: Gen8
         , fOptn :: Optn
         , fExtn :: Extn
         , fSond :: Sond
         , fAgrp :: Agrp
         , fSprt :: Sprt
         , fBgnd :: Bgnd
         , fPath :: Path
         , fScpt :: Scpt
         , fShdr :: Shdr
         , fFont :: Font
         , fTmln :: Tmln
         , fObjt :: Objt
         , fRoom :: Room
         , fDafl :: Dafl
         , fTpag :: Tpag
         , fCode :: Maybe Code
         , fVari :: Maybe Vari
         , fFunc :: Maybe Func
         , fStrg :: Strg
         , fTxtr :: Txtr
         , fAudo :: Audo
         }
       deriving (Show, Eq, Generic, NFData)



-- | Curious information about the videogame.
data Gen8 =
       Gen8
         { gUnknown1       :: Word32
         , gName           :: Pointer BS.ByteString
         , gFilename       :: Pointer BS.ByteString
         , gUnknown2       :: Word32
         , gUnknown3       :: Word32
         , gUnknown4       :: Word32
         , gUnknown5       :: Word32
         , gUnknown6       :: Word32
         , gUnknown7       :: Word32
         , gUnknown8       :: Word32
         , gName_          :: Pointer BS.ByteString
         , gMajor          :: Word32
         , gMinor          :: Word32
         , gRelease        :: Word32
         , gBuild          :: Word32
         , gDefaultHeight  :: Word32
         , gDefaultWidth   :: Word32
         , gInfo           :: InfoFlags
         , gLicenseMD5     :: MD5
         , gLicenseCRC32   :: CRC32
         , gTimestamp      :: UTCTime
         , gUnknown9       :: Word32
         , gDisplayName    :: Pointer BS.ByteString
         , gUnknown10      :: Word32
         , gUnknown11      :: Word32
         , gUnknown12      :: Word32
         , gUnknown13      :: Word32
         , gUnknown14      :: Word32
         , gUnknown15      :: Word32
         , gRooms          :: Vector Word32
         }
       deriving (Show, Eq, Generic, NFData)

instance Unpack Gen8 where
  unpack form =
    chunk "GEN8" $ \_ -> do
      Gen8
        <$> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> unpack form
        <*> do size <- fromIntegral <$> getWord32le
               Vec.replicateM size getWord32le



-- | Garbage. Identical between Linux and Windows versions.
newtype Optn = Optn { unOptn :: BSL.ByteString }
               deriving (Show, Eq)

instance NFData Optn where
  rnf (Optn bs) = rnf bs

instance Unpack Optn where
  unpack _ =
    chunk "OPTN" $ \_ -> 
      Optn
        <$> getRemainingLazyByteString



-- | Seemingly bindings between GameMaker IO operations and system ones
--   (just based on the names pointed to), however most of this data
--   is just ones and twos scattered around.
data Extn =
       Extn
         { eUnknown1 :: Dictionary ExtnTriplet
         , eUnknown2 :: Dictionary ExtnSegment
         , eUnknown3 :: BSL.ByteString
         }
       deriving (Show, Eq, Generic, NFData)

instance Unpack Extn where
  unpack form =
    chunk "EXTN" $ \_ ->
      Extn
        <$> unpack form
        <*> unpack form
        <*> getRemainingLazyByteString

data ExtnTriplet =
       ExtnTriplet
         { etUnknown1 :: Pointer BS.ByteString
         , etUnknown2 :: Pointer BS.ByteString
         , etUnknown3 :: Pointer BS.ByteString
         }
       deriving (Show, Eq, Generic, NFData, Unpack)

data ExtnSegment =
       ExtnSegment
         { esName       :: ExtnTriplet
         , esUnknown1   :: Word32
         , esOperations :: Dictionary ExtnOperation
         }
       deriving (Show, Eq, Generic, NFData, Unpack)

data ExtnOperation =
       ExtnOperation
         { eoOperationFs :: Pointer BS.ByteString
         , eoOperationId :: Word32
         , eoUnknown1    :: Word32
         , eoUnknown2    :: Word32
         , eoOperation   :: Pointer BS.ByteString
         , eoUnknown3    :: Vector Word32
         }
       deriving (Show, Eq, Generic, NFData)

instance Unpack ExtnOperation where
  unpack form =
    ExtnOperation
      <$> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> do size <- fromIntegral <$> getWord32le
             Vec.replicateM size $ unpack form



-- | Sound files and information.
newtype Sond = Sond { unSond :: Dictionary SondElement }
               deriving (Show, Eq)

instance NFData Sond where
  rnf (Sond dict) = rnf dict

instance Unpack Sond where
  unpack form =
    chunk "SOND" $ \_ -> do
      Sond
        <$> unpack form

data SondElement =
       SondElement
         { seName       :: Pointer BS.ByteString
         , seFlags      :: SoundEntryFlags
         , seExtension  :: Pointer BS.ByteString -- Always ".ogg"
         , seFilename   :: Pointer BS.ByteString
         , seUnknown1   :: Word32                -- Always zero
         , seVolume     :: Float
         , sePitch      :: Float
         , seGroupId    :: Float
         , seIdentifier :: Int32
         }
       deriving (Show, Eq, Generic, NFData, Unpack)



-- | Empty in both files
newtype Agrp = Agrp { unAgrp :: Dictionary ()}
               deriving (Show, Eq)

instance NFData Agrp where
  rnf (Agrp dict) = rnf dict

instance Unpack Agrp where
  unpack form =
    chunk "AGRP" $ \_ ->
      Agrp
        <$> unpack form



-- | Foreground sprites with all the masks and stuff.
newtype Sprt = Sprt { unSprt :: DictionaryS SprtElement }
               deriving (Show, Eq)

instance NFData Sprt where
  rnf (Sprt dict) = rnf dict

instance Unpack Sprt where
  unpack form =
    chunk "SPRT" $ \_ ->
      Sprt
        <$> unpack form

data SprtElement =
       SprtElement
         { speName         :: Pointer BS.ByteString
         , speWidth        :: Int32
         , speHeight       :: Int32
         , speMarginLeft   :: Int32
         , speMarginRight  :: Int32
         , speMarginTop    :: Int32
         , speMarginBottom :: Int32
         , speUnknown1     :: Int32                 -- Always zero
         , speUnknown2     :: Int32                 -- Always zero
         , speUnknown3     :: Int32                 -- Always zero
         , speBBoxMode     :: Int32
         , speSepMasks     :: Int32
         , speOriginX      :: Int32
         , speOriginY      :: Int32
         , speTextures     :: Vector (Pointer TpagElement)
         , speRemaining    :: BSL.ByteString
         }
       deriving (Show, Eq, Generic, NFData)

instance Unpack SprtElement where
  unpack form =
    SprtElement
      <$> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> do size <- fromIntegral <$> getWord32le
             Vec.replicateM size $ unpack form
      <*> getRemainingLazyByteString



-- | Background sprites.
newtype Bgnd = Bgnd { unBgnd :: Dictionary BgndElement }
               deriving (Show, Eq)

instance NFData Bgnd where
  rnf (Bgnd dict) = rnf dict

instance Unpack Bgnd where
  unpack form =
    chunk "BGND" $ \_ ->
      Bgnd
        <$> unpack form

data BgndElement =
       BgndElement
         { beName     :: Pointer BS.ByteString
         , beUnknown1 :: Word32                  -- Always zero
         , beUnknown2 :: Word32                  -- Always zero
         , beUnknown3 :: Word32                  -- Always zero
         , beTexture  :: Pointer TpagElement
         }
       deriving (Show, Eq, Generic, NFData, Unpack)



-- | Empty in both files
newtype Path = Path { unPath :: Dictionary () }
               deriving (Show, Eq)

instance NFData Path where
  rnf (Path dict) = rnf dict

instance Unpack Path where
  unpack form =
    chunk "PATH" $ \_ ->
      Path
        <$> unpack form



-- | Bindings of script functions to identifiers.
--
--   I suppose this is an extremely elaborate strategy to bind 'CodeFunction's to
--   'FuncPosition's or something, however the difference between the two is just a prefix.
newtype Scpt = Scpt { unScpt :: Dictionary ScptBinding }
               deriving (Show, Eq)

instance NFData Scpt where
  rnf (Scpt dict) = rnf dict

instance Unpack Scpt where
  unpack form =
    chunk "SCPT" $ \_ ->
      Scpt
        <$> unpack form

data ScptBinding =
       ScptBinding
         { sbPointer    :: Pointer BS.ByteString
         , sbIdentifier :: Word32
         }
       deriving (Show, Eq, Generic, NFData, Unpack)



-- | Garbage. For the record shaders are stored in STRG.
newtype Shdr = Shdr { unShdr :: BSL.ByteString }
               deriving (Show, Eq)

instance NFData Shdr where
  rnf (Shdr dict) = rnf dict

instance Unpack Shdr where
  unpack _ =
    chunk "SHDR" $ \_ ->
      Shdr
        <$> getRemainingLazyByteString



-- | Font descriptions because GameMaker can't tug around font files, so they're shoved
--   in textures.
newtype Font = Font { unFont :: Dictionary FontElement }
               deriving (Show, Eq)

instance NFData Font where
  rnf (Font dict) = rnf dict

instance Unpack Font where
  unpack form =
    chunk "FONT" $ \_ -> do
      font <- Font
                <$> unpack form
      _ <- getRemainingLazyByteString
      return font

data FontElement =
       FontElement
         { feType         :: Pointer BS.ByteString
         , feName         :: Pointer BS.ByteString
         , feEmSize       :: Word32
         , feBold         :: Bool
         , feItalic       :: Bool
         , feRangeStart   :: Int16
         , feCharset      :: Word8
         , feAntialiasing :: Word8
         , feRangeEnd     :: Word32
         , feTexture      :: Pointer TpagElement
         , feScaleX       :: Float
         , feScaleY       :: Float
         , feCharacters   :: Dictionary FontBit
         }
       deriving (Show, Eq, Generic, NFData, Unpack)

data FontBit =
       FontBit
         { fbCharacter :: Char
         , fbOffsetX   :: Int16
         , fbOffsetY   :: Int16
         , fbWidth     :: Int16
         , fbHeight    :: Int16
         , fbAdvance   :: Int16
         , fbBearingX  :: Int16
         , fbBearingY  :: Int16
         }
       deriving (Show, Eq, Generic, NFData, Unpack)



-- | Always a single 32bit zero.
newtype Tmln = Tmln { unTmln :: BSL.ByteString }
               deriving (Show, Eq)

instance NFData Tmln where
  rnf (Tmln bs) = rnf bs

instance Unpack Tmln where
  unpack _ =
    chunk "TMLN" $ \_ ->
      Tmln
        <$> getRemainingLazyByteString



-- | Pretty much garbage. Extremely bulky, confusing and utterly undecryptable since
--   Risk of Rain barely uses physics at all.
newtype Objt = Objt { unObjt :: Dictionary ObjtElement }
               deriving (Show, Eq)

instance NFData Objt where
  rnf (Objt dict) = rnf dict

instance Unpack Objt where
  unpack form =
    chunk "OBJT" $ \_ -> do
      objt <- Objt
                <$> unpack form
      _ <- getRemainingLazyByteString
      return objt

-- | The only thing from here we know for sure are name and sprite index
data ObjtElement =
       ObjtElement
         { oeName            :: Pointer BS.ByteString
         , oeSpriteIndex     :: Int32
         , oeVisible         :: Bool
         , oeSolid           :: Bool
         , oeDepth           :: Int32
         , oePersistent      :: Bool
         , oeParentId        :: Int32
         , oeTextureMaskId   :: Int32
         , oeUnknown1        :: Int32
         , oeUnknown2        :: Int32
         , oeUnknown3        :: Int32
         , oeUnknown4        :: Float
         , oeUnknown5        :: Float
         , oeUnknown6        :: Float
         , oeUnknown7        :: Float
         , oeUnknown8        :: Float
         , oeShapePointCount :: Int32
         , oeUnknown9        :: Float
         , oeUnknown10       :: Int32
         , oeUnknown11       :: Int32
         , oeShapePoints     :: Vector (Float, Float)
         , oeUnknown12       :: Dictionary ObjtSub -- Always holds 12 elements
         }
       deriving (Show, Eq, Generic, NFData)

instance Unpack ObjtElement where
  unpack form = do
    unknown1  <- unpack form
    unknown2  <- unpack form
    unknown3  <- unpack form
    unknown4  <- unpack form
    unknown5  <- unpack form
    unknown6  <- unpack form
    unknown7  <- unpack form
    unknown8  <- unpack form
    unknown9  <- unpack form
    unknown10 <- unpack form
    unknown11 <- unpack form
    unknown12 <- unpack form
    unknown13 <- unpack form
    unknown14 <- unpack form
    unknown15 <- unpack form
    unknown16 <- unpack form
    adNumber  <- unpack form
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
      <$> unpack form
      <*> unpack form
      <*> unpack form
      <*> Vec.replicateM (fromIntegral adNumber) (unpack form)
      <*> unpack form

newtype ObjtSub = ObjtSub { unObjtSub :: Dictionary ObjtMeta }
                  deriving (Show, Eq, Generic, NFData, Unpack)

-- | Only 'objtMetaUnknown1' seems to change meaningfully in this one, the rest is garbage.
data ObjtMeta = ObjtMeta
                  { omUnknown1   :: Int32
                  , omUnknown2   :: Int32
                  , omUnknown3   :: Int32 -- Points to eighth byte of this structure
                  , omUnknown4   :: Int32
                  , omUnknown5   :: Int32
                  , omUnknown6   :: Int32
                  , omUnknown7   :: Int32
                  , omUnknown8   :: Int32
                  , omUnknown9   :: Int32
                  , omUnknown10  :: Int32
                  , omUnknown11  :: Int32 -- Points to an empty string
                  , omIdentifier :: Int32
                  , omUnknown13  :: Int32
                  , omUnknown14  :: Int32
                  , omUnknown15  :: Int32
                  , omUnknown16  :: Int32
                  , omUnknown17  :: Int32
                  }
                deriving (Show, Eq, Generic, NFData, Unpack)



-- | Room data.
newtype Room = Room { unRoom :: Dictionary RoomElement }
               deriving (Show, Eq)

instance NFData Room where
  rnf (Room dict) = rnf dict

instance Unpack Room where
  unpack form =
    chunk "ROOM" $ \_ ->
      Room <$> unpack form

data RoomElement =
       RoomElement
         { reName           :: Pointer BS.ByteString
         , reCaption        :: Pointer BS.ByteString
         , reWidth          :: Word32
         , reHeight         :: Word32
         , reSpeed          :: Word32
         , rePersistent     :: Bool
         , reRgba           :: RGBA
         , reDrawBGColor    :: Bool
         , reUnknown1       :: Word32
         , reFlags          :: RoomEntryFlags
         , reBgOffset       :: Word32
         , reViewOffset     :: Word32
         , reObjOffset      :: Word32
         , reTileOffset     :: Word32
         , reWorld          :: Word32
         , reTop            :: Word32
         , reLeft           :: Word32
         , reRight          :: Word32
         , reBottom         :: Word32
         , reGravityX       :: Float
         , reGravityY       :: Float
         , reMetersPerPixel :: Float
         , reBackgrounds    :: Dictionary RoomBackground
         , reViews          :: Dictionary RoomView
         , reObjects        :: Dictionary RoomObject
         , reTiles          :: Dictionary RoomTile
         }
       deriving (Show, Eq, Generic, NFData, Unpack)

data RoomBackground =
       RoomBackground
         { rbEnabled    :: Bool
         , rbForeground :: Bool
         , rbBgDefIndex :: Int32
         , rbX          :: Int32
         , rbY          :: Int32
         , rbTileX      :: Bool
         , rbTileY      :: Bool
         , rbSpeedX     :: Int32
         , rbSpeedY     :: Int32
         , rbIdentifier :: Int32
         }
       deriving (Show, Eq, Generic, NFData, Unpack)

data RoomView =
       RoomView
         { rvEnabled    :: Bool
         , rvViewX      :: Int32
         , rvViewY      :: Int32
         , rvViewWidth  :: Int32
         , rvViewHeight :: Int32
         , rvPortX      :: Int32
         , rvPortY      :: Int32
         , rvPortWidth  :: Int32
         , rvPortHeight :: Int32
         , rvBorderX    :: Int32
         , rvBorderY    :: Int32
         , rvSpeedX     :: Int32
         , rvSpeedY     :: Int32
         , rvIdentifier :: Int32
         }
       deriving (Show, Eq, Generic, NFData, Unpack)

data RoomObject =
       RoomObject
         { roX          :: Int32
         , roY          :: Int32
         , roIdentifier :: Int32
         , roInitCode   :: Int32
         , roUnknown5   :: Int32
         , roScaleX     :: Float
         , roScaleY     :: Float
         , roUnknown8   :: Int32
         , roRotation   :: Float
         }
       deriving (Show, Eq, Generic, NFData, Unpack)

data RoomTile =
       RoomTile
         { rtX          :: Int32
         , rtY          :: Int32
         , rtBgDefIndex :: Int32
         , rtSourceX    :: Int32
         , rtSourceY    :: Int32
         , rtWidth      :: Int32
         , rtHeight     :: Int32
         , rtTileDepth  :: Int32
         , rtIdentifier :: Int32
         , rtScaleX     :: Float
         , rtScaleY     :: Float
         , rtUnknown12  :: Int32
         }
       deriving (Show, Eq, Generic, NFData, Unpack)



-- | Empty.
newtype Dafl = Dafl { unDafl :: BSL.ByteString }
               deriving (Show, Eq)

instance NFData Dafl where
  rnf (Dafl bs) = rnf bs

instance Unpack Dafl where
  unpack _ =
    chunk "DAFL" $ \_ ->
      Dafl
        <$> getRemainingLazyByteString



-- | Sprite information as related to textures they reside in.
newtype Tpag = Tpag { unTpag :: Dictionary TpagElement }
               deriving (Show, Eq)

instance NFData Tpag where
  rnf (Tpag dict) = rnf dict

instance Unpack Tpag where
  unpack form =
    chunk "TPAG" $ \_ ->
      Tpag
        <$> unpack form

data TpagElement =
       TpagElement
         { teOffsetX        :: Word16
         , teOffsetY        :: Word16
         , teWidth          :: Word16
         , teHeight         :: Word16
         , teRenderX        :: Word16
         , teRenderY        :: Word16
         , teBoundingX      :: Word16
         , teBoundingY      :: Word16
         , teBoundingWidth  :: Word16
         , teBoundingHeight :: Word16
         , teImageId        :: Word16
         }
       deriving (Show, Eq, Generic, NFData, Unpack)



-- | Interpreted GameMaker code chunk. Will be empty if the game was compiled.
data Code =
       Code
         { cOffset     :: Int            -- Not part of CODE chunk but we need this
         , cOperations :: CodeOps
         , cElements   :: Vector CodeFunction
         }
       deriving (Show, Eq, Generic, NFData)

instance Unpack (Maybe Code) where
  unpack form = do
    chunk "CODE" $ \cSize ->
      msum [ do True <- isEmpty
                return Nothing
           , do
                -- Size of the pointer list
                size <- getWord32le
                -- The pointer list itself
                _ <- skip $ 4 * fromIntegral size
                Just <$> do
                       -- Offsets are local and are adjusted to global on function decoding
                  Code (4 + 4 * fromIntegral size)
                    <$> do CodeOps <$> do getLazyByteString . fromIntegral $ cSize - 4 * 6 * size - 4
                           -- Elements pointed to by the pointer list are at the very end
                    <*> do Vec.replicateM (fromIntegral size) $ do
                             CodeFunction
                               <$> unpack form
                               <*> unpack form
                               <*> unpack form
                               <*> do offsetLocal <- fromIntegral <$> bytesRead
                                      (offsetLocal +) . fromIntegral <$> getInt32le
                               <*> unpack form
           ]

-- | Unpacked in "RiskOfRain.Decompilation"
newtype CodeOps = CodeOps BSL.ByteString
                  deriving (Show, Eq)

instance NFData CodeOps where
  rnf (CodeOps dict) = rnf dict

instance Unpack CodeOps where
  unpack _ =
    CodeOps <$> getRemainingLazyByteString

-- | A map of functions over 'CodeOps'. None of them overlap and there is no leftover code.
data CodeFunction =
       CodeFunction
         { cfName       :: Pointer BS.ByteString
         , cfSize       :: Word32
         , cfUnknown1   :: Word32
         , cfOffset     :: Int    -- Realigned this to global values
         , cfUnknown2   :: Word32
         }
       deriving (Show, Eq, Generic, NFData)



-- | Variable names and where they occur in code.
data Vari =
       Vari
         { vUnknown1 :: Word32
         , vUnknown2 :: Word32
         , vUnknown3 :: Word32
         , vElements :: Vector VariElement
         }
       deriving (Show, Eq, Generic, NFData)

instance Unpack (Maybe Vari) where
  unpack form =
    chunk "VARI" $ \_ ->
      msum
        [ do True <- isEmpty
             return Nothing
        , Just <$> do
            Vari
              <$> unpack form
              <*> unpack form
              <*> unpack form
              <*> do Vec.fromList <$> many (unpack form)
        ]

-- | Every address points to the next address and it repeats @occurences@ times.
data VariElement =
       VariElement
         { veName       :: Pointer BS.ByteString
         , veUnknown1   :: Int32
         , veUnknown2   :: Int32
         , veOccurences :: Int32
         , veAddress    :: Int32
         }
       deriving (Show, Eq, Generic, NFData, Unpack)



-- | Function information.
data Func =
       Func
         { fPositions :: Vector FuncPosition
         , fArguments :: Vector FuncArguments
         }
       deriving (Show, Eq, Generic, NFData)

instance Unpack (Maybe Func) where
  unpack form =
    chunk "FUNC" $ \_ ->
      msum
        [ do True <- isEmpty
             return Nothing
        , Just <$> do
            Func
              <$> do tripSize <- getWord32le
                     Vec.replicateM (fromIntegral tripSize) $ unpack form
              <*> do elemSize <- getWord32le
                     Vec.replicateM (fromIntegral elemSize) $ unpack form
        ]

-- | Same reasoning as with 'VariElement'.
data FuncPosition =
       FuncPosition
         { fpName       :: Pointer BS.ByteString
         , fpOccurences :: Word32
         , fpAddress    :: Word32
         }
       deriving (Show, Eq, Generic, NFData, Unpack)

-- | Arguments each function consumes.
--
--   Note: first argument is always @"arguments"@, then optionally other ones.
data FuncArguments =
       FuncArguments
         { faName      :: Pointer BS.ByteString
         , faArguments :: Vector (Pointer BS.ByteString)
         }
       deriving (Show, Eq, Generic, NFData)

instance Unpack FuncArguments where
  unpack form = do
    size <- fromIntegral <$> getWord32le
    FuncArguments
      <$> unpack form
      <*> do Vec.replicateM size $ do
               _argPositionId <- getWord32le
               unpack form



-- | Strings.
newtype Strg = Strg { unStrg :: Dictionary StrgString }
               deriving (Show, Eq)

instance NFData Strg where
  rnf (Strg dict) = rnf dict

instance Unpack Strg where
  unpack form =
    chunk "STRG" $ \_ -> do
      strg <- Strg
                <$> unpack form
      _ <- getRemainingLazyByteString
      return strg

newtype StrgString = StrgString { unStrgString :: BS.ByteString }
                     deriving (Show, Eq)

instance NFData StrgString where
  rnf (StrgString bs) = rnf bs

instance Unpack StrgString where
  unpack form = do
    _size <- getWord32le
    StrgString
      <$> unpack form



-- | Raw textures.
newtype Txtr = Txtr { unTxtr :: DictionaryS TxtrElement }
               deriving (Show, Eq)

instance NFData Txtr where
  rnf (Txtr tx) = rnf tx

instance Unpack Txtr where
  unpack form =
    chunk "TXTR" $ \_ -> do
      txtr <- Txtr
                <$> unpack form
      _ <- getRemainingLazyByteString
      return txtr

data TxtrElement =
       TxtrElement
         { teUnknown1 :: Word32
         , teImage    :: Pointer PNG
         }
       deriving (Show, Eq, Generic, Unpack)

instance NFData TxtrElement



-- | Raw audio files.
newtype Audo = Audo { unAudo :: DictionaryS AudoFile }
               deriving (Show, Eq)

instance NFData Audo where
  rnf (Audo au) = rnf au

instance Unpack Audo where
  unpack form =
    chunk "AUDO" $ \_ -> do
      audo <- Audo
                <$> unpack form
      _ <- getRemainingLazyByteString
      return audo

newtype AudoFile = AudoFile { unAudoFile :: BS.ByteString }
                   deriving (Show, Eq)

instance NFData AudoFile where
  rnf (AudoFile bs) = rnf bs

instance Unpack AudoFile where
  unpack _ = do
    audo <- AudoFile
              <$> do size <- fromIntegral <$> getWord32le
                     getByteString size
    _ <- getRemainingLazyByteString -- the first blob leaves 3 bytes hanging, after that
                                    -- it's mostly 2 or somethimes none.
    return audo
