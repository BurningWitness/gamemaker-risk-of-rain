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
import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Int
import           Data.Time.Clock (UTCTime)
import           Data.Word
import           GHC.Generics
import           Prelude



decodeForm :: BSL.ByteString -> Either [Char] Form
decodeForm file =
  case runGetOrFail (unpack file) file of
    Left (_, _, err) -> Left err
    Right (_, _, a) -> Right a

data Form =
       Form
         { formGen8 :: Gen8
         , formOptn :: Optn
         , formExtn :: Extn
         , formSond :: Sond
         , formAgrp :: Agrp
         , formSprt :: Sprt
         , formBgnd :: Bgnd
         , formPath :: Path
         , formScpt :: Scpt
         , formShdr :: Shdr
         , formFont :: Font
         , formTmln :: Tmln
         , formObjt :: Objt
         , formRoom :: Room
         , formDafl :: Dafl
         , formTpag :: Tpag
         , formCode :: Maybe Code
         , formVari :: Maybe Vari
         , formFunc :: Maybe Func
         , formStrg :: Strg
         , formTxtr :: Txtr
         , formAudo :: Audo
         }
       deriving (Show, Eq)

instance Unpack Form where
  unpack form =
    chunk "FORM" $ \_ -> 
      Form
        <$> do "Chunk GEN8" `label` unpack form
        <*> do "Chunk OPTN" `label` unpack form
        <*> do "Chunk EXTN" `label` unpack form
        <*> do "Chunk SOND" `label` unpack form
        <*> do "Chunk AGRP" `label` unpack form
        <*> do "Chunk SPRT" `label` unpack form
        <*> do "Chunk BGND" `label` unpack form
        <*> do "Chunk PATH" `label` unpack form
        <*> do "Chunk SCPT" `label` unpack form
        <*> do "Chunk SHDR" `label` unpack form
        <*> do "Chunk FONT" `label` unpack form
        <*> do "Chunk TMLN" `label` unpack form
        <*> do "Chunk OBJT" `label` unpack form
        <*> do "Chunk ROOM" `label` unpack form
        <*> do "Chunk DAFL" `label` unpack form
        <*> do "Chunk TPAG" `label` unpack form
        <*> do "Chunk CODE" `label` unpack form
        <*> do "Chunk VARI" `label` unpack form
        <*> do "Chunk FUNC" `label` unpack form
        <*> do "Chunk STRG" `label` unpack form
        <*> do "Chunk TXTR" `label` unpack form
        <*> do "Chunk AUDO" `label` unpack form



-- | Curious information about the videogame.
data Gen8 =
       Gen8
         { gen8Unknown1       :: Word32
         , gen8Name           :: Pointer BS.ByteString
         , gen8Filename       :: Pointer BS.ByteString
         , gen8Unknown2       :: Word32
         , gen8Unknown3       :: Word32
         , gen8Unknown4       :: Word32
         , gen8Unknown5       :: Word32
         , gen8Unknown6       :: Word32
         , gen8Unknown7       :: Word32
         , gen8Unknown8       :: Word32
         , gen8NameUnderscore :: Pointer BS.ByteString
         , gen8Major          :: Word32
         , gen8Minor          :: Word32
         , gen8Release        :: Word32
         , gen8Build          :: Word32
         , gen8DefaultHeight  :: Word32
         , gen8DefaultWidth   :: Word32
         , gen8Info           :: InfoFlags
         , gen8LicenseMD5     :: MD5
         , gen8LicenseCRC32   :: CRC32
         , gen8Timestamp      :: UTCTime
         , gen8Unknown9       :: Word32
         , gen8DisplayName    :: Pointer BS.ByteString
         , gen8Unknown10      :: Word32
         , gen8Unknown11      :: Word32
         , gen8Unknown12      :: Word32
         , gen8Unknown13      :: Word32
         , gen8Unknown14      :: Word32
         , gen8Unknown15      :: Word32
         , gen8Rooms          :: [Word32]
         }
       deriving (Show, Eq)

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
               replicateM size getWord32le



-- | Garbage. Identical between Linux and Windows versions.
newtype Optn = Optn { optnUnOptn :: BSL.ByteString }
               deriving (Show, Eq)

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
         { extnUnknown1 :: Dictionary ExtnTriplet
         , extnUnknown2 :: Dictionary ExtnSegment
         , extnUnknown3 :: BSL.ByteString
         }
       deriving (Show, Eq)

instance Unpack Extn where
  unpack form =
    chunk "EXTN" $ \_ ->
      Extn
        <$> unpack form
        <*> unpack form
        <*> getRemainingLazyByteString

data ExtnTriplet =
       ExtnTriplet
         { extnTripletUnknown1 :: Pointer BS.ByteString
         , extnTripletUnknown2 :: Pointer BS.ByteString
         , extnTripletUnknown3 :: Pointer BS.ByteString
         }
       deriving (Show, Eq, Generic, Unpack)

data ExtnSegment =
       ExtnSegment
         { extnSegmentName       :: ExtnTriplet
         , extnSegmentUnknown1   :: Word32
         , extnSegmentOperations :: Dictionary ExtnOperation
         }
       deriving (Show, Eq, Generic, Unpack)

data ExtnOperation =
       ExtnOperation
         { extnOperationOperationFs :: Pointer BS.ByteString
         , extnOperationOperationId :: Word32
         , extnOperationUnknown1    :: Word32
         , extnOperationUnknown2    :: Word32
         , extnOperationOperation   :: Pointer BS.ByteString
         , extnOperationUnknown3    :: [Word32]
         }
       deriving (Show, Eq)

instance Unpack ExtnOperation where
  unpack form =
    ExtnOperation
      <$> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> unpack form
      <*> do size <- fromIntegral <$> getWord32le
             replicateM size $ unpack form



-- | Sound files and information.
newtype Sond = Sond { sondUnSond :: Dictionary SondElement }
               deriving (Show, Eq)

instance Unpack Sond where
  unpack form =
    chunk "SOND" $ \_ -> do
      Sond
        <$> unpack form

data SondElement =
       SondElement
         { sondElementName       :: Pointer BS.ByteString
         , sondElementFlags      :: SoundEntryFlags
         , sondElementExtension  :: Pointer BS.ByteString -- Always ".ogg"
         , sondElementFileName   :: Pointer BS.ByteString
         , sondElementUnknown1   :: Word32                  -- Always zero
         , sondElementVolume     :: Float
         , sondElementPitch      :: Float
         , sondElementGroupId    :: Float
         , sondElementIdentifier :: Int32
         }
       deriving (Show, Eq, Generic, Unpack)



-- | Empty in both files
newtype Agrp = Agrp { agrpUnAgrp :: Dictionary ()}
               deriving (Show, Eq)

instance Unpack Agrp where
  unpack form =
    chunk "AGRP" $ \_ ->
      Agrp
        <$> unpack form



-- | Foreground sprites with all the masks and stuff.
newtype Sprt = Sprt { sprtUnSprt :: DictionaryS SprtElement }
               deriving (Show, Eq)

instance Unpack Sprt where
  unpack form =
    chunk "SPRT" $ \_ ->
      Sprt
        <$> unpack form

data SprtElement =
       SprtElement
         { sprtElementName         :: Pointer BS.ByteString
         , sprtElementWidth        :: Int32
         , sprtElementHeight       :: Int32
         , sprtElementMarginLeft   :: Int32
         , sprtElementMarginRight  :: Int32
         , sprtElementMarginTop    :: Int32
         , sprtElementMarginBottom :: Int32
         , sprtElementUnknown1     :: Int32                 -- Always zero
         , sprtElementUnknown2     :: Int32                 -- Always zero
         , sprtElementUnknown3     :: Int32                 -- Always zero
         , sprtElementBBoxMode     :: Int32
         , sprtElementSepMasks     :: Int32
         , sprtElementOriginX      :: Int32
         , sprtElementOriginY      :: Int32
         , sprtElementTextures     :: [Pointer TpagElement]
         , sprtElementRemaining    :: BSL.ByteString
         }
       deriving (Show, Eq)

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
             replicateM size $ unpack form
      <*> getRemainingLazyByteString




-- | Background sprites.
newtype Bgnd = Bgnd { bgndUnBgnd :: Dictionary BgndElement }
               deriving (Show, Eq)

instance Unpack Bgnd where
  unpack form =
    chunk "BGND" $ \_ ->
      Bgnd
        <$> unpack form

data BgndElement =
       BgndElement
         { bgndElementName     :: Pointer BS.ByteString
         , bgndElementUnknown1 :: Word32                  -- Always zero
         , bgndElementUnknown2 :: Word32                  -- Always zero
         , bgndElementUnknown3 :: Word32                  -- Always zero
         , bgndElementTexture  :: Pointer TpagElement
         }
       deriving (Show, Eq, Generic, Unpack)



-- | Empty in both files
newtype Path = Path { pathUnPath :: Dictionary () }
               deriving (Show, Eq)

instance Unpack Path where
  unpack form =
    chunk "PATH" $ \_ ->
      Path
        <$> unpack form



-- | Bindings of script functions to identifiers.
--
--   I suppose this is an extremely elaborate strategy to bind 'CodeFunction's to
--   'FuncPosition's or something, however the difference between the two is just a prefix.
newtype Scpt = Scpt { scptUnScpt :: Dictionary ScptBinding }
               deriving (Show, Eq)

instance Unpack Scpt where
  unpack form =
    chunk "SCPT" $ \_ ->
      Scpt
        <$> unpack form

data ScptBinding =
       ScptBinding
         { scptBindingPointer    :: Pointer BS.ByteString
         , scptBindingIdentifier :: Word32
         }
       deriving (Show, Eq, Generic, Unpack)



-- | Garbage. For the record shaders are stored in STRG.
newtype Shdr = Shdr { shdrUnShdr :: BSL.ByteString }
               deriving (Show, Eq)

instance Unpack Shdr where
  unpack _ =
    chunk "SHDR" $ \_ ->
      Shdr
        <$> getRemainingLazyByteString



-- | Font descriptions because GameMaker can't tug around font files, so they're shoved
--   in textures.
newtype Font = Font { fontUnFont :: Dictionary FontElement }
               deriving (Show, Eq)

instance Unpack Font where
  unpack form =
    chunk "FONT" $ \_ -> do
      font <- Font
                <$> unpack form
      _ <- getRemainingLazyByteString
      return font

data FontElement =
       FontElement
         { fontElementType_        :: Pointer BS.ByteString
         , fontElementName         :: Pointer BS.ByteString
         , fontElementEmSize       :: Word32
         , fontElementBold         :: Bool
         , fontElementItalic       :: Bool
         , fontElementRangeStart   :: Int16
         , fontElementCharset      :: Word8
         , fontElementAntialiasing :: Word8
         , fontElementRangeEnd     :: Word32
         , fontElementTexture      :: Pointer TpagElement
         , fontElementScaleX       :: Float
         , fontElementScaleY       :: Float
         , fontElementCharacters   :: Dictionary FontBit
         }
       deriving (Show, Eq, Generic, Unpack)

data FontBit =
       FontBit
         { fontBitCharacter :: Char
         , fontBitOffsetX   :: Int16
         , fontBitOffsetY   :: Int16
         , fontBitWidth     :: Int16
         , fontBitHeight    :: Int16
         , fontBitAdvance   :: Int16
         , fontBitBearingX  :: Int16
         , fontBitBearingY  :: Int16
         }
       deriving (Show, Eq, Generic, Unpack)



-- | Always a single 32bit zero.
newtype Tmln = Tmln { tmlnUnTmln :: BSL.ByteString }
               deriving (Show, Eq)

instance Unpack Tmln where
  unpack _ =
    chunk "TMLN" $ \_ ->
      Tmln
        <$> getRemainingLazyByteString



-- | Pretty much garbage. Extremely bulky, confusing and utterly undecryptable since
--   Risk of Rain barely uses physics at all.
newtype Objt = Objt { objtUnObjt :: Dictionary ObjtElement }
               deriving (Show, Eq)

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
         { objtElementName            :: Pointer BS.ByteString
         , objtElementSpriteIndex     :: Int32
         , objtElementVisible         :: Bool
         , objtElementSolid           :: Bool
         , objtElementDepth           :: Int32
         , objtElementPersistent      :: Bool
         , objtElementParentId        :: Int32
         , objtElementTextureMaskId   :: Int32
         , objtElementUnknown1        :: Int32
         , objtElementUnknown2        :: Int32
         , objtElementUnknown3        :: Int32
         , objtElementUnknown4        :: Float
         , objtElementUnknown5        :: Float
         , objtElementUnknown6        :: Float
         , objtElementUnknown7        :: Float
         , objtElementUnknown8        :: Float
         , objtElementShapePointCount :: Int32
         , objtElementUnknown9        :: Float
         , objtElementUnknown10       :: Int32
         , objtElementUnknown11       :: Int32
         , objtElementShapePoints     :: [(Float, Float)]
         , objtElementUnknown12       :: Dictionary ObjtSub -- Always holds 12 elements
         }
       deriving (Show, Eq)

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
      <*> replicateM (fromIntegral adNumber) (unpack form)
      <*> unpack form

newtype ObjtSub = ObjtSub { objtSubUnObjtSub :: Dictionary ObjtMeta }
                  deriving (Show, Eq, Generic, Unpack)

-- | Only 'objtMetaUnknown1' seems to change meaningfully in this one, the rest is garbage.
data ObjtMeta = ObjtMeta
                  { objtMetaUnknown1   :: Int32
                  , objtMetaUnknown2   :: Int32
                  , objtMetaUnknown3   :: Int32 -- Points to eighth byte of this structure
                  , objtMetaUnknown4   :: Int32
                  , objtMetaUnknown5   :: Int32
                  , objtMetaUnknown6   :: Int32
                  , objtMetaUnknown7   :: Int32
                  , objtMetaUnknown8   :: Int32
                  , objtMetaUnknown9   :: Int32
                  , objtMetaUnknown10  :: Int32
                  , objtMetaUnknown11  :: Int32 -- Points to an empty string
                  , objtMetaIdentifier :: Int32
                  , objtMetaUnknown13  :: Int32
                  , objtMetaUnknown14  :: Int32
                  , objtMetaUnknown15  :: Int32
                  , objtMetaUnknown16  :: Int32
                  , objtMetaUnknown17  :: Int32
                  }
                deriving (Show, Eq, Generic, Unpack)



-- | Room data.
newtype Room = Room { roomUnRoom :: Dictionary RoomElement }
               deriving (Show, Eq)

instance Unpack Room where
  unpack form =
    chunk "ROOM" $ \_ ->
      Room <$> unpack form

data RoomElement =
       RoomElement
         { roomElementName           :: Pointer BS.ByteString
         , roomElementCaption        :: Pointer BS.ByteString
         , roomElementWidth          :: Word32
         , roomElementHeight         :: Word32
         , roomElementSpeed          :: Word32
         , roomElementPersistent     :: Bool
         , roomElementRgba           :: RGBA
         , roomElementDrawBGColor    :: Bool
         , roomElementUnknown1       :: Word32
         , roomElementFlags          :: RoomEntryFlags
         , roomElementBgOffset       :: Word32
         , roomElementViewOffset     :: Word32
         , roomElementObjOffset      :: Word32
         , roomElementTileOffset     :: Word32
         , roomElementWorld          :: Word32
         , roomElementTop            :: Word32
         , roomElementLeft           :: Word32
         , roomElementRight          :: Word32
         , roomElementBottom         :: Word32
         , roomElementGravityX       :: Float
         , roomElementGravityY       :: Float
         , roomElementMetersPerPixel :: Float
         , roomElementBackgrounds    :: Dictionary RoomBackground
         , roomElementViews          :: Dictionary RoomView
         , roomElementObjects        :: Dictionary RoomObject
         , roomElementTiles          :: Dictionary RoomTile
         }
       deriving (Show, Eq, Generic, Unpack)

data RoomBackground =
       RoomBackground
         { roomBackgroundEnabled    :: Bool
         , roomBackgroundForeground :: Bool
         , roomBackgroundBgDefIndex :: Int32
         , roomBackgroundX          :: Int32
         , roomBackgroundY          :: Int32
         , roomBackgroundTileX      :: Bool
         , roomBackgroundTileY      :: Bool
         , roomBackgroundSpeedX     :: Int32
         , roomBackgroundSpeedY     :: Int32
         , roomBackgroundIdentifier :: Int32
         }
       deriving (Show, Eq, Generic, Unpack)

data RoomView =
       RoomView
         { roomViewEnabled    :: Bool
         , roomViewViewX      :: Int32
         , roomViewViewY      :: Int32
         , roomViewViewWidth  :: Int32
         , roomViewViewHeight :: Int32
         , roomViewPortX      :: Int32
         , roomViewPortY      :: Int32
         , roomViewPortWidth  :: Int32
         , roomViewPortHeight :: Int32
         , roomViewBorderX    :: Int32
         , roomViewBorderY    :: Int32
         , roomViewSpeedX     :: Int32
         , roomViewSpeedY     :: Int32
         , roomViewIdentifier :: Int32
         }
       deriving (Show, Eq, Generic, Unpack)

data RoomObject =
       RoomObject
         { roomObjectX          :: Int32
         , roomObjectY          :: Int32
         , roomObjectIdentifier :: Int32
         , roomObjectInitCode   :: Int32
         , roomObjectUnknown5   :: Int32
         , roomObjectScaleX     :: Float
         , roomObjectScaleY     :: Float
         , roomObjectUnknown8   :: Int32
         , roomObjectRotation   :: Float
         }
       deriving (Show, Eq, Generic, Unpack)

data RoomTile =
       RoomTile
         { roomTileX          :: Int32
         , roomTileY          :: Int32
         , roomTileBgDefIndex :: Int32
         , roomTileSourceX    :: Int32
         , roomTileSourceY    :: Int32
         , roomTileWidth      :: Int32
         , roomTileHeight     :: Int32
         , roomTileTileDepth  :: Int32
         , roomTileIdentifier :: Int32
         , roomTileScaleX     :: Float
         , roomTileScaleY     :: Float
         , roomTileUnknown12  :: Int32
         }
       deriving (Show, Eq, Generic, Unpack)



-- | Empty.
newtype Dafl = Dafl { unDafl :: BSL.ByteString }
               deriving (Show, Eq)

instance Unpack Dafl where
  unpack _ =
    chunk "DAFL" $ \_ ->
      Dafl
        <$> getRemainingLazyByteString



-- | Sprite information as related to textures they reside in.
newtype Tpag = Tpag { tpagUnTpag :: Dictionary TpagElement }
               deriving (Show, Eq)

instance Unpack Tpag where
  unpack form =
    chunk "TPAG" $ \_ ->
      Tpag
        <$> unpack form

data TpagElement =
       TpagElement
         { tpagElementOffsetX        :: Word16
         , tpagElementOffsetY        :: Word16
         , tpagElementWidth          :: Word16
         , tpagElementHeight         :: Word16
         , tpagElementRenderX        :: Word16
         , tpagElementRenderY        :: Word16
         , tpagElementBoundingX      :: Word16
         , tpagElementBoundingY      :: Word16
         , tpagElementBoundingWidth  :: Word16
         , tpagElementBoundingHeight :: Word16
         , tpagElementImageId        :: Word16
         }
       deriving (Show, Eq, Generic, Unpack)



-- | Interpreted GameMaker code chunk. Will be empty if the game was compiled.
data Code =
       Code
         { codeOffset     :: Int            -- Not part of CODE chunk but we need this
         , codeOperations :: CodeOps
         , codeElements   :: [CodeFunction]
         }
       deriving (Show, Eq)

instance Unpack (Maybe Code) where
  unpack form = do
    cStart <- fromIntegral <$> bytesRead
    chunk "CODE" $ \cSize ->
      msum [ do True <- isEmpty
                return Nothing
           , do
                -- Size of the pointer list
                size <- getWord32le
                -- The pointer list itself
                _ <- skip $ 4 * fromIntegral size
                Just <$> do
                  Code
                           -- This could just be 'bytesRead' instead actually
                    <$> do pure $ cStart + 4 + 4 * fromIntegral size
                    <*> do CodeOps <$> do getLazyByteString . fromIntegral $ cSize - 4 * 6 * size - 4
                           -- Elements pointed to by the pointer list are at the very end
                    <*> do replicateM (fromIntegral size) $ do
                             CodeFunction
                               <$> unpack form
                               <*> unpack form
                               <*> unpack form
                               <*> do offsetLocal <- fromIntegral <$> bytesRead
                                      (cStart +) . (offsetLocal +) . fromIntegral <$> getInt32le
                               <*> unpack form
           ]

-- | Unpacked in "RiskOfRain.Decompilation"
newtype CodeOps = CodeOps BSL.ByteString
                  deriving (Show, Eq)

instance Unpack CodeOps where
  unpack _ =
    CodeOps <$> getRemainingLazyByteString

-- | A map of functions over 'CodeOps'. None of them overlap and there is no leftover code.
data CodeFunction =
       CodeFunction
         { codeFunctionName       :: Pointer BS.ByteString
         , codeFunctionSize       :: Word32
         , codeFunctionUnknown1   :: Word32
         , codeFunctionOffset     :: Int    -- Realigned this to global values
         , codeFunctionUnknown2   :: Word32
         }
       deriving (Show, Eq)



-- | Variable names and where they occur in code.
data Vari =
       Vari
         { variUnknown1 :: Word32
         , variUnknown2 :: Word32
         , variUnknown3 :: Word32
         , variElements :: [VariElement]
         }
       deriving (Show, Eq)

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
              <*> many (unpack form)
        ]

-- | Every address points to the next address and it repeats @occurences@ times.
data VariElement =
       VariElement
         { variElementName       :: Pointer BS.ByteString
         , variElementUnknown1   :: Int32
         , variElementUnknown2   :: Int32
         , variElementOccurences :: Int32
         , variElementAddress    :: Int32
         }
       deriving (Show, Eq, Generic, Unpack)



-- | Function information.
data Func =
       Func
         { funcPositions :: [FuncPosition]
         , funcArguments :: [FuncArguments]
         }
       deriving (Show, Eq)

instance Unpack (Maybe Func) where
  unpack form =
    chunk "FUNC" $ \_ ->
      msum
        [ do True <- isEmpty
             return Nothing
        , Just <$> do
            Func
              <$> do tripSize <- getWord32le
                     replicateM (fromIntegral tripSize) $ unpack form
              <*> do elemSize <- getWord32le
                     replicateM (fromIntegral elemSize) $ unpack form
        ]

-- | Same reasoning as with 'VariElement'.
data FuncPosition =
       FuncPosition
         { funcPositionName       :: Pointer BS.ByteString
         , funcPositionOccurences :: Word32
         , funcPositionAddress    :: Word32
         }
       deriving (Show, Eq, Generic, Unpack)

-- | Arguments each function consumes.
--
--   Note: first argument is always @"arguments"@, then optionally other ones.
data FuncArguments =
       FuncArguments
         { funcArgumentsName      :: Pointer BS.ByteString
         , funcArgumentsArguments :: [Pointer BS.ByteString]
         }
       deriving (Show, Eq)

instance Unpack FuncArguments where
  unpack form = do
    size <- fromIntegral <$> getWord32le
    FuncArguments
      <$> unpack form
      <*> do replicateM size $ do
               _argPositionId <- getWord32le
               unpack form



-- | Strings.
newtype Strg = Strg { strgUnStrg :: Dictionary StrgString }
               deriving (Show, Eq)

instance Unpack Strg where
  unpack form =
    chunk "STRG" $ \_ -> do
      strg <- Strg
                <$> unpack form
      _ <- getRemainingLazyByteString
      return strg

newtype StrgString = StrgString { strgStringUnStrgString :: BS.ByteString }
                     deriving (Show, Eq)

instance Unpack StrgString where
  unpack form = do
    _size <- getWord32le
    StrgString
      <$> unpack form



-- | Raw textures.
newtype Txtr = Txtr { txtrUnTxtr :: DictionaryS TxtrElement }
               deriving (Show, Eq)

instance Unpack Txtr where
  unpack form =
    chunk "TXTR" $ \_ -> do
      txtr <- Txtr
                <$> unpack form
      _ <- getRemainingLazyByteString
      return txtr

data TxtrElement =
       TxtrElement
         { txtrElementUnknown1 :: Word32
         , txtrElementImage    :: Pointer PNG
         }
       deriving (Show, Eq, Generic, Unpack)



-- | Raw audio files.
newtype Audo = Audo { audoUnAudo :: DictionaryS AudoFile }
               deriving (Show, Eq)

instance Unpack Audo where
  unpack form =
    chunk "AUDO" $ \_ -> do
      audo <- Audo
                <$> unpack form
      _ <- getRemainingLazyByteString
      return audo

newtype AudoFile = AudoFile { audoFileUnAudoFile :: BS.ByteString }
                   deriving (Show, Eq)

instance Unpack AudoFile where
  unpack _ = do
    audo <- AudoFile
              <$> do size <- fromIntegral <$> getWord32le
                     getByteString size
    _ <- getRemainingLazyByteString -- the first blob leaves 3 bytes hanging, after that
                                    -- it's mostly 2 or somethimes none.
    return audo
