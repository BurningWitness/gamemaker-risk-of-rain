{-# LANGUAGE BangPatterns
           , DefaultSignatures
           , DeriveGeneric
           , FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings
           , TypeFamilies
           , TypeOperators #-}

module GameMaker.RiskOfRain.Unpacking.Unpack where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Bits
import           Data.Bool (bool)
import           Data.Binary.Get
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char
import           Data.List
import           Data.Int
import qualified Data.Set as S
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Word
import           GHC.Generics
import           Lens.Micro.Platform hiding (to)
import           Lens.Micro.Internal
import           Numeric
import           Prelude



-- | Looks up what chunk the pointer corresponds to. For debug only.
pinpoint :: Int -> BSL.ByteString -> Either [Char] BS.ByteString
pinpoint size =
  bimap (\(_, _, s) -> s) (\(_, _, a) -> a) . runGetOrFail throughForm
  where
    throughForm = do
      "FORM" <- getByteString 4
      formSize <- fromIntegral <$> getWord32le
      if size > formSize + 8
        then fail "Outside the form"
        else through $ size - 8

    through siz = do
      chunkName <- getByteString 4
      chunkSize <- fromIntegral <$> getWord32le
      if siz < chunkSize + 8
        then return chunkName
        else do
          _ <- skip chunkSize
          through (siz - chunkSize - 8)



extChunk :: BS.ByteString -> Get Word32
extChunk name = do
  name' <- getByteString 4
  if name /= name'
    then fail $ "Header name does not match: " <> show name <> " /= " <> show name'
    else getWord32le

-- | Consumes an IFF chunk, isolating the chunk contents.
chunk :: BS.ByteString -> (Word32 -> Get a) -> Get a
chunk name parser = do
  name' <- getByteString 4
  if name /= name'
    then fail $ "Header name does not match: " <> show name <> " /= " <> show name'
    else do
      size <- getWord32le
      isolate (fromIntegral size) (parser size)



-- | A type class for unpacking the GameMaker data file.
class Unpack a where
  -- | The input 'ByteString' is the entire file, specifically for quirky 'Pointer'
  --   shenanigans.
  --
  --   Can't use a 'Reader' here, since 'Parser' is obviously not a monad transformer.
  unpack :: BSL.ByteString -> Get a
  default unpack :: (Generic a, GUnpack (Rep a)) => BSL.ByteString -> Get a
  unpack = genericUnpack


instance Unpack () where
  unpack _ = return ()

instance Unpack Bool where
  unpack _ = (/= 0) <$> getWord32le

instance Unpack Word8 where
  unpack _ = getWord8

-- | 'Char' is used only one in the entire GameMaker data structure and it's 16-bit.
instance Unpack Char where
  unpack _ = chr . fromIntegral <$> getWord16le

instance Unpack Int16 where
  unpack _ = getInt16le

instance Unpack Int32 where
  unpack _ = getInt32le

instance Unpack Word16 where
  unpack _ = getWord16le

instance Unpack Word32 where
  unpack _ = getWord32le

instance Unpack Float where
  unpack _ = getFloatle

-- | This assumes the 'ByteString' is null-terminated
instance Unpack BS.ByteString where
  unpack _ = BSL.toStrict <$> getLazyByteStringNul

instance Unpack UTCTime where
  unpack _ =
    systemToUTCTime . flip MkSystemTime 0 . fromIntegral <$> getWord32le

instance (Unpack a, Unpack b) => Unpack (a, b) where
  unpack form =
    (,)
      <$> unpack form
      <*> unpack form


genericUnpack :: (Generic a, GUnpack (Rep a)) => BSL.ByteString -> Get a
genericUnpack form = to <$> gunpack form

class GUnpack f where
  gunpack :: BSL.ByteString -> Get (f a)

instance GUnpack V1 where
  gunpack _ = return undefined

instance GUnpack U1 where
  gunpack _ = return U1

instance (GUnpack a, GUnpack b) => GUnpack (a :*: b) where
  gunpack form = (:*:) <$> gunpack form <*> gunpack form

instance GUnpack a => GUnpack (M1 i c a) where
  gunpack form = M1 <$> gunpack form

instance Unpack a => GUnpack (K1 i a) where
  gunpack form = K1 <$> unpack form



-- | Just a wrapper so we know we want to decode a 'PNG' file and not some other image.
newtype PNG = PNG { unPNG :: BS.ByteString }
              deriving Eq

instance Show PNG where
  show _ = "PNG"

instance NFData PNG where
  rnf (PNG png) = rnf png

instance Unpack PNG where
  unpack _ = do
    start <- bytesRead
    end <- lookAhead $ do
             signature <- getByteString 8
             if signature /= "\137PNG\r\n\SUB\n"
               then fail "Invalid PNG signature header"
               else
                 let loop = pngIEND <|> (pngChunk >> loop)
                 in loop >> bytesRead
    PNG <$> getByteString (fromIntegral $ end - start)
    where
      pngChunk = do
        n <- fromIntegral <$> getWord32be -- Keep in mind PNG uses big-endian format
        skip 4 -- Chunk name
        skip n
        skip 4 -- CRC
        return ()

      pngIEND = do
        n <- fromIntegral <$> getWord32be
        iend <- getByteString 4
        if iend /= "IEND"
          then fail "Not an IEND chunk"
          else do
            skip n
            skip 4 -- CRC
            return ()



-- | An address within a file. These are pointing from the dead start of the file.
newtype Pointer a = Pointer { unPointer :: a }
                    deriving (Show, Eq)

instance NFData a => NFData (Pointer a) where
  rnf (Pointer a) = rnf a

instance Unpack a => Unpack (Pointer a) where
  unpack form = do
    pntr <- fromIntegral <$> getWord32le
    if pntr > BSL.length form
      then fail $ "Invalid pointer (" <> show pntr <> " > " <> show (BSL.length form) <> ")"
      else
        case runGetOrFail (unpack form) $ BSL.drop (fromIntegral pntr) form of
          Left (_, _, err) -> fail $ "Invalid pointer: " <> err
          Right (_, _, res) -> return $ Pointer res



-- | A number of elements, followed by a list of 'Pointer's to said elements,
--   followed by elements themselves.
--
--   We don't even need the pointers stored because we know types of things we're
--   looking up, if we ever wish to reencode the file we can (and should) just
--   recalculate the pointers.
--
--   Note: this doesn't use 'isolate' because we can't know the size of the last element
--         from pointers alone, otherwise it'd fail miserably in data structures with
--         two 'Dictionary's in them
newtype Dictionary a = Dictionary { unDictionary :: Vector a }
                       deriving (Show, Eq, Foldable)

type instance IxValue (Dictionary a) = a
                    
type instance Index (Dictionary a) = Int

instance Each (Dictionary a) (Dictionary b) a b where
  each f = fmap Dictionary . each f . unDictionary

instance Ixed (Dictionary a) where
  ix k f = fmap Dictionary . ix k f . unDictionary

instance NFData a => NFData (Dictionary a) where
  rnf (Dictionary vec) = rnf vec

instance Unpack a => Unpack (Dictionary a) where
  unpack form = do
    size <- getWord32le
    _ptrs <- Vec.replicateM (fromIntegral size) getWord32le
    Dictionary <$> Vec.replicateM (fromIntegral size) (unpack form)



-- | A special kind of 'Dictionary' that puts every element [but the last one] 
--   in an 'isolate'.
--
--   This is made specifically for the 'SPRT' chunk, because every single element
--   of that one seems to end with random amounts of garbage.
newtype DictionaryS a = DictionaryS { unDictionaryS :: Vector a }
                        deriving (Show, Eq, Foldable)

type instance IxValue (DictionaryS a) = a

type instance Index (DictionaryS a) = Int

instance Each (DictionaryS a) (DictionaryS b) a b where
  each f = fmap DictionaryS . each f . unDictionaryS

instance Ixed (DictionaryS a) where
  ix k f = fmap DictionaryS . ix k f . unDictionaryS

instance NFData a => NFData (DictionaryS a) where
  rnf (DictionaryS vec) = rnf vec

instance Unpack a => Unpack (DictionaryS a) where
  unpack form = do
    size <- getWord32le
    ptrs <- replicateM (fromIntegral size) getWord32le
    let diff = snd $ mapAccumL (\a b -> (b, Just $ b - a)) (headDef 0 ptrs) ptrs
               -- Will convert an array to Just distances between elements
        sizes = tail diff <> [Nothing]
                -- The first element is always zero, so it's removed and
                -- "parse all remaining input" is added as the last element size
    DictionaryS . Vec.fromList <$> do
      forM sizes $ \s ->
        case s of
          Just s' -> isolate (fromIntegral s') $ unpack form
          Nothing -> unpack form
    where
      headDef def []    = def
      headDef _   (a:_) = a



data InfoFlag = Fullscreen
              | SyncVertex1
              | SyncVertex2
              | Interpolate
              | ShowCursor
              | Sizeable
              | ScreenKey
              | SyncVertex3
              | StudioVersionB1
              | StudioVersionB2
              | StudioVersionB3
              | SteamEnabled
              | LocalDataEnabled
              | BorderlessWindow
                deriving (Show, Eq, Ord)

instance NFData InfoFlag where
  rnf !_ = ()

newtype InfoFlags = InfoFlags { unInfoFlags :: S.Set InfoFlag }
                    deriving (Show, Eq)

instance NFData InfoFlags where
  rnf (InfoFlags st) = rnf st

instance Unpack InfoFlags where
  unpack _ = do
    flags <- getWord32le
    let (<==) val b = bool [val] [] $ flags .&. b == 0
    return
      . InfoFlags
          . S.fromList
              $ mconcat
                  [ Fullscreen        <== 0x0001
                  , SyncVertex1       <== 0x0002
                  , SyncVertex2       <== 0x0004
                  , Interpolate       <== 0x0008
                  , ShowCursor        <== 0x0020
                  , Sizeable          <== 0x0040
                  , ScreenKey         <== 0x0080
                  , SyncVertex3       <== 0x0100
                  , StudioVersionB1   <== 0x0200
                  , StudioVersionB2   <== 0x0400
                  , StudioVersionB3   <== 0x0800
                  , SteamEnabled      <== 0x1000
                  , LocalDataEnabled  <== 0x2000
                  , BorderlessWindow  <== 0x4000
                  ]



newtype CRC32 = CRC32 Word32
                deriving (Show, Eq)

instance NFData CRC32 where
  rnf (CRC32 crc) = rnf crc

instance Unpack CRC32 where
  unpack _ =
    CRC32
      <$> getWord32le



data MD5 = MD5 Word32 Word32 Word32 Word32
           deriving (Show, Eq)

instance NFData MD5 where
  rnf (MD5 a b c d) = rnf (a, b, c, d)

instance Unpack MD5 where
  unpack _ = do
    MD5
      <$> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le



data RGBA = RGBA Word8 Word8 Word8 Word8
            deriving (Generic, Eq)

instance NFData RGBA where
  rnf (RGBA r g b a) = rnf (r, g, b, a)

instance Show RGBA where
  show (RGBA r g b a) =
    let pad v | length (showHex v "") < 2 = replicate (2 - length (showHex v "")) '0' <> showHex v ""
              | otherwise                 = showHex v ""
    in '#' : mconcat [ pad r, pad g, pad b, pad a ]

instance Unpack RGBA



data RoomEntryFlag = EnableViews
                   | ShowColor
                   | ClearDisplayBuffer
                     deriving (Show, Eq, Ord)

instance NFData RoomEntryFlag where
  rnf !_ = ()

newtype RoomEntryFlags = RoomEntryFlags { unRoomEntryFlags :: S.Set RoomEntryFlag }
                         deriving (Show, Eq)

instance NFData RoomEntryFlags where
  rnf (RoomEntryFlags st) = rnf st

instance Unpack RoomEntryFlags where
   unpack _ = do
    flags <- getWord32le
    let (<==) val b = bool [val] [] $ flags .&. b == 0
    return
      . RoomEntryFlags
          . S.fromList
              $ mconcat
                  [ EnableViews        <== 0x1
                  , ShowColor          <== 0x2
                  , ClearDisplayBuffer <== 0x4
                  ]



data SoundEntryFlag = Embedded
                    | Compressed
                    | Regular
                      deriving (Show, Eq, Ord)

instance NFData SoundEntryFlag where
  rnf !_ = ()

newtype SoundEntryFlags = SoundEntryFlags { unSoundEntryFlags :: S.Set SoundEntryFlag }
                          deriving (Show, Eq)

instance NFData SoundEntryFlags where
  rnf (SoundEntryFlags st) = rnf st

instance Unpack SoundEntryFlags where
   unpack _ = do
    flags <- getWord32le
    let (<==) val b = bool [val] [] $ flags .&. b == 0
    return
      . SoundEntryFlags
          . S.fromList
              $ mconcat
                  [ Embedded   <== 0x01
                  , Compressed <== 0x02
                  , Regular    <== 0x64
                  ]
