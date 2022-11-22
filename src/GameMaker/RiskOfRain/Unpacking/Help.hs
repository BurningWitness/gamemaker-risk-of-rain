{-# LANGUAGE BangPatterns
           , OverloadedStrings #-}

module GameMaker.RiskOfRain.Unpacking.Help where

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
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Word
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



{-# INLINEABLE extChunk #-}
extChunk :: BS.ByteString -> Get Word32
extChunk name = do
  name' <- getByteString 4
  if name /= name'
    then fail $ "Header name does not match: " <> show name <> " /= " <> show name'
    else getWord32le

{-# INLINEABLE chunk #-}
-- | Consumes an IFF chunk, isolating the chunk contents.
chunk :: BS.ByteString -> (Word32 -> Get a) -> Get a
chunk name parser = do
  name' <- getByteString 4
  if name /= name'
    then fail $ "Header name does not match: " <> show name <> " /= " <> show name'
    else do
      size <- getWord32le
      isolate (fromIntegral size) (parser size)



{-# INLINEABLE getBool #-}
getBool :: Get Bool
getBool = (/= 0) <$> getWord32le

{-# INLINEABLE getByteStringNul #-}
getByteStringNul :: Get BS.ByteString
getByteStringNul = BSL.toStrict <$> getLazyByteStringNul

{-# INLINEABLE getChar #-}
getChar :: Get Char
getChar = chr . fromIntegral <$> getWord16le

{-# INLINEABLE getTime #-}
getTime :: Get UTCTime
getTime = systemToUTCTime . flip MkSystemTime 0 . fromIntegral <$> getWord32le



{-# INLINEABLE getPNG #-}
getPNG :: Get BS.ByteString
getPNG = do
  start <- bytesRead
  end <- lookAhead $ do
           signature <- getByteString 8
           if signature /= "\137PNG\r\n\SUB\n"
             then fail "Invalid PNG signature header"
             else
               let loop = pngIEND <|> (pngChunk >> loop)
               in loop >> bytesRead
  getByteString (fromIntegral $ end - start)
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



{-# INLINEABLE getPtr #-}
getPtr :: BSL.ByteString -> Get a -> Get a
getPtr form f = do
  pntr <- fromIntegral <$> getWord32le
  if pntr > BSL.length form
    then fail $ "Invalid pointer (" <> show pntr <> " > " <> show (BSL.length form) <> ")"
    else
      case runGetOrFail f $ BSL.drop (fromIntegral pntr) form of
        Left (_, _, err)  -> fail $ "Invalid pointer: " <> err
        Right (_, _, res) -> return res



{-# INLINEABLE getDict #-}
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
getDict :: Get a -> Get (Vector a)
getDict f = do
  size <- getWord32le
  _ptrs <- Vec.replicateM (fromIntegral size) getWord32le
  Vec.replicateM (fromIntegral size) f



{-# INLINEABLE getDictSized #-}
-- | A special kind of dictionary that puts every element [but the last one] 
--   in an 'isolate'.
--
--   This is made specifically for the 'SPRT' chunk, because every single element
--   of that one seems to end with random amounts of garbage.
getDictSized :: Get a -> Get (Vector a)
getDictSized f = do
  size <- getWord32le
  ptrs <- replicateM (fromIntegral size) getWord32le
  let diff = snd $ mapAccumL (\a b -> (b, Just $ b - a)) (headDef 0 ptrs) ptrs
             -- Will convert an array to Just distances between elements
      sizes = tail diff <> [Nothing]
              -- The first element is always zero, so it's removed and
              -- "parse all remaining input" is added as the last element size
  Vec.fromList <$> do
    forM sizes $ \s ->
      case s of
        Just s' -> isolate (fromIntegral s') f
        Nothing -> f
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
  {-# INLINEABLE rnf #-}
  rnf !_ = ()

{-# INLINEABLE getInfoFlags #-}
getInfoFlags :: Get (Set InfoFlag)
getInfoFlags = do
  flags <- getWord32le
  let (<==) val b = bool [val] [] $ flags .&. b == 0
  return . Set.fromList $ mconcat
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



data RGBA = RGBA Word8 Word8 Word8 Word8
            deriving Eq

instance NFData RGBA where
  {-# INLINEABLE rnf #-}
  rnf (RGBA r g b a) = rnf (r, g, b, a)

instance Show RGBA where
  show (RGBA r g b a) =
    let pad v | length (showHex v "") < 2 = replicate (2 - length (showHex v "")) '0' <> showHex v ""
              | otherwise                 = showHex v ""
    in '#' : mconcat [ pad r, pad g, pad b, pad a ]

{-# INLINEABLE getRGBA #-}
getRGBA :: Get RGBA
getRGBA =
  RGBA
    <$> getWord8
    <*> getWord8
    <*> getWord8
    <*> getWord8



data RoomEntryFlag = EnableViews
                   | ShowColor
                   | ClearDisplayBuffer
                     deriving (Show, Eq, Ord)

instance NFData RoomEntryFlag where
  {-# INLINEABLE rnf #-}
  rnf !_ = ()

{-# INLINEABLE getRoomEntryFlags #-}
getRoomEntryFlags :: Get (Set RoomEntryFlag)
getRoomEntryFlags = do
  flags <- getWord32le
  let (<==) val b = bool [val] [] $ flags .&. b == 0
  return . Set.fromList $ mconcat
                            [ EnableViews        <== 0x1
                            , ShowColor          <== 0x2
                            , ClearDisplayBuffer <== 0x4
                            ]



data SoundEntryFlag = Embedded
                    | Compressed
                    | Regular
                      deriving (Show, Eq, Ord)

instance NFData SoundEntryFlag where
  {-# INLINEABLE rnf #-}
  rnf !_ = ()

{-# INLINEABLE getSoundEntryFlags #-}
getSoundEntryFlags :: Get (Set SoundEntryFlag)
getSoundEntryFlags = do
  flags <- getWord32le
  let (<==) val b = bool [val] [] $ flags .&. b == 0
  return . Set.fromList $ mconcat
                            [ Embedded   <== 0x01
                            , Compressed <== 0x02
                            , Regular    <== 0x64
                            ]
