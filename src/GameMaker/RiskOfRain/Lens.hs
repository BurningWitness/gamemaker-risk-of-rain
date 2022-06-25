{-# LANGUAGE DataKinds
           , FlexibleInstances
           , FunctionalDependencies
           , KindSignatures
           , PolyKinds
           , TemplateHaskell #-}

module GameMaker.RiskOfRain.Lens where

import           GameMaker.RiskOfRain.Lens.Internal
import           GameMaker.RiskOfRain.Unpacking
import           GameMaker.RiskOfRain.Unpacking.Unpack

import           Data.ByteString (ByteString)
import           Data.Vector (Vector)
import           Lens.Micro
import           Prelude



class HasGen8        s a | s -> a where gen8      :: Lens' s a
class HasOptn        s a | s -> a where optn      :: Lens' s a
class HasExtn        s a | s -> a where extn      :: Lens' s a
class HasSond        s a | s -> a where sond      :: Lens' s a
class HasAgrp        s a | s -> a where agrp      :: Lens' s a
class HasSprt        s a | s -> a where sprt      :: Lens' s a
class HasBgnd        s a | s -> a where bgnd      :: Lens' s a
class HasPath        s a | s -> a where path      :: Lens' s a
class HasScpt        s a | s -> a where scpt      :: Lens' s a
class HasShdr        s a | s -> a where shdr      :: Lens' s a
class HasFont        s a | s -> a where font      :: Lens' s a
class HasTmln        s a | s -> a where tmln      :: Lens' s a
class HasObjt        s a | s -> a where objt      :: Lens' s a
class HasRoom        s a | s -> a where room      :: Lens' s a
class HasDafl        s a | s -> a where dafl      :: Lens' s a
class HasTpag        s a | s -> a where tpag      :: Lens' s a
class HasCode        s a | s -> a where code      :: Lens' s a
class HasVari        s a | s -> a where vari      :: Lens' s a
class HasFunc        s a | s -> a where func      :: Lens' s a
class HasStrg        s a | s -> a where strg      :: Lens' s a
class HasTxtr        s a | s -> a where txtr      :: Lens' s a
class HasAudo        s a | s -> a where audo      :: Lens' s a

class HasArguments   s a | s -> a where arguments   :: Lens' s a
class HasBackgrounds s a | s -> a where backgrounds :: Lens' s a
class HasCaption     s a | s -> a where caption     :: Lens' s a
class HasCharacters  s a | s -> a where characters  :: Lens' s a
class HasDisplayName s a | s -> a where displayName :: Lens' s a
class HasExtension   s a | s -> a where extension   :: Lens' s a
class HasFilename    s a | s -> a where filename    :: Lens' s a
class HasImage       s a | s -> a where image       :: Lens' s a
class HasName        s a | s -> a where name        :: Lens' s a
class HasName_       s a | s -> a where name_       :: Lens' s a
class HasPointer     s a | s -> a where pointer     :: Lens' s a
class HasObjects     s a | s -> a where objects     :: Lens' s a
class HasOperation   s a | s -> a where operation   :: Lens' s a
class HasOperationFs s a | s -> a where operationFs :: Lens' s a
class HasOperations  s a | s -> a where operations  :: Lens' s a
class HasTexture     s a | s -> a where texture     :: Lens' s a
class HasTextures    s a | s -> a where textures    :: Lens' s a
class HasTiles       s a | s -> a where tiles       :: Lens' s a
class HasType_       s a | s -> a where type_       :: Lens' s a
class HasViews       s a | s -> a where views       :: Lens' s a



makePartial "f" ''Form $ \s -> case s of
                                 "sond" -> False
                                 "sprt" -> False
                                 "agrp" -> False
                                 "bgnd" -> False
                                 "path" -> False
                                 "scpt" -> False
                                 "font" -> False
                                 "objt" -> False
                                 "room" -> False
                                 "tpag" -> False
                                 "strg" -> False
                                 "txtr" -> False
                                 "audo" -> False
                                 _      -> True



makePartial "g" ''Gen8 $ \s -> case s of
                                 "name"        -> False
                                 "filename"    -> False
                                 "name_"       -> False
                                 "displayName" -> False
                                 _             -> True

instance HasName Gen8 ByteString where
  name f el =
    f (unPointer $ gName el) <&> \q ->
      el { gName = Pointer q }

instance HasFilename Gen8 ByteString where
  filename f el =
    f (unPointer $ gFilename el) <&> \q ->
      el { gFilename = Pointer q }

instance HasName_ Gen8 ByteString where
  name_ f el =
    f (unPointer $ gName_ el) <&> \q ->
      el { gName_ = Pointer q }

instance HasDisplayName Gen8 ByteString where
  displayName f el =
    f (unPointer $ gDisplayName el) <&> \q ->
      el { gDisplayName = Pointer q }


makeFull "un" ''Optn


makePartial "e" ''Extn $ \s -> case s of
                                 "unknown1" -> False
                                 "unknown2" -> False
                                 _          -> True

instance HasUnknown1 Extn (Vector ExtnTriplet) where
  unknown1 f el =
    f (unDictionary $ eUnknown1 el) <&> \q ->
      el { eUnknown1 = Dictionary q }

instance HasUnknown2 Extn (Vector ExtnSegment) where
  unknown2 f el =
    f (unDictionary $ eUnknown2 el) <&> \q ->
      el { eUnknown2 = Dictionary q }

instance HasUnknown1 ExtnTriplet ByteString where
  unknown1 f el =
    f (unPointer $ etUnknown1 el) <&> \q ->
      el { etUnknown1 = Pointer q }

instance HasUnknown2 ExtnTriplet ByteString where
  unknown2 f el =
    f (unPointer $ etUnknown2 el) <&> \q ->
      el { etUnknown2 = Pointer q }

instance HasUnknown3 ExtnTriplet ByteString where
  unknown3 f el =
    f (unPointer $ etUnknown3 el) <&> \q ->
      el { etUnknown3 = Pointer q }

makePartial "es" ''ExtnSegment $ \s -> case s of
                                         "operations" -> False
                                         _            -> True

instance HasOperations ExtnSegment (Vector ExtnOperation) where
  operations f el =
    f (unDictionary $ esOperations el) <&> \q ->
      el { esOperations = Dictionary q }

makePartial "eo" ''ExtnOperation $ \s -> case s of
                                           "operationFs" -> False
                                           "operation"   -> False
                                           _             -> True

instance HasOperationFs ExtnOperation ByteString where
  operationFs f el =
    f (unPointer $ eoOperationFs el) <&> \q ->
      el { eoOperationFs = Pointer q }

instance HasOperation ExtnOperation ByteString where
  operation f el =
    f (unPointer $ eoOperation el) <&> \q ->
      el { eoOperation = Pointer q }



instance HasSond Form (Vector SondElement) where
  sond f form =
    f (unDictionary . unSond $ fSond form) <&> \q ->
      form { fSond = Sond $ Dictionary q }

makePartial "se" ''SondElement $ \s -> case s of
                                         "name"      -> False
                                         "extension" -> False
                                         "filename"  -> False
                                         _           -> True

instance HasName SondElement ByteString where
  name f el =
    f (unPointer $ seName el) <&> \q ->
      el { seName = Pointer q }

instance HasExtension SondElement ByteString where
  extension f el =
    f (unPointer $ seExtension el) <&> \q ->
      el { seExtension = Pointer q }

instance HasFilename SondElement ByteString where
  filename f el =
    f (unPointer $ seFilename el) <&> \q ->
      el { seFilename = Pointer q }


instance HasAgrp Form (Vector ()) where
  agrp f form =
    f (unDictionary . unAgrp $ fAgrp form) <&> \q ->
      form { fAgrp = Agrp $ Dictionary q }



instance HasSprt Form (Vector SprtElement) where
  sprt f form =
    f (unDictionaryS . unSprt $ fSprt form) <&> \q ->
      form { fSprt = Sprt $ DictionaryS q }

makePartial "spe" ''SprtElement $ \s -> case s of
                                          "name"     -> False
                                          "textures" -> False
                                          _          -> True

instance HasName SprtElement ByteString where
  name f el =
    f (unPointer $ speName el) <&> \q ->
      el { speName = Pointer q }

instance HasTextures SprtElement (Vector TpagElement) where
  textures f el =
    f (unPointer <$> speTextures el) <&> \q ->
      el { speTextures = Pointer <$> q }



instance HasBgnd Form (Vector BgndElement) where
  bgnd f form =
    f (unDictionary . unBgnd $ fBgnd form) <&> \q ->
      form { fBgnd = Bgnd $ Dictionary q }

makePartial "be" ''BgndElement $ \s -> case s of
                                         "name"    -> False
                                         "texture" -> False
                                         _         -> True

instance HasName BgndElement ByteString where
  name f el =
    f (unPointer $ beName el) <&> \q ->
      el { beName = Pointer q }

instance HasTexture BgndElement TpagElement where
  texture f el =
    f (unPointer $ beTexture el) <&> \q ->
      el { beTexture = Pointer q }


instance HasPath Form (Vector ()) where
  path f form =
    f (unDictionary . unPath $ fPath form) <&> \q ->
      form { fPath = Path $ Dictionary q }


instance HasScpt Form (Vector ScptBinding) where
  scpt f form =
    f (unDictionary . unScpt $ fScpt form) <&> \q ->
      form { fScpt = Scpt $ Dictionary q }

makePartial "sb" ''ScptBinding $ \s -> case s of
                                         "pointer" -> False
                                         _         -> True

instance HasPointer ScptBinding ByteString where
  pointer f el =
    f (unPointer $ sbPointer el) <&> \q ->
      el { sbPointer = Pointer q }



makeFull "un" ''Shdr


instance HasFont Form (Vector FontElement) where
  font f form =
    f (unDictionary . unFont $ fFont form) <&> \q ->
      form { fFont = Font $ Dictionary q }

makePartial "fe" ''FontElement $ \s -> case s of
                                         "type"       -> False
                                         "name"       -> False
                                         "texture"    -> False
                                         "characters" -> False
                                         _            -> True

instance HasType_ FontElement ByteString where
  type_ f el =
    f (unPointer $ feType el) <&> \q ->
      el { feType = Pointer q }

instance HasName FontElement ByteString where
  name f el =
    f (unPointer $ feName el) <&> \q ->
      el { feName = Pointer q }

instance HasTexture FontElement TpagElement where
  texture f el =
    f (unPointer $ feTexture el) <&> \q ->
      el { feTexture = Pointer q }

instance HasCharacters FontElement (Vector FontBit) where
  characters f el =
    f (unDictionary $ feCharacters el) <&> \q ->
      el { feCharacters = Dictionary q }

makeFull "fb" ''FontBit


makeFull "un" ''Tmln


instance HasObjt Form (Vector ObjtElement) where
  objt f form =
    f (unDictionary . unObjt $ fObjt form) <&> \q ->
      form { fObjt = Objt $ Dictionary q }

makePartial "oe" ''ObjtElement $ \s -> case s of
                                         "name"      -> False
                                         "unknown12" -> False
                                         _           -> True

instance HasName ObjtElement ByteString where
  name f el =
    f (unPointer $ oeName el) <&> \q ->
      el { oeName = Pointer q }

instance HasUnknown12 ObjtElement (Vector ObjtSub) where
  unknown12 f el =
    f (unDictionary $ oeUnknown12 el) <&> \q ->
      el { oeUnknown12 = Dictionary q }

makeFull "om" ''ObjtMeta


instance HasRoom Form (Vector RoomElement) where
  room f form =
    f (unDictionary . unRoom $ fRoom form) <&> \q ->
      form { fRoom = Room $ Dictionary q }

makePartial "re" ''RoomElement $ \s -> case s of
                                         "name"        -> False
                                         "caption"     -> False
                                         "backgrounds" -> False
                                         "views"       -> False
                                         "objects"     -> False
                                         "tiles"       -> False
                                         _             -> True

instance HasName RoomElement ByteString where
  name f el =
    f (unPointer $ reName el) <&> \q ->
      el { reName = Pointer q }

instance HasCaption RoomElement ByteString where
  caption f el =
    f (unPointer $ reCaption el) <&> \q ->
      el { reCaption = Pointer q }

instance HasBackgrounds RoomElement (Vector RoomBackground) where
  backgrounds f el =
    f (unDictionary $ reBackgrounds el) <&> \q ->
      el { reBackgrounds = Dictionary q }

instance HasViews RoomElement (Vector RoomView) where
  views f el =
    f (unDictionary $ reViews el) <&> \q ->
      el { reViews = Dictionary q }

instance HasObjects RoomElement (Vector RoomObject) where
  objects f el =
    f (unDictionary $ reObjects el) <&> \q ->
      el { reObjects = Dictionary q }

instance HasTiles RoomElement (Vector RoomTile) where
  tiles f el =
    f (unDictionary $ reTiles el) <&> \q ->
      el { reTiles = Dictionary q }

makeFull "rb" ''RoomBackground
makeFull "rv" ''RoomView
makeFull "ro" ''RoomObject
makeFull "rt" ''RoomTile



instance HasTpag Form (Vector TpagElement) where
  tpag f form =
    f (unDictionary . unTpag $ fTpag form) <&> \q ->
      form { fTpag = Tpag $ Dictionary q }

makeFull "te" ''TpagElement


makeFull "c" ''Code
makePartial "cf" ''CodeFunction $ \s -> case s of
                                          "name" -> False
                                          _      -> True

instance HasName CodeFunction ByteString where
  name f el =
    (\t -> el { cfName = Pointer t }) <$> f (unPointer $ cfName el)


makeFull "v" ''Vari
makePartial "ve" ''VariElement $ \s -> case s of
                                         "name" -> False
                                         _      -> True

instance HasName VariElement ByteString where
  name f el =
    (\t -> el { veName = Pointer t }) <$> f (unPointer $ veName el)


makeFull "f" ''Func
makePartial "fp" ''FuncPosition $ \s -> case s of
                                          "name" -> False
                                          _      -> True

instance HasName FuncPosition ByteString where
  name f arg =
    f (unPointer $ fpName arg) <&> \q ->
      arg { fpName = Pointer q }

instance HasName FuncArguments ByteString where
  name f arg =
    f (unPointer $ faName arg) <&> \q ->
      arg { faName = Pointer q }

instance HasArguments FuncArguments (Vector ByteString) where
  arguments f arg =
    f (unPointer <$> faArguments arg) <&> \q ->
      arg { faArguments = Pointer <$> q }


instance HasStrg Form (Vector ByteString) where
  strg f form =
    f (fmap unStrgString . unDictionary . unStrg $ fStrg form) <&> \q ->
      form { fStrg = Strg . Dictionary $ StrgString <$> q }


instance HasTxtr Form (Vector TxtrElement) where
  txtr f form =
    f (unDictionaryS . unTxtr $ fTxtr form) <&> \q ->
      form { fTxtr = Txtr $ DictionaryS q }


makePartial "te" ''TxtrElement $ \s -> case s of
                                         "image" -> False
                                         _       -> True

instance HasImage TxtrElement ByteString where
  image f el =
    (\t -> el { teImage = Pointer $ PNG t }) <$> f (unPNG . unPointer $ teImage el)


instance HasAudo Form (Vector ByteString) where
  audo f form =
    f (fmap unAudoFile . unDictionaryS . unAudo $ fAudo form) <&> \q ->
      form { fAudo = Audo . DictionaryS $ AudoFile <$> q }
