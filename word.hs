{-# LANGUAGE BangPatterns, NoMonomorphismRestriction, 
             RankNTypes, FlexibleContexts, TupleSections #-}

-- A simple wc-like program using Data.Iteratee.
-- Demonstrates a few different ways of composing iteratees.
module Main where

import Prelude as P
import Data.Iteratee as I
import qualified Data.Iteratee as I
                          -- (length, head, Iteratee (..), joinI, run,
                          --  takeUpTo, enumPure1Chunk, filter, drop,
                          --  mapChunksM_, foldl', enumPair)
import qualified Data.ListLike as LL 
-- import Data.Iteratee.Char as C
-- import qualified Data.Iteratee as I (foldl, length, Iteratee (..), joinI)
import qualified Data.ByteString.Char8 as BC

import Data.Word
import Data.Char

import System.Environment (getArgs,) 
import System.Exit (exitWith)
import Control.Monad.Trans
-- import Data.Iteratee.ListLike
import qualified Data.IntMap as  IM
import qualified Data.Map as M
import Data.Maybe
import Control.Monad as CM
import System.IO (hFileSize, hClose, openFile, hGetContents, 
                  IOMode(..),withBinaryFile, hSetBuffering, 
                  BufferMode (..), stdout,Handle(..)
                 )
import Data.Ratio
import Data.Int

import Data.Iteratee.IO (enumFile)
import Text.Regex.Posix ((=~))
import Data.Time (getCurrentTime, diffUTCTime)
import System.CPUTime (getCPUTime)
import System.Process (runCommand, waitForProcess, getProcessExitCode)
-- import System (exitWith,)
import Data.List (elemIndices,sortBy)
import Control.Exception (bracket)
import System.FilePath.Windows  (takeDirectory, takeBaseName)
import System.Directory (copyFile)
-- import Data.Iteratee.Char
import Expr
import RtfEnc
-- import Control.Monad.State
import  Control.Monad.Trans.State
-- import Control.Monad.Reader


--------------------------------------------------

--  An iteratee to calculate the number of characters in a stream.
--  Very basic, assumes ASCII, not particularly efficient.
iNumChars :: Monad m => Iteratee BC.ByteString m Int
iNumChars = I.length

--  An iteratee to calculate the number of words in a stream of Word8's.
-- this operates on a Word8 stream in order to use ByteStrings.
--
-- This function converts the stream of Word8s into a stream of words,
-- then counts the words with Data.Iteratee.length
-- This is the equivalent of "length . BC.words".
-- iNumWords :: Monad m => I.Iteratee BC.ByteString m Int
-- iNumWords = I.joinI $ enumWordsBS I.length

--  Count the number of lines, in the same manner as numWords.
-- iNumLines :: Monad m => I.Iteratee BC.ByteString m Int
-- iNumLines = I.joinI $ enumLinesBS I.length

--  A much more efficient numLines using the foldl' iteratee.
-- Rather than converting a stream, this simply counts newline characters.
iNumLines2 :: Monad m => I.Iteratee BC.ByteString m Int
iNumLines2 = I.foldl' step 0
 where
  step !acc el = if el == (fromIntegral $ ord '\n') then acc + 1 else acc

-- Combine multiple iteratees into a single unit using "enumPair".
-- The iteratees combined with enumPair are run in parallel.
-- Any number of iteratees can be joined with multiple enumPair's.
iComb :: Monad m => I.Iteratee BC.ByteString m (Int, Int)
iComb = iNumLines2 `I.enumPair` iNumChars
  
a1 = P.foldl (\x -> \y -> y ++ ":" ++ x) "_"  ["asd"] -- "asd:_"
a2 = foldr (\x -> \y -> y ++ ":" ++ x) "_"  ["asd"]         -- "_:asd"

iChar  :: Monad m => Char -> I.Iteratee [Word8] m Int
iChar ch  =    I.foldl' (step' ch)  0
            --  Prelude.foldl step 0 [1..100]
-- I.foldl'      :: (a -> el -> a) -> a -> Iteratee s m a
-- foldr         :: (a -> b  -> b) -> b -> [a] -> b
-- Prelude.foldl :: (a -> b  -> a) -> a -> [b] -> a
 where
  step !acc el = do 
    c <- I.head
    if  c == (fromIntegral $ ord ch) 
                   then acc + 1 else acc

step' ch !acc el = if el == (fromIntegral $ ord ch) then acc + 1 else acc

iByteCounter :: Monad m => Iteratee [Word8] m Int
iByteCounter = I.length
 
iHead:: Monad m => Iteratee [Word8] m Word8
iHead = I.head

-- f = "c:\\test.temp.rtf"
enumRtf ii  = do
  i' <- enumFile 8192 testrtf ii -- бьет файл на отрезки, которые передает
                             -- в поток, над потоком выполняется действие ii
  result <- run i' -- выполняем цепочку действий
  return result
--  print result

-- enumTiff' ii  = do
-- --  f:_   <-  getArgs
--   words <-  fileDriverVBuf 65536 ii tiff  -- f
--   print words
--   -- words <- enumFile 8192 testFile twoIter
--   -- r <- run words
--   -- print r

iLogger1 = liftIO . putStrLn -- отображает содержимое текущего чанка
iLogger = mapChunksM_ iLogger1 -- выполнит iLogger1 над всеми чанками 
-- mapChunksM_ - маппит все чанки на функцию, иначе итератор надо 
--               задавать рекурсивно

t3 = enumRtf iByteCounter
-- t4 = enumRtf iNumWords
-- t5 = enumRtf  iNumLines
t6 = enumRtf iNumLines2 
t7 = enumRtf  iLogger
t8 = enumRtf  $ iChar '{'
t11 = enumRtf iHead

-- iter2 = do -- :: Iteratee s m b
--   I.drop 4
--   I.head

-- -- t8  = enumPure1Chunk [1..10] iter2  -- :: m (Iteratee [t] m t)
-- --         >>= run >>= print
  
-- iFilt = joinI -- выковыриваем то что отфильтровали
--              $ I.filter (>5) iter2 -- отфильтровываем во вложенный Stream
-- t9 = enumPure1Chunk [10,1,4,6,7,4,2,8,5,9::Int] iFilt >>= run >>= print


-- -- filt = joinI $ I.filter (=='s') (I.head)

iLogN = joinI $ I.takeUpTo 100 iLogger
t10 = enumRtf iLogN 

--iHead :: Monad m => Iteratee BC.ByteString m Word8
--iHead = I.head
--t11 = enumTiff iHead

----------------------------------------------

-- показываем что потоки разные?
type EnumeratorM sFrom sTo m a = Iteratee sTo m a -> m (Iteratee sFrom m a)

joinL :: (Monad m, Nullable s) => m (Iteratee s m a) -> Iteratee s m a
joinL = join . lift

note :: (MonadIO m, Nullable s) => [String] -> Iteratee s m ()
note = liftIO . putStrLn . concat

note' :: (MonadIO m, Nullable s) => [String] -> Iteratee s m ()
note' = liftIO . print .concat


type TIFFDict = IM.IntMap TIFFDE

data TIFFDE = TIFFDE { tiffde_count :: Int,        -- number of items
                       tiffde_enum  :: TIFFDE_ENUM  -- enumerator to get values
                     } 
              
-- data RTFDE = 
-- instance Show TIFFDE where
--   show a = show $ tiffde_count a

data TIFFDE_ENUM = -- enumerators
      TEN_CHAR (forall a m. Monad m => EnumeratorM [Word8] [Char] m a)
    | TEN_BYTE (forall a m. Monad m => EnumeratorM [Word8] [Word8] m a)
    | TEN_INT  (forall a m. Monad m => EnumeratorM [Word8] [Int] m a)
    | TEN_RAT  (forall a m. Monad m => EnumeratorM [Word8] [Ratio Int] m a)

-- Standard TIFF data types
data TIFF_TYPE = TT_NONE  -- 0
  | TT_byte      -- 1   8-bit unsigned integer
  | TT_ascii     -- 2   8-bit bytes with last byte null
  | TT_short     -- 3   16-bit unsigned integer
  | TT_long      -- 4   32-bit unsigned integer
  | TT_rational  -- 5   64-bit fractional (numer+denominator)
                                -- The following was added in TIFF 6.0
  | TT_sbyte     -- 6   8-bit signed (2s-complement) integer
  | TT_undefined -- 7   An 8-bit byte, "8-bit chunk"
  | TT_sshort    -- 8   16-bit signed (2s-complement) integer
  | TT_slong     -- 9   32-bit signed (2s-complement) integer
  | TT_srational -- 10  "signed rational",  two SLONGs (num+denominator)
  | TT_float     -- 11  "IEEE 32-bit float", single precision (4-byte)
  | TT_double    -- 12  "IEEE 64-bit double", double precision (8-byte)
 deriving (Eq, Enum, Ord, Bounded, Show)

data TIFF_TAG = TG_other Int            -- other than below
  | TG_SUBFILETYPE                     -- subfile data descriptor
  | TG_OSUBFILETYPE                    -- +kind of data in subfile
  | TG_IMAGEWIDTH                      -- image width in pixels
  | TG_IMAGELENGTH                     -- image height in pixels
  | TG_BITSPERSAMPLE                   -- bits per channel (sample)
  | TG_IMAGEDESCRIPTION                -- info about image
  | TG_COMPRESSION              -- data compression technique
  | TG_PHOTOMETRIC              -- photometric interpretation
  | TG_THRESHOLDING             -- +thresholding used on data
  | TG_CELLWIDTH                -- +dithering matrix width
  | TG_CELLLENGTH               -- +dithering matrix height
  | TG_FILLORDER                -- +data order within a byte
  | TG_DOCUMENTNAME             -- name of doc. image is from
  | TG_MAKE                     -- scanner manufacturer name
  | TG_MODEL                    -- scanner model name/number
  | TG_STRIPOFFSETS             -- offsets to data strips
  | TG_ORIENTATION              -- +image orientation
  | TG_SAMPLESPERPIXEL          -- samples per pixel
  | TG_ROWSPERSTRIP             -- rows per strip of data
  | TG_STRIPBYTECOUNTS          -- bytes counts for strips
  | TG_MINSAMPLEVALUE           -- +minimum sample value
  | TG_MAXSAMPLEVALUE           -- maximum sample value
  | TG_XRESOLUTION              -- pixels/resolution in x
  | TG_YRESOLUTION              -- pixels/resolution in y
  | TG_PLANARCONFIG             -- storage organization
  | TG_PAGENAME                 -- page name image is from
  | TG_XPOSITION                -- x page offset of image lhs
  | TG_YPOSITION                -- y page offset of image lhs
  | TG_FREEOFFSETS              -- +byte offset to free block
  | TG_FREEBYTECOUNTS           -- +sizes of free blocks
  | TG_GRAYRESPONSEUNIT         -- gray scale curve accuracy
  | TG_GRAYRESPONSECURVE        -- gray scale response curve
  | TG_GROUP3OPTIONS            -- 32 flag bits
  | TG_GROUP4OPTIONS            -- 32 flag bits
  | TG_RESOLUTIONUNIT           -- units of resolutions
  | TG_PAGENUMBER               -- page numbers of multi-page
  | TG_COLORRESPONSEUNIT        -- color scale curve accuracy
  | TG_COLORRESPONSECURVE       -- RGB response curve
  | TG_SOFTWARE                 -- name & release
  | TG_DATETIME                 -- creation date and time
  | TG_ARTIST                   -- creator of image
  | TG_HOSTCOMPUTER             -- machine where created
  | TG_PREDICTOR                -- prediction scheme w/ LZW
  | TG_WHITEPOINT               -- image white point
  | TG_PRIMARYCHROMATICITIES    -- primary chromaticities
  | TG_COLORMAP                 -- RGB map for pallette image
  | TG_BADFAXLINES              -- lines w/ wrong pixel count
  | TG_CLEANFAXDATA             -- regenerated line info
  | TG_CONSECUTIVEBADFAXLINES   -- max consecutive bad lines
  | TG_MATTEING                 -- alpha channel is present
  deriving (Eq, Show)  

tag_map :: Num t => [(TIFF_TAG, t)]
tag_map = [
   (TG_SUBFILETYPE,254) ,
   (TG_OSUBFILETYPE,255),
   (TG_IMAGEWIDTH,256),
   (TG_IMAGELENGTH,257),
   (TG_BITSPERSAMPLE,258),
   (TG_IMAGEDESCRIPTION,270),
   (TG_COMPRESSION,259),
   (TG_PHOTOMETRIC,262),
   (TG_THRESHOLDING,263),
   (TG_CELLWIDTH,264),
   (TG_CELLLENGTH,265),
   (TG_FILLORDER,266),
   (TG_DOCUMENTNAME,269),
   (TG_MAKE,271),
   (TG_MODEL,272),
   (TG_STRIPOFFSETS,273),
   (TG_ORIENTATION,274),
   (TG_SAMPLESPERPIXEL,277),
   (TG_ROWSPERSTRIP,278),
   (TG_STRIPBYTECOUNTS,279),
   (TG_MINSAMPLEVALUE,280),
   (TG_MAXSAMPLEVALUE,281),
   (TG_XRESOLUTION,282),
   (TG_YRESOLUTION,283),
   (TG_PLANARCONFIG,284),
   (TG_PAGENAME,285),
   (TG_XPOSITION,286),
   (TG_YPOSITION,287),
   (TG_FREEOFFSETS,288),
   (TG_FREEBYTECOUNTS,289),
   (TG_GRAYRESPONSEUNIT,290),
   (TG_GRAYRESPONSECURVE,291),
   (TG_GROUP3OPTIONS,292),
   (TG_GROUP4OPTIONS,293),
   (TG_RESOLUTIONUNIT,296),
   (TG_PAGENUMBER,297),
   (TG_COLORRESPONSEUNIT,300),
   (TG_COLORRESPONSECURVE,301),
   (TG_SOFTWARE,305),
   (TG_DATETIME,306),
   (TG_ARTIST,315),
   (TG_HOSTCOMPUTER,316),
   (TG_PREDICTOR,317),
   (TG_WHITEPOINT,318),
   (TG_PRIMARYCHROMATICITIES,319),
   (TG_COLORMAP,320),
   (TG_BADFAXLINES,326),
   (TG_CLEANFAXDATA,327),
   (TG_CONSECUTIVEBADFAXLINES,328),
   (TG_MATTEING,32995)
   ]

-- rtag_map :: Num t => [(RTF_TAG, t)]
-- rtag_map = [
--    (RTG_SUBFILETYPE,254) ,

tag_map' :: IM.IntMap TIFF_TAG
tag_map' = IM.fromList $ map (\(tag,v) -> (v,tag)) tag_map

testT :: FilePath -> IO ()
testT = fileDriverRandom (tiff_reader >>= process_tiff)

-- extract useful data from dictionary
process_tiff :: MonadIO m => Maybe (IM.IntMap TIFFDE) -> Iteratee [Word8] m ()
process_tiff Nothing     = return ()
process_tiff (Just dict) = do
  note ["dict size: ", show $ IM.size dict] -- размер словаря
  -- дергаем из словаря несколько тэгов для проверки
  -- Check tag values against the known values for the sample image
--  check_tag TG_IMAGEWIDTH  (flip dict_read_int dict) 129
--  check_tag TG_IMAGELENGTH (flip dict_read_int dict) 122
--  check_tag TG_BITSPERSAMPLE (flip dict_read_int dict) 8
  check_tag TG_IMAGEDESCRIPTION 
    (flip dict_read_string dict) "JPEG:gnu-head-sm.jpg 129x122"
  
  (n,hist) <- compute_hist dict
  note ["computed histogram over ", show n, " values\n", show hist]
  -- -- iterReportError >>= maybe (return ()) error
  -- note ["Verifying values of sample pixels"]
  -- verify_pixel_vals dict [(0,255), (17,248)]
  -- --err <- iterReportError
  -- --maybe (return ()) error err
  -- --return err
  where check_tag tag action v = do 
          -- передает тэг в action и сравнивает результат с v
          -- action - выбирает из словаря значение по имени
           vc <- action tag
           case vc of
             Just v' | v' == v -> note ["Tag ",show tag, " value ", show v]
             _ -> error $ unwords ["Tag", show tag, "unexpected:", show vc]

-- sample processing of the pixel matrix: computing the histogram
compute_hist :: MonadIO m =>
                TIFFDict ->
                Iteratee [Word8] m (Int,IM.IntMap Int)
compute_hist dict = I.joinI $ pixel_matrix_enum dict $ compute_hist' 0 IM.empty
 where
 --compute_hist' count = liftI . Cont . step count
 compute_hist' count hist = icont (step count hist) Nothing
 step count hist (Chunk ch)
   | LL.null ch  = icont (step count hist) Nothing
   | otherwise = icont
                 (step (count + LL.length ch) (foldr accum hist ch))
                 Nothing
 step count hist s        = idone (count,hist) s
 accum e = IM.insertWith (+) (fromIntegral e) 1

-- Reading the pixel matrix
-- For simplicity, we assume no compression and 8-bit pixels
pixel_matrix_enum :: MonadIO m => TIFFDict -> Enumeratee [Word8] [Word8] m a
pixel_matrix_enum dict iter = validate_dict >>= proceed
 where
   -- Make sure we can handle this particular TIFF image
   validate_dict = do
      dict_assert TG_COMPRESSION 1
      dict_assert TG_SAMPLESPERPIXEL 1
      dict_assert TG_BITSPERSAMPLE 8
      ncols <- liftM (fromMaybe 0) $ dict_read_int TG_IMAGEWIDTH dict
      nrows <- liftM (fromMaybe 0) $ dict_read_int TG_IMAGELENGTH dict
      strip_offsets <- liftM (fromMaybe [0]) $
                       dict_read_ints TG_STRIPOFFSETS dict
      rps <- liftM (fromMaybe nrows) (dict_read_int TG_ROWSPERSTRIP dict)
      if ncols > 0 && nrows > 0 && rps > 0
        then return $ Just (ncols,nrows,rps,strip_offsets)
        else return Nothing

   dict_assert tag v = do
      vfound <- dict_read_int tag dict
      case vfound of
        Just v' | v' == v -> return $ Just ()
        _ -> throwErr (iterStrExc (unwords ["dict_assert: tag:", show tag,
                                     "expected:", show v, "found:", show vfound])) >>
             return Nothing

   proceed Nothing = throwErr $ iterStrExc "Can't handle this TIFF"

   proceed (Just (ncols,nrows,rows_per_strip,strip_offsets)) = do
     let strip_size = rows_per_strip * ncols
         image_size = nrows * ncols
     note ["Processing the pixel matrix, ", show image_size, " bytes"]
     let loop _pos [] iter'          = return iter'
         loop pos (strip:strips) iter' = do
             I.seek (fromIntegral strip)
             let len = min strip_size (image_size - pos)
             iter'' <- I.take (fromIntegral len) iter'
             loop (pos+len) strips iter''
     loop 0 strip_offsets iter
 
-- Another sample processor of the pixel matrix: verifying values of
-- some pixels
-- This processor does not read the whole matrix; it stops as soon
-- as everything is verified or the error is detected
verify_pixel_vals :: MonadIO m =>
                     TIFFDict -> [(IM.Key, Word8)] -> Iteratee [Word8] m ()
verify_pixel_vals dict pixels = I.joinI $ pixel_matrix_enum dict $
                                verify 0 (IM.fromList pixels)
 where
 verify _ m | IM.null m = return ()
 verify n m = icont (step n m) Nothing
 step n m (Chunk xs)
   | LL.null xs = icont (step n m) Nothing
   | otherwise = let (h, t) = (LL.head xs, LL.tail xs) in
   case IM.updateLookupWithKey (\_k _e -> Nothing) n m of
    (Just v,m') -> if v == h
                     then step (succ n) m' (Chunk t)
                     else let er = (unwords ["Pixel #",show n,
                                             "expected:",show v,
                                             "found", show h])
                          in icont (const . throwErr . iterStrExc $ er) 
                             (Just $ iterStrExc er)
    (Nothing,m')->    step (succ n) m' (Chunk t)
 step _n _m s = idone () s

-- builds the TIFF dictionary
tiff_reader  :: Iteratee [Word8] IO (Maybe TIFFDict)
tiff_reader = do
  endian <- read_magic --  32 или 64 (LSB|MSB)
  check_version endian --  версия tiff формата  должна быть правильная - 42
  case endian of
    Just e -> do -- устанавливаем курсор на позицию в потоке
              endianRead4 e >>= I.seek . fromIntegral
              load_dict e
    Nothing -> return Nothing
  where
   -- Read the magic and set the endianness
   read_magic = do
     c1 <- I.head
     c2 <- I.head
     case (c1,c2) of
      (0x4d, 0x4d) -> return $ Just MSB
      (0x49, 0x49) -> return $ Just LSB
      _ -> (throwErr . iterStrExc $ "Bad TIFF magic word: " ++ show [c1,c2])
           >> return Nothing

   -- Check the version in the header. It is always ...
   tiff_version = 42
   check_version e = do 
     v <- endianRead2 $ fromJust e -- commented MSB, added e 
     if v == tiff_version
       then return ()
       else throwErr (iterStrExc $ "Bad TIFF version: " ++ show v)


-- A few conversion procedures
u32_to_float :: Word32 -> Double
u32_to_float _x =               -- unsigned 32-bit int -> IEEE float
  error "u32->float is not yet implemented"

u32_to_s32 :: Word32 -> Int32   -- unsigned 32-bit int -> signed 32 bit
u32_to_s32 = fromIntegral
-- u32_to_s32 0x7fffffff == 0x7fffffff
-- u32_to_s32 0xffffffff == -1

u16_to_s16 :: Word16 -> Int16   -- unsigned 16-bit int -> signed 16 bit
u16_to_s16 = fromIntegral
-- u16_to_s16 32767 == 32767
-- u16_to_s16 32768 == -32768
-- u16_to_s16 65535 == -1

u8_to_s8 :: Word8 -> Int8   -- unsigned 8-bit int -> signed 8 bit
u8_to_s8 = fromIntegral
-- u8_to_s8 127 == 127
-- u8_to_s8 128 == -128
-- u8_to_s8 255 == -1

tag_to_int :: TIFF_TAG -> Int
tag_to_int (TG_other x) = x
tag_to_int x = fromMaybe (error $ "not found tag: " ++ show x) $ P.lookup x tag_map

int_to_tag :: Int -> TIFF_TAG
int_to_tag x = fromMaybe (TG_other x) $ IM.lookup x tag_map'

-- convert_type' = undefined
-- read_value' = undefined
-- read_entry' e dictM = dictM >>=
--       maybe (return Nothing) (\dict -> do
--         tag   <- endianRead2 e
--         typ'  <- endianRead2 e
--         typ   <- convert_type' (fromIntegral typ')
--         count <- endianRead4 e
--       -- we read the val-offset later. We need to check the size and the type
--       -- of the datum, because val-offset may contain the value itself,
--       -- in its lower-numbered bytes, regardless of the big/little endian
--       -- order!

--      --   note ["TIFFEntry: tag ",show . int_to_tag . fromIntegral $ tag,
--      --         " type ", show typ, " count ", show count]
--         enum_m <- maybe (return Nothing)
--                         (\t -> read_value' t e (fromIntegral count)) typ
--         case enum_m of
--          Just enum ->
--           return . Just $ IM.insert (fromIntegral tag)
--                                     (TIFFDE (fromIntegral count) enum) dict
--          _ -> return (Just dict)
--       )

-- An internal function to load the dictionary. It assumes that the stream
-- is positioned to read the dictionary
a3      = foldr a4 100 [1..5]
a4 a b  = a 
a5      = foldr (const a6) 100 [1..5]
a6 a    = a
a7      = (const id) 1 2 -- (const id 1) 2 => id 2 => 2

load_dict :: MonadIO m => Endian -> Iteratee [Word8] m (Maybe TIFFDict)
load_dict e = do
  nentries <- endianRead2 e -- Endian -> Iteratee s m Word16
  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  --          (f xi Iter) Iter [x1..xn]
  --          ((const g) xi Iter) Iter [xi]  ,where g :: Iter -> Iter
  --          ((const g xi) Iter) Iter [xi]
  --          (g Iter) Iter [xi]
  dict <- foldr 
            (const read_entry) -- избавляемся от [1..nentries] см выше 
            (return $ Just IM.empty) -- мастерим итератор с пустым словарем
            [1..nentries]            -- применяем нашу функцию нужное число раз
  -- dict <- foldr (const (read_entry' e)) (return $ Just IM.empty) [1..nentries]
  -- dict <- foldr read_entry (return $ Just IM.empty) [1..nentries]
  next_dict <- endianRead4 e -- Endian -> Iteratee s m Word32
  when (next_dict > 0) $
      note ["The TIFF file contains several images, ",
            "only the first one will be considered"]
  return dict
  where
    -- read_entry :: (MonadIO m) =>
    --   Iteratee [Word8] m (Maybe (IM.IntMap TIFFDE)) -> 
    --   Iteratee [Word8] m (Maybe (IM.IntMap TIFFDE))
    -- read_entry nn  dictM   =  dictM >>= -- without const
    read_entry dictM   =  dictM >>=
      maybe (return Nothing) (\dict -> do
        tag   <- endianRead2 e
        typ'  <- endianRead2 e
        typ   <- convert_type (fromIntegral typ')
        count <- endianRead4 e
      -- we read the val-offset later. We need to check the size and the type
      -- of the datum, because val-offset may contain the value itself,
      -- in its lower-numbered bytes, regardless of the big/little endian
      -- order!

        note ["TIFFEntry: tag ",show . int_to_tag . fromIntegral $ tag,
              " type ", show typ, " count ", show count]
        enum_m <- maybe (return Nothing)
                        (\t -> read_value t e (fromIntegral count)) typ
        case enum_m of
         Just enum ->
          return . Just $ IM.insert (fromIntegral tag)
                                    (TIFFDE (fromIntegral count) enum) dict
         _ -> return (Just dict)
      )
     
    convert_type :: (Monad m, Nullable s) => Int -> Iteratee s m (Maybe TIFF_TYPE)
    convert_type typ | typ > 0 && typ <= fromEnum (maxBound::TIFF_TYPE)
        = return . Just . toEnum $ typ
    convert_type typ = do
        throwErr . iterStrExc $ "Bad type of entry: " ++ show typ
        return Nothing

    read_value :: MonadIO m => TIFF_TYPE -> Endian -> Int ->
                  Iteratee [Word8] m (Maybe TIFFDE_ENUM)

    read_value typ e' 0 = do
      endianRead4 e'
      throwErr . iterStrExc $ "Zero count in the entry of type: " ++ show typ
      return Nothing

     -- Read an ascii string from the offset in the
  -- dictionary. The last byte of
  -- an ascii string is always zero, which is
  -- included in 'count' but we don't need to read it
    read_value TT_ascii e' count | count > 4 = do -- val-offset is offset
      offset <- endianRead4 e'
      (return . Just . TEN_CHAR) (\iter_char -> return $ do
            I.seek (fromIntegral offset)
            let iter = convStream
                         (liftM ((:[]) . chr . fromIntegral) I.head)
                         iter_char
            I.joinI $ I.joinI $ I.take (pred count) iter)

  -- Read the string of 0 to 3 characters long
  -- The zero terminator is included in count, but
  -- we don't need to read it
    read_value TT_ascii _e count = do     -- count is within 1..4
      let len = pred count                -- string length
      let loop acc 0 = return . Just . reverse $ acc
          loop acc n = I.head >>= (\v -> loop ((chr . fromIntegral $ v):acc)
                                               (pred n))
      str <- loop [] len
      I.drop (4-len)
      case str of
        Just str' -> (return . Just . TEN_CHAR) (immed_value str')
        Nothing   -> return Nothing

  -- Read the array of signed or unsigned bytes
    read_value typ e' count | count > 4 && typ == TT_byte || typ == TT_sbyte = do
      offset <- endianRead4 e'
      (return . Just . TEN_INT) ( \iter_int -> return $ do
            I.seek (fromIntegral offset)
            let iter = convStream
                         (liftM ((:[]) . conv_byte typ) I.head)
                         iter_int
            I.joinI $ I.joinI $ I.take count iter)

  -- Read the array of 1 to 4 bytes
    read_value typ _e count | typ == TT_byte || typ == TT_sbyte = do
      let loop acc 0 = return . Just . reverse $ acc
          loop acc n = I.head >>= (\v -> loop (conv_byte typ v:acc)
                                               (pred n))
      str <- (loop [] count)
      I.drop (4-count)
      case str of
        Just str' -> (return . Just . TEN_INT) (immed_value str')
        Nothing   -> return Nothing

  -- Read the array of Word8
    read_value TT_undefined e' count | count > 4 = do
      offset <- endianRead4 e'
      (return . Just . TEN_BYTE) (\iter -> return $ do
            I.seek (fromIntegral offset)
            I.joinI $ I.take count iter)

  -- Read the array of Word8 of 1..4 elements,
  -- packed in the offset field
    read_value TT_undefined _e count = do
      let loop acc 0 = return . Just . reverse $ acc
          loop acc n = I.head >>= (\v -> loop (v:acc) (pred n))
      str <- loop [] count
      I.drop (4-count)
      case str of
        Just str' -> (return . Just . TEN_BYTE) (immed_value str')
        Nothing   -> return Nothing
      --return . Just . TEN_BYTE $ immed_value str

  -- Read the array of short integers

  -- of 1 element: the offset field contains the value
    read_value typ e' 1 | typ == TT_short || typ == TT_sshort = do
      item <- endianRead2 e'
      I.drop 2                         -- skip the padding
      (return . Just . TEN_INT) ( immed_value [conv_short typ item])

  -- of 2 elements: the offset field contains the value
    read_value typ e' 2 | typ == TT_short || typ == TT_sshort = do
      i1 <- endianRead2 e'
      i2 <- endianRead2 e'
      (return . Just . TEN_INT) 
               (immed_value [conv_short typ i1, conv_short typ i2])

  -- of n elements
    read_value typ e' count | typ == TT_short || typ == TT_sshort = do
      offset <- endianRead4 e'
      (return . Just . TEN_INT) (\iter_int -> return $ do
            I.seek (fromIntegral offset)
            let iter = convStream
                           (liftM ((:[]) . conv_short typ) (endianRead2 e'))
                           iter_int
            I.joinI $ I.joinI $ I.take (2*count) iter)


  -- Read the array of long integers
  -- of 1 element: the offset field contains the value
    read_value typ e' 1 | typ == TT_long || typ == TT_slong = do
      item <-  endianRead4 e'
      (return . Just . TEN_INT) (immed_value [conv_long typ item])

  -- of n elements
    read_value typ e' count | typ == TT_long || typ == TT_slong = do
        offset <- endianRead4 e'
        (return . Just . TEN_INT) (\iter_int -> return $ do
            I.seek (fromIntegral offset)
            let iter = convStream
                         (liftM ((:[]) . conv_long typ) (endianRead4 e'))
                         iter_int
            I.joinI $ I.joinI $ I.take (4*count) iter)


    read_value typ e' count = do -- stub
       _offset <- endianRead4 e'
       note ["unhandled type: ", show typ, " with count ", show count]
       return Nothing

    immed_value :: (Monad m) => [el] -> EnumeratorM [Word8] [el] m a
    immed_value item iter =
     --(I.enumPure1Chunk item >. enumEof) iter >>== I.joinI . return
       return . joinI . return . joinIM $ (enumPure1Chunk item >>> enumEof) iter

    conv_byte :: TIFF_TYPE -> Word8 -> Int
    conv_byte TT_byte  = fromIntegral
    conv_byte TT_sbyte = fromIntegral . u8_to_s8
    conv_byte _ = error "conv_byte called with non-byte type"

    conv_short :: TIFF_TYPE -> Word16 -> Int
    conv_short TT_short  = fromIntegral
    conv_short TT_sshort = fromIntegral . u16_to_s16
    conv_short _ = error "conv_short called with non-short type"

    conv_long :: TIFF_TYPE -> Word32 -> Int
    conv_long TT_long  = fromIntegral
    conv_long TT_slong = fromIntegral . u32_to_s32
    conv_long _ = error "conv_long called with non-long type"

-- A few helpers for getting data from TIFF dictionary

dict_read_int :: Monad m => TIFF_TAG -> TIFFDict -> 
                            Iteratee [Word8] m (Maybe Int)
dict_read_int tag dict = do
  els <- dict_read_ints tag dict
  case els of
   Just (e:_) -> return $ Just e
   _          -> return Nothing

dict_read_ints :: Monad m => TIFF_TAG -> TIFFDict ->
                             Iteratee [Word8] m (Maybe [Int])
dict_read_ints tag dict =
  case IM.lookup (tag_to_int tag) dict of
      Just (TIFFDE _ (TEN_INT enum)) -> do
          e <- joinL $ enum stream2list
          return (Just e)
      _ -> return Nothing

dict_read_rat :: Monad m => TIFF_TAG -> TIFFDict -> 
                            Iteratee [Word8] m (Maybe (Ratio Int))
dict_read_rat tag dict =
  case IM.lookup (tag_to_int tag) dict of
      Just (TIFFDE 1 (TEN_RAT enum)) -> do
          [e] <- joinL $ enum stream2list
          return (Just e)
      _ -> return Nothing

dict_read_string :: Monad m => TIFF_TAG -> TIFFDict -> Iteratee [Word8] m (Maybe String)
dict_read_string tag dict =
  case IM.lookup (tag_to_int tag) dict of
      Just (TIFFDE _ (TEN_CHAR enum)) -> do
          e <- joinL $ enum stream2list
          return (Just e)
      _ -> return Nothing

tiff = -- "vim.tif"  
       "gnu-head-sm.tif"
r1 = fileDriverRandom (tiff_reader >>= process_tiff) tiff -- test tiff
r2 = fileDriverRandom (tiff_reader' >>= process_tiff) tiff 
--------------------------------------------------------------------------

enumTiff ii  = do
  i' <-  -- fileDriverRandom ii tiff
        enumFile 8192 tiff ii -- бьет файл на отрезки, которые передает
                             -- в поток, над потоком выполняется действие ii
  result <- run i' -- выполняем цепочку действий
  return result
 -- print result
  
-- r2 = enumTiff iTwo
-- r3 = enumTiff read_magic'
-- read_magic' :: (LL.ListLike s t, Nullable s, Num t, Monad m) =>
--                 Iteratee BC.ByteString  m (Maybe Endian)

-- i2 :: (LL.ListLike s el, Monad m) => Iteratee BC.ByteString m el
-- i2 :: Monad m => Iteratee BC.ByteString m Word8
-- i2 = I.head
-- r4 = enumTiff i2

tiff_reader' ::  Iteratee [Word8] IO 
                -- (Endian)     -- return endian
                -- (Word8)   -- I.head
                -- (Word32)  -- endianRead4
                -- (Word16) -- endianRead2
                -- () -- I.seek
                (Maybe TIFFDict) -- original
                
tiff_reader' = do
  endian <- read_magic
  check_version endian -- added endian
  -- case endian of
  --   Just e -> do 
  --             endianRead2 e  >>= I.seek . fromIntegral
  --             load_dict e
  --   Nothing -> return Nothing
  -- I.head -- >> 42 :: Word8
  -- endianRead4 endian -- >> 1031929898 :: Word 32 
  -- endianRead2 endian -- >> 42 :: Word16
  -- endianRead4 endian >>= I.seek . fromIntegral
  -- return endian
  endianRead4 endian >>= I.seek . fromIntegral
  load_dict endian
  where
   -- Read the magic and set the endianness
   read_magic = do
     c1 <- I.head
     c2 <- I.head
     case (c1,c2) of
      (0x4d, 0x4d) -> return $  MSB -- Just MSB
      (0x49, 0x49) -> return $  LSB -- Just LSB
      _ -> (throwErr . iterStrExc $ "Bad TIFF magic word: " ++ show [c1,c2])
      --      >> return Nothing

   -- Check the version in the header. It is always ...
   tiff_version = 42
   check_version e = do -- added e
     v <- endianRead2 e -- commented MSB, added e
     if v == tiff_version
       then return ()
       else throwErr (iterStrExc $ "Bad TIFF version: " ++ show v)

r6 = enumTiff tiff_reader'

-- tiff reader - создает словарь тэгов
-- process_tiff - делает что-то по словарю
tt  = fileDriverRandom (tiff_reader >>= process_tiff)  tiff
dict = fileDriverRandom tiff_reader tiff 
-- v1 = dict >>= \x -> return $ fromJust x

------------------------------------------------------------
workdir   = "" -- "/temp/"
testdir   = "c:/rtfout/"
rtferr    = workdir ++ ".err.rtf"
scripted  = workdir ++ "scripted.txt"
kaska     = workdir ++ "kaska.txt"
collected = workdir ++ "collected.txt"
testrtf   = testdir ++ "test1.rtf"
rtfout    = workdir ++ ".out.rtf"
rtfout2   = workdir ++ ".out2.rtf"

type Src = BC.ByteString -- [Word8]

data RTFDE_ENUM = -- enumerators
      REN_CHAR (forall a m. Monad m => EnumeratorM Src [Char] m a)
    -- | REN_BYTE (forall a m. Monad m => EnumeratorM Src [Word8] m a)
    -- | REN_INT  (forall a m. Monad m => EnumeratorM Src [Int] m a)
    -- | REN_RAT  (forall a m. Monad m => EnumeratorM Src [Ratio Int] m a)

data RTF_TAG = RTG_other Int            -- other than below

data RTF_TYPE = RT_NONE  -- 0
  | RT_byte      -- 1   8-bit unsigned integer
  | RT_ascii     -- 2   8-bit bytes with last byte null
  | RT_short     -- 3   16-bit unsigned integer
  | RT_long      -- 4   32-bit unsigned integer
  | RT_rational  -- 5   64-bit fractional (numer+denominator)
                                -- The following was added in TIFF 6.0
  | RT_sbyte     -- 6   8-bit signed (2s-complement) integer
  | RT_undefined -- 7   An 8-bit byte, "8-bit chunk"
  | RT_sshort    -- 8   16-bit signed (2s-complement) integer
  | RT_slong     -- 9   32-bit signed (2s-complement) integer
  | RT_srational -- 10  "signed rational",  two SLONGs (num+denominator)
  | RT_float     -- 11  "IEEE 32-bit float", single precision (4-byte)
  | RT_double    -- 12  "IEEE 64-bit double", double precision (8-byte)
 deriving (Eq, Enum, Ord, Bounded, Show)
                    
---------------------------------------------------------------

readerN  :: Iteratee Src IO (Maybe RTFDict)
readerN =  do readRtfHeader
              load_dictN 
     
rdictN = fileDriverRandom readerN testrtf

type RTFDict  = IM.IntMap RTFDE

data RTFDE = RTFDE { rtfde_pid   :: Int,
                     rtfde_enum  :: RTFDE_ENUM 
                   }

-- load_dictN :: MonadIO m => Iteratee Src  m (Maybe RTFDict)
load_dictN   = do
  nentries <-  I.length   
 -- I.seek 0
  note [show nentries]
  dict <- foldr (const read_entry) (return $ Just IM.empty) [1..5000]
--  dict <- P.foldl read_entryL (return $ Just IM.empty) 
  return dict
 where 
  read_entryL = undefined 
  -- read_entryL dictM el = dictM >>= 
  --   maybe (return Nothing) (\dict -> do
  --     enum_m <- maybe (return Nothing)
  --                    (\t -> read_valueN t LSB 2) (Just RT_ascii)
  --     case enum_m of
  --       Just enum -> return dict -- . Just dict 
  --        --    $ IM.insert (succ $ IM.size dict) (RTFDE 7 enum) dict
  --       _ ->  return dict -- $ Just dict
  --    )   
    
                
  read_entry dictM = dictM >>=
    maybe (return Nothing) (\dict -> do
      enum_m <- maybe (return Nothing)
                     (\t -> read_valueN t LSB 2) (Just RT_ascii)
      case enum_m of
        Just enum -> return . Just 
             $ IM.insert (succ $ IM.size dict) (RTFDE 7 enum) dict
        _ -> return $ Just dict
     )
  
read_valueN :: MonadIO m => RTF_TYPE -> Endian -> Int ->
              Iteratee Src m (Maybe RTFDE_ENUM)
-- read_valueN typ e' 0 = do
--     endianRead4 e'
--     throwErr . iterStrExc $ "Zero count in the entry of type: " ++ show typ
--     return Nothing

read_valueN RT_ascii e' count | count > 4 = do -- val-offset is offset
    (return . Just . REN_CHAR) ( \iter_char -> return $ do
      let iter = convStream 
               (liftM ((:[]) . chr . fromIntegral) I.head) 
               iter_char
      I.joinI $ I.joinI $ I.take (count*1000) iter)

read_valueN RT_ascii _e count | count == 2 =  do 
    str <- loop [] ' ' 
    case str of
      Just str' -> (return . Just . REN_CHAR) (immed_value'  str')
      Nothing   -> return Nothing
  where loop acc c  = I.head >>=   \v -> do
          let curr = chr $ fromIntegral v
          case (curr, c, P.length acc) of 
            ('{' , ' ', _)   -> open curr
            (_   , ' ', _)   -> skip
            ('{' , '{', _)   -> exit
            ('{' , '}', _)   -> exit
            ('\\', '{', 1)   -> save curr
            ('\\',  _ , _)   -> exit 
            (_   , '{', _)   -> save curr
            ('{' , '{', _)   -> save curr
            otherwise        -> skip
            where save ch  = loop (ch : acc) c 
                  skip     = loop acc c 
                  exit     = return . Just . reverse $ acc
                  open ch  = loop "{" '{' 
                  
immed_value' :: (Monad m) => [el] -> EnumeratorM Src [el] m a
immed_value' item iter =
     --(Iter.enumPure1Chunk item >. enumEof) iter >>== Iter.joinI . return
     return . joinI . return . joinIM $ (enumPure1Chunk item >>> enumEof) iter

readRtfHeader :: MonadIO m =>  Iteratee Src m ()
readRtfHeader =   do
  cnt <-  heads $ BC.pack "{\\rtf"
--  cnt <-   I.joinI $ I.take 5 stream2list 
--  let conv str  =  map (chr . fromIntegral) str  ==  "{\\rtf" 
  I.seek 0 
  if cnt == 5  then return () else throwErr $ iterStrExc "Bad RTF header"
  
r11 =  fileDriverRandom readRtfHeader testrtf

r14' :: MonadIO m => Iteratee Src m ([Word8])
r14' = do 
     I.joinI $ I.take 10 stream2list
  where itr =  I.head 
          --    convStream (liftM ((:[]) . chr . fromIntegral) I.head)
  --I.break  (\x -> (chr $ fromIntegral x) == '\\')
  
r14 =  fileDriverRandom r14' testrtf

r15 = enumPure1Chunk [1..1000::Int] 
     (joinI $ (I.take 15) (I.head)) >>= run
  
iter = do
 -- h <- joinI $ I.filter (\x -> x<5) stream2list  -- I.head
 h <- I.break (\x -> x>5) 
 t <- stream2list
 return  (h,t)
 
t12 = enumPureNChunk [1,3,8,9,2,3,4::Int] 3 iter   >>= run >>= print


processN :: MonadIO m => Int -> Maybe (IM.IntMap RTFDE) -> Iteratee Src m ()
processN _  Nothing = return ()
processN nn (Just dict) = do
  note ["dict size: ", show $ IM.size dict]
 -- check_tag 2 (flip dict_read_stringN dict ) ) 
  show_map
  where 
    show_tag tag action  = do
          vc <- action tag 
          case vc of
            Just v' -> note ["Tag ",show tag, " value " , show v']
            _ -> error $ unwords ["Tag", show tag, "unexpected:", show vc]   
    show_map = 
      P.mapM_ (flip show_tag (flip dict_read_stringN dict)) $ [1..(count nn)] 
    count 0 = IM.size dict
    count _ = nn
        
dict_read_stringN :: Monad m => Int -> RTFDict -> Iteratee Src m (Maybe String)
dict_read_stringN tag dict = 
  case IM.lookup tag dict of
      Just (RTFDE _ (REN_CHAR enum)) -> do
          e <- joinL $ enum stream2list
          return (Just e)
      _ -> return Nothing

testN  = fileDriverRandom (readerN >>= processN 100)  testrtf
-- main   = fileDriverRandom (readerN >>= processN 0)  rtf

data RStruct = RStruct { group_pid :: Int,
                         group_tag :: RTFGroup }

data RTFGroup = 
    RTG_FontTable 
  | RTG_ColorTable
  | RTG_Other

groupByName s  | s == ""         = RTG_Other 
               | s == "fonttbl"  = RTG_FontTable 
               | s == "colortbl" = RTG_ColorTable
               | otherwise       = RTG_Other

rtf_open  = '{'
rtf_close = '}'
rtf_delim = '\\'
-----------------------------------------------
data RState      = Empty0 | Open | Close | Tag | Value deriving (Show, Eq, Ord) 
data ScriptState = Scr0 | ScrL | ScrR | Scr   deriving (Eq,Show)
-- \\insrsid \\\\a:=1\\\\   Scr0 -> ScrL -> ScrR -> Scr0
-- \\ insrsid \\\\ssss} {\\insrsid d d} {\\insrsid ff\\\\}
-- Scr0 -> ScrL -> Scr -> ScrR -> Scr0
ri2 = fileDriverRandom iter2  testrtf
type Pos      = Int
type SPos     = Int
type MPos     = Int
type GroupId  = Int
type Len      = Int
type GroupPid = Maybe Int
-- type Script   = [Char]
type Group    = [Char]
type Level    = Int
-- data GStruct = PS (Int, Int) -- позиция - текст
--              | PT (Int, Int) -- позиция - скрипт
--              deriving Show
                      

type NT = IM.IntMap (GroupPid, Level, Pos, Int, Group --, [GStruct]
                  --  ,[Char] -- debug
                    )
type RValues = IM.IntMap (GroupId
                         , Pos    -- rtf file position
                         , SPos   -- script file position  
                         , Len    -- length
                         , Atom   -- flag: script or text
                         , [Char] -- content
                         ) 

data Atom = Script | Txt  
            deriving (Eq,Ord,Show)
type Dict = (Maybe NT, Maybe RValues)
initDict  = (Just IM.empty, Just IM.empty)
data ParseInfo = PS {acc :: String       -- accum chars
                    , nn  :: Int         -- save nn chars 
                    , rstate :: RState   -- rtf state ({},\\,..)
                    , pos :: Int         -- pos in rtf file
                    , pid :: GroupPid    -- parent group id
                    , cg :: Int          -- level
                    , scr :: ScriptState -- script state 
                    , tags :: [String]   -- current group tags
                    , dct :: Dict        -- (map (groupId,groupPid,groupName..),map (script,scriptGroup..))
                    , rtfTo :: Rtf2Rus   -- rus chars map
                    }                     
               deriving (Eq, Show)
initParserState = PS [] 0 Empty0 0 Nothing 0 Scr0 [] initDict rtf2rus
-- build dictionary from rtf file
-- если делать через StateT монаду тормозит ровно в два раза (WordSt.hs)
iter2 :: (MonadIO m) => Iteratee Src m Dict
iter2  =  do 
  jump -- прыгаем куда нужно при отладке
  fill_dict [] 0 Empty0 0 Nothing 0 Scr0 [] (Just IM.empty, Just IM.empty) rtf2rus 
  where 
    -- fill_dict :: ... -> Iteratee ByteString IO (Maybe IM.IntMap, Maybe IM.IntMap)
    fill_dict acc nn state pos pid cg scr tags dict rtfTo =  I.peek >>= 
       maybe (return dict) (\curr ->   
         read_entries (chr $ fromIntegral curr) -- current char
                      acc   -- accum chars
                      nn
                    --  saveV  -- записать acc-save символов (для Value)
                    --  saveS  -- записать acc-save символов (для Struct)
                      state -- rtf state ({},\\,..)
                      pos   -- pos in rtf file
                      pid   -- parent group id
                      cg    -- level
                      scr   -- script state 
                      tags  -- current group tags
                      dict  -- (map (groupId,groupPid,groupName..),map (script,scriptGroup..))
                      rtfTo)-- rus chars map
                             
       
    -- read_entries :: ... -> Iteratee ByteString IO (Maybe IM.IntMap, Maybe IM.IntMap)   
    read_entries curr acc nn state pos pid cg scr ts  dict@(Just  struct, Just values) rtfTo  = 
      do ---------------------------------------
      I.head
      -- при отладке, проверяем не остановиться ли
      case (debug, pos > delta, delta /= 0 ) of 
        (True ,True,True ) -> return dict -- вываливаемся
        otherwise          -> parse -- идем дальше 
      where
        parse =  
          case (curr, state, ln, prev, scr) of
           ('{' ,Value,_,_,_)            -> rec save sn n0 sv sv  Open  scr  []  1
           ('{' ,Tag  ,_,_,_)            -> rec save sn n0 sv sv  Open  scr  []  2 
           ('{' ,Open ,2,_,_)            -> rec save sn n0 sv 0   state scr  []  3 
           ('{' ,_    ,_,_,_)            -> rec save sn n0 sv sv  Open  scr  []  4   
           ('\\',Open ,1,_,_)            -> rec add  sn n0 sv sv  state scr  ts  5
           ('\\',Open ,2,_,_)            -> rec save sn n0 sv 0   Tag   scr  ta  6
           (_   ,Open ,_,_,_)            -> rec add  sn n0 sv sv  state scr  ts  7  
           ('}' ,_    ,_,_        ,ScrL) -> rec []   ss n0 0  sv  Close Scr  ts  8
        -- \\\\....\\\\[}],   ....[}]     
           ('}' ,_    ,_,_        ,ScrR) -> do
                                            let (s,m,g) = case delimCount acc 0 of
                                                         (0,_) -> (add,sv,sn)
                                                         (1,_) -> (add,sv,sn)
                                                         (_,_) -> ([],0,ss) 
                                            rec s    g  n0 m  sv  Close Scr0 ts 9
           ('}' ,Value,_,_        ,Scr)  -> do 
                                          --  let acc' = tryRus [] -- []
                                            rec []   ss n0 0  sv  Close Scr  ts 10 
           ('\r',Value,_,Just '\n',_)    -> rec []   sn n1 sv sv  state scr  ts 11
           ('\n',Value,_,Just '\r',_)    -> rec []   sn n1 sv sv  state scr  ts 12 
           ('\n',Value,_,_        ,Scr)  -> rec save ss n0 0  sv  state Scr  ts 13 
           ('\r',Value,_,_        ,Scr)  -> rec save ss n0 0  sv  state Scr  ts 14 
           ('\\',_    ,_,Just '\\',Scr ) -> rec add  sn n0 sv sv  Value ScrR ts 15
           ('\\',_    ,_,Just '\\',ScrR) -> do 
                                            let scr' = case 
                                                  delimCount acc 0 of
                                                         (1,_)    -> ScrR 
                                                         (_,True) -> Scr0
                                                         (_,False)-> ScrR
                                            rec add  sn n0 sv sv Value scr' ts 16 
           ('\\',Value,_,Just '\\',Scr0) -> do -- text\\[\\];text\\\\[\\];text\\\\\\[\\]
                                            let (scr',m,g,a) = case delimCount acc 0 of
                                                         (1,_)  -> (ScrL,-1,st,"\\\\")
                                                         (_,True)  -> (scr,sv,sn,add)
                                                         (_,False) -> (scr,sv,sn,add)
                                            rec a  g  n0 m  sv Value ScrL ts  17 
           (_   ,Value,_,Just '\\',Scr0) -> do
                                            let (a,s,m,n,g) = case delimCount acc 0 of
                                                         (1,_)    -> (save,Scr0,sv,n1,sn)
                                                         (_,True) -> (save,Scr0,0,n0,ss)
                                                         (_,False)-> (save,Scr0,-1,n2,ss)
                                            rec a    g  n  m sv state s    ts  18
           ('\\',_    ,_,Just '\\',ScrL) -> do
                                            let scr' = case delimCount acc 0 of
                                                         (1,_)    -> Scr   
                                                         (_,True) -> Scr  
                                                         (_,False)-> Scr  
                                            rec add  sn n0 sv sv Value scr' ts 19
           -- \L \L [\r] .....
           ('\r',_    ,_,Just '\\',ScrL) -> rec save ss n0 0  sv state Scr  ts 20                                  
           ('\n',_    ,_,Just '\\',ScrL) -> rec save ss n0 0  sv state Scr  ts 21
           (_   ,_    ,_,_        ,ScrL) -> rec add  sn n0 sv sv state Scr  ts 22 
           -- \\\\asdasdasd\\\\[x] -> вносим скрипт в словарь
           -- \\[p]ar -> не вносим                    
           (_   ,_    ,_,Just '\\',ScrR) -> do
                                            let (r,s,m,n,g) = case delimCount acc 0 of
                                                         (1,_) -> (r,add,sv,n1,sn)
                                                         (_,True) -> (Scr0,[],0,n1,ss) 
                                                         (_,False)-> (r,add,0,n1,st) 
                                            rec s    g  n  m  sv Value   r  ts  23
           ('}' ,Tag  ,_,_,_)            -> rec []   sn n0 sv sv Close scr  []  24   
           (_   ,Tag  ,_,Just ' ' ,_)    -> toValue
           ('\\',Tag  ,_,_,_)            -> rec save sn n0 sv sv state scr  ta  25
           ('\\',Close,_,_,_)            -> rec save sn n0 sv sv Tag   scr  []  26
           ('}' ,Value,_,_,Scr0)         -> rec []   st n0 0  sv Close scr  []  27 
           ('}' ,Value,_,_,_)            -> rec []   ss n0 0  sv Close scr  []  28
           ('}' ,Open ,_,_,_)            -> rec []   sn n0 sv 0  Close scr  []  29  
           ('}' ,_    ,_,_,_)            -> rec []   sn n0 sv sv Close scr  []  30   
           ('\\',Value,_,_,Scr0)         -> rec add  sn n1 sv sv state scr  []  31 
           (_   ,Empty0,_,_,_)           -> rec []   sn n0 sv sv state scr  []  32
           (_   ,Tag  ,_,_,_)            -> rec add  sn n0 sv sv state scr  ts  33
           -- буфер 30 может быть очень большим, т.к он накапливает текст
           -- трем его через 29 ветку
           (_   ,Value,_,_,Scr0)         -> rec []   sn n1 sv sv state scr  ts  34
           (_,Value,_,Just '\r',Scr)     -> rec save sn n0 sv sv state scr  ts  35 
           (_,Value,_,Just '\n',Scr)     -> rec save sn n0 sv sv state scr  ts  36
           -- ('\\',Value,_  , _ , Scr)     -> do 
           --                                  let acc' = tryRus add
           --                                  rec acc' sn n0 sv sv state scr  ts  39                             
           (_   ,Value,_,_,_)            -> rec add  sn n0 sv sv state scr  ts  37
           otherwise                     -> rec []   sn n0 sv sv state scr  ts  38
        
        -- tryRus nextAcc = case P.length acc > 2 of 
        --                     True -> if acc!!2 == '\''  then rus else nextAcc
        --                     _    -> nextAcc 
        --   where rus = do let rtfc = [acc !! 1] ++ [acc !! 0]
        --                  case M.lookup rtfc rtfTo of
        --                    Nothing -> nextAcc
        --                    Just  v -> v:(P.drop 3 acc)
        ss = Just Script -- save as script
        st = Just Txt    -- save as text
        sn = Nothing
        add    = [curr] ++ acc
        skip   = acc
        save   = [curr]
        sv = if nn > 0 then nn else P.length acc
        n1 = nn + 1
        n2 = nn + 2
        n0 = 0
        res_str     = reverse acc
        sz_str      = P.length acc
        delimCount str c  = case P.length str of
                              0 -> (c, even c) -- дошли до конца (всего, четные?)
                              _ -> do case P.head str of  
                             -- если предыдущий - \\ - смотрим на парность
                                             '\\' -> delimCount (tail str) (c + 1)
                                             _    -> (c, even c) 
                                                     
        toValue = do 
          let state'= case ((tryTag "insrsid")||(tryTag "charrsid"),
                                   (tryAccTag "insrsid")||(tryAccTag "charrsid")) of
                              (False,False) -> state
                              otherwise     -> Value
          let (scr',acc') = case (state',scr) of
                            (Value,Scr ) -> case (curr,prev) of
                                             ('\\', Just '\\') -> (ScrR,save) 
                                             _                 -> (Scr,save)
                            (Value,Scr0) -> (scr ,save)
                            otherwise    -> (scr,[]) 
                            
          rec acc' sn n1 sv sv state'   scr'  ts  99

        lt = if P.length ts == 0 then "" else P.head ts
        ta = 
          case P.length ts of  
                 0  -> [res_str] ++ ts
                 1  -> [res_str] ++ ts
                 _  -> [res_str] ++ [P.head ts]
  
        tryTag name    = lt =~ name :: Bool        
        tryAccTag name = (res_str) =~ name :: Bool  
       -- tagToDict name = if tryTag name then structI else dict
        prev           = if ln == 0 then Nothing else Just $ P.head acc
        ln =  case sz_str of
               0 -> 0
               1 -> 1
               _ -> 2
              
        rec acc' save' nn' sv' si' state' scr' tags' deb  = do
           let cg' = case curr of
                       '}' -> pred cg
                       '{' -> succ cg
                       otherwise -> cg
           let m1 =  case save' of 
                 Just _  ->  values'
                 Nothing ->  values  
                 
           let m2 =  case sv - si' of 
                 0 ->  struct
                 _ ->  structI
           
           if (debug)&&(showSteps)
             then do let upd = if m1 == values then "" else " **"
                     -- let save'' = case save' of 
                     --                Nothing -> ""
                     --                Just v  -> show v
                     note ["#" ++ show deb ++ " " ++ show pos ++ -- debug
                           ", " ++ "|" ++ curr' ++ "|" ++ ", acc="  ++ show acc ++ 
                           ", st=" ++ show state ++ ":" ++  show  state' ++
                           ", scr=" ++ show scr' ++ " prev=" ++ show prev ++
                           ", (stype,nn,nn',sv,sv')= " ++ show (save',nn,nn',sv,sv') ++ upd
                        --   " save'= " -- ++ save'' ++ upd
                           ]
             else return ()

           fill_dict acc' nn' state' (succ pos) pid cg'  scr' tags' 
                                           (Just m2, Just m1) rtfTo -- n
           where
             curr' = case curr of
                       '\r' -> "R"
                       '\n' -> "N" 
                       _  -> [curr]
             countS      = IM.size struct                   
             new_idS     = succ countS
             last_idS    = countS 
             start_pos   = pos - sz_str 
             gen_valueS  = (pid, cg, start_pos , 0 , res_str)
             structI     = IM.insert new_idS  gen_valueS struct
             structU     = dict -- dict --do if n>0 
                            -- then (Just struct, Just $ IM.update updV prevV values)
                            -- else dict
             new_idV     = succ $ IM.size values
             prevV       = IM.lookup (IM.size values) values -- prevVn 0 
             --   prevVn n    = IM.lookup ((IM.size values) - n) values
             -- вычисляет новую позицию в скрипте или файле с текстом
             -- по длине предыдущего элемента
             prevSPos t   = f $ IM.size values
               where f n = case  IM.lookup n values of
                             Nothing               -> 0
                             Just (_, _, v,l,t',_) -> if t'==t then v + l
                                                               else f' n
                     f' n = let n' = n-1 in n' `seq` f n' 
  
             gen_valueV    = do
                             let (sval,stype,slen,pos') =  
                                   case save' of
                                         Just Txt   -> 
                                           if sv' == sv 
                                             then ("", Txt, nn, pos-nn)
                                             else ("", Txt, nn+sv', pos-nn)
                                         Just Script -> 
                                           if sv' == sv 
                                             then do let str = res_str -- srtf2rus res_str rtfTo
                                                     (str, Script,sz_str,pos-sz_str)
                                             else do let sv'' = if sv'>0 then sv' else  (-1)*sv'
                                                     let str' = reverse $ P.drop sv'' acc   
                                                     let str  = str'  --  srtf2rus str' rtfTo
                                                     (str,Script,P.length str',pos-sz_str)
                               
                          --   let slen' = if slen == 0 then P.length sval else slen 
                             if slen  > 0 then Just (1, pos', prevSPos stype , slen, stype, sval)
                                          else Nothing                  
  
             gen_valueV0 = (1, start_pos, 0, 0, Script, [])
             values'     = case gen_valueV of
                             Just val -> IM.insert new_idV val values
                             Nothing  -> values
             values0     = IM.insert new_idV gen_valueV0 values
      
-- iter3 :: MonadIO m => (Maybe NT, Maybe RValues) -> Iteratee Src m ()
iter3 path dict@(Just struct, Just values) = do
    if debug
      then do show_map struct
              note ["---"]
              show_map values        
              note ["struct-map size: ", show $ IM.size struct]
              note ["values-map size: ", show $ IM.size values]
              return ()
        --    check_script values
      else return ()
    saveToFile values (path ++ scripted)
    return dict
    -- mapChunksM_ (liftIO . putStrLn . BC.unpack )
    where 
      show_map map = 
        P.mapM_ (\x -> note ["#" ++ show x ++ " " ++
                             (show $ IM.lookup x map)]) [1..IM.size  map] 
      check_script map = P.mapM_ (check map) [1..IM.size map]  
      count 0  = IM.size struct
      count nn = nn
      -- saveToFile map  =  P.mapM_ (save []) [1..IM.size map] -- пишем в текстовый файл
      --        where save acc el = do Just (_,_,_,_,s,v) <-  return $ IM.lookup el map
      --                               case s of
      --                                 Script -> acc ++ v
      --                                 _      -> return ()
      --              trySave acc = undefined -- append scripted $ BC.pack v
      saveToFile map file = P.foldr rem (return [] ) $ reverse [1..IM.size map] 
        where rem el acc =  do Just (_,_,_,_,s,v) <-  return $ IM.lookup el map
                               acc' <- case s of
                                      Script -> acc >>= \x -> return $ x ++ v
                                      _      -> acc
                               if (P.length acc' > 5000)||(el == IM.size map) 
                                 then do append  file $ BC.pack acc'
                                         return []
                                 else do return acc'       
      check map nn = undefined  -- do
        -- let (id,pos,script) = fromJust $ IM.lookup nn map
        -- I.seek (fromIntegral $ pos + skip')
        -- hh <-  joinI $ (I.take $ P.length script) stream2list
        -- note ["at : " ++ show (pos + skip') ++ " " ++ show (P.map (chr . fromIntegral) hh )]
        -- note [script] -- script

type ChunkId   = Int
type CurrRec   = Int

append file src = liftIO $ BC.appendFile file src  

iterChunks f  = do
   s  <- I.chunkLength
 --  note ["^^^ " ++ show s]
   case s  of
     Just 0  -> return ()
     Just s' -> f --s'
     Nothing -> return ()

-- пользуясь словарем вырезает из ртф файла скрипт, сравнивает его с 
-- образцом и сохраняет в файл, если скрипт не совпадает - ругнется,
-- на выходе будет голый ртф файл, файл со скриптом и если нужно
-- файл с текстом, если при вычислении скрипта были ошибки -      
-- в шаблоне.err.rtf будут проставлены замечания в виде {..}
     
type CollectText = Bool
type HasErrors   = Bool
type WorkDir     = FilePath

iter4 :: MonadIO m 
         => (Maybe NT, Maybe RValues) -- (группы ртф, позиции скриптов)
         -> Rus2Rtf
         -> (Maybe Toks,HasErrors)    -- Nothing -> просто удалит скрипт, иначе подставит значения
         -> CurrRec 
         -> Pos  
         -> Skip 
         -> CollectText -- писать текст в текстовый файл
         -> WorkDir
         -> Iteratee Src m ()
iter4 dict@(Just struct, Just  values) toRtf res@(toks,err) curr pos st collectText dir = do
  if curr == 0 
    then do
         jump -- при отладке, если необходимо начинаем не с начала
         iter4 dict toRtf res 1 pos st collectText dir -- парсим
    else do                
     case (debug, pos  > delta, delta /= 0 ) of
       (True ,True,True ) -> return () -- вываливаемся
       otherwise          -> iterChunks $ save pos st -- парсим дальше
   where 
     outfile  = dir ++ rtfout
     errfile  = dir ++ rtferr
     textfile = (takeDirectory dir) ++ "/" ++ collected
     len = I.chunkLength >>= return . fromJust -- 
     
     save filepos st  = do 
               (curr',pos',st') <- save'  curr  filepos Nothing st
               iter4 dict toRtf res curr' pos' st' collectText dir
               
     save' recId  filepos rest st =  do 
     -- определяем нашу позицию в отрезке, и относительно ее обрабатываем данные
       len1 <- len 
       let  val = IM.lookup recId values -- скрипт в словаре
       case val of
         Just (id,pos,spos,slen,stype,sval) -> do
             -- note ["val " ++ show val ++ " len+filepos " ++ 
             --       show (len1 + filepos) ++ " len=" ++ show len1
             --       ++ " rest=" ++ show rest]
             case rest of  
                Just rest' -> do
                     st' <- readRest  rest' filepos sval spos st stype slen
                     save' (recId+1) filepos Nothing st'
                Nothing    -> do
                  if pos  <= len1 + filepos 
                    then do 
                      (recId',pos',rest', st') <- 
                          read recId filepos pos sval spos st stype slen
                      len2 <- len
                      save' recId' pos' rest' st'
                    else readAll recId filepos st
         Nothing -> readAll recId filepos st           
     
     read recId filepos pos sval spos st stype count = do
          len1 <- len
          -- let count  = P.length sval
          -- note [show (len1,pos)]
          src <- I.takeFromChunk $ pos - filepos -- берем ртф до текущего фрагмента
       --   note [show (st,stype,count)]
          -- note [BC.unpack src]
          append outfile src
          if err then append errfile src else return ()
          let rest = len1 - (pos - filepos)
          let (count',rest',sval',curr') = 
                if count - rest > 0 
                  then (rest, Just $ count-rest, P.take rest sval,recId) 
                  else (count, Nothing, sval,recId+1)
          st' <- replaceScript 
                    spos pos sval' filepos rest' len1 count' count st stype
          len2 <- len
          -- note [show (len2,pos)]
          return (curr', pos + count, rest', st') 
     
     replaceScript spos pos sval' filepos rest' len1 count' count st stype = do
       src <- I.takeFromChunk count' >>= return .  BC.unpack  -- выдираем весь скрипт
       -- note [show (src,stype,st)]
       case stype of                      
         Txt -> do if collectText 
                      then do append textfile $ BC.pack $ P.filter (\x->x/='\n'&&x/='\t') src 
                      else do return ()
                   -- note[ "text:" ++ src ++ ", st:"++ show st]       
                   case st of
                             Accept -> do append outfile $ BC.pack src 
                             _      -> do return ()
                   if err then append errfile $ BC.pack src
                          else return ()        
       
                   return st 
         Script ->
            case toks of -- toks - map который содержит пары (позиция, 
                         -- значение + флаг :скрыть/показать )
              Nothing -> do -- если пусто - ничем не подменяем скрипт и  пишем в файл
                            let svalR = srus2rtf sval' toRtf
                            if (svalR /= src)&&(debug)   --  сравниваем с образцом    
                              then do showDebug src svalR
                                      I.length -- stop 
                                      return Accept
                              else do return Accept
              Just toks'  -> if not err then do --  подставляем все результаты вычислений с ключами попадающими в наш интервал
                                             let tk = fst $ IM.split (spos + count) $ snd $ IM.split spos toks'
                                             let (st', tokstr) = toks2str err toRtf tk st
                                             -- note ["script: " ++src++ ", st:" ++(show st')++ ", tk:" ++show tk]
                                             case st' of 
                                                Accept -> append outfile $ BC.pack tokstr -- то что идет вместо скрипта
                                                _      -> return ()
                                             return st'
                                        else do let tk = fst $ IM.split (spos + count) $ snd $ IM.split spos toks'
                                                let (st',tokstr) =  toks2str err toRtf tk st
                                                append errfile $ BC.pack $ src ++ tokstr
                                                return Accept
                                                
       where showDebug s r =  note ["@" ++ show pos ++ "--------------------\n"
                                ++ show r ++ "\n" ++ "->"  ++  show s 
                                ++ "\n" ++ "len1=" ++ show len1 
                                ++ " filepos=" ++ show filepos 
                                ++ " pos-filepos=" ++  show (pos-filepos)
                                ++ " count=" ++ show count 
                                ++ " rest=" ++ show rest']
                   
     readRest rest' filepos  sval spos st stype count = do 
       -- note ["val " ++ show val ++ " len+filepos " ++ 
       --        show (len1 + filepos) ++ " len=" ++ show len1
       --        ++ " rest=" ++ show rest]
       let sval' = snd $ P.splitAt (count - rest') sval
       replaceScript spos pos sval' filepos rest' 0 0 0 st stype 
     
     readAll recId filepos st = do
       --   note ["readAll " ++ show (recId,filepos)]
          len1 <- len
          src <- I.getChunk
          append outfile src
          if err then append errfile src else return ()
          return (recId,len1 + filepos, st)
          
       
-- -- build dictionary from txt(scripted) file
-- -- data SS = SEmpty | SId | SVal
-- -- iter6 :: MonadIO m => Iteratee Src m (Maybe NT, Maybe RValues)
-- -- iter6 = do
-- --   dict <- fill_dict SEmpty [] [] (Just IM.empty, Just IM.empty)
-- --   return dict
-- --   where 
    
-- --     fill_dict state pos val dict  =  I.peek >>=  maybe (return dict) 
-- --                                           (\curr ->  read_entries 
-- --                                                      (chr $ fromIntegral  curr) state pos val dict)
       
-- --     read_entries curr state pos val dict@(Just struct, Just values)= do
-- --       I.head
-- --       case (curr, state) of
-- --         ('#', SEmpty)  -> fill_dict SId   []        []         dict
-- --         ('#', SId   )  -> fill_dict SVal  pos       []         dict
-- --         (_  , SId   )  -> fill_dict SId   (curr:pos)[]         dict
-- --         ('#', SVal  )  -> fill_dict SId   []        []         dict'
-- --         (_  , SVal  )  -> fill_dict SVal  pos       (curr:val) dict  
-- --         otherwise      -> fill_dict state pos       val        dict
-- --      where
-- --        dict' = (Just struct, 
-- --                 Just $ IM.insert (succ $ IM.size values) (0,read $ reverse pos,reverse val) values)

-- iter7 :: MonadIO m => 
--          (Maybe NT, Maybe RValues) -> CurrRec -> Pos  -> Iteratee Src m ()
-- iter7 dict@(Just struct, Just values) curr pos  = 
--   iterChunks $ save pos 
--   where
--     len = I.chunkLength >>=  return . fromJust 
 
--     save filepos   = do 
--       (curr',pos') <- save' curr filepos 
--       iter7 dict curr' pos' 
    
--     save' recId filepos  = do
--       len1 <- len
--       let val = IM.lookup recId values
--       case val of
--         Just (id,pos,spos,script) ->  do
--         --  note [show (pos,len1+filepos)]
--           if pos  <= len1 + filepos 
--                     then do 
--                       (recId',pos') <- read recId filepos pos script 
--                       save' recId' pos'  
--                     else readAll recId filepos 
--         Nothing  -> readAll recId filepos 
                        
--     readAll recId filepos  = do
--           len1 <- len
--           src <- I.getChunk
--           -- note' [BC.unpack  src]
--           append rtfout2 src
--           return (recId,len1 + filepos)
    
--     read recId filepos pos script'  = do
--           len1 <- len
--           let script = script2str script'
--           src <-  I.takeFromChunk $ pos - filepos
--           -- note' ["--" ++ script]
--           -- note' ["##" ++ (BC.unpack src') ]
--           append rtfout2 $ BC.append src (BC.pack script)
--           return (recId + 1, pos + P.length script) 
          
startPos    = 0
startRec    = 0
startSkip   = Accept
hasErrors c = c>0

ri3 file bufSize  = do --------------------------------------
      
      dir <- prepareFiles
 
      putStrLn $ "bufsize: " ++ show bufSize
      s1 <- getCurrentTime   -- 
      dict@(Just struct, Just scripts) <- fileDriverRandom (iter2 >>= iter3 dir) file
      time s1 $ "build dictionary from rtf file, save script to " ++ dir++scripted
      
      s1   <- getCurrentTime
      vars <- getVars $ dir++kaska
      ve@(values,errc) <-  parseScripted vars $ dir++scripted
      time s1 $ "parse script, " ++ show (IM.size values) ++ " items parsed"
            
      s1 <- getCurrentTime
      -- enum (iter4 dict rus2rtf (Nothing,hasErrors 0) startRec startPos startSkip collectText) rtf
      -- time s1 "extract script" 
      enum (iter4 dict rus2rtf (Just values,hasErrors errc) startRec 
                  startPos startSkip collectText (dir++takeBaseName file)) file
      time s1 "replace script" 
      
      -- s1 <- getCurrentTime
      -- dict <- fileDriverRandom iter6 scripted
      -- -- print $ show dict
      -- time s1 "build dictionary from txt file"
      -- return ()
      
      -- s1 <- getCurrentTime
      -- resetFile rtfout2
      -- enum (iter7 dict 1 0 ) rtfout
      -- sz <- cmpFiles rtf rtfout2
      -- time s1 $ "script injection" 
      -- if not sz then putStrLn $ rtf ++ " /= " ++ rtfout2 else return  ()
      
                                                                
      where 
         enum = case bufSize of
                     Nothing -> fileDriverRandom
                     Just s  -> fileDriverRandomVBuf s
           
         cmpFiles f1 f2 = 
           withBinaryFile f1 ReadMode $ \x ->
           withBinaryFile f2 ReadMode $ \y -> do
             s1 <- hFileSize x 
             s2 <- hFileSize y
             return (s1 == s2)
           
         resetFile file = BC.writeFile file BC.empty
         time s ss = do
           s2 <- getCurrentTime
           putStrLn  $ show  (diffUTCTime s2 s) ++ " " ++ ss 
         prepareFiles  =  do
            let dir = (takeDirectory file) ++ "/"
            resetFile $ dir ++ pref ++ rtfout    -- результат
            resetFile $ dir ++ scripted  -- сюда идет скрипт
            resetFile $ dir ++ collected -- cюда идет текст
            resetFile $ dir ++ pref ++ rtferr    -- ошибки
            if dir/=testdir then copyFile (testdir++kaska)  (dir++kaska) 
                            else return ()
            return  dir
              where pref =  takeBaseName file

-- parsing options: ------------------------------------------
           
collectText = True
showSteps   = not True
debug =  not True
skip' = 0 --56800
jump  = I.seek skip' -- debug
delta = 0 --70000   -- debug
main  = do
  arg <-  getArgs
  if "-testbuf" `elem` arg 
    then testBufSize testrtf
    else parse testrtf
  -- if "-openMSWord" `elem` arg
  --    then runFile 

bufSize = Just 215040
testBufSize file = CM.mapM_ (ri3 file)  $ map Just $ map (1024*) [1,20..400]
parse file  = do ri3 file bufSize
              --  runFile rtfout
test = parse testrtf

runFile f = do -- пускаем Word
  pid <- runCommand f
  waitForProcess pid >>= exitWith 

sw = runFile testrtf
-- so = runFile rtfout
-- ss = runFile scripted
-- so2 = runFile rtfout2


-- Iteratee and monad transformers ---------------------------------
p1 = fileDriver' p2 testrtf
data AStreamState = AState !(Maybe Int) | NoState  
  deriving (Eq, Show)
type AMonad = StateT AStreamState IO

fileDriver' :: Iteratee String AMonad a
            -> FilePath
            -> IO a
fileDriver' i  fp  = runAM' (fileDriverRandom 
                               (i>>i) 
                                 fp)
  where runAM' = runAM

p2 = do 
      st <- lift get
      case st of
        NoState          -> lift $ put $ AState $ Just 12
        AState (Just s') -> lift $ put $ AState $ Just (s'+12)
      I.length >>= \x -> return $ [x]
     
runAM :: AMonad a -> IO a
runAM am = do
  (a,s) <- runStateT am NoState
  case s of
    NoState    -> return a
    AState {}  -> runMState (put s >> return a)
                    
runMState :: AMonad a -> IO a
runMState m = evalStateT (m >>= (\a -> closeA >> return a))
                           (AState Nothing)
closeA :: AMonad ()
closeA = do
  s <- get
  case s of
    AState (Just h)  -> do
      liftIO $ putStrLn $ show h 
    AState Nothing   -> error "Can't close file: no handle"
    x -> error $ "Can't close file: isn't a WAVE file: " ++ show x


-- lazyness ----------------------------------------------------
    
ss = IM.fromList 
     [(1,1),(2,2),(3,3)]                                   
xx mm 0 = 0
xx mm n = if IM.lookup n mm == Nothing 
            then 0
            else do let new = n-1 
                     in new `seq` xx mm new
----------------------------------------------------------------
