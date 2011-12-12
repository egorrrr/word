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

type EnumeratorM sFrom sTo m a = Iteratee sTo m a -> m (Iteratee sFrom m a)

joinL :: (Monad m, Nullable s) => m (Iteratee s m a) -> Iteratee s m a
joinL = join . lift

note :: (MonadIO m, Nullable s) => [String] -> Iteratee s m ()
note = liftIO . putStrLn . concat

note' :: (MonadIO m, Nullable s) => [String] -> Iteratee s m ()
note' = liftIO . print .concat

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
                    
---------------------------------------------------------------

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
-- в шаблоне.err.rtf будут проставлены замечания в виде {***..***}

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


