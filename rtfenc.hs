module RtfEnc (rtf2rus, rus2rtf, frtf2rus, srtf2rus, srus2rtf, 
                 Rtf2Rus(..), Rus2Rtf (..), RtfRus(..))
       where

import Prelude as P
import Data.Map as M
import Codec.Binary.UTF8.String (encodeString, decodeString)
import qualified Data.ByteString.Char8 as BC
import System.FilePath.Windows (replaceBaseName,takeBaseName)
import Text.Parsec.Prim 
import Data.Functor.Identity (Identity(..))
import Text.Parsec.Combinator 
import Text.Parsec.Char 
import Text.Parsec.Error 
import Text.Parsec.String 
import Data.Time (getCurrentTime, diffUTCTime)

rusChars = 
  "ёйцукенгшщзхъфывапролджэячсмитьбюЁЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮЂЃ‚ѓ„…†‡€‰Љ‹ЊЌЋЏђ’“”•–—™љ›њќћџЎўЈ¤Ґ¦§Ё©Є«¬®Їіїґєё·»ѕЅјµ¶№°±І\n\t\b\r" 
 
rtfRus =
  "\\'b8\\'e9\\'f6\\'f3\\'ea\\'e5\\'ed\\'e3\\'f8\\'f9\\'e7\\'f5\\'fa\\'f4\\'fb\\'e2\\'e0\\'ef\\'f0\\'ee\\'eb\\'e4\\'e6\\'fd\\'ff\\'f7\\'f1\\'ec\\'e8\\'f2\\'fc\\'e1\\'fe\\'a8\\'c9\\'d6\\'d3\\'ca\\'c5\\'cd\\'c3\\'d8\\'d9\\'c7\\'d5\\'da\\'d4\\'db\\'c2\\'c0\\'cf\\'d0\\'ce\\'cb\\'c4\\'c6\\'dd\\'df\\'d7\\'d1\\'cc\\'c8\\'d2\\'dc\\'c1\\'de\\'80\\'81\\'82\\'83\\'84\\'85\\'86\\'87\\'88\\'89\\'8a\\'8b\\'8c\\'8d\\'8e\\'8f\\'90\\lquote \\rquote \\'93\\'94\\bullet \\endash \\emdash \\'99\\'9a\\'9b\\'9c\\'9d\\'9e\\'9f\\'a1\\'a2\\'a3\\'a4\\'a5\\'a6\\'a7\\'a8\\'a9\\'aa\\'ab\\'ac\\'ae\\'af\\'b3\\'bf\\'b4\\'ba\\'b8\\'b7\\'bb\\'be\\'bd\\'bc\\'b5\\'b6\\'b9\\'b0\\'b1\\'b2\\par\\tab\\line\\page"

enc = case parse prRtf   "" rtfRus  of
             Right v   ->  v
             Left  err -> [] --show err

prRus  :: ParsecT [Char] u Identity [Char]
prRus    = do string "\\'" -- \\'d1
              s <- many1 hexDigit
              return $ "\\'" ++  s

prSpec :: ParsecT [Char] u Identity [Char]
prSpec   = do char '\\' -- \\par\\
              s <- letter
              (r,e) <- (do r' <- try $ manyTill letter $ char ' '
                           return (r'," ")) 
                       <|> 
                       (do r' <- try $ manyTill letter $ try $ lookAhead $ char '\\'
                           return (r', ""))
                       <|>
                       (do r' <- try $ manyTill letter eof
                           return (r',""))
              return $  "\\" ++ [s] ++ r ++ e
            
              
-- prSpec' :: ParsecT [Char] u Identity [Char]
-- prSpec'  = do char '\\' -- \\par"
--               s <- letter
--               r <- manyTill (letter<|>space) eof
--               return $  "\\" ++ [s] ++ r
              
prAny :: ParsecT [Char] u Identity [Char]
prAny   = do  s <- anyChar
              r <- manyTill (anyChar) $ try $ lookAhead $ char '\\'
              return $ [s] ++ r  
              
prAny' :: ParsecT [Char] u Identity [Char]
prAny'  = do  s <- anyChar
              r <- manyTill (anyChar) eof
              return $ [s] ++ r

prRtf  = do s <- many1 $ 
                   try prRus <|> try prSpec --  <|> try prSpec'
                             <|> try prAny <|> prAny' 
            eof
            return s  

mapChars a b = M.fromList $ P.zip a b

type Rus2Rtf = Map Char  [Char]
type Rtf2Rus = Map [Char] Char
type RtfRus = (Rtf2Rus,Rus2Rtf)

rus2rtf :: Rus2Rtf
rus2rtf = mapChars rusChars enc

rtf2rus :: Rtf2Rus
rtf2rus = mapChars enc rusChars

srtf2rus :: String -> Rtf2Rus -> String
srtf2rus src dict = -- encodeString $ 
                      parseRtf dict src 

srus2rtf :: String -> Rus2Rtf -> String
srus2rtf src dict = conv2rtf dict src

s1 = srtf2rus "\\rquote \\'b8\\'e9\\'f6\\'f3\\'ea\\'e5\\'ed\\rquote \\par\\par  as    sfsdfsdfsd  " rtf2rus 
s2 = srus2rtf 
     "\209\145\208\185\209\134\209\131\208\186\208\181\208\189" -- "апатиты" 
       rus2rtf                            

frtf2rus fl = do src <- readFile fl  
                 let  fl' = replaceBaseName fl (takeBaseName fl ++ ".rus") 
                 BC.writeFile  fl' $ BC.pack $ parseRtf rtf2rus  src

conv2rtf dict src =  P.foldl (parse dict) [] src -- $ decodeString src                
                      
  where  parse dict acc el = case M.lookup el dict of
           Nothing -> acc ++ [el]
           Just v  -> acc ++ v

-- parsec

-- prsRus :: ParsecT [Char] EParserState Identity [Char]
prsRus   = do st <- getState
              prRus >>= return . replaceRtf (chars st)
prsSpec  = do st <- getState
              prSpec >>= return . replaceRtf (chars st) 
-- prsSpec' = do st <- getState
--               prSpec' >>= return . replaceRtf (chars st)
prsAny   = do st <- getState
              prAny >>= return
prsAny'  = do st <- getState
              prAny' >>= return
              
replaceRtf dict key = do
   -- let key' = unwords $ words key
    case M.lookup key dict of
       Nothing -> key
       Just v  -> [v] 
               
prsRtf =  many1 (try prsRus <|> try prsSpec -- <|> try prsSpec' 
                            <|> try prsAny <|> prsAny')
parseRtf dict src = do case makeParser prsRtf src  of
                         Right v   -> concat v
                         Left  err -> show err
  where 
      --  makeParser ::  Parsec String EParserState [String] -> String -> Either ParseError [String]
        makeParser ps src  = runParser ps (EParserState dict) "" src


data EParserState = EParserState {chars :: Rtf2Rus} deriving (Show)
type EParser a = GenParser Char EParserState a

-- tests

large = concat $ take 2 $ iterate (++rtfRus) []

time s ss = do
             s2 <- getCurrentTime
             putStrLn  $ show  (diffUTCTime s2 s) ++ " " ++ ss 

t1 = srtf2rus  large rtf2rus
test1 = do
  s2 <- getCurrentTime
  putStrLn $ (show $ length t1) ++ " elems"
  time s2 " - parsec"

 
d1 = parse d2 "" "\\asd\\sdsd\\sdsd"
d2 = many (d3 <|> d4)
d3 = char '\\'
d4 = letter
