
{-# LANGUAGE NoMonomorphismRestriction, 
             ScopedTypeVariables 
 #-}

module Expr (parseScripted, Toks(..), toks2str, showToks, Skip (..), getVars) 
  where

import Prelude as P
import Text.Parsec hiding (spaces)
import Text.Parsec.Char hiding (spaces,)
import Text.Parsec.String hiding (parseFromFile)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Text.Parsec.Combinator
import Text.Parsec.Error (errorPos)
-- import Data.Iteratee (enumPure1Chunk, run, stream2list)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error
import qualified Data.IntMap as  IM
import qualified Data.Map as M
import Data.Function (on)
import Data.List (sortBy, groupBy, (!!),intersperse)
import Data.Char (toLower, toUpper)
import Data.Functor.Identity (Identity(..))
import Codec.Binary.UTF8.String (encodeString, decodeString) -- string from/to utf8
import Data.ByteString.UTF8 (toString -- utf8 bytestring to string
                            , fromString) 
import qualified Data.ByteString as BS
import RtfEnc  

scripted = "c:/rtfout/scripted.txt"
logfile  = "c:/rtfout/log.txt"
appendLog s = appendFile logfile s

type ErrCount  = Int -- [Tok] -- Int
data Skip      = Skip | SkipAll  | Accept deriving (Eq,Show)
type TPos      = (Int,Int,Skip)
type Statement = (Skip, Keyword)
accept True    = Accept
accept False   = Skip

nn  = (-1,-1,Accept)

data Tok   = TS TPos String 
           | TI TPos Integer 
           | TD TPos Double 
           | TB TPos Bool  
           | TL TPos [Tok]  
           | TV TPos String  
           | TNull TPos   
           | TK TPos Keyword [Tok]
           | TErr TPos TokError
           deriving (Show, Eq)
             
data Keyword = TIf | TElse | TElsif | TEndif | TScan | TEndscan 
  deriving (Show,Eq)
           
type Toks = IM.IntMap Tok

setTokPos t pos st = do
   case t of
    TS _ v -> TS pos' v
    TI _ v -> TI pos' v
    TD _ v -> TD pos' v
    TB _ v -> TB pos' v
    TL _ v -> TL pos' v
    TV _ v -> TV pos' v
    TNull _ -> TNull pos'
    TK _ v z -> TK pos' v z
    TErr _ v -> TErr pos' v
 where pos' = (sourceLine pos ,sourceColumn pos, st)  

   
getTokPos t = case t of
 TS p _ -> p
 TI p _ -> p
 TD p _ -> p
 TB p _ -> p
 TL p _ -> p
 TV p _ -> p
 TK p _ _ -> p
 TErr p _ -> p
 TNull p -> p

getTokCol tok = do let (_,v,_) = getTokPos tok
                   v

showTok t  = do
   case t of
    TS _ v -> v
    TI _ v -> show v
    TD _ v -> show v
    TB _ v -> show v
    TL _ v -> show v
    TV _ v -> show v
    TNull _  -> ""
    TK _ v z -> ""
    TErr _ v -> ""
 
showToks toks  = concat $ intersperse "," $  map showTok toks
  
--  errors -----------------------------------
 
data TokError = NumArgs Int [Tok]
              | TypeMismatch String [Tok]
          --    | Parser ParseError  
              | NotFunction String String 
              | UndefinedOperator String [Tok]  
              | UndefinedError String
              | Default String  
   deriving (Eq)

showError :: TokError -> String
showError (UndefinedError message )     = "Undefined error: " ++ message
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected  ++ " args; found (" 
                                          ++ (showToks found) ++ ")"
showError (UndefinedOperator op params) = do let str = case P.length params of
                                                        0 -> op 
                                                        1 -> (showTok $ P.head params) 
                                                             ++  " " ++ op
                                                        _ -> (showTok $ P.head params)   
                                                             ++ " " ++ op ++ " " ++
                                                             (showToks $ P.tail params)
                                             "Undefiend using operator: " ++ str
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ showToks found
-- showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show TokError where show = showError

isErr tok = case tok of
               TErr _ _ -> True
               _        -> False

throwErr err = do TErr nn err
errNull = 0 -- [] -- 0
-- instance Error TokError where
--   noMsg = Default "An error has occured"
--   strMsg = Default

-- type ThrowsError = Either TokError

-- trapError action = catchError action (return . show)

-- extractValue :: ThrowsError a -> a
-- extractValue (Right val) = val
  
-- parser state --------------------------------------

data EParserState = EParserState {vars :: M.Map String  Tok, skipt :: 
                                     Skip, errCount :: ErrCount }
                    deriving (Show)

type EParser a = GenParser Char EParserState a
updateSkip val      = updateState $ \st -> st {skipt = val}
updateErrCount  err = updateState $ \st -> st {errCount = succ $ errCount st}
                     -- updateState $ \st -> st {errCount = [err] ++ errCount st}
updateVars var val  = updateState $ \st -> st {vars = doAssign var val $ vars st} 
   
runPs p input st  = do 
  case (runParser p' (EParserState st Accept errNull)  "" input) of
    Left err         -> ([],Just (take 20 $ snd $ splitAt 
                                  (sourceColumn $  errorPos err) input,err), st,errNull)
    Right (val, st')   -> (val, Nothing, vars st', errCount st')
  where p' = do e  <- p
                eof 
                st <- getState
                return (e,st)
        -- cmp tok = do let (a,b,c) = gp tok        
        --              if b /= -1 then (tok, takeWhile (/='\\') 
        --                                       $ snd $ splitAt (b-1) input)      
        --                         else (tok, "") 
        
          
initialState = EParserState genVars Accept errNull 

-- runI ps st =  do
--   input <- stream2list
--   liftIO $ putStrLn input
--   let e =  runPs ps input st
--   return e 
  
runF ps st fl =  do
  input <- (BS.readFile fl >>= return  . toString) -- ByteString -> UTF8 -> String
  runS ps st input
    
runS ps st input =  do 
 -- putStrLn $ show  input
  let e = runPs ps input st
  return e 

whiteSpace= T.whiteSpace lexer
lexeme    = T.lexeme     lexer
-- symbol    = T.symbol     lexer
natural   = T.natural    lexer
float     = T.float      lexer
parens    = T.parens     lexer
-- brackets  = T.brackets   lexer
-- semi      = T.semi       lexer
identifier = T.identifier lexer
reserved   = T.reserved   lexer
reservedOp = T.reservedOp lexer

lexer :: T.TokenParser  EParserState
lexer  =  T.makeTokenParser $ def
             -- haskellDef 
             --   { T.reservedOpNames = ["*","/","+","-",":="]}

def =  emptyDef{ T.commentStart = "{-"
               , T.commentEnd = "-}"
             --  , T.commentLine = "//"                 
               , T.identStart = letter
               , T.identLetter = alphaNum
               , T.opStart = oneOf "-+*/=:"
               , T.opLetter = oneOf "-+*/=:"
           --    , T.reservedOpNames = map fst . fst ops
               , T.reservedNames = 
                   ["true"
                   ,"false"
                   ,"if"
                   ,"then"
                   ,"else" 
                   ,"elsif"
                   ,"while"
                   ,"do"
                   ,"scan"
                   ,"endscan","rquote"]
               }

ops = [ ("-",  8, AssocNone,  1),
        ("++", 8, AssocNone,  1),
        ("--", 8, AssocNone,  1),
        ("+",  6, AssocLeft,  2),
        ("++", 6, AssocLeft,  2),
        ("-",  6, AssocLeft,  2),
        ("*",  7, AssocLeft,  2),
        ("**", 7, AssocLeft,  2),
        ("^",  7, AssocLeft,  2),
        ("/",  7, AssocLeft,  2),
        ("==", 4, AssocNone,  2),
        ("=",  4, AssocNone,  2),
        ("<>", 4, AssocNone,  2),
        ("<=", 4, AssocNone,  2),
        (">=", 4, AssocNone,  2),
        ("<",  4, AssocNone,  2),
        (">",  4, AssocNone,  2),
        ("&&", 3, AssocRight, 2),
        ("||", 2, AssocRight, 2)
      ]

opTable = 
  reverse . map (map mkOp) 
          . groupBy ((==) `on` prec) . sortBy (compare `on` prec) $ ops
  where 
      prec (a, b, c, d) = b
    
mkOp (s, _, a, c) = case c of 
     2 ->  Infix  (do reservedOp s; return $ \ x y -> eOp2N s x y ) a
     1 ->  Prefix (do reservedOp s; return $ \ x  -> eOp1 s x) 

eOp1 :: [Char] -> Tok -> Tok -- ThrowsError Tok
eOp1 op x  =  case (op, x) of         
  ("-"  , TNull _)-> TI nn 0
  ("-",   TD _ a) -> TD nn $ negate a
  ("-",   TI _ a) -> TI nn $ negate a        
  -- ("--",  TNull)-> TI $ -1
  -- ("--",  TD a) -> TD $ a - 1.00
  -- ("--",  TI a) -> TI $ a - 1
  -- ("++",  TNull)-> TI 1
  -- ("++",  TD a) -> TD $ a + 1.00
  -- ("++",  TI a) -> TI $ a + 1
  (_   ,  TErr _ _ ) -> x
  (_   ,  _        ) -> throwErr $ UndefinedOperator op [x]
  
eOp2N op x y = case (op, x, y) of
  (_, TNull _, TI _ _ ) -> eOp2 op (TI nn 0) y
  (_, TNull _, TD _ _ ) -> eOp2 op (TD nn 0.00) y
  (_, TI _ _ , TNull _) -> eOp2 op x (TI nn 0) 
  (_, TD _ _ , TNull _) -> eOp2 op x (TD nn 0.00)
  (_, TS _ _ , TNull _) -> eOp2 op x (TS nn "") 
  (_, TNull _, TS _  _) -> eOp2 op (TS nn "") y 
  (_, _      ,_       ) -> eOp2 op x y

eOp2 op x y =  do 
  case (op, x, y) of         
   ("*",  TD _ a,  TD _ b) -> TD nn $ a * b
   ("*",  TI _ a,  TI _ b) -> TI nn $ a * b
   ("*",  TD _ a,  TI _ b) -> TD nn $ a * (i b)
   ("*",  TI _ a,  TD _ b) -> TD nn $ (i a) * b
   ("**", TI _ a,  TI _ b) -> TI nn $ a ^ b
   ("**", TD _ a,  TI _ b) -> TD nn $ a ^ b
   ("^",  TI _ a,  TI _ b) -> TI nn $ a ^ b
   ("^",  TD _ a,  TI _ b) -> TD nn $ a ^ b
   ("+",  TD _ a,  TD _ b) -> TD nn $ a + b
   ("+",  TI _ a,  TI _ b) -> TI nn $ a + b
   ("+",  TD _ a,  TI _ b) -> TD nn $ a + (i b)
   ("+",  TI _ a,  TD _ b) -> TD nn $ (i a) + b
   ("+",  TS _ a,  TS _ b) -> TS nn $ a ++ b
   ("++", TS _ a,  TS _ b) -> TS nn $ a ++ b
   ("-",  TD _ a,  TD _ b) -> TD nn $ a - b
   ("-",  TI _ a,  TI _ b) -> TI nn $ a - b
   ("-",  TD _ a,  TI _ b) -> TD nn $ a - (i b)
   ("-",  TI _ a,  TD _ b) -> TD nn $ (i a) - b
   ("/",  _   ,  TD _ 0.00)-> TD nn 0
   ("/",  _   ,  TI _ 0)   -> TI nn 0
   ("/",  TD _ a,  TD _ b) -> TD nn $ a / b
   ("/",  TI _ a,  TI _ b) -> do 
                          let r = (i a) / (i b)
                          case (r /= i ( a `div` b)) of
                             False  -> TI nn $ truncate r 
                             True   -> TD nn $ r
   ("/",  TD _ a,  TI _ b) -> TD nn $ a / i b
   ("/",  TI _ a,  TD _ b) -> TD nn $ i a / b 
   ("==", TNull _, TNull _)-> TB nn True
   ("==", TNull _, _   )   -> TB nn False
   ("==", _    , TNull _)  -> TB nn False
   ("=", TNull _, TNull _) -> TB nn True
   ("=", TNull _, _   )    -> TB nn False
   ("=", _    , TNull _)   -> TB nn False
   ("<>", TNull _, TNull _)-> TB nn False
   ("<>", TNull _, _   )   -> TB nn True
   ("<>", _    , TNull _)  -> TB nn True
   ("==", TB _ a,  TB _ b) -> TB nn $ a == b
   ("==", TI _ a,  TI _ b) -> TB nn $ a == b
   ("==", TD _ a,  TD _ b) -> TB nn $ a == b
   ("==", TI _ a,  TD _ b) -> TB nn $ i a == b
   ("==", TD _ a,  TI _ b) -> TB nn $ a == i b
   ("==", TS _ a,  TS _ b) -> TB nn $ a == b
   ("=", TB _ a,  TB _ b)  -> TB nn $ a == b
   ("=", TI _ a,  TI _ b)  -> TB nn $ a == b
   ("=", TD _ a,  TD _ b)  -> TB nn $ a == b
   ("=", TI _ a,  TD _ b)  -> TB nn $ i a == b
   ("=", TD _ a,  TI _ b)  -> TB nn $ a == i b
   ("=", TS _ a,  TS _ b)  -> TB nn $ a == b

   ("<>", TB _ a,  TB _ b) -> TB nn $ a /= b
   ("<>", TI _ a,  TI _ b) -> TB nn $ a /= b
   ("<>", TD _ a,  TD _ b) -> TB nn $ a /= b
   ("<>", TI _ a,  TD _ b) -> TB nn $ i a /= b
   ("<>", TD _ a,  TI _ b) -> TB nn $ a /= i b
   ("<>", TS _ a,  TS _ b) -> TB nn $ a /= b
   ("<" , TB _ a,  TB _ b) -> TB nn $ a < b
   ("<" , TI _ a,  TI _ b) -> TB nn $ a < b
   ("<" , TD _ a,  TD _ b) -> TB nn $ a < b
   ("<" , TI _ a,  TD _ b) -> TB nn $ i a < b
   ("<" , TD _ a,  TI _ b) -> TB nn $ a < i b
   ("<" , TS _ a,  TS _ b) -> TB nn $ a < b
   (">" , TB _ a,  TB _ b) -> TB nn $ a > b
   (">" , TI _ a,  TI _ b) -> TB nn $ a > b
   (">" , TD _ a,  TD _ b) -> TB nn $ a > b
   (">" , TI _ a,  TD _ b) -> TB nn $ i a > b
   (">" , TD _ a,  TI _ b) -> TB nn $ a > i b
   (">" , TS _ a,  TS _ b) -> TB nn $ a > b
   ("<=", TB _ a,  TB _ b) -> TB nn $ a <= b
   ("<=", TI _ a,  TI _ b) -> TB nn $ a <= b
   ("<=", TD _ a,  TD _ b) -> TB nn $ a <= b
   ("<=", TI _ a,  TD _ b) -> TB nn $ i a <= b
   ("<=", TD _ a,  TI _ b) -> TB nn $ a <= i b
   ("<=", TS _ a,  TS _ b) -> TB nn $ a <= b
   (">=", TB _ a,  TB _ b) -> TB nn $ a >= b
   (">=", TI _ a,  TI _ b) -> TB nn $ a >= b
   (">=", TD _ a,  TD _ b) -> TB nn $ a >= b
   (">=", TI _ a,  TD _ b) -> TB nn $ i a >= b
   (">=", TD _ a,  TI _ b) -> TB nn $ a >= i b
   (">=", TS _ a,  TS _ b) -> TB nn $ a >= b
   ("||", TB _ a,  TB _ b) -> TB nn $ a || b
   ("||", TB _ _,  TNull _)-> x
   ("||", TNull _ , TB _ _) -> y
   ("&&", TB _ a,  TB _ b) -> TB nn $ a && b
   ("&&", TB _ _,  TNull _)-> TB nn False
   ("&&", TNull _, TB _ _ )-> TB nn False
   ------------------------- 
   (_  ,  TNull _, TNull _)-> TNull nn
   (_  ,  _      , TNull _)-> x
   (_  ,  TNull _, _      )-> y
   (_  ,  TI _ _ , TB _ _) -> throwErr $ TypeMismatch "number"  [y]
   (_  ,  TI _ _ , TS _ _) -> throwErr $ TypeMismatch "number"  [y]
   (_  ,  TS _ _ , TI _ _) -> throwErr $ TypeMismatch "string"  [y]
   (_  ,  TS _ _ , TB _ _) -> throwErr $ TypeMismatch "string"  [y]
   (_  ,  TB _ _ , TI _ _) -> throwErr $ TypeMismatch "boolean" [y]
   (_  ,  TB _ _ , TS _ _) -> throwErr $ TypeMismatch "boolean" [y]
   (_  ,  TErr _ _ , _   ) -> x
   (_  ,  _   , TErr _ _ ) -> y
   (_  ,  _      , _     ) -> throwErr $ UndefinedOperator op [x,y]
   -- ("sin", TI _ a, _)  -> TD nn $ sin $ fromIntegral a
   where i = fromIntegral
        
comma  = "\\\\"

expr = do toks <- manyTill (try pe <|> pp) $ try $ eof  
          return  $ filter ss toks
          where pp = between pa pa pExp -- \\\\...\\\\
                pa = string comma
                pe = do {string (comma ++ comma); return $ TNull nn} -- \\\\\\\\
                -- формируем то что пойдет на выход
                ss tok  = do let (a,b,c) = getTokPos tok
                             ss' tok -- && c==Accept
                             where ss' (TNull _)   = False  -- не идет ничего   
                                --   ss' (TK    _ _) = False  -- подавляем управляющие конструкции
                                   ss' _           = True   -- пропускаем
    
pExp =  do p <- getPosition
           res <- pAssign <|> 
                  pOExp <?> "statement"
           s <- if (isErr res)&&(getTokCol res > -1) 
                  then do updateErrCount res -- trap errors
                          getState
                  else do getState
           return $ setTokPos res p $ skipt s -- trap position
           
-- pOExp :: EParser Tok
pOExp  = buildExpressionParser opTable pAExp <?> "expression"

pAExp =  do pSpaces             -- ____
            s <- -- try pFun
                  parens pExp   -- ()     
              <|> try pElsif
              <|> try pIf
              <|> try pElse
              <|> try pEndif
              <|> try pScan
              <|> try pEndScan
              <|> try pString   -- TS "str"
              <|> try pFun      -- Tok  
              <|> try pDouble   -- TD 23.45
              <|> try pInteger  -- TI 234
              <|> try pBool     -- TB True
              <|> try pValue    -- Tok
             -- -- <?> "simple expression" 
            pSpaces    -- ____         
            return s -- вернем любой токен (даже результат которого не применяется)

-- -- pRest    = many anyChar >>= return . TR nn        

-- pInteger :: ParsecT String EParserState Identity (ThrowsError Tok)
pInteger =  natural >>= return . TI nn 
      
pSpaces  = skipMany space
              

-- rquote  =               
-- lquote  = "\\lquote "
                  
pQuotes =       (try $ string "\\rquote ") 
            <|> (try $ string "\\lquote ") 
            <|> (try $ string "\"") 
            <|> (try $ string "'")
               --    <|> (try $ string "\8217") <|> (try $ string "\8220")  
               -- <?> "quote char"                     

pString  = do qq <- pQuotes
              v <- case qq of  
                     "\\rquote " ->  manyTill anyChar $ try lr
                     "\\lquote " ->  manyTill anyChar $ try lr
                     _           ->  manyTill anyChar $ try $ string qq
                   -- pSpaces
              return $ TS nn v
  where lr = (try $ string "\\lquote ") <|> (try $ string "\\rquote ")
                                
pDouble  = float >>= return . TD nn

pBool    =   do v <- (string "True"<|> string "true")  
                return $ TB nn True
             <|>   
             do v <- (string "False"<|> string "false")    
                return $ TB nn False

pIf    = do reserved "if" <?> "IF"
            st <- getState
            v  <- pExp
            (t,res) <- case v of
              (TB _ v')   -> return (v',[]) 
              (TErr _ _)  -> return (False,[v])
              _           -> return (False,[])
            case skipt st of
                    Accept  -> updateSkip $ accept t
                    Skip    -> updateSkip SkipAll
                    SkipAll -> return ()
            if isErr v then return v
                       else return $ TK nn TIf res
            
pElsif = do reserved "elsif" <?> "ELSIF"
            v  <- pExp
            (t,res) <- case v of
              (TB _ v')   -> return (v',[]) 
              (TErr _ _)  -> return (False,[v])
              _           -> return (False,[])
            updateSkip $ accept t
            if isErr v then return v
                       else return $  TK nn TElsif res         

pEndif = do reserved "endif" <?> "ENDIF"
            st' <- getState
            case skipt st' of
              Skip    -> updateSkip Accept
              Accept  -> return ()
              SkipAll -> updateSkip Skip
            return $ TK nn TEndif []
                    
pElse  = do reserved "else" <?> "ELSE"
            st' <- getState
            case skipt st' of
              Accept  -> updateSkip Skip
              Skip    -> updateSkip Accept
              SkipAll -> return ()
            return $ TK nn TElse []
                    
pWhile = undefined

pScan = do reserved "scan" <?> "SCAN"
           e <- pExp
           return $ TK nn TScan []
           
pEndScan = do reserved "endscan" <?> "SCAN"
              return $ TK nn TEndscan []
           
pIdent = do first <- letter' <|> esc' -- xzxzx <|> \\'
            b <-  many ps  
            return $ first  ++ concat b 
            where 
              esc'    = (many1 $ try $ string "\\'") >>= return . concat
              letter' = ((try $ many1 letter) <|> (try $ many1 $ oneOf "_")) >>= return
              digit'  = many1 digit  >>= return
              ident'  = identifier   >>= return
              ps      = letter' <|> digit'  <|> esc' -- zxcxzc<|>312312<|>\\'
                           
            
pVar     = do var <-  try ( do var1 <- pIdent -- Header.Id or Header:Id
                               pt   <- try (char '.') <|> try (char ':')
                               var2 <- pIdent
                               return $ var1 ++ [pt] ++ var2)
                  <|> try pIdent -- Var  
              return $ TV nn var
     
pValue   = do (TV _ var) <- pVar
              notFollowedBy $ char '('
              st <- getState
              case M.lookup var (vars st) of
                 Nothing -> do updateVars (TV nn var) (TNull nn)    
                               return $ TNull nn
                 Just v  -> do return  v

pAssign = try (assign)
  where assign = do
               --  pos <- getPosition
                 var <- pVar
                 pSpaces
                 lexeme $ string ":="
                 pSpaces
                 e <- pOExp 
                 st <- getState
                 -- в режиме пропуска не меняем значения переменных
                 res <- case skipt st of
                          Accept -> do updateVars var e
                                       return $ TNull nn   
                          _      -> do return $ TNull nn            
                 -- при присвоении на выход не идет ничего, кроме ошибки
                 if isErr e then return e else return res 

doAssign var@(TV pos varName) val map = 
  case M.lookup varName map of
      Nothing -> M.insert varName val map
      _ ->  M.update (\_ -> Just val) varName map
      
pFun = do
  fname  <- identifier
  params <- lexeme formalparams
  return $ applyFunc (map toLower fname) params
  where
    formalparams = lexeme (char '(') >> params False
      where params bComma = do char ')'
                               return []
                            <|>   
                            do char ','
                               e <- pOExp
                               r <- params bComma
                               return $ e : r
                            <|>   
                            do when bComma $ 
                                 lexeme (char '(') >> return ()
                               e <- pOExp
                               r <- params True
                               return $ e : r
  
  
    applyFunc fname params = call $ lookup fname functbl
      where
        functbl = [("sin",          1),
                   ("cos",          1),
                   ("tan",          1),
                   ("log",          1),
                   ("sqrt",         1),
                   ("var",          100),
                   ("userlowcase",  1),
                   ("oaotoabbr",    1),
                   ("frtf",         1),
                   ("getcasedolgn", 2),
                   ("getcasefio",   2),
                   ("getabbrfio",   1),
                   ("dscount",      1)
                   ]
              
        call Nothing =  throwErr $ NotFunction "Unrecognized function" fname
        call (Just argnum) 
             | P.length params /= argnum && argnum /= 100    
                          = throwErr $ NumArgs argnum  params
             | otherwise  = apply fname argnum params

        apply f n x  = case (f, x) of
          ("var", _)       -> TNull nn
          ("sin", [TI _ a])  -> TD nn $ sin $ i a
          ("sin", [TD _ a])  -> TD nn $ sin a
          ("cos", [TI _ a])  -> TD nn $ cos $ i a
          ("cos", [TD _ a])  -> TD nn $ cos a
          ("tan", [TI _ a])  -> TD nn $ tan $ i a
          ("tan", [TD _ a])  -> TD nn $ tan a
          ("log", [TI _ a])  -> TD nn $ log $ i a
          ("log", [TD _ a])  -> TD nn $ log a
          ("sqrt", [TI _ a]) -> TD nn $ sqrt $ i a
          ("sqrt", [TD _ a]) -> TD nn $ sqrt a
          ("userlowcase", [TS _ a]) -> TS nn $ srus2rtf  
                                       (map toLower $ srtf2rus  a rtf2rus) rus2rtf
          ("userlowcase", [TNull _ ]) -> TNull nn
          ("oaotoabbr", [TS _ a])   -> TS nn a
          ("oaotoabbr", [TNull _])  -> TNull nn 
          ("oaotoabbr", _)          -> throwErr $ TypeMismatch "string" x
          ("frtf", [TS _ a])   -> TS nn $ "todo: " ++ f
          ("frtf", [TNull _])  -> TNull nn -- {\rtlch\fcs1 \af0\afs24 \ltrch\fcs0 \f0\fs24\cf11\insrsid7800639\charrsid16087838 )\\}
          ("frtf", _)          -> throwErr $ TypeMismatch "string" x
          ("getcasedolgn", [TS _ a , TS _ b]) -> TS nn $ "todo: " ++ f
          ("getcasedolgn", [TNull _, _     ]) -> TNull nn
          ("getcasedolgn", [TS _ a ,TNull _]) -> TS nn a
          ("getcasedolgn", [_,TNull _])       -> throwErr $ TypeMismatch "string" [head x]
          ("getcasedolgn", [_,_])             -> throwErr $ TypeMismatch "string" [head $ tail x]
          ("getcasefio", [TS _ a , TS _ b])   -> TS nn $ "todo: " ++ f
          ("getcasefio", [TNull _, _     ])   -> TNull nn
          ("getcasefio", [TS _ a ,TNull _])   -> TS nn a
          ("getcasefio", [_,TNull _])         -> throwErr $ TypeMismatch "string" [head x]
          ("getcasefio", [_,_])               -> throwErr $   
                                                 TypeMismatch "string" [head $ tail x]
          ("getabbrfio", [TS _ a])   -> TS nn $ "todo: " ++ f
          ("getabbrfio", [TNull _])  -> TNull nn 
          ("getabbrfio", _)          -> throwErr $ TypeMismatch "string" x
          ("dscount", [TS _ a])      -> TS nn $ "todo: " ++ f
          ("dscount", [TNull _])     -> TNull nn 
          ("dscount", _)             -> throwErr $ TypeMismatch "string" x
          _                    -> throwErr $ UndefinedError $ show f  ++ "!!"
          
          where i = fromIntegral
    

-- i2p  prs st src  = enumPure1Chunk src (runI prs st)  >>= run 
s0 s  = runS expr genVars s'
  where s' = if head s == head comma then s
                                     else comma ++ s ++ comma
s1  = s0 "(1=1)"   
s2  = s0 "2.67*3 + 345" 
s3  = s0 "\"dsfsdf\" ++ \"DDD\"" 
s4  = s0 "\\\\\\\\"
-- s5 = i2p "\\1+1\\\\2+2\\" expr
s6 st = s0 "s:=sin(34)+Header.Id"
-- s7  = do (a,b)    <- s3 
--          (a1,b1)  <- s6 b
--          print (a1,b1)
s8  = s0 "Header.Id := 23"
s9  = runParser (do{s99;char '(';s99;char '('}) initialState  "" "elsif(if("
s99 = (reserved "if")<|>(reserved "elsif")
s10 = s0 "\\\\a:=10\\\\\\\\a+c10\\\\"
s11 = s0 "sa:= 12 = 12.23"
s12 = s0 "\\\\if(1=0)\\\\\\\\12\\\\\\\\elsif(1=1)\\\\\\\\23\\\\"
s14 = s0 "\"a\" == \"a\""
s15 = s0 "\\\\a:= -Header.Id\\\\"
s16 = s0 "\\\\if(5=0)\\\\\\\\12+24\\\\\\\\a1:=24\\\\\\\\elsif(1=1)\\\\\\\\1+3\\\\\\\\a2:=34\\\\\\\\endif\\\\"
s17 = s0 "\\\\if(1<0)\\\\\\\\if(3=3)\\\\\\\\57\\\\\\\\else\\\\\\\\endif\\\\\\\\else\\\\\\\\11\\\\\\\\endif\\\\"
s18 = s0 "\\\\if(1=2)\\\\\\\\11\\\\\\\\else\\\\\\\\if(2=2)\\\\\\\\if(3=4)\\\\\\\\15\\\\\\\\else\\\\\\\\16\\\\\\\\endif\\\\\\\\20\\\\\\\\else\\\\\\\\13\\\\\\\\endif\\\\\\\\14\\\\\\\\endif\\\\"
s19 = runF expr genVars
s20 = s0 "\\\\b:=1\\\\\\\\a:=100\\\\\\\\a+b\\\\"
s21 = s0 
      "\\\\a1:=102\\\\\\\\if(a=0)\\\\\\\\a1\\\\\\\\else\\\\\\\\44\\\\\\\\endif\\\\\\\\Qeader:\\'c7\\'e0\\'e3\\'ee\\'ebdff\\'ee\\'e2\\'ee\\'ea\\\\\\\\a:=1\\\\\\\\\\\\\\\\\\\\\\\\c:=2\\\\\\\\a :=4\\\\\\\\b:=5 \\\\\\\\a+b\\\\\\\\c\\\\\\\\tttttttttttttttttttttttttttttt\\\\\\\\b\\\\\\\\a\\\\\\\\b\\\\\\\\c\\\\\\\\Dataset:RecId\\\\\\\\Header:\\'c7\\'e0\\'e3\\'ee\\'eb\\'ee\\'e2\\'ee\\'ea\\\\\\\\A:=1\\\\\\\\B:=2\\\\\\\\a+b\\\\"
s26 = do str <- BS.readFile scripted >>= return . toString 
         putStrLn $ show str
         runS expr genVars str
       --  return $ runParser (expr) initialState "" str 
s24 =  s0  $ decodeString "\\\\\\rquote \208\144\208\159\208\144\208\162\208\152\208\162\208\171\\rquote \\\\"
s25 =  s0  "\\\\\\rquote \1040\1055\1040\1058\1048\1058\1067 \\rquote \\\\"
s27 =  s0 "\\\\a:=\8217\1040\1055\1040\1058\1048\1058\1067\8217\\\\" -- \\\\a\\\\\\\\userlowcase(a)\\\\"
s28 =  s0  "\\\\if(DSCount(SROActionsWhat:RecId)>0)\\\\"

-- n2 = do --
--         s <- manyTill n3 $ try $ eof --string comma --pIdent
--        -- string "\\\\"
--         return s

-- n3 = 
--      do  string comma
--          s <- try  p1 <|> try p2
--          string comma
--          return s
 
-- p1 = do s1 <- p2
--         string ":="
--         d <- many1 digit 
--         return  $ ":=" ++  d ++ s1
-- p2 = do a1 <- letter
--         b <- many body
--         return $ [a1] ++ concat b --[a1] ++ concat b
--         where  body =  many1 letter
--                    <|> many1 digit
--                    <|> do s <- many1 esc
--                           return $ concat s --concat s
--                esc = try $ string "\\'"
 
-- n4 =  do sep
--          words <- sepBy wo sep
--          eof
--          return $ concat words  
--          where sep = string "\\\\"     
--                wo  = manyTill (try p1 <|> try p2) $ try $ string comma

-- s26 = i2p "\\\\f1\\'23\\\\\\\\f2:=23\\\\" n4 genVars

-- n5  = do manyTill pp $ try $ eof  
--            where pp = between (string comma) (string comma) 
--                         (try p1  <|> try p2)
 
-- s27 = i2p "\\\\f1\\'23\\\\\\\\f2:=23\\\\" n5 genVars

parseScripted ::  Maybe (M.Map String Tok) -> String -> IO (Toks, ErrCount)
parseScripted vars fl = 
  do let vars' = case vars of
                   Just v  -> v
                   Nothing -> genVars
     (toks,err,vars'',errcount) <- runF expr vars' fl
     case err of
               --  Just e -> 
           Nothing -> do -- putStrLn $ show vars''
                         -- putStrLn $ show toks
                         return (P.foldr f IM.empty toks, errcount)
           where f el  acc = do let (a,b,c) = getTokPos el
                                IM.insert b el acc
     
toks2str :: Bool -> Rus2Rtf -> IM.IntMap Tok -> Skip -> (Skip, String)
toks2str showErr dict toks skip =   P.foldl f  (skip,[]) $ IM.toList  toks
  where 
    f  (st, ls) (pos,el) = 
          if showErr then  case el of 
                           TErr _ v   -> (Accept, err ls v)
                        --   TK   _ v r -> (Accept, ls ++ P.foldl f' [] r)   
                           _          -> (Accept, ls)
                     else case el of 
                           TS    (_,_,st') v    -> (st',ls ++ v) -- srus2rtf  v dict )
                           TI    (_,_,st') v    -> (st',ls ++ show v)
                           TD    (_,_,st') v    -> (st',ls ++ show v)
                           TB    (_,_,st') v    -> (st',ls ++ show v)
                           TL    (_,_,st') v    -> (st',ls ++ show v)
                           TV    (_,_,st') v    -> (st',ls ++ show v)
                           TErr  (_,_,st') v    -> (st',ls)
                           TNull (_,_,st')      -> (st', ls)
                           TK    (_,_,st') v _  -> (st',ls)
                                                 
    -- f' (TErr pos res) acc = err acc res                                             
    -- f' _              acc = acc                           
    err a e = a ++ "***" ++ show e ++ "***"

t1 =  parseScripted Nothing scripted  >>= return . toks2str True  rus2rtf . fst
t2 =  parseScripted Nothing scripted  >>= return . toks2str False rus2rtf . fst

genVars =  do
           let rr = rus2rtf
           foldr (genVars' rr)  M.empty [("Header.Id"        , TI nn 100)
                                        ,("выбранПРЭ"        , TS nn "1") 
                                        ,("DS_ISOORG:OAO"    , TS nn $ ss rr "ОАО")
                                        ,("DS_ISOORG:ABBR"   , TS nn $ ss rr "Ситком")
                                        ,("DS_ISOORG:ADDR"   , TS nn $ ss rr "Москва")
                                        ,("DS_ISOSYSTEM:NAME", TS nn $ ss rr "Еврорус")
                                        ,("ПравоваяФорма" ,    TS nn $ ss rr "ООО")
                                        ,("КраткоеНазвание",   TS nn $ ss rr "Пельмень")
                                        ,("ИНН",               TS nn "123123123")
                                        ,("Переоформление",    TS nn "1")
                                  ]
              where ss r s = srus2rtf s r
                    genVars' r (name,val) map  = M.insert (srus2rtf name r) val  map

------------------------------------------------------------------------------

kaska = "c:/rtfout/kaska.txt"
getVars fl = do src <- (BS.readFile fl >>= return . toString)
                (_,err,vars) <- return $ runKParser kParser src M.empty
                return $ Just vars

type KVars =  M.Map String Tok
data KParserState = 
  KParserState {kVars :: KVars, dict :: Rus2Rtf} deriving (Show)
type KParser a = GenParser Char KParserState a
updateKVars var val  = updateState $ \st -> st {kVars = assign var val (dict st) $ kVars st}
  where 
     assign var val dict map = do
       let var' = srus2rtf var dict                   
       let val' = TS nn $ srus2rtf val dict
       case M.lookup var map of
                           Nothing -> M.insert var' val' map
                           _ ->  M.update (\_ -> Just val') var' map
      
runKParser
  :: ParsecT String KParserState Identity [String]
     -> String
     -> KVars
     -> ([String], Maybe (String, ParseError), KVars)
runKParser p input st = 
  case runParser p' (KParserState st rus2rtf)  "" input of
    Left err         -> ([],Just (take 20 $ snd $ splitAt 
                                  (sourceColumn $  errorPos err) input,err), st)
    Right (val, st')   -> (val, Nothing, kVars st')
  where p' = do e  <- p
                eof 
                st <- getState
                return (e,st)
                   
kParser = do kSkip  
             many1 $ (try kVar <|> try kEnd) -- <|> kRest
                
kSkip   = do manyTill anyChar $ lookAhead $ char '['
             return ()
kVar    = do char '['
             var <- manyTill anyChar $ char ']'
             val <- manyTill anyChar $ try $ lookAhead $ char '['
             updateKVars var val
             return [] -- $ var ++ val
          
kEnd = do string "[END]"
          return []
         
kRest = many anyChar
          
