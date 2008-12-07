-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is part of the ATerm library for Haskell. It contains functions
-- for reading and writing ATerms from and to Strings. Two ATerm formats are
-- supported:
--
--      * AT:   plain (non-shared) textual ATerms
--      
--      * TAF:  shared textual ATerms
--
--  The binary ATerm format (BAF) is not supported.
--
--  Current limitations:
--
--      * BLOBS and place-holders are not supported.
--      
--      * Annotations are not supported.
--
-----------------------------------------------------------------------------

module ATermReadWrite (
        readATerm,
        writeATerm,
        writeSharedATerm
) where

import ATermAbstractSyntax
import Data.Map as FM
import Char

-----------------------------------------------------------------------------
-- * From String to ATerm

-- | Parse the given string into an ATerm.
readATerm               :: String -> ATerm
readATerm ('!':str)     = let (t,_,_,_) = readTAF str emptyRT 0 in t
readATerm str           = let (t,_)     = readAT str            in t

                                                               -- non-shared --

readAT ('[':str)        =  let (kids, str') = readATs (dropSpaces str)
                           in (AList kids, str')
readAT str@(c:cs)
  | isIntHead c         =  let (i,str') = span isDigit cs
                           in (AInt (read (c:i)),str')
  | otherwise           =  let (c,str')      = readAFun str
                               (kids, str'') = readParenATs (dropSpaces str')
                           in (AAppl c kids, str'')

readAFun ('"':str)      =  let (c,('"':str')) = spanNotQuote' str 
                           in (quote c,str')
readAFun str            =  spanAFunChar str

readParenATs ('(':str)  =  readATs (dropSpaces str)
readParenATs str        =  ([],str)


readATs (')':str)       =  ([],str)
readATs (']':str)       =  ([],str)
readATs str             =  readATs1 str

readATs1 str            =  let (t,str')   = readAT (dropSpaces str)
                               (ts,str'') = readATs' (dropSpaces str')
                           in (t:ts,str'')

readATs' (',':str)      = readATs1 (dropSpaces str)
readATs' (')':str)      = ([],str)
readATs' (']':str)      = ([],str)

                                                                   -- shared --
                                                                   
readTAF                 :: String -> ReadTable -> Int
                        -> (ATerm,String,ReadTable,Int)
readTAF ('#':str) tbl l =  let (i,str') = spanAbbrevChar str
                           in (getElementRT (deAbbrev i) tbl, 
                               str',tbl,l+(length i)+1)    
readTAF ('[':str) tbl l =  let (kids, str',tbl',l') 
                                  = readTAFs (dropSpaces str) tbl 1
                               t  = AList kids
                           in (t, str',condAddElementRT t l' tbl',l+l')
readTAF str@(c:cs) tbl l
  | isIntHead c         =  let (i,str') = span isDigit cs
                               ci       = (c:i)
                               l'       = length ci
                               t        = AInt (read ci) 
                               tbl'     = condAddElementRT t l' tbl
                           in (t,str',tbl',l+l')
  | otherwise           =  let (c,str') = readAFun str
                               (kids, str'',tbl',l') 
                                   = readParenTAFs (dropSpaces str') tbl 0
                               t   = AAppl c kids
                               lks = if Prelude.null kids then 0 else l'
                               l'' = (length c) + lks
                           in (t, str'',condAddElementRT t l'' tbl',l'')

readParenTAFs ('(':str) tbl l   =  readTAFs (dropSpaces str) tbl l
readParenTAFs str tbl l         =  ([],str,tbl,l)

readTAFs (')':str) tbl l        =  ([],str,tbl,l+1)
readTAFs (']':str) tbl l        =  ([],str,tbl,l+1)
readTAFs str tbl l              =  readTAFs1 str tbl l

readTAFs1 str tbl l     =  let (t,str',tbl',l')
                                   = readTAF (dropSpaces str) tbl l
                               (ts,str'',tbl'',l'') 
                                   = readTAFs' (dropSpaces str') tbl' l'
                           in (t:ts,str'',tbl'',l'')

readTAFs' (',':str) tbl l       = readTAFs1 (dropSpaces str) tbl (l+1)
readTAFs' (')':str) tbl l       = ([],str,tbl,l+1)
readTAFs' (']':str) tbl l       = ([],str,tbl,l+1)

                                                                  -- helpers --

dropSpaces              = dropWhile isSpace
spanAFunChar            = span isAFunChar
isAFunChar c            = (isAlphaNum c) || (c `elem` "-_*+")
spanNotQuote            = span (/='"')
spanAbbrevChar          = span (`elem` toBase64)
isIntHead c             = (isDigit c) || (c=='-')
quote str               = ('"':str)++"\""

spanNotQuote' []                = ([],[])
spanNotQuote' xs@('"':xs')      = ([],xs)
spanNotQuote' xs@('\\':'"':xs') = ('\\':'"':ys,zs) 
                                  where (ys,zs) = spanNotQuote' xs'
spanNotQuote' xs@('\\':'\\':xs')= ('\\':'\\':ys,zs) 
                                  where (ys,zs) = spanNotQuote' xs'
spanNotQuote' xs@(x:xs')        = (x:ys,zs) 
                                  where (ys,zs) = spanNotQuote' xs'

-----------------------------------------------------------------------------
-- * From ATerm to String 

-- | Write the given ATerm to non-shared textual representation (TXT format).
writeATerm              :: ATerm -> String
writeATerm t            = writeAT t

-- | Write the given ATerm to fully shared textual representation (TAF format).
writeSharedATerm        :: ATerm -> String
writeSharedATerm t      = let (s,_) = writeTAF t emptyWT in ('!':s)

                                                               -- non-shared --

writeAT                 :: ATerm -> String
writeAT (AAppl c ts)    =  writeATermAux c (Prelude.map writeAT ts)
writeAT (AList ts)      =  bracket (commaSep (Prelude.map writeAT ts))
writeAT (AInt i)        =  show i

                                                                   -- shared --

writeTAF                :: ATerm -> WriteTable -> (String,WriteTable)
writeTAF t tbl          =  case getIndexWT t tbl of
                             (Just i) -> (makeAbbrev i,tbl) 
                             Nothing  -> (str, condAddElementWT t str tbl')
                                         where (str,tbl') = writeTAF' t tbl

writeTAF' (AAppl c ts) tbl      = let (kids,tbl') = writeTAFs ts tbl
                                  in (writeATermAux c kids,tbl')
writeTAF' (AList ts) tbl        = let (kids,tbl') = writeTAFs ts tbl
                                  in (bracket (commaSep kids),tbl')
writeTAF' (AInt i) tbl          = (show i,tbl)

writeTAFs [] tbl                =  ([],tbl)
writeTAFs (t:ts) tbl    =  let (str,tbl')   = writeTAF t tbl
                               (strs,tbl'') = writeTAFs ts tbl'
                           in ((str:strs),tbl'')

                                                                  -- helpers --
 
writeATermAux c []      =  c
writeATermAux c ts      =  c++(parenthesise (commaSep ts))

sepBy sep (x:y:ys)      =  x:sep:sepBy sep (y:ys)
sepBy sep ys            =  ys

commaSep strs           =  concat (sepBy "," strs)
bracket str             = "["++str++"]"
parenthesise str        = "("++str++")"

-----------------------------------------------------------------------------
-- * Tables of ATerms 

                                                -- For reading (remove sharing)
-- Using reversed List
type ReadTable          = (Integer,[ATerm])
emptyRT                 = (0,[])
-- Hook to prevent expansion of productions. This is useful when reading
-- huge AsFix files. 
getElementRT' i tbl     = case getElementRT i tbl of
                            (AAppl "prod" _) -> AAppl "#prod" []
                            t -> t
getElementRT  i tbl     = (snd tbl)!!!((fst tbl)-i-1)
addElementRT a tbl      = ((fst tbl)+1,a:(snd tbl))
sizeOfRT tbl            = fst tbl
{--- Using Finite Map
type ReadTable          = FiniteMap Integer ATerm
emptyRT                 = emptyFM
getElementRT  i tbl     = lookupWithDefaultFM tbl (error "getElement") i
addElementRT a tbl      = addToFM tbl (sizeOfRT tbl) a
sizeOfRT tbl            = toInteger (sizeFM tbl)
-}
{--- Using some imported Edison sequence
type ReadTable          = T.Seq ATerm
emptyRT                 = T.empty :: ReadTable
getElementRT  i tbl     = T.lookup tbl (toInt i)
addElementRT t tbl      = T.snoc tbl t
sizeOfRT tbl            = T.size tbl
-}

condAddElementRT t l tbl
  = if (length next_abbrev) < l then
       addElementRT t tbl
    else
       tbl
    --where next_abbrev = makeAbbrev (toInteger (sizeOfRT tbl)+1)
    where next_abbrev = makeAbbrev (toInteger (sizeOfRT tbl))
    
condAddElementRT' t str tbl
  = if (length next_abbrev) < (length str) then
       addElementRT t tbl
    else
       tbl
    where next_abbrev = makeAbbrev (toInteger (sizeOfRT tbl)+1)
 
                                                -- For writing (create sharing)
-- Using FiniteMap
type WriteTable         = FM.Map ATerm Integer
emptyWT                 = FM.empty
getIndexWT    a tbl     = FM.lookup a tbl
addElementWT a tbl      = FM.insert a (sizeOfWT tbl) tbl
sizeOfWT tbl            = toInteger (FM.size tbl)

condAddElementWT t str tbl
  = if (length next_abbrev) < (length str) then
       addElementWT t tbl
    else
       tbl
    --where next_abbrev = makeAbbrev (toInteger (sizeOfWT tbl)+1)
    where next_abbrev = makeAbbrev (toInteger (sizeOfWT tbl))

-----------------------------------------------------------------------------
-- * Base 64 encoding

mkAbbrev x
  | x == 0      = [toBase64!!0]
  | otherwise   = reverse (mkAbbrevAux x)

mkAbbrevAux x
  | x == 0      = []
  | x > 0       = (toBase64!!!m:mkAbbrevAux d) where (d,m) = divMod x 64

deAbbrev x              =  deAbbrevAux (reverse x)

deAbbrevAux []          =  0
deAbbrevAux (c:cs)      =  let (Just i) = indexOf c toBase64
                               r        = deAbbrevAux cs
                           in (i + 64*r)

toBase64 =
  [ 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
    'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
    'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
    'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/' 
  ]
  
makeAbbrev i = '#':mkAbbrev i

-----------------------------------------------------------------------------
-- * Helpers 

indexOf t []            =  Nothing
indexOf t (x:xs)        =  if t==x
                           then (Just (0::Integer))
                           else case indexOf t xs of
                                 (Just i)   -> Just (i+1)
                                 Nothing    -> Nothing
(!!!)              :: [b] -> Integer -> b
(x:_)  !!! 0       =  x
(_:xs) !!! n | n>0 =  xs !!! (n-1)
(_:_)  !!! _       =  error "!!!: negative index"
[]     !!! _       =  error "!!!: index too large"

-----------------------------------------------------------------------------
-- * Future work

{- The code could be made more readable by introducing
   special monads that hide the consumed String/ATerm and the 
   table that is built during reading/writing
   
newtype ReadMonad t     = RM { runRM :: String -> ReadTable 
                                     -> (t,String,ReadTable) }
instance Monad ReadMonad where
  return t = RM $ \str tbl -> (t,str,tbl)
  rm >>= f = RM $ \str tbl -> let (t,str',tbl') = runRM rm str tbl
                              in runRM (f t) str' tbl'
                              
  I guess this should be a combined ParserMonad and StateMonad.                       
-}

-------------------------------------------------------------------------------
-- * Testing

-- | Test whether reading and writing to and from shared aterm representations
--   (TAF format) deals correctly with the bounderies of the base-64 encoding.
testAbbrevBoundaries :: Bool
testAbbrevBoundaries 
  = writeSharedATerm term == shared && readATerm shared == term
  where
    term = AList $ Prelude.map AInt ([100..164]++[100..164])
    shared = "![100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,#A,#B,#C,#D,#E,#F,#G,#H,#I,#J,#K,#L,#M,#N,#O,#P,#Q,#R,#S,#T,#U,#V,#W,#X,#Y,#Z,#a,#b,#c,#d,#e,#f,#g,#h,#i,#j,#k,#l,#m,#n,#o,#p,#q,#r,#s,#t,#u,#v,#w,#x,#y,#z,#0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#+,#/,164]"

-------------------------------------------------------------------------------
