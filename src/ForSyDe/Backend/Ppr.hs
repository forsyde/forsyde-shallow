{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Ppr
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- ForSyDe pretty-printing class and auxiliar functions.
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.Ppr where

import Text.PrettyPrint.HughesPJ

-- | Pretty printing class
class Ppr a where
 ppr :: a -> Doc

-- identity instantiation
instance Ppr Doc where
 ppr = id

-- | Pretty printing class with associated printing options
class PprOps ops toPpr | toPpr -> ops where
 -- NOTE: Would it be better to use a State Monad?
 -- i.e. pprOps :: toPpr -> State ops Doc
 pprOps :: ops -> toPpr -> Doc 


-- dot
dot :: Doc
dot = char '.'

-- One line vertical space
vSpace :: Doc
vSpace = text ""

-- Multi-line vertical space
multiVSpace :: Int -> Doc
multiVSpace  n = vcat (replicate n (text ""))  

-- Pretty-print a list supplying the document joining function
ppr_list :: Ppr a => (Doc -> Doc -> Doc) -> [a] -> Doc
ppr_list _ []    = empty
ppr_list join (a1:rest) = go a1 rest 
  where go a1 []        = ppr a1
        go a1 (a2:rest) = ppr a1 `join` go a2 rest

-- Pretty-print a list supplying the document joining function
-- (PprOps version)
pprOps_list :: PprOps ops toPpr => ops -> (Doc -> Doc -> Doc) -> [toPpr] -> Doc
pprOps_list _ _ [] = empty
pprOps_list ops join (a1:rest) = go a1 rest
 where go a1 [] = pprOps ops a1
       go a1 (a2:rest) = pprOps ops a1 `join` go a2 rest

-- | Join two documents vertically leaving n vertical spaces between them
vNSpaces :: Int -> Doc -> Doc -> Doc
vNSpaces n doc1 doc2 = doc1 $+$ 
                        multiVSpace n $+$
                       doc2

-- Join two documents vertically putting a semicolon in the middle
vSemi :: Doc -> Doc -> Doc
vSemi doc1 doc2 = doc1 <> semi $+$ doc2


-- Join two documents vertically putting a comma in the middle
vComma :: Doc -> Doc -> Doc
vComma doc1 doc2 = doc1 <> comma $+$ doc2

-- Join two documents horizontally putting a comma in the middle
hComma :: Doc -> Doc -> Doc
hComma doc1 doc2 = doc1 <> comma <+> doc2


-- | apply sep to a list of prettyprintable elements, 
--   previously interspersing commas
commaSep :: Ppr a => [a] -> Doc
commaSep = sep.(punctuate comma).(map ppr)
 

-- | Only append if both of the documents are non-empty
($++$) :: Doc -> Doc -> Doc
d1 $++$ d2 
 | isEmpty d1 || isEmpty d2 = empty
 | otherwise = d1 $+$ d2


-- | Only append if both of the documents are non-empty
(<++>) :: Doc -> Doc -> Doc
d1 <++> d2 
 | isEmpty d1 || isEmpty d2 = empty
 | otherwise = d1 <+> d2

-- | Enclose in parenthesis only if the document is non-empty
parensNonEmpty :: Doc -> Doc
parensNonEmpty doc | isEmpty doc = empty
parensNonEmpty doc = parens doc

-- | Enclose in parenthesis only if the predicate is True
parensIf :: Bool -> Doc -> Doc
parensIf p d = if p then parens d else d
