-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.GraphML.Ppr
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- GraphML pretty printing module.
--
-----------------------------------------------------------------------------

-- FIMXE: this module is ugly, need functions for printing tags to avoid
--        print < /> and ="" all the time
--        maybe reuse a XML pretty printer?

module ForSyDe.Backend.GraphML.Ppr where

import Text.PrettyPrint.HughesPJ
import ForSyDe.Backend.GraphML.AST
import ForSyDe.Netlist
import ForSyDe.Netlist.Traverse

-- | Pretty printing class
class Ppr a where
 ppr :: a -> Doc

-- | Number of spaces used for indentation
nestVal :: Int
nestVal = 5


instance Ppr GraphMLGraph where
 ppr (GraphMLGraph id nodes edges) =
  text "<graph" <+> text ("id=\"" ++ id ++ "\"") <+> text "edgedefault=\"directed\" >" $+$
    nest nestVal (vSpace $+$    
                  ppr_list (vNSpaces 1) nodes $+$
                  vSpace $+$
                  ppr_list (vNSpaces 1) edges $+$
                  vSpace) $+$
  text "</graph>" 


instance Ppr GraphMLNode where
 ppr (ProcNode ins outs) =
   text "<node" <+> text ("id=\"" ++ id ++ "\"") <> text ">" $+$
   nest nestVal (vcat (map port portIds)) $+$
   text "</node>" 
  where id = case ins of
               InPort id -> id
               Proc id _ -> id
        portIds = arguments ins ++ outs
 -- FIXME: UGLY replication
 ppr (OutNode id portid) =        
   text "<node" <+> text ("id=\"" ++ id ++ "\"") <> text ">" $+$
   nest nestVal (port portid) $+$
   text "</node>" 


instance Ppr GraphMLEdge where
 ppr (GraphMLEdge origN origP destN destP) = 
  text "<edge" <+> text ("source=\"" ++ origN ++ "\"") <+> 
                   text ("sourceport=\"" ++ origP ++ "\"") <+>
                   text ("target=\"" ++ destN ++ "\"") <+> 
                   text ("targetport=\"" ++ destP ++ "\"") <+> text "/>"



--FIMXE: the name of this funcion in not intuitive, it ptins the graphml tag as well

-- | pretty print a Graph with XML headers and key definitions
pprGraphWithHeaders :: GraphMLGraph -> Doc
pprGraphWithHeaders graph = 
 text "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" $+$
 text "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"" <+>  
 text "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" <+>
 text "xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns" <+> 
 text "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">" $+$
   nest nestVal (ppr graph) $+$
 text "</graphml>"


-------------------------
-- Tag printing functions
-------------------------

port :: GraphMLPortId -> Doc
port id = text "<port" <+> text ("name=\"" ++ id ++ "\"") <> text "/>"   


-------------------
-- Helper Functions
-------------------

-- FIMXE: some of this functions are not used and replicated from the VHDL backend

-- dot
dot :: Doc
dot = char '.'

-- One line vertical space
vSpace :: Doc
vSpace = text ""

-- Multi-line vertical space
multiVSpace :: Int -> Doc
multiVSpace  n = vcat (replicate n (text ""))  

-- Pretty print a list supplying the document joining function
ppr_list :: Ppr a => (Doc -> Doc -> Doc) -> [a] -> Doc
ppr_list _ []    = empty
ppr_list join (a1:rest) = go a1 rest 
  where go a1 []        = ppr a1
        go a1 (a2:rest) = ppr a1 `join` go a2 rest


-- | Join two documents vertically leaving n vertical spaces between them
vNSpaces :: Int -> Doc -> Doc -> Doc
vNSpaces n doc1 doc2 = doc1 $+$ 
                        foldr ($+$) empty (replicate n vSpace) $+$
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

-- | Only append if both of the documents are non-empty
(<++>) :: Doc -> Doc -> Doc
d1 <++> d2 
 | isEmpty d1 || isEmpty d2 = empty
 | otherwise = d1 <+> d2

-- | Enclose in parenthesis only if the document is non-empty
parensNonEmpty :: Doc -> Doc
parensNonEmpty doc | isEmpty doc = empty
parensNonEmpty doc = parens doc