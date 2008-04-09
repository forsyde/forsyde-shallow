{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Ppr
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- VHDL pretty printing module.
--
-----------------------------------------------------------------------------


-- FIXME:  we could avoid the LANGUAGE extensions by adding ppr_list to the Ppr 
--         class (see Language.Haskell.TH.Ppr)

module ForSyDe.Backend.VHDL.Ppr where

import Text.PrettyPrint.HughesPJ hiding (Mode)
import ForSyDe.Backend.VHDL.AST

-- | Pretty printing class
class Ppr a where
 ppr :: a -> Doc

-- | Number of spaces used for indentation
nestVal :: Int
nestVal = 5

instance Ppr a => Ppr (Maybe a) where
 ppr (Just a) = ppr a
 ppr Nothing  = empty 

instance (Ppr a, Ppr b) => Ppr (a,b) where
 ppr (a,b) = ppr a <+> ppr b

instance Ppr VHDLId where
 ppr = text.fromVHDLId
 

instance Ppr DesignFile where
 ppr (DesignFile cx lx) = vNSpaces spaces (ppr_list ($+$) cx) 
                                          (ppr_list (vNSpaces spaces) lx)  $+$
                          vSpace
  where spaces = 2


        
instance Ppr ContextItem where
 ppr (Library id) = text "library" <+> ppr id <> semi
 ppr (Use name) = text "use" <+> ppr name <> semi
 

instance Ppr LibraryUnit where
 ppr (LUEntity entityDec) = ppr entityDec
 ppr (LUArch archBody)  = ppr archBody
 ppr (LUPackage packageDec) = ppr packageDec 

instance Ppr EntityDec where
 ppr (EntityDec id ifaceSigDecs) = 
  text "entity" <+> idDoc <+> text "is" $+$
     nest nestVal (ppr ifaceSigDecs) $+$
  text "end entity" <+> idDoc <> semi
  where idDoc = ppr id  

instance Ppr [IfaceSigDec] where
-- FIXME: improve pretty printing of the
--        declarations
-- This:
-- S1 : bit
-- Ssdds : bit
-- should really be printed as
-- S1    : bit
-- Ssdds : bit
 ppr []   = empty
 ppr decs = text "port"  $$ nest identL (parens decDoc <> semi) 
   where identL = length "port " 
         decDoc = ppr_list vSemi decs


instance Ppr IfaceSigDec where
 ppr (IfaceSigDec id mode typemark) = 
   ppr id <+> colon <+> ppr mode <+> ppr typemark


instance Ppr Mode where
 ppr In  = text "in"
 ppr Out = text "out"

instance Ppr ArchBody where
 ppr (ArchBody id enName decs conSms) = 
  text "architecture" <+> idDoc <+> text "of" <+> ppr enName <+> text "is" $+$
    nest nestVal (ppr decs) $+$
  text "begin" $+$
    nest nestVal (ppr_list (vNSpaces 1) conSms) $+$
  text "end architecture" <+> idDoc <> semi
  where idDoc = ppr id

instance Ppr PackageDec where
 ppr (PackageDec id typeDecs) =
  text "package" <+> idDoc <+> text "is" $+$
   nest nestVal (ppr_list (vNSpaces 1) typeDecs) $+$
  text "end package" <+> idDoc <> semi
  where idDoc = ppr id 


instance Ppr TypeDec where
 ppr (TypeDec id typeDef) = 
  text "type" <+> ppr id <+> text "is" $$
   nest  indentL (ppr typeDef <> semi)
  where indentL = length "type " + (length.fromVHDLId) id + 1

instance Ppr TypeDef where
 ppr (TDA arrayTD) = ppr arrayTD
 ppr (TDR recordTD) = ppr recordTD

instance Ppr ArrayTypeDef where
 ppr (ArrayTypeDef init last tm) = 
  text "array" <+> parens (int init <+> text "to" <+>  int last) <+>
   text "of" <+> ppr tm

instance Ppr RecordTypeDef where
 ppr (RecordTypeDef elementDecs) =
  text "record" $+$
   nest nestVal (ppr_list ($+$) elementDecs) $+$ 
  text "end record"

instance Ppr ElementDec where
 ppr (ElementDec id tm) = ppr id <+> colon <+> ppr tm <> semi 

instance Ppr VHDLName where
 ppr (NSimple simple) = ppr simple
 ppr (NSelected selected) = ppr selected

instance Ppr SelectedName where
 ppr (prefix :.: suffix) = ppr prefix <> dot <> ppr suffix

instance Ppr Suffix where
 ppr (SSimple simpleName) = ppr simpleName
 ppr All = text "all"

instance Ppr [BlockDecItem] where
 ppr = ppr_list ($+$)

instance Ppr BlockDecItem where
 ppr (BDISPB subProgBody) = ppr subProgBody
 ppr (BDISD  sigDec)      = ppr sigDec


instance Ppr SubProgBody where
 ppr (SubProgBody subProgSpec statements) = 
  ppr subProgSpec <+> text "is" $+$
  text "begin" $+$
    nest nestVal  (ppr_list ($+$) statements) $+$
-- TODO, show the id here
  text "end" <> semi

instance Ppr SubProgSpec where
-- FIXME: improve pretty printing of the
--        declarations
-- This:
-- (S1 : bit;
--  Ssdds : bit)
-- should really be printed as
-- (S1    : bit;
--  Ssdds : bit)
 ppr (Function name decList returnType) =
    text "function"  <+> ppr name               $$
       nest indentL (parens (ppr_decs decList)) $$
       nest indentL (text "return" <+> ppr returnType)
   where nameDoc = ppr name
         indentL = length "function " + (length.render) nameDoc + 1
         ppr_decs [] = empty
         ppr_decs ds = ppr_list vSemi ds

         

instance Ppr IfaceVarDec where
 ppr (IfaceVarDec id typemark) = ppr id <+> colon <+> ppr typemark

instance Ppr SeqSm where
 ppr (IfSm  cond sms elseIfs maybeElse) = 
   text "if" <+> ppr cond <+> text "then" $+$
       nest nestVal (ppr sms) $+$
   ppr elseIfs $+$
   ppr maybeElse $+$
   text "end if" <> semi
 ppr (CaseSm patern alts) = text "case" <+> ppr patern <+> text "is" $+$
                               nest nestVal (ppr alts) $+$
                            text "end case" <> semi
 ppr (ReturnSm maybeExpr) = text "return" <+> ppr maybeExpr <> semi

instance Ppr [SeqSm] where
 ppr = ppr_list ($+$)

instance Ppr [ElseIf] where
 ppr = ppr_list ($+$)

instance Ppr ElseIf where
 ppr (ElseIf cond sms) = text "elseif" <+> ppr cond <+> text "then" $+$
                            nest nestVal (ppr sms)

instance Ppr Else where
 ppr (Else sms) = text "else" $+$
                     nest nestVal (ppr sms)


instance Ppr [CaseSmAlt] where
 ppr = ppr_list ($+$)

instance Ppr Choice where
 ppr (ChoiceE expr) = ppr expr
 ppr Others         = text "others" 

instance Ppr CaseSmAlt where
 ppr (CaseSmAlt alts sms) = 
    text "when" <+> ppr_list joinAlts alts <+> text "=>" $+$
       nest nestVal (ppr sms)
  where joinAlts a1 a2 = a1 <+> char '|' <+> a2

instance Ppr SigDec where
 ppr (SigDec id typemark mInit) = 
   text "signal" <+> ppr id <+> colon <+> ppr typemark <+> 
                    (text ":=" <++>  ppr mInit) <> semi

instance Ppr [ConcSm] where
 ppr = ppr_list ($$)
  
instance Ppr ConcSm where
 ppr (CSBSm blockSm) = ppr blockSm
 ppr (CSSASm conSigAssignSm) = ppr conSigAssignSm
 ppr (CSISm compInsSm) = ppr compInsSm

instance Ppr BlockSm where
 ppr (BlockSm label ifaceSigDecs pMapAspect blockDecItems concSms) =
   labelDoc <+> colon <+> text "block" $+$
              nest nestVal (ppr ifaceSigDecs)          $+$
              nest nestVal (ppr pMapAspect) <> semi    $+$
              nest nestVal (ppr blockDecItems)         $+$
       text "begin" $+$
              nest nestVal (ppr concSms) $+$
       text "end block" <+> labelDoc <> semi
  where labelDoc = ppr label
   

instance Ppr PMapAspect where
 ppr (PMapAspect assocs) = text "port map" $$ nest identL (ppr assocs)
  where identL = length "port map "

instance Ppr [AssocElem] where
 ppr [] = empty
 ppr assocs = parens (ppr_list vComma assocs)
         

instance Ppr AssocElem where
 ppr (formalPart :=>: actualPart) = formalPartDoc <+> ppr actualPart
  where formalPartDoc = maybe empty (\fp -> ppr fp <+> text "=>") formalPart

instance Ppr ActualDesig where
 ppr (ADName name) = ppr name
 ppr (ADExpr expr) = ppr expr
 ppr Open          = text "open"

instance Ppr ConSigAssignSm where
 ppr (target :<==: cWforms) = targetDoc <+> text "<=" $$
                                nest indentL (ppr cWforms) <> semi
  where  targetDoc = ppr target
         indentL = (length.render) targetDoc + length " <= "

instance Ppr ConWforms where
 ppr (ConWforms whenElses wform lastElse) = ppr_list ($+$) whenElses $+$
                                            ppr wform <+> ppr lastElse

instance Ppr WhenElse where
 ppr (WhenElse wform expr) = ppr wform <+> text "when" <+> 
                             ppr expr  <+> text "else"

instance Ppr When where
 ppr (When cond) = text "when" <+> ppr cond

instance Ppr Wform where
 ppr (Wform elems) = ppr_list hComma elems
 ppr Unaffected    = text "unaffected"

instance Ppr CompInsSm where
 ppr (CompInsSm label insUnit assocElems) =
   ppr label <+> colon $$ nest labelL (
            ppr insUnit $+$
              nest nestVal (ppr assocElems)<>semi
            )  
  where labelL = (length.fromVHDLId) label 

instance Ppr InsUnit where
 ppr (IUEntity name) = text "entity" <+> ppr name

-- FIXME, remove parenthesis according to precedence
instance Ppr Expr where
 ppr (And e1 e2)  = parens (ppr e1) <+> text "and"  <+> parens (ppr e2) 
 ppr (Or  e1 e2)  = parens (ppr e1) <+> text "or"   <+> parens (ppr e2)
 ppr (Xor e1 e2)  = parens (ppr e1) <+> text "xor"  <+> parens (ppr e2)
 ppr (Nand e1 e2) = parens (ppr e1) <+> text "nand" <+> parens (ppr e2)
 ppr (Nor  e1 e2) = parens (ppr e1) <+> text "nor"  <+> parens (ppr e2)
 -- Relational Operators
 ppr (e1 :=:  e2) = parens (ppr e1) <+> text "="  <+> parens (ppr e2)
 ppr (e1 :/=: e2) = parens (ppr e1) <+> text "/=" <+> parens (ppr e2)
 ppr (e1 :<:  e2) = parens (ppr e1) <+> text "<"  <+> parens (ppr e2) 
 ppr (e1 :<=: e2) = parens (ppr e1) <+> text "<=" <+> parens (ppr e2)
 ppr (e1 :>:  e2) = parens (ppr e1) <+> text ">"  <+> parens (ppr e2)
 ppr (e1 :>=: e2) = parens (ppr e1) <+> text ">=" <+> parens (ppr e2)
 -- Shift Operators
 ppr (Sll e1 e2) = parens (ppr e1) <+> text "sll" <+> parens (ppr e2)
 ppr (Srl e1 e2) = parens (ppr e1) <+> text "srl" <+> parens (ppr e2)
 ppr (Sla e1 e2) = parens (ppr e1) <+> text "sla" <+> parens (ppr e2)
 ppr (Sra e1 e2) = parens (ppr e1) <+> text "sra" <+> parens (ppr e2)
 ppr (Rol e1 e2) = parens (ppr e1) <+> text "rol" <+> parens (ppr e2)
 ppr (Ror e1 e2) = parens (ppr e1) <+> text "ror" <+> parens (ppr e2)
 -- Adding Operators
 ppr (e1 :+: e2) = parens (ppr e1) <+> text "+" <+> parens (ppr e2) 
 ppr (e1 :-: e2) = parens (ppr e1) <+> text "-" <+> parens (ppr e2)
 ppr (e1 :&: e2) = parens (ppr e1) <+> text "&" <+> parens (ppr e2)
 -- Sign Operators
 ppr (Neg e) = text "-" <> parens(ppr e) 
 ppr (Pos e) = text "+" <> parens(ppr e)
 -- Multiplying Operators
 ppr (e1 :*: e2) = parens (ppr e1) <+> text "*" <+> parens (ppr e2) 
 ppr (e1 :/: e2) = parens (ppr e1) <+> text "/" <+> parens (ppr e2)
 ppr (Mod e1 e2) = parens (ppr e1) <+> text "mod" <+> parens (ppr e2)
 ppr (Rem e1 e2) = parens (ppr e1) <+> text "rem" <+> parens (ppr e2)
 -- Miscellaneous Operators
 ppr (e1 :**: e2) = parens (ppr e1) <+> text "**" <+> parens (ppr e2)
 ppr (Abs e)      = text "abs" <+> parens (ppr e)
 ppr (Not e)      = text "not" <+> parens (ppr e)
 -- Primary expressions
 -- Only literals, names and function calls  are allowed
 ppr (PrimName name)    = ppr name
 ppr (PrimLit  lit)     = text lit
 ppr (PrimFCall fCall)  = ppr fCall
 -- Composite-type  expressions
 ppr (Aggregate exps)        = parens (ppr_list hComma  exps)
 ppr (IndexedExp exp1 exp2) = ppr exp1 <> parens (ppr exp2)
 ppr (SelectedExp exp1 exp2) = ppr exp1 <> dot <> ppr exp2     

instance Ppr FCall where
 ppr (FCall name assocs) = ppr name <+> ppr assocs


-------------------
-- Helper Functions
-------------------

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

-- Only append if both of the documents are non-empty
(<++>) :: Doc -> Doc -> Doc
d1 <++> d2 
 | isEmpty d1 || isEmpty d2 = empty
 | otherwise = d1 <+> d2