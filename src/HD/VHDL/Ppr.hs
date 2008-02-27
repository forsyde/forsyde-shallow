{-# OPTIONS_GHC -fglasgow-exts #-}
-- The use of -fglasgow-exts is due to non-standard instances such as
-- "instance Ppr [IfaceSigDec]" which is not haskell98

-- FIXME:  we could avoid the -fglasgow-exts by adding ppr_list to the Ppr 
--         class (see Language.Haskell.TH.Ppr)

module HD.VHDL.Ppr where

import Text.PrettyPrint.HughesPJ hiding (Mode)
import HD.VHDL.AST

class Ppr a where
 ppr :: a -> Doc


nestVal = 5

instance Ppr a => Ppr (Maybe a) where
 ppr (Just a) = ppr a
 ppr Nothing  = empty 

instance (Ppr a, Ppr b) => Ppr (a,b) where
 ppr (a,b) = ppr a <+> ppr b

instance Ppr VHDLId where
 ppr = text.fromVHDLId
 

instance Ppr DesignFile where
 ppr (DesignFile cx lx) = (ppr_list ($+$) cx) $$ 
                          vBlock              $$
                          (ppr_list vSep lx)  $$
                          vSpace
  where vBlock = multiVSpace 2
        vSep doc1 doc2 = doc1 $+$ vBlock $+$ doc2 
        
instance Ppr ContextItem where
 ppr (Library id) = text "library" <+> ppr id <> semi
 ppr (Use name) = text "use" <+> text name <> semi
 

instance Ppr LibraryUnit where
 ppr (LUEntity entityDec) = ppr entityDec
 ppr (LUArch archBody)  = ppr archBody

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
    nest nestVal (ppr_list vSep conSms) $+$
  text "end architecture" <+> idDoc <> semi
  where vSep doc1 doc2 = doc1 $+$ vSpace $+$ doc2 
        idDoc = ppr id
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
   where indentL = length "function " + (length.fromVHDLId) name + 1
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
 ppr (SigDec id typemark) = 
   text "signal" <+> ppr id <+> colon <+> ppr typemark <> semi

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

instance Ppr ActualPart where
 ppr (ADName name) = ppr name
 ppr (ADExpr expr) = ppr expr
 ppr Open          = text "open"

instance Ppr ConSigAssignSm where
 ppr (target :<==: cWforms) = ppr target <+> text "<=" $$
                                nest indentL (ppr cWforms) <> semi
  where indentL = (length.fromVHDLId) target + length " <= "

instance Ppr ConWforms where
 ppr (ConWforms whenElses wform lastElse) = ppr_list ($+$) whenElses $+$
                                            ppr wform <+> ppr lastElse

instance Ppr WhenElse where
 ppr (WhenElse wform expr) = ppr wform <+> text "when" <+> 
                             ppr expr  <+> text "else"

instance Ppr When where
 ppr (When cond) = text "when" <+> ppr cond

instance Ppr Wform where
 ppr (Wform elems) = ppr_list (\e1 e2 -> e1 <> comma <+> e2) elems
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
 ppr (Nor  e1 e2) = parens (ppr e2) <+> text "nor"  <+> parens (ppr e2)
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
 ppr (PrimLit  lit)     = ppr lit
 ppr (PrimFCall fCall)  = ppr fCall
         

instance Ppr FCall where
 ppr (FCall name assocs) = ppr name <+> ppr assocs


instance Ppr Literal where
 ppr = text

-------------------
-- Helper Functions
-------------------

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

-- Join two documents vertically putting a semicolon in the middle
vSemi :: Doc -> Doc -> Doc
vSemi doc1 doc2 = doc1 <> semi $+$ doc2


-- Join two documents vertically putting a comma in the middle
vComma :: Doc -> Doc -> Doc
vComma doc1 doc2 = doc1 <> comma $+$ doc2
