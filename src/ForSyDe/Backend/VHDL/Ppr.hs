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

module ForSyDe.Backend.VHDL.Ppr(Ppr(..)) where

import Text.PrettyPrint.HughesPJ hiding (Mode)
import ForSyDe.Backend.VHDL.AST

-- | Pretty printing class
class Ppr a where
 ppr :: a -> Doc

-- identity instantiation
instance Ppr Doc where
 ppr = id

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
 ppr (LUPackageDec packageDec) = ppr packageDec 
 ppr (LUPackageBody packageBody) = ppr packageBody

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
 ppr decs = text "port" <+> (parens (ppr_list vSemi decs) <> semi)


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
 ppr (PackageDec id decs) =
  text "package" <+> idDoc <+> text "is" $+$
   vSpace $++$
   nest nestVal (ppr_list (vNSpaces 1) decs) $++$
   vSpace $+$
  text "end package" <+> idDoc <> semi
  where idDoc = ppr id 

instance Ppr PackageDecItem where
 ppr (PDITD typeDec) = ppr typeDec
 ppr (PDISD subtypeDec) = ppr subtypeDec
 ppr (PDISS subProgSpec) = ppr subProgSpec <> semi

instance Ppr PackageBody where
 ppr (PackageBody id decs) =
  text "package body" <+> idDoc <+> text "is" $+$
   vSpace $++$
   nest nestVal (ppr_list (vNSpaces 1) decs) $++$
   vSpace $+$
  text "end package body" <+> idDoc <> semi
  where idDoc = ppr id 

instance Ppr SubtypeDec where
 ppr (SubtypeDec id si) = text "subtype" <+> ppr id <+> text "is" <+> ppr si <> semi

instance Ppr SubtypeIn where
 ppr (SubtypeIn tm mCons) = ppr tm <+> (ppr mCons)


instance Ppr Range  where
 ppr (AttribRange an) = ppr an
 ppr (ToRange exp1 exp2) = ppr exp1 <+> text "to" <+> ppr exp2


instance Ppr IndexConstraint where
 ppr (IndexConstraint dr) = parens (ppr_list hComma dr)
 

instance Ppr TypeDec where
 ppr (TypeDec id typeDef) = 
  hang (text "type" <+> ppr id <+> text "is") nestVal (ppr typeDef <> semi)

instance Ppr TypeDef where
 ppr (TDA arrayTD) = ppr arrayTD
 ppr (TDR recordTD) = ppr recordTD
 ppr (TDE enumTD) = ppr enumTD

instance Ppr ArrayTypeDef where
 ppr (UnconsArrayDef unconsIxs elemsTM) = 
   text "array" <+> parens (ppr_list hComma $ map pprUnconsRange unconsIxs) <+>
    text "of" <+> ppr elemsTM
  where pprUnconsRange tm = ppr tm <+> text "range <>"

 ppr (ConsArrayDef consIxs elemsTM) = 
  text "array" <+> ppr consIxs <+> text "of" <+> ppr elemsTM 

instance Ppr RecordTypeDef where
 ppr (RecordTypeDef elementDecs) =
  text "record" <+> (ppr_list ($+$) elementDecs) $+$ 
  text "end record"

instance Ppr ElementDec where
 ppr (ElementDec id tm) = ppr id <+> colon <+> ppr tm <> semi 

instance Ppr EnumTypeDef where
 ppr (EnumTypeDef ids) = lparen <> ppr_list hComma ids <> rparen

instance Ppr VHDLName where
 ppr (NSimple simple) = ppr simple
 ppr (NSelected selected) = ppr selected
 ppr (NIndexed indexed) = ppr indexed
 ppr (NSlice slice) = ppr slice 
 ppr (NAttribute attrib) = ppr attrib

instance Ppr SelectedName where
 ppr (prefix :.: suffix) = ppr prefix <> dot <> ppr suffix

instance Ppr Suffix where
 ppr (SSimple simpleName) = ppr simpleName
 ppr All = text "all"

instance Ppr IndexedName where
 ppr (IndexedName prefix indexList) = 
  ppr prefix <> parens ( ppr_list hComma indexList )

instance Ppr SliceName where
 ppr (SliceName prefix discreteRange) = ppr prefix <> parens (ppr discreteRange)

instance Ppr AttribName where
 ppr (AttribName prefix simpleName mExpr) = 
   ppr prefix <> char '\'' <> ppr simpleName <> parensNonEmpty (ppr mExpr)

instance Ppr [BlockDecItem] where
 ppr = ppr_list ($+$)

instance Ppr BlockDecItem where
 ppr (BDISPB subProgBody) = ppr subProgBody
 ppr (BDISD  sigDec)      = ppr sigDec


instance Ppr SubProgBody where
 ppr (SubProgBody subProgSpec decItems statements) = 
  ppr subProgSpec <+> text "is" $+$
    nest nestVal (ppr_list ($+$) decItems) $+$
  text "begin" $+$
    nest nestVal  (ppr_list ($+$) statements) $+$
-- TODO, show the id here
  text "end" <> semi


instance Ppr SubProgDecItem where
 ppr (SPVD vd) = ppr vd
 ppr (SPSB sb) = ppr sb

instance Ppr VarDec where
 ppr (VarDec id st mExpr) = 
    text "variable" <+> ppr id <+> colon <+> ppr st <+>
                        (text ":=" <++>  ppr mExpr) <> semi

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
    text "function"  <+> ppr name  <+> (parensNonEmpty (ppr_decs decList) $+$
                                        text "return" <+> ppr returnType)
   where ppr_decs ds = ppr_list vSemi ds

         

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
 ppr (ForSM id dr sms) = 
   text "for" <+> ppr id <+> text "in" <+> ppr dr <+> text "loop" $+$
     nest nestVal (ppr sms) $+$
   text "end loop" <> semi
 ppr (target := orig) = ppr target <+> text ":=" <+> ppr orig <> semi
 ppr (WaitFor expr) = text "wait for" <+> ppr expr <> semi
 ppr (targ `SigAssign` orig) = ppr targ <+> text "<=" <+> ppr orig <> semi
 ppr (ProcCall name assocs) = 
   ppr name <> parensNonEmpty (commaSep assocs) <> semi

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
 ppr (CSPSm procSm) = ppr procSm

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
 ppr (PMapAspect assocs) = text "port map" $$ 
  (nest identL $ parensNonEmpty (ppr_list vComma assocs))
  where identL = length "port map "
         

instance Ppr AssocElem where
 ppr (formalPart :=>: actualPart) = formalPartDoc <+> ppr actualPart
  where formalPartDoc = maybe empty (\fp -> ppr fp <+> text "=>") formalPart

instance Ppr ActualDesig where
 ppr (ADName name) = ppr name
 ppr (ADExpr expr) = ppr expr
 ppr Open          = text "open"

instance Ppr ConSigAssignSm where
 ppr (target :<==: cWforms) = ppr target <+> text "<=" <+> (ppr cWforms <> semi)


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

instance Ppr WformElem where
 ppr (WformElem exp mAfter) = ppr exp <+> (text "after" <++> ppr mAfter)


instance Ppr CompInsSm where
 ppr (CompInsSm label insUnit assocElems) =
   ppr label <+> colon <+> (ppr insUnit $+$
                            nest nestVal (ppr assocElems) <> semi)

instance Ppr ProcSm where
 ppr (ProcSm label sensl seqSms) =
   ppr label <+> colon <+> text "process" <+> 
   parensNonEmpty (ppr_list hComma sensl) $+$
   text "begin" $+$
        nest nestVal (ppr seqSms) $+$
   text "end process" <+> labelDoc <> semi
  where labelDoc = ppr label

instance Ppr InsUnit where
 ppr (IUEntity name) = text "entity" <+> ppr name

-- FIXME, remove parenthesis according to precedence
instance Ppr Expr where
 ppr = pprExprPrec 0 ""


-- | Prettyprint an binary infix operator 
pprExprPrecInfix :: Int -- ^ Accumulated precedence value (initialized to 0)
                 -> String -- ^ Enclosing operator (initialized to \"\")
                 -> Int -- ^ Precedence of current infix operator   
                 -> Expr -- ^ lhs expression
                 -> String -- ^ operator name
                 -> Expr -- ^ rhs expression
                 -> Doc
-- To avoid priting parenthesis based on the left
-- associativity of all operators within the same precedence class, 
-- the precedence passed to the left branch is curr instead of
-- curr+1.
--
-- However, the VHDL grammar sometimes doesn't allow it 
-- e.g. "and a or b and d" is illegal. In fact,
-- you can only put two operators together without parenthesis
-- in some cases:
-- 
-- * logical operators: "and", "or", "xor", "xnor", but cannot be mixed up.
-- * relational operators: none
-- * shift operators: none
-- * adding operators: all, can be mixed up
-- * multiplying operator: all, can be mixed up
-- * misc operator: none
--
-- Thus, we keep track of the enclosing
-- operator, and skip parenthesis according to left parsing associativity
-- in the cases stated above.
--
-- Note that we are only making use of syntactic associativity. e.g.
-- even if "+" is semantially associative: a+(b+c)=(a+b)+c,
-- we will only skip parenthesis in (a+b)+c

pprExprPrecInfix ac encop curr lhs op rhs = 
 parensIf (ac>curr || (ac == curr && not skipParenSameLevel)) $
   sep [pprExprPrec curr op lhs <+> text op, pprExprPrec (curr+1) op rhs]
  where skipParenSameLevel = curr == plusPrec || curr == multPrec ||
            (encop == op && op `elem` ["and","or","xor","xnor"])

-- | Prettyprint unary prefix operators
pprExprPrecPrefix :: Int -- ^ Accumulated precedence value (initialized to 0)
                  -> Int -- ^ Precedence of current infix operator
                  -> String -- ^ operator name
                  -> Expr -- ^ operator argument
                  -> Doc
pprExprPrecPrefix ac curr op arg = parensIf (ac>curr) $
  text op <+> pprExprPrec (curr+1) op arg


-- | Prints an expression taking precedence and left associativity
--   in account
pprExprPrec :: Int  -- ^ Accumulated precedence value (initialized to 0)
            -> String -- ^ Enclosing operator
            -> Expr -- ^ Expression curently prettyprinted 
            -> Doc
-- Logical operations
pprExprPrec p e (And e1 e2)  = pprExprPrecInfix p e logicalPrec e1 "and"  e2
pprExprPrec p e (Or  e1 e2)  = pprExprPrecInfix p e logicalPrec e1 "or"   e2
pprExprPrec p e (Xor e1 e2)  = pprExprPrecInfix p e logicalPrec e1 "xor"  e2
pprExprPrec p e (Nand e1 e2) = pprExprPrecInfix p e logicalPrec e1 "nand" e2
pprExprPrec p e (Nor  e1 e2) = pprExprPrecInfix p e logicalPrec e1 "nor"  e2
pprExprPrec p e (Xnor e1 e2) = pprExprPrecInfix p e logicalPrec e1 "xnor" e2
-- Relational Operators
pprExprPrec p e (e1 :=:  e2) = pprExprPrecInfix p e relationalPrec e1 "="  e2
pprExprPrec p e (e1 :/=: e2) = pprExprPrecInfix p e relationalPrec e1 "/=" e2
pprExprPrec p e (e1 :<:  e2) = pprExprPrecInfix p e relationalPrec e1 "<"  e2
pprExprPrec p e (e1 :<=: e2) = pprExprPrecInfix p e relationalPrec e1 "<=" e2
pprExprPrec p e (e1 :>:  e2) = pprExprPrecInfix p e relationalPrec e1 ">"  e2
pprExprPrec p e (e1 :>=: e2) = pprExprPrecInfix p e relationalPrec e1 ">=" e2
-- Shift Operators
pprExprPrec p e (Sll e1 e2) = pprExprPrecInfix p e shiftPrec e1 "sll" e2
pprExprPrec p e (Srl e1 e2) = pprExprPrecInfix p e shiftPrec e1 "srl" e2
pprExprPrec p e (Sla e1 e2) = pprExprPrecInfix p e shiftPrec e1 "sla" e2
pprExprPrec p e (Sra e1 e2) = pprExprPrecInfix p e shiftPrec e1 "sra" e2
pprExprPrec p e (Rol e1 e2) = pprExprPrecInfix p e shiftPrec e1 "rol" e2
pprExprPrec p e (Ror e1 e2) = pprExprPrecInfix p e shiftPrec e1 "ror" e2
-- Adding Operators
pprExprPrec p e (e1 :+: e2) = pprExprPrecInfix p e plusPrec e1 "+" e2
pprExprPrec p e (e1 :-: e2) = pprExprPrecInfix p e plusPrec e1 "-" e2
pprExprPrec p e (e1 :&: e2) = pprExprPrecInfix p e plusPrec e1 "&" e2
-- Sign Operators
pprExprPrec p _ (Neg e) = pprExprPrecPrefix p signPrec "-" e 
pprExprPrec p _ (Pos e) = pprExprPrecPrefix p signPrec "+" e 
-- Multiplying Operators
pprExprPrec p e (e1 :*: e2) = pprExprPrecInfix p e multPrec e1 "*" e2
pprExprPrec p e (e1 :/: e2) = pprExprPrecInfix p e multPrec e1 "/" e2
pprExprPrec p e (Mod e1 e2) = pprExprPrecInfix p e multPrec e1 "mod" e2
pprExprPrec p e (Rem e1 e2) = pprExprPrecInfix p e multPrec e1 "rem" e2
-- Miscellaneous Operators
pprExprPrec p e (e1 :**: e2) = pprExprPrecInfix p e miscPrec e1 "**" e2
pprExprPrec p _ (Abs e) = pprExprPrecPrefix p signPrec "abs" e 
pprExprPrec p _ (Not e) = pprExprPrecPrefix p signPrec "not" e 
-- Primary expressions
pprExprPrec _ _ (PrimName name)    = ppr name
pprExprPrec _ _ (PrimLit  lit)     = text lit
pprExprPrec _ _ (PrimFCall fCall)  = ppr fCall
-- Composite-type  expressions
pprExprPrec _ _ (Aggregate assocs) = parens (ppr_list hComma  assocs)

instance Ppr ElemAssoc where
 ppr (ElemAssoc mChoice expr) = (ppr mChoice <++> text "=>") <+> ppr expr

instance Ppr FCall where
 ppr (FCall name assocs) = 
   ppr name <> parensNonEmpty (commaSep assocs)


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