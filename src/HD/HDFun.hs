{-# OPTIONS_GHC -fglasgow-exts -fth -fno-warn-deprecations #-}
-- The use of -fglasgow-exts is due to I# and Int#
-- The use of -fth is due to a massive use of Template Haskell
-- The use of -fth -fno-warn-deprecations is due to a mandatory use of
-- PackedStrings

-- TODO: include export list
module HD.HDFun (module HD.HDFun, module Data.Bits) where
import HD.Types

import Data.Bits
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Data.PackedString (PackedString, packString, unpackPS)
import GHC.Exts (Int (I#),Int# )

---------
-- HDFun
---------

-- We keep: 
--  * The function's name as a String
--  * Its type (in our internal representation)
--  * The function's AST  in order to translate it to the target HDL. 
--    Only one clause is admited (there is no pattern matching support)
--  TODO: whe should keep the function itself to simulate 
--        the circuit afterwards

data HDPrimFun = HDPrimFun String HDFunType Clause 

-- Type-safety layer shown to the end user, we include the
-- function itself to make sure Template Haskell does the type checking
data HDFun a = HDFun HDPrimFun a


-----------------------------------------------------------------------------
-- Generate the AST for HDFuns which later will be translated
-- to a function in a target HDL 

-- HDFuns  must be declared using a subset of Haskell. For that reason, to 
-- represent their AST we reuse a subset of the types of 
-- Language.Haskell.TH.Syntax taking care to throw an error if the user made 
-- use of a feature out of the supported Haskell subset.

-- Furthermore, the AST is generated within the mkHDFun splice for later 
-- translation to the HDL and thus, it must be wrapped into a 
-- Language.Haskell.TH.Syntax.Exp (a metaAST).
------------------------------------------------------------------------------


-- Helper constructor function
-- TODO: document it properly
-- FIXME: allow the user to avoid suplying the function signature 
mkHDFun :: Q [Dec] -> ExpQ
mkHDFun qd = do (fName, clause, fType) <- checkFunD qd
                let strFName = nameBase fName
                [| let fItself    = $(letEmbed qd fName)
                       hdPrimFun  = HDPrimFun strFName fType clause
                   in  HDFun hdPrimFun fItself |]
  where  checkFunD :: Q [Dec] -> Q (Name, Clause, HDFunType)
         checkFunD qdx = qdx >>= doCheckFunD
         doCheckFunD [SigD nm1 t, FunD nm2 [clause@(Clause pats _ _)]] 
           | nm1 == nm2 && length pats == (length types)- 1 && all varp pats = 
             return (nm1, clause, ftype)
            where ftype@(HDFunType types) = type2HDFunType t
                  varp (VarP _) = True
                  varp _        = False
         doCheckFunD _  = 
          die $ "Only one function signature and declaration " ++
                "(with unique clause and explicit parameter names) supported"
         letEmbed :: Q [Dec] -> Name -> ExpQ
         -- Embeds a declaration block inside a let expression
         -- inserting a variable with the provided name in the "in" section
         letEmbed qd nm = qd >>= (\qx -> letE  (map return qx)(varE nm))

mkMetaAST :: Lift a => a -> ExpQ
mkMetaAST a = do ast <- [| a |] -- get the AST of the expresetion
                 [| ast |]      -- get the AST of the AST: metaAST

------------------------------
-- Lift Instance for HDFunType
------------------------------

instance Lift HDType where
 lift Bool = [|Bool|]
 lift Int  = [|Int |]

instance Lift HDFunType where
 lift (HDFunType ts) = [|HDFunType ts|]

---------------------------------------------------------------
-- Lift instances for Language.Haskell.TH.Syntax.(Exp,Type,Dec) 
---------------------------------------------------------------
-- This allows us to save and splice a TH AST (in a metaAST) 
-- for later processing


-- TODO: improve user errors, recode die

-----------------------------
-- Lift instances (meta AST)
-----------------------------

------------------------
-- Lift instance of Name
------------------------

-- OccName and ModName are packed strings
instance Lift PackedString where
 lift pstr = appE ([| packString |]) 
                  (litE.stringL $ unpacked) 
  where unpacked = unpackPS pstr

instance Lift NameSpace where
  lift VarName   = [| VarName   |]
  lift DataName  = [| DataName  |]
  lift TcClsName = [| TcClsName |]

{-- you cannot pass Int#'s to polymorphic functions! (it's an unboxed type)
 instance Lift Int# where
  lift i = [| i' |]
   where i' = I# i
--}

liftInt# :: Int# -> ExpQ
liftInt# i = litE.intPrimL.fromIntegral $ boxedI
  where boxedI = I# i

instance Lift NameFlavour where
  lift NameS                   = [| NameS                 |]
  lift (NameQ mnm)             = [| NameQ mnm             |]
  lift (NameU i)               = [| NameU $(liftInt# i)   |]
  lift (NameL i)               = [| NameL $(liftInt# i)   |]
  lift (NameG nmspc pkgnm mnm) = [| NameG nmspc pkgnm mnm |] 
     where  

instance Lift Name where
 lift (Name occnm nmfv) = [| Name occnm nmfv |]

------------------------
-- Lift instance of  Lit
------------------------

instance Lift Lit where
 lift (IntegerL il)    = [| IntegerL il    |]
 lift (IntPrimL  il)   = [| IntPrimL  il   |] 
 lift l                = die "Literal not supported" 

-------------------------
-- Lift instance of Match
------------------------- 

instance Lift Match where
 -- case e of { pat -> body where decs } 
 lift (Match pat@(LitP _) body []) = 
   case pat of
     (LitP _) -> lifted
     WildP    -> lifted
     _        -> die "Invalid pattern inside case"  
     where lifted = [| Match pat body |]
 lift _ = die "Where declarations not supported"
   
----------------------- 
-- Lift instance of Pat
-----------------------

instance Lift Pat where
 -- 'a'
 lift (LitP lit) = [| LitP lit  |]
 -- x 
 lift (VarP nm)  = [| VarP nm  |]
 -- _
 lift WildP      = [| WildP |]
 lift _          = die "Invalid pattern matching expression"


------------------------
-- Lift instance of Body
------------------------

instance Lift Body where
 lift (NormalB e)  = [| NormalB e |]
 lift (GuardedB _) = die "Guards are not supported"

------------------------
-- Lift instance of Exp
------------------------

instance Lift Exp where 
 -- Variable Expression
 lift (VarE nm) = [| VarE nm |] 
 -- Constructor expression
 lift (ConE nm)  = [| ConE nm |]
 -- Literals
 lift (LitE lit) = [| LitE lit |]
 -- Function application
 lift (AppE e1 e2) = [|AppE e1 e2 |]
 -- Infix expression
 lift (InfixE l@(Just el) e@(VarE _) r@(Just er))= [| InfixE l e r |]
 lift (InfixE _ _ _)       = 
    die "Sections or infix constructors not supported"
 -- Lambda expressions
 lift (LamE _  _) = die "Lambda expressions not supported"
 -- Tuples
 lift (TupE es)   = die "Tuples not supported" 
 -- If then else
 lift (CondE e1 e2 e3) = [| CondE e1 e2 e3 |]
 -- Let expression
 lift (LetE _ _) = die "Let expression not supported"
 -- Case expression
 lift (CaseE e ms) = [| CaseE e ms |]
 -- Do expression                     
 lift (DoE _) = error "Do expressions not supported"
 -- Comprehension lists
 lift (CompE _) = error "Comprehension lists not supported" 
 -- Arithmetic sequences ([ 1 ,2 .. 10 ]) 
 lift (ArithSeqE _) = error "Arithmetic sequences not supported"
 -- Lists
 lift (ListE _) = error "Lists not supported"
 -- Type signatures (e :: t)
 lift (SigE _  _) = die "Expression signatures not supported"
 -- Record Constructions
 lift (RecConE _ _) = die "Records not supported"
 -- Record update
 lift (RecUpdE _ _) = die "Records not supported"


--------------------------
-- Lift instance of Clause
--------------------------

instance Lift Clause where
 lift (Clause ps bd ds) = if null ds then [| Clause ps bd ds |]
                          else die "Where declarations are not supported"

-----------------------
-- Lift instance of Dec
-----------------------


instance Lift Dec where
 -- Function declaration
 -- {f p1 p2 = b where decs}
 -- we only admit one clause per function 
 -- (we don't support pattern matching so it would be pointless to do it)
 lift (FunD nm [c]) = [| FunD nm [c] |]
 lift (FunD _  xs) = error "Only one clause per function is supported"
 lift _ = error "Only function declartions are supported"


------------------------
-- Lift instance of Type
------------------------

instance Lift Type where
 -- forall <vars>. <ctxt> -> <type>
 lift (ForallT _ _ _) = die "Forall types not supported"
 -- a
 lift (VarT nm)       = [| VarT nm |]
 -- T
 lift (ConT nm)       = [| ConT nm |]
 -- (,), (,,), etc.
 lift (TupleT n)      = die "Tuples not supported"
 -- -> 
 lift  ArrowT         = [| ArrowT |]
 -- [ ]
 lift ListT           = die "Lists not supported"
 -- T a b
 lift  (AppT t1 t2)   = [| AppT t1 t2 |]


-------------------
-- Helper functions
-------------------

-- FIXME: avoid using die, use only report, 
-- see http://www.haskell.org/bz/thdoc.htm

die :: String -> Q a
die str = report True str >> fail undefined


type2HDFunType :: Type  -> HDFunType
-- Get the "unarrowed" type of a primary function
type2HDFunType = HDFunType. doIt
 where doIt ((ArrowT `AppT` t1 ) `AppT` t2) = (type2HDType t1) : doIt t2
       doIt t                               = [type2HDType t] 


type2HDType :: Type -> HDType
type2HDType t = case t of
                 ConT name ->
                      case nameBase name of
                         "Int"  -> Int 
                         "Bool" -> Bool
                         _      -> error err
                 _         -> error err
 
 where err = "Internal error: unexpected Template Haskell Type"