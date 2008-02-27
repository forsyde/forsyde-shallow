{-# OPTIONS_GHC -fglasgow-exts #-}
-- The use of -fglasgow-exts is due to multiparameter instances

module HD.HDSynchronousLib where
import HD.Types
import HD.HDFun
import HD.HDSignal
import SynchronousLib
import Control.Monad (msum)



instance HDPrimType a => SynchronousD HDSignal a where
 delaySY c (HDSignal ps) = ((wrap t) (DelaySY pc ps)) 
  where t         = sTypeOfC c
        pc        = toPConstant c


instance (HDPrimType a, HDPrimType b)
  => SynchronousM (HDFun (a->b)) HDSignal a b where
 mapSY (HDFun pf@(HDPrimFun _ (HDFunType types) _)  _) 
       (HDSignal ps)  = ((wrap sTr) (MapSY pf ps)) 
  where [_,sTr] = map  HDSignalType $ types



instance (HDPrimType a, HDPrimType b, HDPrimType c) =>
   SynchronousZ (HDFun (a->b->c)) HDSignal a b c  where
 zipWithSY (HDFun pf@(HDPrimFun _ (HDFunType types) _) _) 
           (HDSignal ps1) 
           (HDSignal ps2) = 
    wrap sTr (ZipWithSY pf ps1 ps2)
   where [_,_,sTr] = map  HDSignalType $ types

