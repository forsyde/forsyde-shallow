-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.TestBench
-- Copyright   :  (c) The ForSyDe Team 2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions used to generate a VHDL test bech.
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.TestBench (writeVHDLTestBench) where


import ForSyDe.Backend.VHDL.Constants
import ForSyDe.Backend.VHDL.AST
import ForSyDe.Backend.VHDL.Translate
import ForSyDe.Backend.VHDL.Traverse.VHDLM
import ForSyDe.Backend.VHDL.Generate
import ForSyDe.Backend.VHDL.FileIO

import ForSyDe.Ids
import ForSyDe.System.SysDef

import qualified Language.Haskell.TH as TH (Exp)
import Control.Monad.State
import Data.List (transpose)
import System.Directory
import System.FilePath

-- | write a test bench, using a clock cycle of 10 ns
writeVHDLTestBench :: Maybe Int -- ^ Number of cycles to simulate
                                --   if 'Nothing' the number will be determined
                                --   by the length of the input stimulti.
                                --   Useful when the system to simulate doesn't
                                --   have inputs or the inputs provided are 
                                --   infinite
                -> [[TH.Exp]] -- ^ Input stimuli (one list per signal)
                -> VHDLM Int -- ^ Number of cycles simulated
writeVHDLTestBench mCycles stimuli = do
 sys <- gets (globalSysDef.global)
 let sysId = sid sys
     cxt = genVHDLTestBenchContext sysId
     ent = genVHDLTestBenchEntity sysId
 (arch, cycles) <- genVHDLTestBenchArch mCycles stimuli
 let design = DesignFile cxt [LUEntity ent, LUArch arch]
     tbpath = "test" </> (sysId ++ "_tb.vhdl")
 liftIO $ createDirectoryIfMissing True "test" 
 liftIO $ writeDesignFile design tbpath
 return cycles

-- | Generate the Context Clause
genVHDLTestBenchContext :: SysId -- ^ Main system Id
                        -> [ContextItem]
genVHDLTestBenchContext id = commonContextClause ++
  [Library libId,
   Use     $ NSelected (NSimple libId :.: SSimple typesId)  :.: All,
   Use     $ NSelected (NSimple stdId :.: SSimple textioId) :.: All]
 where libId = unsafeVHDLBasicId (id ++ "_lib")
  
-- | Generates an empty entity fot the testbench
genVHDLTestBenchEntity :: SysId -- ^ Main system Id 
                       -> EntityDec
genVHDLTestBenchEntity id = EntityDec (unsafeVHDLBasicId (id ++ "_tb")) []

--------------------------
-- Test Bench Architecture
--------------------------


-- | generate the architecture
genVHDLTestBenchArch :: Maybe Int -- ^ Maximum number of cycles
                     -> [[TH.Exp]] -- ^ Input stimuli 
                     -> VHDLM (ArchBody, Int) -- ^ Number of cycles simulated
genVHDLTestBenchArch mCycles stimuli = do
 sys <- gets (globalSysDef.global)
 let sysId    = sid sys
     iface    = iIface sys
     oface    = oIface sys
     l        = logic sys
     iIds     = map fst iface
     iVHDLIds = map unsafeVHDLExtId iIds
     oIds     = map fst oface
 -- Get the signal declarations for the input signals
 iDecs <- mapM 
     (\(pId, t) -> transVHDLName2SigDec (unsafeVHDLExtId pId) t Nothing) iface
 let finalIDecs = iDecs ++ 
                  [SigDec clockId std_logicTM (Just $ PrimLit "'0'"),
                   SigDec resetId std_logicTM (Just $ PrimLit "'0'")]
 -- Get the component instantiation and the signal declarations for the output
 -- signals
 (ins, outDecs) <- 
     transSysIns2CompIns l
                         (unsafeVHDLBasicId "totest") 
                         iVHDLIds 
                         (map (\(id, t) -> (unsafeVHDLExtId id,t)) oface)
                         sysId
                         iIds
                         oIds
 -- Generate the signal assignments
 (stimuliAssigns, cycles) <- genStimuliAssigns mCycles stimuli iVHDLIds
 -- Add an assignment to turn off the reset signal after 10 ns
 let finalAssigns = 
      (NSimple resetId :<==: 
        ConWforms [] 
                  (Wform [WformElem (PrimLit "'1'") (Just $ PrimLit "10 ns")])
                  Nothing) : stimuliAssigns
 -- Get the two processes (clock and output)
     clkProc = genClkProc
     outputProc = genOutputProc (map unsafeVHDLExtId oIds)
 -- return the architecture
 return $ (ArchBody 
             (unsafeVHDLBasicId "test") 
             (NSimple $ unsafeVHDLBasicId (sysId ++ "_tb"))
             (map BDISD (finalIDecs ++ outDecs))
             ( (CSPSm clkProc) : (CSPSm clkProc) :(map CSSASm finalAssigns) ),
           cycles)

-- | generate the assignments for the input stimuli
genStimuliAssigns :: Maybe Int -- ^ Maximum number of cycles
                  -> [[TH.Exp]] -- ^ Input stimuli 
                  -> [VHDLId] -- ^ Input signal ids
                  -> VHDLM ([ConSigAssignSm], Int) -- ^ (Assignments,
                                                   --    number of cycles 
                                                   --    simulated)
genStimuliAssigns mCycles stimuli signals = do
  let genWformElem time thExp  = 
         do vExp <- transExp2VHDL thExp
            return (WformElem vExp (Just $ PrimLit (show time ++ " ns")))
  wformElems <- mapM (zipWithM  genWformElem [0,10..]) stimuli
  let (normWformElems, cycles) = normalize maxCycles wformElems
  return $ (zipWith 
                (\s elems -> NSimple s :<==: ConWforms [] (Wform elems) Nothing)
                signals 
                normWformElems,
            cycles)
 where maxCycles = maybe 0 id mCycles 
       -- FIXME: this is not efficient at all
       -- Normalize a matrix. Make sure that all the rows in a matrix have the
       -- same length, setting a maximum row length (0 establishes no limit)
       normalize :: Int     -- ^ maximum row-length to process 
                 -> [[a]]   -- ^ input matrix
                 -> ([[a]], Int) -- ^ (output matrix, maximum row length)
       normalize max xss
         | any null xss = (take l (repeat []), 0)
         | otherwise = let (transres, acum) = normalize' max (transpose xss)
      	      	       in (transpose transres, acum)
        where l = length xss
              normalize' max (xs:xss)
                 | length xs == l && max > 0 =
                     let (nextlist, nextacum) = normalize' (max-1)  xss
                     in (xs : nextlist, nextacum+1)
              normalize' _ _ = ([], 0)

-- | generates a clock process with a period of 10ns
genClkProc :: ProcSm
genClkProc = ProcSm (unsafeVHDLBasicId "clkproc") [] sms
 where sms = -- wait for 5 ns -- (half a cycle)
             [WaitFor $ PrimLit "5 ns",
              -- clk <= not clk;
              NSimple clockId `SigAssign` 
                 Wform [WformElem (Not (PrimName $ NSimple clockId)) Nothing]]

-- | generate the output process
genOutputProc :: [VHDLId] -- ^ output signals
              -> ProcSm  
genOutputProc outs = 
  ProcSm (unsafeVHDLBasicId "writeoutput") 
         [clockId]
         [IfSm clkPred (writeOuts outs) [] Nothing]
 where clkPred = PrimName (NAttribute $ AttribName (NSimple clockId) 
                                                   eventId
                                                   Nothing          ) `And` 
                 (PrimName (NSimple clockId) :=: PrimLit "'1'")
       writeOuts []  = []
       writeOuts [i] = [writeOut i (PrimLit "LF")]
       writeOuts (i:is) = writeOut i (PrimLit "','") : writeOuts is
       writeOut outSig suffix = 
         genExprProcCall2 writeId 
                          (PrimName $ NSimple outputId)
                          (genExprFCall1 showId (PrimName $ NSimple outSig) :&: 
                           suffix) 
