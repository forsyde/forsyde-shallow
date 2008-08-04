{-# LANGUAGE TemplateHaskell #-}
-- An identity system of six inputs formed by various zipping and unzipping
-- processes
module ZipTwist where

import ForSyDe
import Data.Param.FSVec
import Data.TypeLevel.Num.Reps
import Data.Int

zip6xSY :: ProcId -> FSVec D6 (Signal Int32) -> Signal (FSVec D6 Int32)
zip6xSY = zipxSY

zipTwistFun :: Signal Int32 -> Signal Int32 -> Signal Int32 
            -> Signal Int32 -> Signal Int32 -> Signal Int32
            -> (Signal Int32, Signal Int32, Signal Int32,
                Signal Int32, Signal Int32, Signal Int32)
zipTwistFun i1 i2 i3 i4 i5 i6 =  (o1,o2,o3,o4,o5,o6)
 where [i1',i2',i3',i4',i5',i6'] = (fromVector.unzipxSY "unzipxSY" . zip6xSY "zip6xsy")
                                   $ reallyUnsafeVector  [i1,i2,i3,i4,i5,i6]
       zip13 = zip3SY "zip13" i1' i2' i3'
       zip46 = zip3SY "zip46" i4' i5' i6'
       (o1,o2,o3) = unzip3SY "unzip13" zip13
       (o4,o5,o6) = unzip3SY "unzip46" zip46

zipTwistSys :: SysDef (Signal Int32 -> Signal Int32 -> Signal Int32 
            -> Signal Int32 -> Signal Int32 -> Signal Int32
            -> (Signal Int32, Signal Int32, Signal Int32,
                Signal Int32, Signal Int32, Signal Int32))
zipTwistSys = $(newSysDefTHName 'zipTwistFun 
                             ["in1","in2","in3","in4","in5","in6"]
                             ["out1","out2","out3","out4","out5","out6"])

simZipTwist = simulate zipTwistSys

writeVHDLZipTwist = writeVHDL zipTwistSys