\begin{code}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import IO
import ForSyDe
import AudioFilter
import AudioAnalyzer
import EqualizerTypes
import Equalizer

import Data.List (intersperse)

eqSysFun :: Signal (AbstExt Sensor)
         -> Signal (AbstExt Sensor)
         -> Signal (AbstExt Sensor)
         -> Signal (AbstExt Sensor)
         -> Signal Double
         -> Signal Double
eqSysFun = equalizer lpCoeffD bpCoeffD hpCoeffD n
 where n = 64
       
       lpCoeffD = vector [
         0.03898579822345,
         0.09739504968381,
         0.15360490491115,
         0.19416166962179,
         0.20893350067585,
         0.19416166962179,
         0.15360490491115,   
         0.09739504968381,
         0.03898579822345
        ]

       bpCoeffD = vector [
        -0.07845593083988,
         0.00000000000000,
        -0.30707118658796,
         0.00000000000000,
         0.58268794919522,
         0.00000000000000,
        -0.30707118658796,
         0.00000000000000,
        -0.07845593083988
        ]    

       hpCoeffD = vector [
         0.03898579822345,  
        -0.09739504968381,   
         0.15360490491115,
        -0.19416166962179,
         0.20893350067585,
        -0.19416166962179,
         0.15360490491115,
        -0.09739504968381,
         0.03898579822345
        ]


eqSysDef :: SysDef (Signal (AbstExt Sensor)
                    -> Signal (AbstExt Sensor)
                    -> Signal (AbstExt Sensor)
                    -> Signal (AbstExt Sensor)
                    -> Signal Double
                    -> Signal Double)
eqSysDef = $(newSysDef 'eqSysFun 
                       ["bassUp","bassDn", "terbleUp", "trebleDn", "audioIn"] 
                       ["audioOut"])


eqSim :: [AbstExt Sensor]
      -> [AbstExt Sensor]
      -> [AbstExt Sensor]
      -> [AbstExt Sensor]
      -> [Double]
      -> [Double]

eqSim = $(simulate 'eqSysDef)






main :: IO ()
main = do audioInFile <- openFile "Test/AudioIn.mat" ReadMode
	  audioOutFile <- openFile "Test/AudioOutFSD.ext" WriteMode
	  contents <- hGetContents audioInFile
          let audioIn :: [Double]
              audioIn = (map read . words) contents
              audioOut = eqSim bassUp1 bassDn1 trebleUp1 trebleDn1 audioIn
	  putStr "Simulating ... "
          hPutStr audioOutFile 
                  (concat ((intersperse "\n" . map show) audioOut))
	  putStr "Done.\n"
 where bassUp1 = replicate 512 Abst
       bassDn1 = replicate 512 Abst
       trebleUp1 = replicate 512 Abst
       trebleDn1 = replicate 512 Abst


\end{code}


