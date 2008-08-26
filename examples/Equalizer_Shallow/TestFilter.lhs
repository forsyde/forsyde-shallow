\begin{code}
module Main where

import IO
import ForSyDe.Shallow
--import FixedPoint
import AudioFilter

zeros = infiniteS id 0.0

s = signal [0.1,0.05,0.3,0.2,-0.5,0.2,0.1,0.3,0.1,-0.1,0.0,0.2]

fs = 200
t = takeS 400 (infiniteS (+1/200) 0.0)
 
cos10 x = cos (2*pi*10*x)
cos50 x = cos (2*pi*50*x)
cos90 x = cos (2*pi*90*x)

modCos x = cos10 x + cos50 x + cos90 x

audioIn = mapSY modCos t

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

--lpCoeffF8 = mapV real2Fixed8 lpCoeffD
--bpCoeffF8 = mapV real2Fixed8 bpCoeffD
--hpCoeffF8 = mapV real2Fixed8 hpCoeffD
--sF8	  = mapSY real2Fixed8 s

--lpCoeffF16 = mapV real2Fixed16 lpCoeffD
--bpCoeffF16 = mapV real2Fixed16 bpCoeffD
--hpCoeffF16 = mapV real2Fixed16 hpCoeffD
--sF16	  = mapSY real2Fixed16 s

audioFilterD = audioFilter lpCoeffD bpCoeffD hpCoeffD zeros zeros
outDouble = audioFilter lpCoeffD bpCoeffD hpCoeffD zeros zeros s
--outF16	  = audioFilter lpCoeffF16 bpCoeffF16 hpCoeffF16 (mapSY real2Fixed16 zeros) (mapSY real2Fixed16 zeros) sF16
--outF8	  = audioFilter lpCoeffF8 bpCoeffF8 hpCoeffF8 (mapSY real2Fixed8 zeros)(mapSY real2Fixed8 zeros) sF8

--writeAudioOut = writeFile "Test/AudioOut.for") . writeS 
readAudioIn = readFile "Test/AudioIn.mat"

testSeries = do contents <- readAudioIn
		writeFile "Test/AudioOutFSD.ext" (writeS (audioFilterD (readS contents)))
--		writeAudioIn

\end{code}