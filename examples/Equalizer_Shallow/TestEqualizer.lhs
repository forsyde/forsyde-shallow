\begin{code}
module TestEqualizer where

import System.IO
import Equalizer
import EqualizerTypes
import ForSyDe.Shallow
import AudioFilter
import AudioAnalyzer

audioIn = takeS (pts * 4) $ infiniteS (id) 1.0

bassUp = signal [Prst Active, Prst Active, Abst, Abst,
		 Prst Active, Prst Active, Abst, Abst,
		 Prst Active, Prst Active, Abst, Abst,
		 Prst Active, Prst Active, Abst, Abst]

bassDn = signal [Abst, Abst, Prst Active, Abst,
		 Abst, Abst, Prst Active, Abst,
		 Abst, Abst, Prst Active, Abst,
		 Abst, Abst, Prst Active, Abst]

trebleUp = signal [Abst, Abst, Abst, Abst,
		 Abst, Abst, Abst, Prst Active,
		 Abst, Abst, Abst, Abst,
		 Abst, Abst, Abst, Prst Active]

trebleDn = signal [Abst, Abst, Abst, Prst Active,
		 Abst, Abst, Abst, Prst Active,
		 Abst, Abst, Abst,  Prst Active,
		 Abst, Abst, Abst, Prst Active]

overrides = signal [Abst, Abst, Abst, Abst,
		    Prst Lock, Abst, Abst, Abst,
		    Abst, Prst CutBass, Abst, Abst,
		    Prst Release,  Abst, Abst, Abst]
bass = infiniteS id 0.0
treble = infiniteS id 0.0
dataflow = audioAnalyzer 2 (audioFilter lpCoeff bpCoeff hpCoeff bass treble audioIn)

testEqualizer = equalizer lpCoeff bpCoeff hpCoeff 2 bassUp bassDn trebleUp trebleDn audioIn
--testButtonInterface = buttonInterface  bassUp bassDn trebleUp trebleDn 
\end{code}

\begin{code}
lpCoeff = vector
     [ 0.01392741661548, 0.01396895728902,
       0.01399870011280, 0.01401657422649,
       0.01402253700635, 0.01401657422649,
       0.01399870011280, 0.01396895728902,
       0.01392741661548 ]

bpCoeff = vector
     [ 0.06318761339784, 0.08131651217682,
       0.09562326700432, 0.10478344432968,
       0.10793629404886, 0.10478344432968,
       0.09562326700432, 0.08131651217682,
       0.06318761339784 ]

hpCoeff = vector
     [ -0.07883878579454, -0.09820015927379,
       -0.11354603917221, -0.12339860164118,
        0.87320570334018, -0.12339860164118,
       -0.11354603917221, -0.09820015927379,
       -0.07883878579454 ]

zeros = infiniteS id 0.0

audioFilterD = audioFilter lpCoeff bpCoeff hpCoeff zeros zeros
pts = 4

main = do 
          -- Test AudioFilter
          putStr "\n-->Test AudioFilter \n"
          filterInfile <- openFile "Test/AudioIn.mat" ReadMode
          filterContents <- hGetContents filterInfile 
          putStr (show (audioFilterD (readS filterContents)))
          writeFile "Test/audioOut.dat" (writeS (audioFilterD (readS filterContents)))        
          -- Test AudioAnalyzer
          putStr "\n--> Test AudioAnalyzer \n"
          analyzerInfile <- openFile "Test/audioOut.dat" ReadMode
	  analyzerOutfile <- openFile "Test/analyzerOut.dat" WriteMode
	  analyzerContents <- hGetContents analyzerInfile
          putStr (show (audioAnalyzer pts ((readS analyzerContents) :: Signal Double)))
	  hPutStr analyzerOutfile (writeS (audioAnalyzer pts ((readS analyzerContents) :: Signal Double)))
          
	  --fftInfile <- openFile "audioOut.txt" ReadMode
	  --fftOutfile <- openFile "fftOut.txt" WriteMode
	  --contents <- hGetContents fftInfile
	  --putStr (writeS (((readS contents) :: Signal Double)))
	  --hPutStr fftOutfile (writeS (((readS contents) :: Signal Double)))
	  putStr "\nDone.\n"
\end{code}

