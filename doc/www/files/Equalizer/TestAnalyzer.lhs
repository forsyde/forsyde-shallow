\begin{code}
module Main(main) where

import IO
import ForSyDe.Shallow
import AudioFilter
import AudioAnalyzer

input = 0.1 :- 0.2 :- input
zeros = (0.0, 0.0) :- zeros


k = 8
n = 2 ^ k 
sig = takeS (2*(2^k)) input
pts = 4

--testFilter = audioFilter zeros input

main = do fftInfile <- openFile "Test/audioOut.dat" ReadMode
	  fftOutfile <- openFile "Test/fftOut.dat" WriteMode
	  contents <- hGetContents fftInfile
--	  hPutStr fftOutfile (writeS (audioAnalyzer 6 ((readS contents2) :: Signal Double)))
	  putStr (show (audioAnalyzer pts ((readS contents) :: Signal Double)))
	  hPutStr fftOutfile (writeS (audioAnalyzer pts ((readS contents) :: Signal Double)))
--	  hClose fftOutfile
	  putStr "\nDone.\n"
\end{code}


