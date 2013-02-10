\begin{code}
module TestFFT where

import ForSyDe.Shallow
import Data.Complex

toComplex a = a:+0

testDFT = dft 8 v3
testFFT = fft 8 v3

testBoth = zipWithV (-) testDFT testFFT

v1 = mapV toComplex (vector [1, 2])
v2 = mapV toComplex (vector [1, 2,3,4])
v3 = mapV toComplex (vector [1, 2, 3, 4, 5, 6, 7, 8])
\end{code}
