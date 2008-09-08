\begin{code}
module TestFIR(main) where

import ForSyDe.Shallow
import ForSyDe.Shallow.FIR
coeff = vector [0.1, -0.2, 0.5, 0.2]
s = signal [1.0, 0, 0, 0, 0]
main = do putStr (show (firSY coeff s))
          putStr "\nDone\n."
\end{code}