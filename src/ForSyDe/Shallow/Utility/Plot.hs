-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.Utility.Plot
-- Copyright   :  (c) ForSyDe Group, KTH 2018-2019
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This is the plot library for ForSyDe heterogeneous MoCs - CT-MoC, SR-MoC,
-- and Untimed-MoC.
--
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Utility.Plot (
  plot, plotCT, plotCT', showParts, vcdGen
  ) where

import ForSyDe.Shallow.MoC
import ForSyDe.Shallow.Core
import Control.Exception as Except
import System.Directory
import Data.Ratio
import System.Process
import System.Time


-- The revision number of this file:
revision :: String
revision=filter (\ c -> (not (c=='$'))) "$Revision: 1.7 $, $Date: 2007/07/11 08:38:34 $"

-----------------------------------------------------------------------
-- Functions to display and plot signals:
-----------------------------------------------------------------------
-- The function 'sample' evaluates the signal and returns a list of
-- (time,value) pairs, which can be displayed as text or used in any other way.
{- $plotdoc
   Several functions are available to display a signal in textual or
   graphics form. All displaying of signals is based on sampling and
   evaluation the signal at regular sampling points.

   'showParts' does not evaluate the signal; it only shows how it is
   partitioned. Hence, it returns a sequence of intervals.

   'plot', 'plotCT' and 'plotCT'' can plot a signal or a list of signals
   in a graph. They use @gnuplot@ for doing the actual work.
   They are in the IO monad because they write to the file system.

   'plot' is defined in terms of 'plotCT' but it uses the default sampling
   period 'timeStep' and it can plot only one signal in a plot.

   'plotCT' can plot a list of signals in the same plot.
   'plotCT' is defined in terms of 'plotCT'' but uses
   default label names for the plot.

   'vcdGen' writes the values of signals in Value Change Dump (VCD) format to
   a file. There are public domain wave viewers which understand this format
   and can display the signals.
-}

-- |'sample' computes the values of a signal with a given step size.
-- It returns a list with (x, (f x)) pairs of type [(Rational,Rational)].
sample :: (Num a, Show a) =>
          Rational    -- ^ The sampling period
       -> Signal (SubsigCT a) -- ^The signal to be sampled
       -> [(Rational,a)]  -- ^The list of (time,value) pairs of the
          -- evaluated signal
sample _ NullS = []
sample step (ss :- s) = sampleSubsig step ss ++ (sample step s)

-- sampleSubsig samples a Subsig signal:
sampleSubsig :: (Num a, Show a) => Rational -> (SubsigCT  a) -> [(Rational,a)]
sampleSubsig step (SubsigCT (f,(a,b)))
  | b>a = (a,(f a)) : (sampleSubsig step (SubsigCT (f,(a+step,b))))
  | otherwise = []

-- |'showParts' allows to see how a signal is partitioned into
-- sub-signals.  It returns the sequence of intervals.
showParts :: (Num a, Show a) =>
             Signal (SubsigCT a)   -- ^The partitioned signal
          -> [(Double,Double)] -- ^The sequence of intervals
showParts NullS = []
showParts (SubsigCT (_,(a,b)):-s) = (fromRational a,fromRational b) : (showParts s)

-----------------------------------------------------------------------------
-- |'plot' plots one signal in a graph with the default sampling
-- period of 1\/200 of the duration of the signal.
plot :: (Num a, Show a) =>
        Signal (SubsigCT a) -- ^The signal to be plotted.
     -> IO String           -- ^A reporting message.
plot s = plotCT step [s]
  where step = (duration s) / 200.0

-- |'plotCT' plots a list of signals in the same graph. The sampling
-- period has to be given as argument. In the graph default label
-- names are used to identify the signals.
plotCT :: (Num a, Show a) =>
          Rational              -- ^The sampling period
       -> [Signal (SubsigCT a)] -- ^The list of signals to be ploted
                                -- in the same graph
       -> IO String             -- ^A messeage reporting what has been done.
plotCT step sigs = plotCT' step (map (\ s -> (s,"")) sigs)

{- |
   'plotCT'' is the work horse for plotting and the functions 'plot' and
   'plotCT' use it with specialising arguments.

   'plotCT'' plots all the signals in the list in one graph. If a label is
   given for a signal, this label appears in the graph. If the label string is
   \"\", a default label like \"sig-1\"  is used.

   In addition to displaying the graph on the screen, the following files
   are created in directory .\/fig:

   [ct-moc-graph.eps]      an eps file of the complete graph

   [ct-moc-graph.pdf]      A pdf file of the complete graph

   [ct-moc-graph-latex.eps]    included by ct-moc-graph-latex.tex

   [ct-moc-graph-latex.tex]    This is the tex file that should be included
           by your latex document. It in turn includes
        the file ct-moc-graph-latex.eps.
       These two files have to be used together;
        the .eps file contains only the graphics,
       while the .tex file contains the labels and
           text.
-}
plotCT' :: (Num a, Show a) =>
           Rational -- ^Sampling period
        -> [(Signal (SubsigCT a), String)]
        -- ^A list of (signal,label) pairs. The signals are plotted and
        -- denoted by the corresponding labels in the plot.
        -> IO String -- ^A simple message to report completion
plotCT' _ [] = return []
plotCT' 0 _    = error "plotCT: Cannot compute signal with step=0.\n"
plotCT' step sigs = plotSig (expandSig 1 sigs)
  where
    expandSig :: (Num a, Show a) =>
                 Int -> [(Signal (SubsigCT a),String)]
              -> [(Int,String,[(Rational,a)])]
    expandSig _ [] = []
    expandSig i ((sig,label):sigs)
      = (i, label, (sample step sig)) : (expandSig (i+1) sigs)
    plotSig :: (Num a, Show a) => [(Int,String,[(Rational,a)])] -> IO String
    plotSig sigs
      = do mkDir "./fig"
           writeDatFiles sigs
           -- We write the gnuplot script to a file;
           -- But we try several times with a different name because
           -- with ghc on cygwin we cannot write to a script file while
           -- gnuplot is still running with the old script file:
           fname <- tryNTimes 10
                    (\ file -> (writeFile file
                                (mkPlotScript (map mkDatFileName sigs))))
           -- We fire up gnuplot:
           _ <- system ("gnuplot -persist " ++ fname)
           -- We return some reporting string:
           return ("Signal(s) " ++(mkAllLabels sigs) ++ " plotted.")
    writeDatFiles [] = return ()
    writeDatFiles (s@(_, _, sig): sigs)
      = do writeFile (fst (mkDatFileName s)) (dumpSig sig)
           writeDatFiles sigs
    mkDatFileName :: (Int,String,a) -> (String,String)
    mkDatFileName (sigid,label,_) = ("./fig/ct-moc-" ++ (replChar ">" label)
                                     ++(show sigid)++".dat",
                                     (mkLabel label sigid))
    mkLabel :: String -> Int -> String
    mkLabel "" n = "sig-" ++ show n
    mkLabel l _  = l
    mkAllLabels :: (Num a) => [(Int,String,[(Rational,a)])] -> String
    mkAllLabels sigs = drop 2 (foldl f "" sigs)
      where f labelString (n,label,_)
              = labelString ++ ", " ++ (mkLabel label n)
    replChar :: String -- all characters given in this set are replaced by '_'
             -> String -- the string where characters are replaced
             -> String -- the result string with all characters replaced
    replChar [] s = s
    replChar _ [] = []
    replChar replSet (c:s) | elem c replSet = '_' : (replChar replSet s)
                           | otherwise  = c   : (replChar replSet s)

    dumpSig :: (Num a, Show a) => [(Rational,a)] -> String
    dumpSig points = concatMap f points
      where f (x,y) = show ((fromRational x) :: Float) ++ "    "
                      ++ show (y) ++ "\n"

    mkPlotScript :: [(String  -- the file name of the dat file
                     ,String  -- the label for the signal to be drawn
                     )] -> String  -- the gnuplot script
    mkPlotScript ns = "set xlabel \"seconds\" \n"
                      ++ "plot " ++ (f1 ns) ++ "\n"
                      ++ "set terminal postscript eps color\n"
                      ++ "set output \"" ++ plotFileName++".eps\"\n"
                      ++ "replot \n"
                      ++ "set terminal epslatex color\n"
                      ++ "set output \"" ++ plotFileName++"-latex.eps\"\n"
                      ++ "replot\n"
                      -- ++ "set terminal pdf\n"
                      -- ++ "set output \"fig/ct-moc-graph.pdf\"\n"
                      -- ++ "replot\n"
      where f1 :: [(String,String)] -> String
            f1 ((datfilename,label):(n:ns))
              = "\t\"" ++ datfilename
                ++ "\" with linespoints title \""++label++"\",\\\n"
                ++ "    " ++ (f1 (n:ns))
            f1 ((datfilename,label):[])
              = "\"" ++ datfilename
                ++ "\" with linespoints title \""++label++"\"\n"
            f1 [] = ""
            plotFileName = "fig/ct-moc-graph-" ++ (f2 ns)
            -- f2 generates part of the filename for the eps and latex
            -- files, which is determined by the signal labels.
            f2 :: [(String,String)] -> String
            f2 [] = ""
            f2 ((_,label):[]) = label
            f2 ((_,label):_) = label ++ "_"
    -- tryNTimes applies a given actions at most n times. Everytime
    -- the action is applied and an error occurrs, it tries again but
    -- with a decremented first argument. It also changes the file name
    -- because the file name uses the n as part of the name.
    -- The idea is that the action tries different files to operate on.
    -- The problem was that when gnuplot was called on a gnuplot script
    -- file, it was not possible to write a new script file with the same
    -- name and start a new gnuplot process (at least not with ghc or ghci on
    -- cygwin; it worked fine with hugs on cygwin).
    -- So we go around the problem here by trying different file names until
    -- we succeed or until the maximum number of attempts have been performed.
    tryNTimes :: Int -> (String -> IO ()) -> IO String
    tryNTimes n a | n <= 0 = error "tryNTimes: not succedded"
                  | n > 0 =
                      do Except.catch (action fname a) (handler a)
                           where handler :: (String -> IO()) -> IOError -> IO String
                                 handler a _ = tryNTimes (n-1) a
                                 fname = "./fig/ct-moc-" ++ (show n) ++ ".gnuplot"
                                 action :: String -> (String -> IO ()) -> IO String
                                 action fname a = do (a fname)
                                                     return fname
    tryNTimes _ _ = error "tryNTimes: Unexpected pattern."

----------------------------------------------------------------------------
{- |
vcdGen dumps the values of a list of signal in VCD (Value Change Dump) format
(IEEE Std 1364-2001), which is part of the Verilog standard
(<http://en.wikipedia.org/wiki/Value_change_dump>).
There are public domain tools to view VCD files. For instance,
GTKWave (<http://home.nc.rr.com/gtkwave/>) is a popular viewer available for
Windows and Linux.

The values are written to the file ./fig/ct-moc.vcd. If the file exists, it
is overwritten. If the directory does not exist, it is created.

-}
vcdGen :: (Num a, Show a)
      => Rational     -- ^Sampling period; defines for what
              -- time stamps the values are written.
      -> [(Signal (SubsigCT a), String)]
      -- ^A list of (signal,label) pairs. The signal values written and
      -- denoted by the corresponding labels.
      -> IO String    -- ^A simple message to report completion
vcdGen _ [] = return []
vcdGen 0    _  = error "vcdgen: Cannot compute signals with step=0.\n"
vcdGen step sigs =
    do
  -- putStr (show (distLabels (expandSig 1 sigs)))
  plotSig (expandSig 1 sigs)
    where
  expandSig :: (Num a, Show a) =>
       Int -> [(Signal (SubsigCT a),String)]
           -> [(Int,String,[(Rational,a)])]
  expandSig _ [] = []
  expandSig i ((sig,label):sigs)
      = (i, label, (sample step sig)) : (expandSig (i+1) sigs)
  plotSig :: (Num a, Show a) => [(Int,String,[(Rational,a)])] -> IO String
  plotSig sigs
      = do writeVCDFile sigs
           -- We return some reporting string:
           return ("Signal(s) " ++(mkAllLabels sigs) ++ " dumped.")
  mkLabel :: String -> Int -> String
  mkLabel "" n = "sig-" ++ show n
  mkLabel l _  = l
  mkAllLabels sigs = drop 2 (foldl f "" sigs)
      where f labelString (n,label,_)
              = labelString ++ ", " ++ (mkLabel label n)
  writeVCDFile :: (Show a) => [(Int,String,[(Rational,a)])] -> IO()
  writeVCDFile sigs
    = do mkDir "./fig"
         clocktime <- getClockTime
         let {date = calendarTimeToString (toUTCTime clocktime);
              labels = getLabels sigs;
              timescale = findTimescale sigs;}
           in writeFile mkVCDFileName ((vcdHeader timescale labels date)
                                       ++ (valueDump timescale (prepSigValues sigs)))
  mkVCDFileName :: String
  mkVCDFileName = ("./fig/ct-moc.vcd")

mkDir :: String -> IO()
mkDir dir = do dirExists <- doesDirectoryExist dir
               if (not dirExists)
                 then (createDirectory dir)
                 else return ()

-- prepSigValues rearranges the [(label,[(time,value)])] lists such
-- that we get a list of time time stamps and for each time stamp
-- we have a list of (label,value) pairs to be dumped:
prepSigValues :: (Show a) => [(Int,String,[(Rational,a)])]
      -> [(Rational,[(String,a)])]
prepSigValues sigs = f2 (distLabels sigs)
    where
  -- f2 transforms a [[(label,time,value)]]
  -- into a [(time, [label,value])] structure:
  f2 :: (Show a)
    => [[(String,Rational,a)]] -> [(Rational,[(String,a)])]
  f2 [] = []
  f2 ([]:_) = []
  f2 xs = f3 hdxs : f2 tailxs
      where
    -- here we take all first elements of the lists in xs
    -- and the tail of the lists in xs:
    (hdxs,tailxs) = (map g1 xs,
             map (\ (_:ys)-> ys) xs)
    g1 [] = error ("prepSig.f2.g1: first element of xs is empty:"
           ++ "xs="++show xs)
    g1 (y:_) = y
    f3 :: (Show a)
      => [(String,Rational,a)] -> (Rational,[(String,a)])
    f3 (valList@((_, time, _):_)) = (time, f4 time valList)
    f3 [] = error ("prepSigValues.f2.f3: "
           ++ "empty (label,time,value)-list")
    f4 :: (Show a)
      => Rational -> [(String,Rational,a)] -> [(String,a)]
    f4 _ [] = []
    f4 time ((label,time1,value):valList)
       | time == time1 = (label,value) : f4 time valList
       | otherwise
       = error ("prepSigValues: Time stamps in different"
            ++ " signals do not match: time="
            ++(show time)++", time1="++(show time1)
            ++", label="++label++", value="++(show value)
            ++"!")
-- distLabels inserts the labels into its corresponding
-- (time,value) pair list to get a (label,time,value) triple:
distLabels :: [(Int,String,[(Rational,a)])]
       -> [[(String,Rational,a)]]
distLabels [] = []
distLabels ((_,label,valList):sigs)
    = (map (\ (t,v) -> (label,t,v)) valList) : (distLabels sigs)
getLabels :: [(Int,String,[(Rational,a)])] -> [String]
getLabels = map (\(_,label,_)-> label)
vcdHeader :: Rational -> [String] -> String -> String
vcdHeader timescale labels date = "$date\n"
           ++ date ++ "\n"
           ++ "$end\n"
           ++ "$version\n"
           ++ "ForSyDe CTLib " ++ revision ++ "\n"
           ++ "$end\n"
           ++ "$timescale 1 " ++ (timeunit timescale) ++ " $end\n"
           ++ "$scope module top $end\n"
           ++ (concatMap (\ label -> ("$var real 64 "++label
                  ++ " " ++ label
                  ++ " $end\n")) labels)
           ++ "$upscope $end\n"
           ++ "$enddefinitions $end\n"
           ++ "#0\n"
           ++ "$dumpvars\n"
           ++ (concatMap (\ label -> "r0.0 "++label++ "\n")
             labels)
           ++ "\n"
valueDump :: (Show a) => Rational -> [(Rational,[(String,a)])] -> String
valueDump _ [] = ""
valueDump timescale ((t,values):valList)
  = "#"++(show (g (t/timescale)))++"\n"
    ++ (f values) ++ (valueDump timescale valList)
  where
    f :: (Show a) => [(String,a)] -> String
    f [] = ""
    f ((l,v):values) = "r"++(show v)++" "++l++"\n" ++ (f values)
    g :: Rational -> Integer
    -- Since the VCD format expects integers for the timestamp, we make
    -- sure that only an integer is printed in decimal format (no exponent):
    g t = round t


timeunit :: Rational -> String
timeunit timescale | timescale == 1 % 1    = "s"
       | timescale == 1 % 1000 = "ms"
       | timescale == 1 % 1000000 = "us"
       | timescale == 1 % 1000000000 = "ns"
       | timescale == 1 % 1000000000000 = "ps"
       | timescale == 1 % 1000000000000000 = "fs"
       | otherwise = error ("timeunit: unexpected timescale: "
                ++ (show timescale))

findTimescale :: [(Int,String,[(Rational,a)])] -> Rational
findTimescale sigs
    = f 1 (concatMap (\ (_,_,valList) -> (fst (unzip valList))) sigs)
  where
    f :: Rational -> [Rational] -> Rational
    f scale [] = scale
    f scale (x:xs) | r == 0    = f scale xs
           | otherwise = f (scale/1000) xs
           where (_,r) = (properFraction (abs (x / scale)))
                         :: (Int,Rational)
