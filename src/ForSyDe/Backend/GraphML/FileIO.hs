-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.GraphML.FileIO
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions working with files in the GraphML backend. 
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.GraphML.FileIO where

import ForSyDe.Backend.GraphML.AST
import ForSyDe.Backend.GraphML.Ppr(YFilesMarkup, pprGraphWithHeaders)

import System.IO
import Text.PrettyPrint.HughesPJ

-- | Write a design file to a file in disk
writeGraph :: YFilesMarkup -> GraphMLGraph -> FilePath -> IO ()
writeGraph yFiles graph fp = do
 handle     <- openFile fp WriteMode
 hPutStr handle $ (render . pprGraphWithHeaders yFiles) graph
 hClose handle