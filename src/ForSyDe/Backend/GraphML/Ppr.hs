{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.GraphML.Ppr
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- GraphML pretty printing instances.
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.GraphML.Ppr where

import ForSyDe.Backend.Ppr
import ForSyDe.Ids
import ForSyDe.Process.ProcVal
import ForSyDe.Process.ProcFun
import ForSyDe.Backend.GraphML.AST
import ForSyDe.Netlist
import ForSyDe.Netlist.Traverse
import ForSyDe.System.SysDef
import ForSyDe.OSharing

import Data.Maybe (fromJust)
import Data.List (findIndex)
import qualified Data.Foldable as DF (foldr, toList)
import Language.Haskell.TH (pprint, Dec(FunD), Exp, nameBase)
import Text.PrettyPrint.HughesPJ


-- | The only accepted pretyprinting option
type YFilesMarkup = Bool

-- | Number of spaces used for indentation
nestVal :: Int
nestVal = 5


instance PprOps YFilesMarkup GraphMLGraph where
 pprOps yFiles (GraphMLGraph id nodes edges) =
  text "<graph" <+> text ("id=\"" ++ id ++ "\"") <+> 
                     text "edgedefault=\"directed\" >" $+$
    nest nestVal (vSpace $+$    
                  pprOps_list yFiles (vNSpaces 1) nodes $+$
                  vSpace $+$
                  pprOps_list yFiles (vNSpaces 1) edges $+$
                  vSpace) $+$
  text "</graph>" 

instance PprOps YFilesMarkup GraphMLNode where
 pprOps yFiles node =
   text "<node" <+> text ("id=\"" ++ id ++ "\"") <> text ">" $+$   
   nest nestVal (
     (case node of
        ProcNode ins _ ->
          case ins of
            InPort  _ -> 
              process_type "InPort" $+$
              yFilesNodeTags dim "#000000" "rectangle" (Just "w") id 
            Proc _ (Const pval) -> 
              let arg = (expVal.valAST) pval
              in process_type "ConstSY" $+$
                 value_arg  arg $+$
                 yFilesNodeTags dim "#FFFFFF" "ellipse" Nothing ("ConstSY\n" ++ show id ++ "\nval=" ++ pprint arg)
            Proc _ (ZipWithNSY tpf i) -> 
              let nins = length i
                  typ = case nins of
                           1 -> "MapSY"
                           _ -> "ZipWith" ++ show nins ++ "SY"
                  pfAST = (tpast.tast) tpf
              in process_type "ZipWithNSY" $+$
                 procfun_arg pfAST $+$
                 yFilesNodeTags dim "#6F7DBC" "roundrectangle" Nothing (typ ++ "\n" ++ show id ++ "\nfName=" ++ nameBase (name pfAST))
            Proc _ (ZipWithxSY tpf _) -> 
              process_type "ZipWithxSY" $+$
              procfun_arg ((tpast.tast) tpf) $+$
              yFilesNodeTags dim "#AFADFC" "rectangle" Nothing ("ZipWithxSY\n" ++ show id)
            Proc _ (UnzipNSY t _ _) -> 
              let typ = "Unzip" ++ show (length t) ++ "SY"
              in process_type "UnzipNSY"  $+$
                 yFilesNodeTags dim "#5993A3" "roundrectangle" Nothing (typ ++ "\n" ++ show id)
            Proc _ (UnzipxSY _ _ _ _) -> 
              process_type "UnzipxSY" $+$
              yFilesNodeTags dim "#99D3E3" "rectangle" Nothing ("UnzipxSY\n" ++ show id )
            Proc _ (DelaySY pval _) -> 
              let arg = (expVal.valAST) pval
              in process_type "DelaySY" $+$
                 value_arg  arg $+$
                 yFilesNodeTags dim "#FF934C" "diamond" Nothing ("DelaySY\n" ++ show id ++ "\nval=" ++ pprint arg)
            Proc _ (SysIns psd _) -> 
              let parId = (sid.readURef.unPrimSysDef) psd
              in process_type "SysIns" $+$
                 instance_parent parId $+$
                 yFilesNodeTags dim "#FF934C" "rectangle" Nothing ("SysIns\n" ++ show id ++ "\nparent=" ++ parId)
        OutNode _ _ -> 
          process_type "OutPort"  $+$
          yFilesNodeTags dim "#000000" "rectangle"  (Just "e") id 
        ) $+$ vcat (map port portIds)      
       ) $+$
   text "</node>" 
  where 
   (id, portIds) = 
           case node of
                ProcNode ins outs ->
                   let pids = arguments ins ++ outs
                   in case ins of
                       InPort id -> (id, pids)
                       Proc id _ -> (id, pids)
                OutNode id portid -> (id,[portid])
   dim = nodeDims node
   -- write the yFiles specific markup for the node
   yFilesNodeTags (xsize, ysize) color shape mSide label =
    let labelLocation = maybe "modelName=\"internal\" modelPosition=\"c\""
                              (\s -> "modelName=\"sides\" modelPosition=\""++
                                     s ++ "\"")
                              mSide in            
     if yFiles 
      then 
        text "<data key=\"d0\">" $+$
         nest nestVal 
          (text "<y:ShapeNode>" $+$
           nest nestVal 
            (text "<y:Geometry height=\"" <> float ysize <> text "\" width=\"" <> float xsize <> text "\" x=\"0.0\" y=\"0.0\"/>" $+$
             text "<y:Fill color=\"" <> text color <> text "\" transparent=\"false\"/>" $+$
             text "<y:NodeLabel alignment=\"center\" autoSizePolicy=\"content\" fontFamily=\"Dialog\" fontSize=\"12\" fontStyle=\"plain\" hasBackgroundColor=\"false\" hasLineColor=\"false\"" <+> text labelLocation <+> text "textColor=\"#000000\" visible=\"true\">" <> text label <> text "</y:NodeLabel>" $+$
             text "<y:Shape type=\"" <> text shape <> text "\"/>"
           ) $+$
          text "</y:ShapeNode>" 
         ) $+$
       text "</data>"
     else empty

instance PprOps YFilesMarkup  GraphMLEdge where
 pprOps yFiles (GraphMLEdge origN origP targetN targetP) = 
    text "<edge" <+> text ("source=\"" ++ origId ++ "\"") <+> 
                     text ("sourceport=\"" ++ origP ++ "\"") <+>
                     text ("target=\"" ++ targetId ++ "\"") <+> 
                     text ("targetport=\"" ++ targetP ++ "\"") <> 
    if not yFiles 
      then text "/>"
      else char '>' $+$
           nest nestVal
            (text "<data key=\"d2\">" $+$
               nest nestVal
                (text "<y:PolyLineEdge>" $+$
                 nest nestVal
                  (text "<y:Path sx=\"" <> float edgeOrigX <> text "\" sy=\"" <> float edgeOrigY <> text "\" tx=\"" <> float edgeTargetX <> text "\" ty=\""<> float edgeTargetY <> text "\"/>" $+$
                   text "<y:LineStyle color=\"#000000\" type=\"line\" width=\"1.0\"/>" $+$
                   text "<y:Arrows source=\"none\" target=\"standard\"/>" $+$
		   text "<y:BendStyle smoothed=\"false\"/>"
                  ) $+$
                 text "</y:PolyLineEdge>") $+$
             text "</data>") $+$
           text "</edge>" 
  where -- Origin Node identifier
       origId = getId origN
       -- Target Node Identifier
       targetId = getId targetN
       -- Calculate the edge connection point for yFiles markup
       (edgeOrigX, edgeOrigY) = edgeConnection True 
                                                origNodeDims nOPortsOrig
                                                (findOutOrder origN origP)
       (edgeTargetX, edgeTargetY) = edgeConnection False 
                                               targetNodeDims nIPortsTarget
                                               (findInOrder targetN targetP)  
       (_, nOPortsOrig) = nIOPorts origN
       origNodeDims  = nodeDims origN
       (nIPortsTarget, _) = nIOPorts targetN
       targetNodeDims = nodeDims targetN       
       -- Function to calculate where to connect an edge to a node
       -- note that in yfiles the coordinates origin of a node
       -- is located in the center, but the Y axis is inverted 
       -- (negative values are in the upper side)
       edgeConnection isSource (nodeXSize, nodeYSize) totalPorts portOrder = 
                                                                         (x,y)
          where x = if isSource then nodeXSize / 2 else -(nodeXSize/2) 
                ySep = nodeYSize/(fromIntegral totalPorts)
                -- Absolut value of y measure from the top
                yAbs = ySep/2 + (fromIntegral portOrder) * ySep
                y = yAbs - (nodeYSize / 2)

       -- helper functions
       -------------------
       -- Find the order (starting at 0) of a input Port in a node
       findInOrder node portid = findList list
          where findList = fromJust . findIndex (==portid)
                list = case node of 
                 OutNode _ pid -> [pid]
                 ProcNode ins _ -> DF.toList ins
       -- Find the order (starting at 0) of an output Port in a node
       findOutOrder node portid = findList list 
          where findList = fromJust . findIndex (==portid)
                list = case node of 
                 OutNode _ pid -> [pid]
                 ProcNode _ outs -> outs
       -- Get the identifier of a node
       getId node  = case node of
         OutNode id _ -> id
         ProcNode n _ -> case n of
                  InPort pid -> pid
                  Proc pid _ -> pid

-- | pretty print a Graph with XML headers and key definitions
pprGraphWithHeaders :: YFilesMarkup -> GraphMLGraph -> Doc
pprGraphWithHeaders yFiles graph = 
  text "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" $+$
  text "<!-- Automatically generated by ForSyDe -->" $+$
  text "<graphml" <+> xmlns <+>  
  text "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" <+>
  xmlns_y <+>

  xsi_schemaLocation <>
  char '>' $+$ 
  nest nestVal (
    text "<key id=\"process_type\" for=\"node\" attr.name=\"process_type\" attr.type=\"string\"/>" $+$
    text "<key id=\"value_arg\" for=\"node\" attr.name=\"value_arg\" attr.type=\"string\"/>" $+$
    text "<key id=\"profun_arg\" for=\"node\" attr.name=\"procfun_arg\" attr.type=\"string\"/>" $+$
    text "<key id=\"instance_parent\" for=\"node\" attr.name=\"instance_parent\" attr.type=\"string\"/>" $+$
    yFilesAttribs $+$
    pprOps yFiles graph) $+$
  text "</graphml>"
 where
  -- For some silly reason, yFiles uses a different GraphML target namesapce
  -- different to the one used in grapdrawing.org's GraphML primer
  xmlns = if yFiles 
    then text "xmlns=\"http://graphml.graphdrawing.org/xmlns/graphml\""
    else text "xmlns=\"http://graphml.graphdrawing.org/xmlns\""
  xmlns_y = if not yFiles then empty else
    text "xmlns:y=\"http://www.yworks.com/xml/graphml\""
  xsi_schemaLocation = if yFiles 
   then text "xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns/graphml http://www.yworks.com/xml/schema/graphml/1.0/ygraphml.xsd\""
   else   text "xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\"" 
  yFilesAttribs = if not yFiles then empty else 
   text "<key for=\"node\" id=\"d0\" yfiles.type=\"nodegraphics\"/>"  $+$
   text "<key attr.name=\"description\" attr.type=\"string\" for=\"node\" id=\"d1\"/>" $+$
   text "<key for=\"edge\" id=\"d2\" yfiles.type=\"edgegraphics\"/>" $+$
   text "<key attr.name=\"description\" attr.type=\"string\" for=\"edge\" id=\"d3\"/>" 

-------------------------
-- Tag printing functions
-------------------------
  
port :: GraphMLPortId -> Doc
port id = text "<port" <+> text ("name=\"" ++ id ++ "\"") <> text "/>"   


process_type :: String -> Doc
process_type str = 
 text "<data key=\"process_type\">" <> text str <> text "</data>"   


value_arg :: Exp -> Doc
value_arg exp = 
 text "<data key=\"value_arg\">" <> text (pprint exp)  <> text "</data>"   

procfun_arg :: ProcFunAST -> Doc
-- FIXME: support default parameters
procfun_arg (ProcFunAST n cls _) = 
 text "<data key=\"value_arg\">" $+$
  nest nestVal (text $ pprint (FunD n cls)) $+$
 text "</data>"   


instance_parent :: SysId -> Doc
instance_parent  id = 
 text "<data key=\"instance_parent\">" <> text id <> text "</data>"   

-------------------------
-- Other helper functions
-------------------------

-- Location of Edge connections and node size using yFiles Markup
-- ==============================================================
-- * All Nodes (except ports, of 7x7) have a constant width of 100
-- * The height depends on the node:
--   * ConstSY has a constant height of 100 
--   * DelaySY has a constant height of 100
--   * Nodes with three lines of text (ZipWithNSY, SysIns) have a minimum of 55
--   * Nodes with two lines of text (the rest) have a minimum height of 40
--
-- ** The final height of nodes with minimum height is
--    Max(minheight, MaxS*ps)
--       where MaxS = Max(number of input signals, number of output signals)
--             ps = inter-port separation
-- ** The location where both ends of an edge is trivially calculated
--   using the order of the corresponding port, the final size of the
--   node, "bi" and "ps"

-- | port separation space when connecting to a node which surpasses the 
--   minimum height
portSep :: Float 
portSep = 15

-- | Calculate the dimensions of a Node
nodeDims :: GraphMLNode  -> (Float, Float) -- ^ Node dimensions (x,y)
nodeDims node = case node of
   OutNode _ _ -> (7,7)
   ProcNode n _ ->
      case n of
       InPort _ -> (7,7)
       Proc _ n' ->
         case n' of
           Const _ -> (100,100)  
           DelaySY _ _ -> (100,100)
           ZipWithNSY _ _ -> (100, height 55 maxio)
           SysIns _ _ -> (100, height 55 maxio)
           _ -> (100, height 40 maxio)  
 where height :: Float -- ^ Minimum height 
              -> Int   -- ^ Max(input port number, output port number)
              -> Float -- ^ Final height
       height min maxio = max min 
                              (portSep*(fromIntegral maxio))
       maxio :: Int -- ^ Max(input port number, output port number) 
       maxio = uncurry max $ nIOPorts node


-- | Calculate the number of input and output ports of a node
nIOPorts :: GraphMLNode -> (Int, Int)
nIOPorts node = 
      case node of
          ProcNode ins outs -> (DF.foldr (\_ b -> b+1) 0 ins, length outs)
          OutNode _ _ -> (1,0)
