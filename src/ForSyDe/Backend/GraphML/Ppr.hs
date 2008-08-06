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
              yFilesNodeTags (20, 20) "#DDDDDD" "ellipse" ("I `"++ id ++ "'") 
            Proc _ (Const pval) -> 
              let arg = (expVal.valAST) pval
              in process_type "Const" $+$
                 value_arg  arg $+$
                 yFilesNodeTags (100, 100) "#FFFFFF" "ellipse" ("Const\n`" ++ id ++ "'\nval=" ++ pprint arg)
            Proc _ (ZipWithNSY tpf i) -> 
              let nins = length i
                  typ = case nins of
                           1 -> "MapSY"
                           _ -> "ZipWith" ++ show nins ++ "SY"
                  pfAST = (tpast.tast) tpf
              in process_type "ZipWithNSY" $+$
                 procfun_arg pfAST $+$
                 yFilesNodeTags (100, 100) "#6F7DBC" "roundrectangle" (typ ++ "\n`" ++ id ++ "'\nfName=" ++ nameBase (name pfAST))
            Proc _ (ZipWithxSY tpf _) -> 
              process_type "ZipWithxSY" $+$
              procfun_arg ((tpast.tast) tpf) $+$
              yFilesNodeTags (100, 100) "#AFADFC" "rectangle" ("ZipWithxSY\n`" ++ id ++"'")
            Proc _ (UnzipNSY t _ _) -> 
              let typ = "Unzip" ++ show (length t) ++ "SY"
              in process_type "UnzipNSY"  $+$
                 yFilesNodeTags (100, 100) "#5993A3" "roundrectangle" (typ ++ "\n`" ++ id ++"'")
            Proc _ (UnzipxSY _ _ _ _) -> 
              process_type "UnzipxSY" $+$
              yFilesNodeTags (100, 100) "#99D3E3" "rectangle" ("UnzipxSY\n`" ++ id ++"'")
            Proc _ (DelaySY pval _) -> 
              let arg = (expVal.valAST) pval
              in process_type "DelaySY" $+$
                 value_arg  arg $+$
                 yFilesNodeTags (100, 100) "#FF934C" "diamond" ("DelaySY\n`" ++ id ++ "'\nval=" ++ pprint arg)
            Proc _ (SysIns psd _) -> 
              let parId = (sid.readURef.unPrimSysDef) psd
              in process_type "SysIns" $+$
                 instance_parent parId $+$
                 yFilesNodeTags (100, 100) "#FF934C" "rectangle" ("SysIns\n`" ++ id ++ "'\nparent=" ++ parId)
        OutNode _ _ -> 
          process_type "OutPort"  $+$
          yFilesNodeTags (20, 20) "#DDDDDD" "ellipse" ("O `"++ id ++ "'") 
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
   yFilesNodeTags (xsize, ysize) color shape label =
    if yFiles 
     then 
       text "<data key=\"d0\">" $+$
         nest nestVal 
          (text "<y:ShapeNode>" $+$
           nest nestVal 
            (text "<y:Geometry height=\"" <> float xsize <> text "\" width=\"" <> float ysize <> text "\" x=\"0.0\" y=\"0.0\"/>" $+$
             text "<y:Fill color=\"" <> text color <> text "\" transparent=\"false\"/>" $+$
             text "<y:NodeLabel alignment=\"center\" autoSizePolicy=\"content\" fontFamily=\"Dialog\" fontSize=\"12\" fontStyle=\"plain\" hasBackgroundColor=\"false\" hasLineColor=\"false\" modelName=\"internal\" modelPosition=\"c\" textColor=\"#000000\" visible=\"true\">" <> text label <> text "</y:NodeLabel>" $+$
             text "<y:Shape type=\"" <> text shape <> text "\"/>"
           ) $+$
          text "</y:ShapeNode>" 
         ) $+$
       text "</data>"
     else empty

instance PprOps YFilesMarkup  GraphMLEdge where
 pprOps yFiles (GraphMLEdge origN origP destN destP) = 
  text "<edge" <+> text ("source=\"" ++ origN ++ "\"") <+> 
                   text ("sourceport=\"" ++ origP ++ "\"") <+>
                   text ("target=\"" ++ destN ++ "\"") <+> 
                   text ("targetport=\"" ++ destP ++ "\"") <+> text "/>"


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
    text "<key id=\"process_type\" for=\"node\" attrb.name=\"process_type\" attrb.type=\"string\"/>" $+$
    text "<key id=\"value_arg\" for=\"node\" attrb.name=\"value_arg\" attrb.type=\"string\"/>" $+$
    text "<key id=\"profun_arg\" for=\"node\" attrb.name=\"procfun_arg\" attrb.type=\"string\"/>" $+$
    text "<key id=\"instance_parent\" for=\"node\" attrb.name=\"instance_parent\" attrb.type=\"string\"/>" $+$
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
   text "<key attr.name=\"description\" attr.type=\"string\" for=\"edge\" id=\"d3\"/>" $+$
   text "<key for=\"graphml\" id=\"d4\" yfiles.type=\"resources\"/>"   

-------------------------
-- Tag printing functions
-------------------------
  
port :: GraphMLPortId -> Doc
port id = text "<port" <+> text ("name=\"" ++ id ++ "\"") <> text "/>"   


process_type :: String -> Doc
process_type str = 
 text "<data key=\"process_type\">" <+> text str <+> text "</data>"   


value_arg :: Exp -> Doc
value_arg exp = 
 text "<data key=\"value_arg\">" <+> text (pprint exp)  <+> text "</data>"   

procfun_arg :: ProcFunAST -> Doc
-- FIXME: support default parameters
procfun_arg (ProcFunAST n cls _) = 
 text "<data key=\"value_arg\">" $+$
  nest nestVal (text $ pprint (FunD n cls)) $+$
 text "</data>"   


instance_parent :: SysId -> Doc
instance_parent  id = 
 text "<data key=\"instance_parent\">" <+> text id <+> text "</data>"   


