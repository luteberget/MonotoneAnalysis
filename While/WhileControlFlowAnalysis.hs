module WhileControlFlowAnalysis where

-- Control flow analysis for the While language.
-- Labels are ints, labeling starts at 1

import WhileAST
import ControlFlow

controlFlowAnalysis :: Program -> ControlFlowGraph Program
controlFlowAnalysis p = g
  where (g,_,_) = controlFlowAnalysisAdd 1 p

controlFlowAnalysisAdd :: Label -> Statement -> (ControlFlowGraph Program, Label, [Label])

controlFlowAnalysisAdd label s@Skip = res
  where res = ((nodes, edges), nextlabel, endlabels)
        nodes = [(label, s)]
        edges = []
        nextlabel = label+1
        endlabels = [label]

controlFlowAnalysisAdd label s@(Assignment _ _) = res
  where res = ((nodes, edges), nextlabel, endlabels)
        nodes = [(label, s)]
        edges = []
        nextlabel = label+1
        endlabels = [label]

controlFlowAnalysisAdd label (Sequential s1 s2) = ((nodes,edges), nextlabel, endlabels)
  where ((s1gn, s1ge), s1nextlabel, s1endlabels) = controlFlowAnalysisAdd label s1
        ((s2gn, s2ge), s2nextlabel, s2endlabels) = controlFlowAnalysisAdd s1nextlabel s2
        nodes = s2gn ++ s1gn
        edges = s2ge ++ (map (\x -> (x,s1nextlabel)) s1endlabels) ++ s1ge
        nextlabel = s2nextlabel
        endlabels = s2endlabels

controlFlowAnalysisAdd label s@(While _ body) = ((nodes,edges), nextlabel, endlabels)
  where ((bgn, bge), bnextlabel, bendlabels) = controlFlowAnalysisAdd (label+1) body
        nodes = bgn ++ [(label, s)]
        edges = (map (\x -> (x,label)) bendlabels) ++ bge ++ [(label,label+1)]
        nextlabel = bnextlabel
        endlabels = [label]

controlFlowAnalysisAdd label s@(IfThenElse _ s1 s2) = ((nodes,edges), nextlabel, endlabels)
  where ((s1gn, s1ge), s1nextlabel, s1endlabels) = controlFlowAnalysisAdd (label+1) s1
        ((s2gn, s2ge), s2nextlabel, s2endlabels) = controlFlowAnalysisAdd s1nextlabel s2
        nodes = s2gn ++ s1gn ++ [(label, s)]
        edges = s2ge ++ s1ge ++ [(label,s1nextlabel),(label,label+1)]
        nextlabel = s2nextlabel
        endlabels = s2endlabels ++ s1endlabels
