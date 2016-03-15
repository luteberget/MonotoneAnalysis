module MonotoneFrameworkAnalysis where

-- Monotone framework analysis, as described in
-- F. Nielson, H R. Nielson, C. Hankin: Principles of Program Analysis

import ControlFlow -- Label and Edge definitions (ints)
import Data.Maybe

justLookup :: Eq a => a -> [(a,b)] -> b
justLookup e l = fromJust $ lookup e l

subset :: (Eq a) => [a] -> [a] -> Bool
subset a b = all (\x -> elem x b) a

superset :: (Eq a) => [a] -> [a] -> Bool
superset a b = subset b a

type Analysis domain = [(Label,domain)]
type Worklist = [Edge]

data OutputTransform = PreTransform | PostTransform deriving (Eq)

  -- Configuration record containing the information needed
  -- to express the monotone framework analysis procedure
data MFInstance statement domain =
  MFInstance {
    -- statement is an AST
    -- domain is the lattice on which the analysis is performed
    partialOrder :: domain -> domain -> Bool,
    leastUpperBound :: domain -> domain -> domain,
    leastElement :: domain,
    initialElement :: domain,
    labeledProgram :: [(Label,statement)],
    flow :: [(Label,Label)],
    extremalLabels :: [Label],
    transformation :: (Label,statement) -> domain -> domain,
    output :: OutputTransform
  }

  -- Overall algorithm
monotoneFrameworkAnalysis :: (Eq domain, Show domain) =>
  MFInstance statement domain -> Analysis domain
monotoneFrameworkAnalysis config =
  case output config of PreTransform -> pre
                        PostTransform -> post

  where -- Initialize (step 1 in table 2.8)
        init = initialization config

        -- Iteration (step 2 in table 2.8)
        iterator = iteration config

        -- Iterate until fix point (step 2 in table 2.8)
        (_,fixpoint) = until (\x -> x == iterator x) iterator init

        -- Extract result after fix-point is reached (step 3 in table 2.8)
        (pre,post) = extractResult config fixpoint


-- Initialize work list and intermediate analysis result by setting:
-- 1. work list to include all edges in the control flow graph
-- 2. intermediate analysis results to contain the least element
--    or initial elements (see MF config)
initialization :: MFInstance statement domain -> (Worklist, Analysis domain)
initialization config = ((flow config),analysis)
  where analysis = extremal ++ others

        -- Put initial elements into the extremal labels (e.g. '?' on initial block
        -- for all variables in reaching definitions analysis)
        extremal = [(l,initialElement config) | l <- extremalLabels config]

        -- Put the least element into all other labels.
        others = [(l, leastElement config) |
                  (l,_) <- (labeledProgram config),
                  not (l `elem` (extremalLabels config))]


iteration :: (Eq domain, Show domain) =>
  MFInstance statement domain -> (Worklist, Analysis domain) -> (Worklist, Analysis domain)

-- Uncomment to print work list
-- iteration _ (wl,analysis) |
--   trace ("Work list: " ++ show wl ++ show analysis) Prelude.False = undefined

-- If work list is empty, return the same value. A fixed point will be detected by the solver.
iteration _ ([],analysis) = ([],analysis)

-- Work list is not empty
iteration config ((l1,l2):ws,analysis)

  -- Constraint already satisfied, return rest of work list.
  | ((partialOrder config) fal al) = (ws,analysis)

  -- Constraint not satisfied, expand set and add neighbors to work list
  | otherwise = (neighbors ++ ws, newanalysis)

  where statement = (l1, justLookup l1 (labeledProgram config))

        -- Current newanalysis result
        al = justLookup l2 analysis

        -- Applied mapping to current newanalysis result
        fal = (transformation config) statement (justLookup l1 analysis)

        -- Combination of current result and mapping
        combination = (leastUpperBound config) al fal

        -- New intermediate newanalysis result
        newanalysis = (l2, combination):other

        -- Remove l2 from newanalysis, as it is re-added above
        other = filter (\pair -> (fst pair) /= l2) analysis

        -- Add all edges leading out of l2
        neighbors = [(l2,l3) | (lx,l3) <- (flow config), lx == l2]


-- Calculate the MPF with and without "filled dot", as the notation is in the book.
extractResult :: MFInstance statement domain ->
  Analysis domain -> (Analysis domain,Analysis domain)
extractResult config a = (a,fa)
  where fa = map pair (labeledProgram config)
        pair = (\(l,s) -> (l,(transformation config) (l,s) (justLookup l a)))
