module MapReduce where 
    
import Control.Parallel.Strategies
import Control.Parallel (pseq)

simpleMapReduce :: (a -> b)
                -> ([b] -> c)
                -> [a]
                -> c
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc

mapReduce :: Strategy b
           -> (a -> b)
           -> Strategy c 
           -> ([b] -> c)
           -> [a]
           -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult  `pseq` reduceResult
  where
    mapResult = parMap mapStrat mapFunc input
    reduceResult = reduceFunc mapResult `using` reduceStrat