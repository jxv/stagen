module Stagen.Job where

import Control.Monad (sequence)
import Control.Parallel.Strategies

runJobs :: Monad m => Int -> [m a] -> m [a]
runJobs n = sequence . runEval . evalBuffer n rseq
