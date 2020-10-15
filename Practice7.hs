module Practice7 where 

import Control.Monad.State

lengthM :: [a] -> State Int ()
lengthM xs = do
  put (length xs)

reduce :: Monad m => m (m a) -> m a
reduce a = (>>=) a id