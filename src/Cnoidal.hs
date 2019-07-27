module Cnoidal
    ( module Cnoidal.Media
    , module Cnoidal.Music
    , module Cnoidal.Player

    , times
    ) where

import Cnoidal.Media
import Cnoidal.Music
import Cnoidal.Player

-- | Repeat an element from a 'Semigroup' /n/ times.
-- Particularly useful for the types '[a]' and 'Media a'.
times :: Semigroup a => Int -> a -> a 
times = stimes
