{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module SharedTypes where

import Language.Fay.Yesod
import Data.Data
#ifdef FAY
import FFI
#else
import Fay.FFI
#endif

data Command = RollDie (Returns Text)
    deriving (Read, Typeable, Data)
