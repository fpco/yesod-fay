{-# LANGUAGE EmptyDataDecls #-}

-- | This is a slimmed-down jQuery binding, providing just enough functionality
-- for our demo. Do not rely on this module! Instead, please see:
--
-- <https://github.com/faylang/fay-jquery/blob/master/Language/Fay/JQuery.hs>
module Language.Fay.JQuery  where

import Fay.FFI
import Language.Fay.Yesod

data JQuery
instance Foreign JQuery
instance Show JQuery

data EventObject
instance Foreign EventObject
instance Show EventObject

void :: Fay a -> Fay ()
void m = m >> return ()

select :: String -> Fay JQuery
select = ffi "window['jQuery'](%1)"

onClick :: (EventObject -> Fay Bool) -> JQuery -> Fay JQuery
onClick = ffi "%2['click'](%1)"

alert :: Text -> Fay ()
alert = ffi "window.alert(%1)"
