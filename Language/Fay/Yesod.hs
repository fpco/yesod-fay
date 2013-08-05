{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP                #-}
-- | Module to be shared between server and client.
--
-- This module must be valid for both GHC and Fay.
module Language.Fay.Yesod where

import           Prelude
#ifdef FAY
import           FFI
#else
import           Fay.FFI
#endif
import           Data.Data

#ifdef FAY

data Text = Text
    deriving (Show, Read, Eq, Typeable, Data)

fromString :: String -> Text
fromString = ffi "%1"

toString :: Text -> String
toString = ffi "%1"

#else

import qualified Data.Text as T

type Text = T.Text

fromString :: String -> Text
fromString = T.pack

toString :: Text -> String
toString = T.unpack

#endif

-- | A proxy type for specifying what type a command should return. The final
-- field for each data constructor in a command datatype should be @Returns@.
data Returns a = Returns
    deriving (Eq, Show, Read, Data, Typeable)

-- | Call a command.
call :: (Returns a -> command)
     -> (a -> Fay ()) -- ^ Success Handler
     -> Fay ()
call f g = ajaxCommand (f Returns) g

-- ! Call a command, handling errors as well
callWithErrorHandling
     :: (Returns a -> command)
     -> (a -> Fay ()) -- ^ Success Handler
     -> (Fay ())      -- ^ Failure Handler
     -> Fay ()
callWithErrorHandling f g h = ajaxCommandWithErrorHandling (f Returns) g h

-- | Run the AJAX command.
ajaxCommand :: Automatic command
            -> (Automatic a -> Fay ()) -- ^ Success Handler
            -> Fay ()
ajaxCommand = ffi "jQuery['ajax']({ url: window['yesodFayCommandPath'], type: 'POST', data: { json: JSON.stringify(%1) }, dataType: 'json', success : %2})"

-- | Run the AJAX command, handling errors as well
ajaxCommandWithErrorHandling
            :: Automatic command
            -> (Automatic a -> Fay ()) -- ^ Success Handler
            -> (Fay ())      -- ^ Failure Handler
            -> Fay ()
ajaxCommandWithErrorHandling = ffi "jQuery['ajax']({ url: window['yesodFayCommandPath'], type: 'POST', data: { json: JSON.stringify(%1) }, dataType: 'json', success : %2, error: %3})"

