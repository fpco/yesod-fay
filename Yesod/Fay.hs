{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | Utility functions for using Fay from a Yesod application.
--
-- This module is intended to be used from your Yesod application, not from
-- your Fay programs.
--
-- We assume a specific file structure, namely that there is a @fay@ folder
-- containing client-side code, and @fay-shared@ containing code to be used by
-- both the client and server.
--
-- The @Language.Fay.Yesod@ module (part of this package) is
-- required by both client and server code. However, since Fay does not
-- currently have package management support, we use a bit of a hack: the TH
-- calls in this package will automatically create the necessary
-- @fay\/Language\/Fay\/Yesod.hs@ file.  Ultimately, we will use a more elegant
-- solution.
--
-- In the future, if this package proves popular enough, Fay support will
-- likely be built into the scaffolding. In the meantime, you must manually
-- integrate it. In order to take advantage of this module, you should modify
-- your Yesod application as follows:
--
-- * Modify your @cabal@ file to include the @fay-shared@ folder when
--   compiling. This can be done by adding a @hs-source-dirs: ., fay-shared@ line
--   to your library section.
--
-- * Create the module @SharedTypes@ in @fay-shared@ and create a @Command@
--   datatype. For an example of what this file should look like, see
--   <https://github.com/snoyberg/yesod-fay/blob/master/sample/fay-shared/SharedTypes.hs>.
--
-- * Add the function @fayFile@ to your @Import@ module. See
--   <https://github.com/snoyberg/yesod-fay/blob/master/sample/Import.hs> for an
--   example.
--
-- * Add a new route at for the Fay subsite. Generally, it will look like
--   @/fay-command FaySiteR FaySite getFaySite@.
--
-- * Import the @SharedTypes@ and @Yesod.Fay@ modules into your @Foundation.hs@
--   module. Add an instance of @YesodFay@ for your application. You should set
--   the @YesodFayCommand@ associated type to the @Command@ datatype you created.
--   (You may also need to add a @YesodJquery@ instance.) Note that this instance
--   must appear after your @parseRoutes@. Set the method @fayRoute@ to
--   @FaySiteR@ (or whatever you called the subsite route), and implement
--   @yesodFayCommand@. It will generally look something like @yesodFayCommand
--   render command = case command of { ... }@.
--
-- * In order to use Fay, add @$(fayFile \"MyModule\")@ to a widget, and then
--   write the corresponding @fay/MyModule.hs@ file. For an example, see
--   <https://github.com/snoyberg/yesod-fay/blob/master/sample/fay/Home.hs>.
module Yesod.Fay
    ( -- * Typeclass
      YesodFay (..)
    , YesodFaySettings (..)
    , yesodFaySettings
      -- * Include Fay programs
    , fayFileProd
    , fayFileReload
    , FayFile
      -- * Commands
    , CommandHandler
    , Returns
      -- * Subsite
    , FaySite
    , getFaySite
    , Route (..)
      -- * Reexports
    , YesodJquery (..)
    ) where

import           Control.Monad              (unless, when)
import           Control.Monad.Loops        (anyM)
import           Control.Applicative
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.UTF8  as BSU
import           Data.Default               (def)
import           Data.Digest.Pure.MD5       (md5)
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (isNothing)
import           Data.Monoid                ((<>), mempty)
import           Data.Text                  (pack, unpack)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Text.Lazy.Builder     (fromText, toLazyText, Builder)
import           Filesystem                 (createTree, isFile, readTextFile)
import           Filesystem.Path.CurrentOS  (directory, encodeString, decodeString)
import           Fay                        (compileFileWithState, CompileState(..), getRuntime)
import           Fay.Convert                (showToFay)
import           Fay.Types                  (CompileConfig(..),
                                             configDirectoryIncludes,
                                             configTypecheck,
                                             configExportRuntime,
                                             CompileError)
import           Language.Fay.Yesod         (Returns (Returns))
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL),
                                             Q,
                                             qAddDependentFile, qRunIO)
import           System.Environment         (getEnvironment)
import           System.Directory
import           Text.Julius                (Javascript (Javascript), julius)
import           Yesod.Core
import           Yesod.Form.Jquery          (YesodJquery (..))
import           Yesod.Static
import           Yesod.Fay.Data

jsMainCall :: Bool -> String -> Builder
jsMainCall False _ = mempty
jsMainCall True mn' =
    "Fay$$_(" <> mn <> ".main);"
  where
    mn = fromText $ T.pack mn'

-- | Type class for applications using Fay.
--
-- We depend on @YesodJquery@ since the generated client-side code uses jQuery
-- for making Ajax calls. We have an associated type stating the command
-- datatype. Since this datatype must be used by both the client and server,
-- you should place its definition in the @fay-shared@ folder.
class YesodJquery master => YesodFay master where
    -- | User-defined function specifying how to respond to commands. Using the
    -- above datatype, this might look like:
    --
    -- > yesodFayCommand render command =
    -- >     case command of
    -- >         GetFib index r = render r $ fibs !! index
    yesodFayCommand :: CommandHandler master

    -- | Where in the routing tree our Fay subsite is located. This is
    -- generally named @FaySiteR@, e.g.:
    --
    -- > mkYesod "YourSite" [parseRoutes
    -- > ...
    -- > /fay-command FaySiteR FaySite getFaySite
    -- > |]
    -- >
    -- > instance YesodFay YourSite where
    -- >     fayRoute = FaySiteR
    fayRoute :: Route FaySite -> Route master

-- | A function provided by the developer describing how to answer individual
-- commands from client-side code.
--
-- Due to restrictions of the type system in Fay, we use a relatively simple
-- approach for encoding the return type. In order to specify this, an extra
-- parameter- @Returns@- is passed around, with a phantom type variable stating
-- the expected return type.
--
-- The first argument to your function is the \"respond\" function: it takes
-- the extra @Returns@ parameter as well as the actual value to be returned,
-- and produces the expected result.
type CommandHandler master
    = forall s.
      (forall a. Show a => Returns a -> a -> HandlerT master IO s)
   -> Value
   -> HandlerT master IO s

-- | A setttings data type for indicating whether the generated Javascript
-- should contain a copy of the Fay runtime or not.
data YesodFaySettings = YesodFaySettings
    { yfsModuleName      :: String
    , yfsSeparateRuntime :: Maybe (FilePath, Exp)
    , yfsPostProcess     :: String -> IO String
    , yfsExternal        :: Maybe (FilePath, Exp)
    , yfsRequireJQuery   :: Bool
    -- ^ Note that the server call functions provided for your Fay code require
    -- jQuery to be present. If you disable this option and still use the
    -- provided server call functions, your code will break.
    }

yesodFaySettings :: String -> YesodFaySettings
yesodFaySettings moduleName = YesodFaySettings moduleName Nothing return Nothing True

updateRuntime :: FilePath -> IO ()
updateRuntime fp = getRuntime >>= \js -> createTree (directory $ decodeString fp) >> copyFile js fp

instance YesodFay master => YesodSubDispatch FaySite (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesFaySite)

-- | To be used from your routing declarations.
getFaySite :: a -> FaySite
getFaySite _ = FaySite

postFayCommandR :: YesodFay master => HandlerT FaySite (HandlerT master IO) Value
postFayCommandR =
    lift $ runCommandHandler yesodFayCommand
  where
    -- | Run a command handler. This provides server-side responses to Fay queries.
    runCommandHandler :: YesodFay master
                      => CommandHandler master
                      -> HandlerT master IO Value
    runCommandHandler f = do
        mtxt <- lookupPostParam "json"
        case mtxt of
            Nothing -> invalidArgs ["No JSON provided"]
            Just txt ->
                case decode (L.fromChunks [encodeUtf8 txt]) of
                    Nothing -> error $ "Unable to parse input: " ++ show txt
                    Just cmd -> f go cmd
      where
        go Returns = returnJson . showToFay

langYesodFay :: String
langYesodFay = $(qRunIO $ fmap (LitE . StringL . unpack) $ readTextFile "Language/Fay/Yesod.hs")

writeYesodFay :: IO ()
writeYesodFay = do
    let fp = "fay/Language/Fay/Yesod.hs"
        content = "-- NOTE: This file is auto-generated.\n" ++ langYesodFay
    exists <- isFile fp
    mcurrent <-
        if exists
            then fmap (Just . unpack) $ readTextFile fp
            else return Nothing
    unless (mcurrent == Just content) $ do
        createTree $ directory fp
        writeFile (encodeString fp) content

maybeRequireJQuery :: YesodFay master => Bool -> WidgetT master IO ()
maybeRequireJQuery needJQuery = when needJQuery requireJQuery

requireJQuery :: YesodFay master => WidgetT master IO ()
requireJQuery = do
    master <- getYesod
    addScriptEither $ urlJqueryJs master
    render <- getUrlRender
    -- FIXME get rid of toLazyText call below
    toWidgetHead [julius|window.yesodFayCommandPath = #{toJSON $ render $ fayRoute FayCommandR};|]

mkfp :: String -> FilePath
mkfp name = "fay/" ++ name ++ ".hs"

requireFayRuntime :: YesodFaySettings -> Q Exp
requireFayRuntime settings = do
    maybe (return ())
        (\(path,_) -> qRunIO $ updateRuntime (path ++ "/fay-runtime.js"))
        (yfsSeparateRuntime settings)
    case yfsSeparateRuntime settings of
        Nothing -> [| return () |]
        Just (_, exp') -> do
            hash <- qRunIO $ getRuntime >>= fmap base64md5 . L.readFile
            [| addScript ($(return exp') (StaticRoute ["fay-runtime.js"] [(T.pack hash, "")])) |]

-- | A function that takes a String giving the Fay module name, and returns an
-- TH splice that generates a @Widget@.
type FayFile = String -> Q Exp

compileFayFile :: FilePath
               -> CompileConfig
               -> IO (Either CompileError String)
compileFayFile fp conf = do
  result <- getFileCache fp
  case result of
    Right cache -> return (Right cache)
    Left refreshTo -> do
      packageConf <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
      result <- compileFileWithState conf {
          configPackageConf = packageConf
        } fp
      case result of
        Left e -> return (Left e)
        Right (source,state) -> do
          let files = stateImported state
              (fp_hi,fp_o) = refreshTo
          writeFile fp_hi (unlines (filter ours (map snd files)))
          writeFile fp_o source
          return (Right source)

  where ours x = isPrefixOf "fay/" x || isPrefixOf "fay-shared/" x

-- | Return the cached output file of the given source file, if:
--
--  1. The source file hasn't changed.
--  2. Any of the source file's dependencies haven't changed.
--
--  Otherwise, return filepaths needed to store meta data and the new cache.
--
getFileCache :: FilePath -> IO (Either (FilePath,FilePath) String)
getFileCache fp = do
  let dir = "dist/yesod-fay-cache/"
      guid = show (md5 (BSU.fromString fp))
      fp_hi = dir ++ guid ++ ".hi"
      fp_o = dir ++ guid ++ ".o"
      refresh = return $ Left (fp_hi,fp_o)
  createDirectoryIfMissing True dir
  exists <- doesFileExist fp_hi
  if not exists
     then refresh
     else do thisModTime <- getModificationTime fp_o
             modules <- fmap ((fp :) . lines) (readFile fp_hi)
             changed <- anyM (fmap (> thisModTime) . getModificationTime) modules
             if changed
                then refresh
                else fmap Right (readFile fp_o)

-- | Does a full compile of the Fay code via GHC for type checking, and then
-- embeds the Fay-generated Javascript as a static string. File changes during
-- runtime will not be reflected.
fayFileProd :: YesodFaySettings -> Q Exp
fayFileProd settings = do
    let needJQuery = yfsRequireJQuery settings
    qAddDependentFile fp
    qRunIO writeYesodFay
    eres <- qRunIO $ compileFayFile fp config
        { configExportRuntime = exportRuntime
        }
    case eres of
        Left e -> error $ "Unable to compile Fay module \"" ++ name ++ "\": " ++ show e
        Right s -> do
            s' <- qRunIO $ yfsPostProcess settings s
            let contents = fromText (pack s') <> jsMainCall (not exportRuntime) name

            case yfsExternal settings of
                Nothing ->
                    [| do
                        maybeRequireJQuery needJQuery
                        $(requireFayRuntime settings)
                        toWidget $ const $ Javascript $ fromText $ pack
                          $(return $ LitE $ StringL $ TL.unpack $ toLazyText contents)
                    |]
                Just (fp', exp') -> do
                    let name' = concat ["faygen-", hash, ".js"]
                        hash = base64md5 contents'
                        contents' = TLE.encodeUtf8 $ toLazyText contents
                    qRunIO $ L.writeFile (concat [fp', "/", name']) contents'
                    [| do
                        maybeRequireJQuery needJQuery
                        $(requireFayRuntime settings)
                        addScript $ $(return exp') $ StaticRoute [pack name'] []
                        |]
  where
    name = yfsModuleName settings
    exportRuntime = isNothing (yfsSeparateRuntime settings)
    fp = mkfp name

config :: CompileConfig
config = def {
      configDirectoryIncludes
        = (Nothing, "fay")
        : (Nothing, "fay-shared")
        : configDirectoryIncludes def
    }

-- | Performs no type checking on the Fay code. Each time the widget is
-- requested, the Fay code will be compiled from scratch to Javascript.
fayFileReload :: YesodFaySettings -> Q Exp
fayFileReload settings = do
    let needJQuery = yfsRequireJQuery settings
    qRunIO writeYesodFay
    [|
        liftIO (compileFayFile (mkfp name) config
                { configTypecheck = False
                , configExportRuntime = exportRuntime
                })
                >>= \eres -> do
        (case eres of
              Left e -> error $ "Unable to compile Fay module \"" ++ name ++ "\": " ++ show e
              Right s -> do
                maybeRequireJQuery needJQuery
                $(requireFayRuntime settings)
                toWidget (const $ Javascript $ fromText (pack s) <> jsMainCall (not exportRuntime) name))|]
  where
    name = yfsModuleName settings
    exportRuntime = isNothing (yfsSeparateRuntime settings)
