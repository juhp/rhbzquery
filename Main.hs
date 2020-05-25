{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder as BL
import Data.Maybe
import qualified Data.Text as T
import Data.Version.Extra
import Network.HTTP.Types
#if MIN_VERSION_simple_cmd_args(0,1,7)
#else
import Options.Applicative
#endif
import SimpleCmdArgs
import System.FilePath
import Web.Bugzilla
import Web.Bugzilla.Search

data ProductVersion = FedoraProduct
                    | Fedora Int
                    | Rawhide
                    | EPELProduct
                    | EPEL Int
                    | RHEL Version

readProductVersion :: String -> Maybe ProductVersion
readProductVersion "fedora" = Just FedoraProduct
readProductVersion "rawhide" = Just Rawhide
readProductVersion ('f':ver) = Just $ Fedora $ read ver
readProductVersion "epel" = Just EPELProduct
readProductVersion "epel8" = Just $ EPEL 8
readProductVersion "epel7" = Just $ EPEL 7
readProductVersion ('r':'h':'e':'l':ver) = Just $ RHEL $ readVersion ver
readProductVersion _ = Nothing

productVersionQuery :: ProductVersion -> SearchExpression
productVersionQuery FedoraProduct = ProductField .==. "Fedora"
productVersionQuery Rawhide = ProductField .==. "Fedora" .&&.
                              VersionField .==. "rawhide"
productVersionQuery (Fedora n) = ProductField .==. "Fedora" .&&.
                                 VersionField .==. T.pack (show n)
productVersionQuery EPELProduct = ProductField .==. "Fedora EPEL"
productVersionQuery (EPEL n) = ProductField .==. "Fedora EPEL" .&&.
                               VersionField .==. T.pack (show n)
productVersionQuery (RHEL ver) =
  case versionBranch ver of
    [] -> error "Can't search RHEL without version"
    [major] ->
      ProductField .==. "Red Hat Enterprise Linux " <> T.pack (show major)
    (major:minor) ->
      ProductField .==. "Red Hat Enterprise Linux " <> T.pack (show major) .&&.
      VersionField .==. T.pack (showVersion ver)

main :: IO ()
main = simpleCmdArgs Nothing "Bugzilla query tool"
       "Tool for query bugzilla" $
       run <$> productVersionArg <*> strArg "PACKAGE"
  where
    productVersionArg :: Parser (Maybe ProductVersion)
    productVersionArg = optional (argumentWith (maybeReader readProductVersion) "ProductVersion")

--brc :: T.Text
brc = "bugzilla.redhat.com"

run :: Maybe ProductVersion -> String -> IO ()
run mprodVer pkg = do
  let mprodVerQ = productVersionQuery <$> mprodVer
      component = ComponentField .==. [T.pack pkg]
      status = StatusField .==. "__open__"
      query = case mprodVerQ of
        Nothing -> status .&&. component
        Just prodver -> prodver .&&. status .&&. component
  BL.putStrLn $ "https://" <> brc <> "/buglist.cgi" <> BL.toLazyByteString (renderQueryText True (evalSearchExpr query))
