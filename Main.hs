{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import qualified Data.ByteString.Char8 as B
--import qualified Data.ByteString.Builder as BL
import Data.Char
import Data.Either
import qualified Data.List as L
import Data.Maybe
--import qualified Data.Text as T
import Data.Version.Extra
import Network.HTTP.Types
import Numeric.Natural
#if MIN_VERSION_simple_cmd_args(0,1,7)
#else
import Options.Applicative
#endif
import SimpleCmdArgs
import System.FilePath

data ProductVersion = Fedora (Maybe Natural)
                    | Rawhide
                    | EPEL (Maybe Natural)
                    | RHEL Version

data BzFields = BzProduct
              | BzVersion
              | BzComponent
              | BzStatus
--              | BzFlag String
              | BzMeta Char Natural
  deriving Eq

instance Show BzFields where
  show BzProduct = "product"
  show BzVersion = "version"
  show BzComponent = "component"
  show BzStatus = "bug_status"
  -- showField (BzFlag f) =
  show (BzMeta c n) = c: show n

data ArgType = ArgProdVer ProductVersion
             | ArgSST String
             | ArgStatus String
--             | ArgParameter
             | ArgOther String
--  deriving Eq

readBzQueryParam :: String -> ArgType
readBzQueryParam s =
  case readProductVersion s of
    Just prodver -> ArgProdVer prodver
    Nothing ->
      if "sst_" `L.isPrefixOf` s then ArgSST s
      else
        let caps = map toUpper s in
        if caps `elem` statusList
        then ArgStatus caps
        else ArgOther s

readProductVersion :: String -> Maybe ProductVersion
readProductVersion "fedora" = Just (Fedora Nothing)
readProductVersion "rawhide" = Just Rawhide
readProductVersion ('f':ver) | all isDigit ver = Just $ Fedora (Just (read ver :: Natural))
readProductVersion "epel" = Just (EPEL Nothing)
readProductVersion ('e':'p':'e':'l':v) | all isDigit v = Just (EPEL (Just (read v :: Natural)))
readProductVersion ('r':'h':'e':'l':ver) = Just $ RHEL (readVersion ver)
readProductVersion _ = Nothing

statusList :: [String]
statusList = ["NEW", "ASSIGNED", "POST", "MODIFIED", "ON_QA", "VERIFIED", "RELEASE_PENDING", "CLOSED"]

argToFields :: Natural -> ArgType -> (Natural,[(BzFields,String)])
argToFields i arg =
  case arg of
    (ArgProdVer prodver) -> (i,productVersionQuery prodver)
    (ArgSST sst) -> (i+1,[(BzMeta 'f' i,"agile_team.name")
                         ,(BzMeta 'o' i,"equals")
                         ,(BzMeta 'v' i,sst)])
    (ArgStatus st) -> (i,[(BzStatus,st)])
    (ArgOther c) -> (i,[(BzComponent,c)])

productVersionQuery :: ProductVersion -> [(BzFields,String)]
productVersionQuery (Fedora Nothing) = [(BzProduct, "Fedora")]
productVersionQuery Rawhide = [(BzProduct, "Fedora")
                              ,(BzVersion, "rawhide")]
productVersionQuery (Fedora (Just n)) = [(BzProduct, "Fedora")
                                        ,(BzVersion, show n)]
productVersionQuery (EPEL Nothing) = [(BzProduct, "Fedora EPEL")]
productVersionQuery (EPEL (Just n)) = [(BzProduct, "Fedora EPEL")
                                      ,(BzVersion, show n)]
productVersionQuery (RHEL ver) =
  case versionBranch ver of
    [] -> error "Can't search RHEL without version"
    [major] -> [(BzProduct, "Red Hat Enterprise Linux " ++ show major)]
    (major:_) ->  [(BzProduct, "Red Hat Enterprise Linux " ++ show major)
                  ,(BzVersion, showVersion ver)]

main :: IO ()
main = simpleCmdArgs Nothing "Bugzilla query tool"
       "Tool for query bugzilla" $
       run <$> some (strArg "QUERY")

brc :: B.ByteString
brc = "bugzilla.redhat.com"

run :: [String] -> IO ()
run args = do
  let params = (numberMetaFields . map readBzQueryParam) args
      status = [(BzStatus, "__open__") | not (isStatusSet params)]
      query = L.nub $ status ++ params
  B.putStrLn $ "https://" <> brc <> "/buglist.cgi" <> renderQuery True (bzQuery query)

bzQuery :: [(BzFields,String)] -> Query
bzQuery = map (bimap (B.pack . show) (Just . B.pack))

numberMetaFields :: [ArgType] -> [(BzFields,String)]
numberMetaFields =
  snd . foldr (\arg (i,flds) -> let (i',fld) = argToFields i arg in (i', flds ++ fld)) (0,[])

isStatusSet :: [(BzFields,String)] -> Bool
isStatusSet [] = False
isStatusSet ((BzStatus, _):_) = True
isStatusSet (_:rest) = isStatusSet rest
