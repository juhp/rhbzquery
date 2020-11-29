module ParseArg (
  readBzQueryParam,
  ArgType(..),
  ProductVersion(..)
  )
where

import Data.Char
import Data.List.Extra as L
import Data.Version.Extra
import Numeric.Natural

data ArgType = ArgProdVer ProductVersion
             | ArgSST String
             | ArgStatus String
             | ArgParameter String String
             | ArgOther String
--  deriving Eq

readBzQueryParam :: String -> ArgType
readBzQueryParam s =
  case readProductVersion s of
    Just prodver -> ArgProdVer prodver
    Nothing ->
      if "sst_" `L.isPrefixOf` s then ArgSST s
      else
        let caps = upper s in
        if caps `elem` statusList
        then ArgStatus caps
        else case parseParam s of
               Just (p,v) -> ArgParameter p v
               Nothing -> ArgOther s

data ProductVersion = Fedora (Maybe Natural)
                    | Rawhide
                    | EPEL (Maybe Natural)
                    | RHEL Version

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

parseParam :: String -> Maybe (String,String)
parseParam ps =
  case splitOn "=" ps of
    [a,b] -> if null a || null b
             then error $ "bad parameter: " ++ ps
             else Just (a,b)
    _ -> Nothing
