module ParseArg (
  argHelp,
  readBzQueryArg,
  ArgType(..),
  ProductVersion(..),
  statusList
  )
where

import Control.Applicative ((<|>))
import Data.Char
import Data.List.Extra as L
import Data.Version.Extra
import Numeric.Natural

argHelp :: String
argHelp = "[COMPONENT|STATUS|PRODUCTVERSION|FIELD=VALUE]..."

data ArgType = ArgProdVer ProductVersion
             | ArgStatusAll
             | ArgParameter String String
             | ArgOther String

readBzQueryArg :: String -> Maybe ArgType
readBzQueryArg s =
  ArgProdVer <$> readProductVersion s <|>
  parseStatus s <|>
  parseParam s <|>
  pure (ArgOther s)

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

parseStatus :: String -> Maybe ArgType
parseStatus s =
  let caps = upper s in
    if caps == "ALL"
    then Just ArgStatusAll
    else
      ArgParameter "bug_status" <$> find (== upper s) statusList

parseParam :: String -> Maybe ArgType
parseParam ps =
  case splitOn "=" ps of
    [a,b] -> if null a || null b
             then error $ "bad parameter: " ++ ps
             else Just (ArgParameter a b)
    _ -> Nothing
