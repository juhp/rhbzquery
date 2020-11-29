module ParseArg (
  argHelp,
  readBzQueryParam,
  ArgType(..),
  ProductVersion(..)
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
             | ArgStatus String
             | ArgParameter String String
             | ArgOther String
--  deriving Eq

readBzQueryParam :: String -> Maybe ArgType
readBzQueryParam s =
  ArgProdVer <$> readProductVersion s <|>
  ArgStatus <$> parseStatus s <|>
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

parseStatus :: String -> Maybe String
parseStatus s =
  find (== upper s) ["NEW", "ASSIGNED", "POST", "MODIFIED", "ON_QA", "VERIFIED", "RELEASE_PENDING", "CLOSED"]

parseParam :: String -> Maybe ArgType
parseParam ps =
  case splitOn "=" ps of
    [a,b] -> if null a || null b
             then error $ "bad parameter: " ++ ps
             else Just (ArgParameter a b)
    _ -> Nothing
