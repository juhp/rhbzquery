module ParseArg (
  argHelp,
  readBzQueryArg,
  ArgType(..),
  Operator(..),
  showOp,
  ProductVersion(..),
  statusList
  )
where

import Control.Applicative ((<|>))
import Data.Char
import Data.List.Extra as L
import Data.Version.Extra
import Numeric.Natural

import Common

argHelp :: String
argHelp = "[COMPONENT|STATUS|PRODUCTVERSION|FIELD=VALUE|FIELDopVALUE]..."

data Operator = Equals | NotEqual
              | Contains | NotContain
              | ContainsCase
              | Regexp |NotRegexp
              | ContainsAll | NotContainAll
              | ContainsWords | NotContainWords
              -- | ContentMatches | ContentNotMatch
  deriving Eq

showOp :: Operator -> String
showOp Equals = "equals"
showOp NotEqual = "notequals"
showOp Contains = "substring"
showOp NotContain = "notsubstring"
showOp ContainsCase = "casesubstring"
showOp Regexp = "regexp"
showOp NotRegexp = "notregexp"
showOp ContainsAll = "allwordssubstr"
showOp NotContainAll = "nowordssubstr"
showOp ContainsWords = "allwords"
showOp NotContainWords = "nowords"
--showOp IsEmpty = "isempty"
--showOp IsNotEmpty = "isnotempty"

data ArgType = ArgProdVer ProductVersion
             | ArgStatusAll
             | ArgParameter String Operator String
             | ArgParameterEmpty String Bool
             | ArgComponent String

readBzQueryArg :: String -> Maybe ArgType
readBzQueryArg s =
  ArgProdVer <$> readProductVersion s <|>
  parseStatus s <|>
  parseParam s <|>
  parseComponent s

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
      ArgParameter "bug_status" Equals <$> find (== upper s) statusList

parseParam :: String -> Maybe ArgType
parseParam ps =
  parseParamWith "!=~" NotRegexp <|>
  parseParamWith "=~" Regexp <|>
  parseParamWith "!=" NotEqual <|>
  parseParamWith "=" Equals <|>
  parseParamWith "!~" NotContain <|>
  parseParamWith "~a~" ContainsAll <|>
  parseParamWith "!a~" NotContainAll <|>
  parseParamWith "~c~" ContainsCase <|>
  parseParamEmpty "~e~" True <|>
  parseParamEmpty "!e~" False <|>
  parseParamWith "~w~" ContainsWords <|>
  parseParamWith "!w~" NotContainWords <|>
--  parseParamWith "~m~" ContentMatches <|>
--  parseParamWith "!m~" ContentNotMatch <|>
  parseParamWith "~" Contains
  where
    parseParamWith :: String -> Operator -> Maybe ArgType
    parseParamWith op oper =
      if op `isInfixOf` ps then
        case splitOn op ps of
          (f:val) -> Just (ArgParameter f oper (intercalate op val))
          _ -> Nothing
      else Nothing

    parseParamEmpty :: String -> Bool -> Maybe ArgType
    parseParamEmpty op empty =
      case stripSuffix op ps of
        Just a -> if null a
                  then error' $ "bad parameter: " ++ ps
                  else Just (ArgParameterEmpty a empty)
        _ -> Nothing

-- https://fedoraproject.org/wiki/Packaging:Naming?rd=Packaging:NamingGuidelines#Common_Character_Set_for_Package_Naming
-- abcdefghijklmnopqrstuvwxyz
-- ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- 0123456789-._+
-- Bugzilla components can contain a space though
parseComponent :: String -> Maybe ArgType
parseComponent ps =
  if all isPackageChar ps
  then Just (ArgComponent ps)
  else error' $ "Invalid component name: " ++ ps
  where
    isPackageChar :: Char -> Bool
    isPackageChar c =
      isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` " -._+"
