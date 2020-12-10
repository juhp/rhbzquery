module ParseArg (
  argHelp,
  readBzQueryArg,
  ArgType(..),
  Operator(..),
  operators,
  showOp,
  showOpHelp,
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
              | Regexp |NotRegexp
              | Substring | NotSubstring
              | CaseSubstring
              | AllWordsSubstr | NoWordsSubstr
              | AllWords | NoWords
              | IsEmpty | IsNotEmpty
              -- | ContentMatches | ContentNotMatch
  deriving (Eq,Show,Enum,Bounded)

showOp :: Operator -> String
showOp = lower . show

operators :: [Operator]
operators = enumFromTo minBound maxBound

data OperatorData = OpUnary String | OpNull String
  deriving Eq

opSyntax :: OperatorData -> String
opSyntax (OpUnary s) = s
opSyntax (OpNull s) = s

showOpHelp :: Operator -> String
showOpHelp op =
  "'" ++ opSyntax (opData op) ++ "'(" ++ showOp op ++ ")"

instance Ord OperatorData where
  compare op1 op2 =
    let sop1 = opSyntax op1
        sop2 = opSyntax op2
    in
      if sop1 `isInfixOf` sop2 then GT
      else if sop2 `isInfixOf` sop1 then LT
           else if sop1 == sop2
                then error' $ sop1 ++ " listed twice in 'operators'"
                else compare (reverse sop1) (reverse sop2)

-- FIXME check all unique
opData :: Operator -> OperatorData
opData Equals = OpUnary "="
opData NotEqual = OpUnary "!="
opData Substring = OpUnary "~"
opData NotSubstring = OpUnary "!~"
opData CaseSubstring = OpUnary "~c~"
opData Regexp = OpUnary "=~"
opData NotRegexp = OpUnary "!=~"
opData AllWordsSubstr = OpUnary "~a~"
opData NoWordsSubstr = OpUnary "!a~"
opData AllWords = OpUnary "~w~"
opData NoWords = OpUnary "!w~"
opData IsEmpty = OpNull "~e~"
opData IsNotEmpty = OpNull "!e~"
-- -- only for 'content':
-- opData Matches = OpUnary "~m~"
-- opData NotMatches = OpUnary "!m~"

data ArgType = ArgProdVer ProductVersion
             | ArgStatusAll
             | ArgParameter String Operator String
             | ArgParameterEmpty String Bool
             | ArgComponent String

readBzQueryArg :: String -> Maybe ArgType
readBzQueryArg s =
  ArgProdVer <$> readProductVersion s <|>
  parseStatus s <|>
  parseField s <|>
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

parseField :: String -> Maybe ArgType
parseField ps =
  foldr ((<|>) . parseParam) Nothing $ sortOn opData operators
  where
    parseParam :: Operator -> Maybe ArgType
    parseParam oper =
      case opData oper of
        OpUnary op ->
          if op `isInfixOf` ps then
            case splitOn op ps of
              (f:val) -> Just (ArgParameter f oper (intercalate op val))
              _ -> Nothing
          else Nothing
        OpNull op ->
          case stripSuffix op ps of
            Just a ->
              if null a
              then error' $ "bad parameter: " ++ ps
              else Just (ArgParameterEmpty a (oper == IsEmpty))
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
