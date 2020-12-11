{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if !MIN_VERSION_simple_cmd_args(0,1,3)
import Control.Applicative ((<|>))
#endif
import Control.Monad.Extra
import Data.Bifunctor
import qualified Data.ByteString.Char8 as B
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.List as L
import Network.HTTP.Types
import Options.Applicative (fullDesc, header, progDescDoc,
#if !MIN_VERSION_simple_cmd_args(0,1,4)
                            many
#endif
  )
import SimpleCmd (cmd_)
import SimpleCmdArgs
import System.Directory

import Bugzilla
import Common
import Fields
import Help
import ParseArg
import Paths_rhbzquery
import User

data QueryMode = BugList | ListFields | ListOperators | CreateBug | APIQuery

main :: IO ()
main =
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Bugzilla query tool" <> progDescDoc (Just detailedHelp)) $
  run <$>
  switchWith 'n' "dryrun" "Do not open url" <*>
  switchWith 'm' "mine" "My bugs" <*>
  (flagWith' ListFields 'l' "list-fields" "List query FIELDs" <|>
   flagWith' ListOperators 'o' "list-operators" "List op search operator types" <|>
   flagWith' CreateBug 'f' "file" "File a bug" <|>
   flagWith BugList APIQuery 'w' "api" "Web API query") <*>
  many (strArg argHelp)
  where
    run :: Bool -> Bool -> QueryMode -> [String] -> IO ()
    run _ _ ListFields _ = mapM_ putStrLn allBzFields
    run _ _ ListOperators _ = do
      mapM_ putStrLn $ map showOpHelp operators ++
        ["", "content~ uses matches", "content!~ uses notmatches"]
    -- FIXME should really use some and many
    run _ _ _ [] = error' "please give an argument or --help"
    run dryrun mine mode args = do
      user <- if mine
        then do
        mail <- getBzUser
        return [ArgParameter "assigned_to" Equals mail]
        else return []
      let argtypes = mapMaybe readBzQueryArg args
          url =
            let query =
                  case mode of
                    CreateBug -> L.nub $ concatMap argToSimpleField argtypes
                    _ ->
                      let status = [ArgParameter "bug_status" Equals "__open__" | not (hasStatusSet argtypes)]
                      in L.nub $ numberMetaFields $ status ++ user ++ argtypes
            in queryURL mode query
      B.putStrLn url
      unless dryrun $ do
        whenJustM (findExecutable "xdg-open") $ \xdgOpen ->
          cmd_ xdgOpen [B.unpack url]

    queryURL :: QueryMode -> [(BzFields,String)] -> B.ByteString
    queryURL mode query =
      "https://" <> B.pack brc <> "/" <>
      case mode of
        APIQuery -> "rest/bug"
        BugList -> "buglist.cgi"
        CreateBug -> "enter_bug.cgi"
        ListFields -> "query.cgi" -- unused
        ListOperators -> "query.cgi" -- unused
      <> renderQuery True (bzQuery query)

    hasStatusSet :: [ArgType] -> Bool
    hasStatusSet [] = False
    hasStatusSet (ArgStatusAll:_) = True
    hasStatusSet ((ArgParameter "bug_status" _ _):_) = True
    hasStatusSet ((ArgParameter "status" _ _):_) = True
    hasStatusSet (_:rest) = hasStatusSet rest

    numberMetaFields :: [ArgType] -> [(BzFields,String)]
    numberMetaFields =
      snd . foldr (\arg (i,flds) -> let (i',fld) = argToFields i arg in (i', fld ++ flds)) (0,[])

    bzQuery :: [(BzFields,String)] -> Query
    bzQuery = map (bimap (B.pack . show) (Just . B.pack))
