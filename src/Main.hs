{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

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
                            some
#endif
  )
import SimpleCmd
import SimpleCmdArgs
import System.Directory

import Bugzilla
import Fields
import Help
import ParseArg
import Paths_rhbzquery
import User

main :: IO ()
main =
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Bugzilla query tool" <> progDescDoc (Just detailedHelp)) $
  run <$>
  switchWith 'n' "dryrun" "Do not open url" <*>
  switchWith 'm' "mine" "My bugs" <*>
  switchWith 'f' "file" "File a bug" <*>
  some (strArg argHelp)
  where
    run :: Bool -> Bool -> Bool -> [String] -> IO ()
    run dryrun mine file args = do
      user <- if mine && not file
        then do
        mail <- getBzUser
        return [ArgParameter "assigned_to" mail]
        else return []
      let argtypes = mapMaybe readBzQueryArg args
          url = if file
            then
            let query = L.nub $ concatMap argToSimpleField argtypes
            in "https://" <> B.pack brc <> "/enter_bug.cgi" <> renderQuery True (bzQuery query)
            else
            let status = [ArgParameter "bug_status" "__open__" | not (hasStatusSet argtypes)]
                query = L.nub $ numberMetaFields $ status ++ user ++ argtypes
            in "https://" <> B.pack brc <> "/buglist.cgi" <> renderQuery True (bzQuery query)
      B.putStrLn url
      unless dryrun $ do
        whenJustM (findExecutable "xdg-open") $ \xdgOpen ->
          cmd_ xdgOpen [B.unpack url]

    hasStatusSet :: [ArgType] -> Bool
    hasStatusSet [] = False
    hasStatusSet (ArgStatusAll:_) = True
    hasStatusSet ((ArgParameter "bug_status" _):_) = True
    hasStatusSet ((ArgParameter "status" _):_) = True
    hasStatusSet (_:rest) = hasStatusSet rest

    numberMetaFields :: [ArgType] -> [(BzFields,String)]
    numberMetaFields =
      snd . foldr (\arg (i,flds) -> let (i',fld) = argToFields i arg in (i', fld ++ flds)) (0,[])

    bzQuery :: [(BzFields,String)] -> Query
    bzQuery = map (bimap (B.pack . show) (Just . B.pack))
