{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Extra
import Data.Bifunctor
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Network.HTTP.Types
import Options.Applicative (fullDesc, header, progDescDoc)
#if !MIN_VERSION_simple_cmd_args(0,1,4)
import Options.Applicative(some)
#endif
import SimpleCmd
import SimpleCmdArgs
import System.Directory

import Bugzilla
import Fields
import Help
import ParseArg
import Paths_bzquery
import User

main :: IO ()
main =
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Bugzilla query tool" <> progDescDoc (Just detailedHelp)) $
  run <$>
  switchWith 'n' "dryrun" "no browser or query" <*>
  switchWith 'm' "mine" "My bugs" <*>
  some (strArg argHelp)
  where
    run :: Bool -> Bool -> [String] -> IO ()
    run dryrun mine args = do
      user <- if mine
        then do
        mail <- getBzUser
        return [(BzParameter "assigned_to", mail)]
        else return []
      let params = (numberMetaFields . map readBzQueryParam) args
          status = [(BzStatus, "__open__") | not (isStatusSet params)]
          query = L.nub $ user ++ status ++ params
          url = "https://" <> B.pack brc <> "/buglist.cgi" <> renderQuery True (bzQuery query)
      -- FIXME check xdg-open available
      unless dryrun $ do
        whenJustM (findExecutable "xdg-open") $ \xdgOpen ->
          cmd_ xdgOpen [B.unpack url]
      B.putStrLn url

    bzQuery :: [(BzFields,String)] -> Query
    bzQuery = map (bimap (B.pack . show) (Just . B.pack))

    numberMetaFields :: [ArgType] -> [(BzFields,String)]
    numberMetaFields =
      snd . foldr (\arg (i,flds) -> let (i',fld) = argToFields i arg in (i', fld ++ flds)) (0,[])

    isStatusSet :: [(BzFields,String)] -> Bool
    isStatusSet [] = False
    isStatusSet ((BzStatus, _):_) = True
    isStatusSet (_:rest) = isStatusSet rest
