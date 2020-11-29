{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Network.HTTP.Types
#if !MIN_VERSION_simple_cmd_args(0,1,7)
import Options.Applicative
#endif
import SimpleCmd
import SimpleCmdArgs

import Fields
import ParseArg

brc :: B.ByteString
brc = "bugzilla.redhat.com"

main :: IO ()
main = simpleCmdArgs Nothing "Bugzilla query tool"
       "Tool for query bugzilla" $
       run <$> some (strArg "[COMPONENT|STATUS|PRODUCTVERSION]...")
  where
    run :: [String] -> IO ()
    run args = do
      let params = (numberMetaFields . map readBzQueryParam) args
          status = [(BzStatus, "__open__") | not (isStatusSet params)]
          query = L.nub $ status ++ params
          url = "https://" <> brc <> "/buglist.cgi" <> renderQuery True (bzQuery query)
      B.putStrLn url
      cmd_ "xdg-open" [B.unpack url]

    bzQuery :: [(BzFields,String)] -> Query
    bzQuery = map (bimap (B.pack . show) (Just . B.pack))

    numberMetaFields :: [ArgType] -> [(BzFields,String)]
    numberMetaFields =
      snd . foldr (\arg (i,flds) -> let (i',fld) = argToFields i arg in (i', flds ++ fld)) (0,[])

    isStatusSet :: [(BzFields,String)] -> Bool
    isStatusSet [] = False
    isStatusSet ((BzStatus, _):_) = True
    isStatusSet (_:rest) = isStatusSet rest
