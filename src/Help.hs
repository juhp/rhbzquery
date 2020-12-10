{-# LANGUAGE CPP #-}

module Help (detailedHelp)
where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Options.Applicative.Help.Pretty as P

import ParseArg (operators, showOpHelp, statusList)

detailedHelp :: P.Doc
detailedHelp =
  P.vcat
  [ P.empty
  , P.text "Tool for generating bugzilla queries"
  , P.empty
  , P.text "STATUS = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map P.text (statusList ++ ["ALL"]))) <> P.rbrace)
  , P.empty
  , P.text "PRODUCTVERSION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map P.text ["rawhide", "fedora", "fXY", "epel", "epelX", "rhel8", "rhel7", "rhelX.Z"])) <> P.rbrace)
  , P.empty
  , P.text "'op' is " <> P.align (P.fillCat (P.punctuate P.comma (map P.text (map showOpHelp operators))))
  , P.empty
  , P.text "See https://github.com/juhp/rhbzquery#readme for examples"
  ]
