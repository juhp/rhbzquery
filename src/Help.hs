{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: GPL-2.0-or-later

module Help (detailedHelp)
where

import Data.List.Extra
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Options.Applicative.Help.Pretty as P

import ParseArg (statusList)

detailedHelp :: P.Doc
detailedHelp =
  P.vcat
  [ P.pretty "Tool for generating bugzilla queries"
  , P.pretty ""
  , P.pretty "STATUS = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map P.pretty (map lower statusList ++ ["all (open and closed)", "'<STATE'", "'>STATE'"]))) <> P.rbrace)
  , P.pretty "PRODUCTVERSION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map P.pretty ["rawhide", "fedora", "fXY", "epel", "epelX", "rhel8", "rhel7", "rhelX.Z"])) <> P.rbrace)
  , P.pretty "op = search operator (eg '~' for substring: \"summary~akeyword\")"
  , P.pretty ""
  , P.pretty "See https://github.com/juhp/rhbzquery#readme for examples"
  ]
