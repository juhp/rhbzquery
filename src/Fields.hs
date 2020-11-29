module Fields (
  BzFields(..),
  argToFields
  )
where

import Data.Version
import Numeric.Natural

import ParseArg (ArgType(..), ProductVersion(..))

data BzFields = BzProduct
              | BzVersion
              | BzComponent
              | BzStatus
              | BzParameter String
              | BzMeta Char Natural
  deriving Eq

instance Show BzFields where
  show BzProduct = "product"
  show BzVersion = "version"
  show BzComponent = "component"
  show BzStatus = "bug_status"
  show (BzParameter f) = mapFields f
  show (BzMeta c n) = c: show n

mapFields :: String -> String
mapFields "itm" = "cf_internal_target_milestone"
mapFields "itr" = "cf_internal_target_release"
mapFields "verified" = "cf_verified"
mapFields s = s

mapComplex :: String -> String
mapComplex "sst" = "agile_team.name"
mapComplex "flag" = "flagtypes.name"
mapComplex "flags" = "flagtypes.name"
mapComplex p = p

argToFields :: Natural -> ArgType -> (Natural,[(BzFields,String)])
argToFields i arg =
  case arg of
    ArgProdVer prodver -> (i,productVersionQuery prodver)
    ArgSST sst ->
      (i+1,[(BzMeta 'f' i, mapComplex "sst")
           ,(BzMeta 'o' i, "substr")
           ,(BzMeta 'v' i, sst)])
    ArgParameter "sst" v -> argToFields i (ArgSST ("sst_" ++ v))
    ArgStatus st -> (i,[(BzStatus,st)])
    ArgParameter param v ->
      let p = mapComplex param
      in if '.' `elem` p
         then (i+1,[(BzMeta 'f' i, p)
                   ,(BzMeta 'o' i, "substr")
                   ,(BzMeta 'v' i, v)])
         else (i,[(BzParameter p, v)])
    ArgOther c -> (i,[(BzComponent,c)])

productVersionQuery :: ProductVersion -> [(BzFields,String)]
productVersionQuery (Fedora Nothing) = [(BzProduct, "Fedora")]
productVersionQuery Rawhide = [(BzProduct, "Fedora")
                              ,(BzVersion, "rawhide")]
productVersionQuery (Fedora (Just n)) = [(BzProduct, "Fedora")
                                        ,(BzVersion, show n)]
productVersionQuery (EPEL Nothing) = [(BzProduct, "Fedora EPEL")]
productVersionQuery (EPEL (Just n)) = [(BzProduct, "Fedora EPEL")
                                      ,(BzVersion, show n)]
productVersionQuery (RHEL ver) =
  case versionBranch ver of
    [] -> error "Can't search RHEL without version"
    [major] -> [(BzProduct, "Red Hat Enterprise Linux " ++ show major)]
    (major:_) ->  [(BzProduct, "Red Hat Enterprise Linux " ++ show major)
                  ,(BzVersion, showVersion ver)]
