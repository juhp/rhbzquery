module Fields (
  BzFields(..),
  argToFields,
  argToSimpleField
  )
where

import Data.Version
import Numeric.Natural

import ParseArg (ArgType(..), Operator(..), ProductVersion(..), showOp)

data BzFields = BzProduct
              | BzVersion
              | BzComponent
              | BzParameter String
              | BzMeta Char Natural
  deriving Eq

instance Show BzFields where
  show BzProduct = "product"
  show BzVersion = "version"
  show BzComponent = "component"
  show (BzParameter f) = f
  show (BzMeta c n) = c: show n

mapField :: String -> String
mapField "itm" = "cf_internal_target_milestone"
mapField "itr" = "cf_internal_target_release"
mapField "status" = "bug_status"
mapField "verified" = "cf_verified"
mapField "sst" = "agile_team.name"
mapField "summary" = "short_desc"
mapField "flag" = "flagtypes.name"
mapField "flags" = "flagtypes.name"
mapField p = p

argToSimpleField :: ArgType -> [(BzFields,String)]
argToSimpleField (ArgProdVer prodver) =
  productVersionQuery prodver
argToSimpleField (ArgParameter p Equals v) =
  [(BzParameter (mapField p), v)]
argToSimpleField (ArgParameter p _ v) =
  error $ "Only '" ++ p ++ "=" ++ v ++ "'allowed here"
argToSimpleField (ArgOther c) =
  [(BzComponent,c)]
-- FIXME or error for "all"?
argToSimpleField _ = []

argToFields :: Natural -> ArgType -> (Natural,[(BzFields,String)])
argToFields i arg =
  case arg of
    ArgProdVer prodver -> (i,productVersionQuery prodver)
    ArgStatusAll  -> (i,[])
    ArgParameter p op v ->
      let param = mapField p
          val = if p == "sst"
                then "sst_" ++ v
                else v
      in if '.' `elem` param || op /= Equals
         then (i+1,[(BzMeta 'f' i, param)
                   ,(BzMeta 'o' i, showOp op)
                   ,(BzMeta 'v' i, val)])
         else (i,[(BzParameter param, val)])
    ArgParameterEmpty p e ->
      let param = mapField p
      in (i+1,[(BzMeta 'f' i, param)
              ,(BzMeta 'o' i, if e then "isempty" else "isnotempty")])
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
