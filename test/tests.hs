import Control.Monad
import SimpleCmd

rhbzquery :: ([String], String) -> IO ()
rhbzquery (args,expect) = do
  out <- cmd "rhbzquery" ("-n" : args)
  let cgi = if "--file" `elem` args then "enter_bug" else "buglist"
      req = removePrefix ("https://bugzilla.redhat.com/" ++ cgi ++ ".cgi?") out
  unless (req == expect) $ do
    cmdN "rhbzquery" ("-n" : args)
    putStrLn $ "returned> " ++ req
    putStrLn $ "expected> " ++ expect
    error' "failed"

tests :: [([String],String)]
tests =
  [(["f33", "pango"],
    "bug_status=__open__&product=Fedora&version=33&component=pango")
  ,(["closed", "rawhide", "xyz"],
    "bug_status=CLOSED&product=Fedora&version=rawhide&component=xyz")
  ,(["rhel8.3", "bash"],
    "bug_status=__open__&product=Red%20Hat%20Enterprise%20Linux%208&version=8.3&component=bash")
  ,(["rhel8", "bash"],
    "bug_status=__open__&product=Red%20Hat%20Enterprise%20Linux%208&component=bash")
  ,(["Package Review", "reporter_realname=Your Name"],
    "bug_status=__open__&component=Package%20Review&reporter_realname=Your%20Name")
  ,(["flag=fedora-review+"],
    "bug_status=__open__&f0=flagtypes.name&o0=substr&v0=fedora-review%2B")
  ,(["summary=bugzilla"],
    "bug_status=__open__&f0=short_desc&o0=substr&v0=bugzilla")
  ,(["--file", "f33", "bugzilla"],
    "product=Fedora&version=33&component=bugzilla")
  ]

main :: IO ()
main = do
  mapM_ rhbzquery tests
  putStrLn $ show (length tests) ++ " tests run"
