import Control.Monad
import SimpleCmd

rhbzquery :: ([String], String) -> IO ()
rhbzquery (args,expect) = do
  out <- cmd "rhbzquery" ("-n" : args)
  let req = removePrefix "https://bugzilla.redhat.com/" out
  unless (req == expect) $ do
    cmdN "rhbzquery" ("-n" : args)
    putStrLn $ "returned> " ++ req
    putStrLn $ "expected> " ++ expect
    error' "failed"

tests :: [([String],String)]
tests =
  [(["f33", "pango"],
    "buglist.cgi?bug_status=__open__&product=Fedora&version=33&component=pango")
  ,(["closed", "rawhide", "xyz"],
    "buglist.cgi?bug_status=CLOSED&product=Fedora&version=rawhide&component=xyz")
  ,(["rhel8.3", "bash"],
    "buglist.cgi?bug_status=__open__&product=Red%20Hat%20Enterprise%20Linux%208&version=8.3&component=bash")
  ,(["rhel8", "bash"],
    "buglist.cgi?bug_status=__open__&product=Red%20Hat%20Enterprise%20Linux%208&component=bash")
  ,(["Package Review", "reporter_realname=Your Name"],
    "buglist.cgi?bug_status=__open__&component=Package%20Review&reporter_realname=Your%20Name")
  ,(["flag~fedora-review+"],
    "buglist.cgi?bug_status=__open__&f0=flagtypes.name&o0=substring&v0=fedora-review%2B")
  ,(["summary~bugzilla"],
    "buglist.cgi?bug_status=__open__&f0=short_desc&o0=substring&v0=bugzilla")
  ,(["--file", "f33", "bugzilla"],
    "enter_bug.cgi?product=Fedora&version=33&component=bugzilla")
  ,(["--api", "fedora", "component~llvm"],
    "rest/bug?bug_status=__open__&product=Fedora&f0=component&o0=substring&v0=llvm")
  ]

main :: IO ()
main = do
  mapM_ rhbzquery tests
  putStrLn $ show (length tests) ++ " tests run"
