module User (getBzUser)
where

import Control.Monad.Extra
import qualified Data.ByteString.Char8 as B
import Data.Ini.Config
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath
import qualified Text.Email.Validate as Email

import Bugzilla

getBzUser :: IO String
getBzUser = do
  home <- getEnv "HOME"
  let rc = home </> ".bugzillarc"
  -- FIXME assumption if file exists then it has b.r.c user
  ifM (doesFileExist rc)
    (readIniConfig rc rcParser id) $
    do
    email <- prompt "Bugzilla Username"
    when (emailIsValid email) $ do
      writeFile rc $ "[" <> brc <> "]\nuser = " <> email <> "\n"
      putStrLn $ "Saved in " ++ rc
    getBzUser
  where
    rcParser :: IniParser String
    rcParser =
      section (T.pack brc) $
      fieldOf (T.pack "user") string

    readIniConfig :: FilePath -> IniParser a -> (a -> b) -> IO b
    readIniConfig inifile iniparser fn = do
      ini <- T.readFile inifile
      return $ either error fn $ parseIniFile ini iniparser

    emailIsValid :: String -> Bool
    emailIsValid = Email.isValid . B.pack

prompt :: String -> IO String
prompt s = do
  putStr $ s ++ ": "
  inp <- getLine
  if null inp
    then prompt s
    else return inp