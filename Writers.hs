module Lib.HGit.Writers
  ( runGitCommand
  , gitInit
  , gitClone
  , gitAdd
  , gitAddFiles
  , gitMv
  , gitRm
  , gitCommit
  , gitTagNew
  , gitTagRm
  ) where

import           System.Command
import           System.IO
import           Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)
import           Data.Text.Encoding as E
import           Data.Maybe

import           Lib.HGit.Type
--Runs a command, returns true if command was successful 
--Returns false otherwise, error is printed to terminal

runGitCommand :: GitCommand -> GitReader Bool 
runGitCommand cmd = do 
  (_,_,errh,_) <- spawnGitProcess cmd
  liftIO $ do 
    e <-T.hGetContents errh
    if e == T.empty 
      then return True
      else do
        putStrLn "Git Error: "
        T.putStrLn e
        return False

gitInit :: GitReader Bool
gitInit = runGitCommand $ makeGitCommand (T.pack "init") []

gitClone :: Text -> GitReader Bool
gitClone url = runGitCommand cmd
  where cmd = makeGitCommand (T.pack "clone") [url]

gitAdd :: FilePath -> GitReader Bool
gitAdd file = gitAddFiles [file]

gitAddFiles :: [FilePath] -> GitReader Bool
gitAddFiles files = runGitCommand cmd
  where cmd = makeGitCommand (T.pack "add") $ map T.pack files

gitMv :: FilePath -> FilePath -> GitReader Bool
gitMv source destination = runGitCommand cmd
  where cmd = makeGitCommand (T.pack "mv") $ map T.pack ["-f", source, destination]

gitRm :: FilePath -> GitReader Bool
gitRm file = runGitCommand $ makeGitCommand (T.pack "rm") [T.pack file]

gitCommit :: Maybe FilePath -> Text -> GitReader Bool
gitCommit file message = runGitCommand cmd
  where cmd = makeGitCommand (T.pack "commit") args
        args = f ++ [T.pack "-m", message]
        f = maybe [T.pack "-a"] f' file
        f' z = [T.pack "-f", T.pack z]

gitTagNew :: Text -> Maybe Text -> Maybe CommitID -> GitReader Bool
gitTagNew title message commit = runGitCommand cmd  
  where cmd = makeGitCommand (T.pack "tag") args
        args = [title] ++ msg ++ comm
        comm = maybe [] singleton commit 
        msg = maybe [] (\x -> T.pack "-m" : singleton x) message
        singleton x = [x]
        
gitTagRm :: Text -> GitReader Bool
gitTagRm tag = runGitCommand $ makeGitCommand (T.pack "tag") [T.pack "-d", tag]
