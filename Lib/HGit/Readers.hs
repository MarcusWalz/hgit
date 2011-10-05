-- | Querys Git 
module Lib.HGit.Readers
  ( gitHeadID
  --, gitHeadTree
  , getTag
  , getTags
  , getBranch
  , getBranches
  , gitIDFromPath
  , gitReadGitProc
  , readTree
  , readCommit
  , revList 
  , catObject
  , unpackFile ) where

import           System.Command
import           System.IO
import           Control.Monad.Reader
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Internal (c2w)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Text.Lazy.Encoding as T
import           Data.Maybe
import           Data.List (find,(\\))
import           Data.Either
import           System.Directory
import           System.FilePath ((</>))

import           Lib.HGit.Data
import           Lib.HGit.Batch
import           Lib.HGit.Parsers

gitHeadID :: GitReader CommitID
gitHeadID = do
  GitConfig {gitCwd = dir} <- ask
  h <- liftIO $ readFile $ dir </> ".git/HEAD"
  c <- liftIO $ T.readFile $ dir </> ".git" </> init ( drop 5 h ) --the commit
  return $ T.init c

--gitHeadTree :: GitReader Trees
--gitHeadTree = do
--  id <- gitHeadID
--  gitCatBatch (id, Nothing) gitHeadTree' 
--
--gitHeadTree' :: Maybe Bool -> Maybe ByteString -> Either (ID, Maybe Bool) Trees
--gitHeadTree' (Just True) (Just str) = Right $ readTreeString str
--gitHeadTree' Nothing     (Just str) = Left (tr $ readCommitStr str, Just True)
--  where tr Commitent {ceTree = t} = t

getDirectory :: FilePath -> GitReader [FilePath]
getDirectory path = do
  GitConfig { gitCwd = c } <- ask
  x <- liftIO $ getDirectoryContents $ c </> path 
  return $ x \\ [".", ".."]

gitOpenFile :: FilePath -> FilePath -> GitReader CommitID
gitOpenFile path obj = do
    GitConfig { gitCwd = c } <- ask
    x <-liftIO $ T.readFile (c </> path </> obj)
    return $ T.init x --remove \n

-- | Returns a list of tags
getTags :: GitReader [FilePath]
getTags =  getDirectory ".git/refs/tags"

-- | Return the CommitID for a given tag
getTag :: FilePath -> GitReader CommitID
getTag = gitOpenFile ".get/refs/tags" 

-- | Returns a list of Branches 
getBranches :: GitReader [FilePath] 
getBranches = getDirectory ".git/refs/heads"

-- | Returns the head CommitID for a given branch
getBranch :: FilePath -> GitReader CommitID
getBranch = gitOpenFile ".git/refs/heads"

gitIDFromPath :: TreeID -> FilePath -> GitReader (Maybe BlobID)
gitIDFromPath tr path = runGitBatch $ gitIDFromPath' tr path

gitIDFromPath' :: TreeID -> FilePath -> GitBatchM (Maybe BlobID)
gitIDFromPath' tr path = do
  t' <- readObject tr
  let (Trees t) = readTreeString $ fromJust t' 
      pathMatch TreeNode{name = n} = n == p
      fi = find pathMatch t
      p = takeWhile (/= '/') path
      p' = drop (length p + 1) path
      obj TreeNode{object = o} = o
  if isNothing fi then return Nothing
    else if p == path 
      then return $ Just $ idFromGitObject $ obj $ fromJust fi
      else gitIDFromPath' (idFromGitObject$  obj $ fromJust fi) p'

-- | Returns output of git command as Text
gitReadGitProc :: GitCommand -> GitReader (Maybe Text)
gitReadGitProc cmd = do  
    (_,outh, errh, pid) <- spawnGitProcess cmd
    e <- liftIO $ T.hGetContents errh
    if e == T.empty
      then liftIO $ do i <- T.hGetContents outh 
                       return $ Just i
      else liftIO $ do T.putStrLn e
                       return Nothing

readTree :: ID -> GitReader Trees 
readTree tree = do
    (Just x) <- catObject (Tree tree) 
    return $ readTreeString x

readRevListLine :: ID -> GitObject 
readRevListLine = Commit

revList :: CommitID -> GitReader [CommitID]
revList c = runGitBatch $ revList' c

-- | Returns a list of CommitID's in reverse chronological order
revList' :: CommitID -> GitBatchM [CommitID]
revList' id = revList'' id >>= \x -> return $ id : x

revList'' :: CommitID -> GitBatchM [CommitID]
revList'' id = do
  c <- readObject id
  if c == Nothing
    then return []
    else do 
      let parents = parens $ readCommitStr $ fromJust c
      pp <-mapM revList'' parents
      return $ parents ++ (concat pp)
  where parens Commitent {ceParents = p} = p 

-- | Cat GitObjects
gitCatBatch :: [GitObject] -> GitReader [Maybe ByteString]
gitCatBatch o = runGitBatch $ gitCatBatch' o

gitCatBatch' :: [GitObject] -> GitBatchM [Maybe ByteString]
gitCatBatch' = mapM (readObject . idFromGitObject)

catObject :: GitObject -> GitReader (Maybe ByteString)
catObject obj = gitCatBatch [obj] >>= \x -> return $ head x 

-- | Creates a temporary file holding the contents of a GitBlob
unpackFile :: BlobID -> GitReader (Maybe FilePath)
unpackFile blob = do 
    l <- gitReadGitProc cmd
    return $ l >>= Just . T.unpack . T.init
  where cmd = GitCommand (T.pack "unpack-file") [blob]

-- Read a single commit
readCommit :: CommitID -> GitReader (Maybe Commitent)
readCommit id = do 
                  obj <- catObject (Commit id) 
                  return $ obj >>= r 
  where r x = Just $ readCommitStr x
