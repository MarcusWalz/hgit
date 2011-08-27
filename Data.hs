{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

import           System.Command
import           System.IO
import           Control.Concurrent
import qualified Control.Exception as C
import           Control.Monad.Reader
import           Data.ByteString.Char8 (ByteString)
import           Data.Maybe
import           Data.List (find)
import qualified Data.ByteString.Char8 as B

data GitConfig  = GitConfig
  { gitCwd   :: FilePath 
  , gitPath  :: Maybe FilePath }
  deriving (Show)

makeGitConfig :: FilePath -> Maybe FilePath -> GitConfig
makeGitConfig cwd' gitPath' = GitConfig {gitCwd = cwd', gitPath = gitPath'}

data GitCommand = GitCommand
  { gitCmd  :: ByteString 
  , args :: [ByteString] }
  deriving (Show)
  
makeGitCommand :: ByteString -> [ByteString] -> GitCommand 
makeGitCommand cmd args' = GitCommand {gitCmd = cmd, args = args'}

newtype GitReader a = GitReader (ReaderT GitConfig IO a)
  deriving (Monad, MonadIO, MonadReader GitConfig)

createGitProcess' :: GitConfig -> GitCommand -> CreateProcess
createGitProcess' GitConfig  { gitCwd = gitCwd', gitPath = gitPath }
                 GitCommand { gitCmd = cmd, args = args'}
  = (proc (fromMaybe "git" gitPath) args'')
  { std_in = CreatePipe 
  , cwd = Just gitCwd' 
  , std_out = CreatePipe 
  , std_err = CreatePipe 
  , close_fds = True }
  where args'' = map B.unpack $ [cmd] ++ args'

createGitProcess :: GitCommand -> GitReader CreateProcess 
createGitProcess command = do
  conf <- ask
  return $ createGitProcess' conf command

-- This function works because of Magic!!! 
spawnGitProcess :: GitCommand 
                -> GitReader (Handle, Handle, Handle, ProcessHandle)
spawnGitProcess command = do
  proc <- createGitProcess command 
  (Just inh, Just outh, Just errh, pid) <- liftIO $ createProcess $ proc 
  return (inh, outh, errh, pid)

runGit :: GitConfig -> GitReader t -> IO t
runGit config (GitReader a) = runReaderT a config

type ID       = ByteString
type CommitID = ID 
type BlobID   = ID 
type TreeID   = ID 
type TagID    = ID

data GitObject = 
                 Commit CommitID 
               | Blob   BlobID
               | Tree   TreeID
               | Tag    TagID
  deriving (Show)

data Person = Person 
  { personName  :: ByteString
  , personEmail :: ByteString
  } deriving (Show)

data Commitent = Commitent
  { ceParents       :: [CommitID]
  , ceTree          :: TreeID
  , ceAuthor        :: Person
  , ceAuthorTime    :: ByteString
  , ceCommitter     :: Person
  , ceCommitterTime :: ByteString 
  , ceCommitMsg     :: ByteString
  } deriving (Show)

data TreeNode = TreeNode
  { mode      :: Int
  , object    :: GitObject
  , name      :: ByteString }
  deriving (Show)

readObjStr' :: ByteString -> Maybe GitObject
readObjStr' str = 
  readObjStr s objT 
  where w = B.words str 
        s = head w
        objT = last w

--wtf!!!!!!!!!

objReader :: [ (ByteString, ID -> GitObject) ]
objReader = [ (B.pack "commit" , Commit )
            , (B.pack "blob"   , Blob   )
            , (B.pack "tag"    , Tag    )
            , (B.pack "tree"   , Tree   ) ]

readObjStr :: ByteString -> ID -> Maybe GitObject
readObjStr t id = find (\(x,n) -> t == x) objReader >>= \(x,n) -> Just (n id)

readTreeNodeLine :: ByteString -> TreeNode
readTreeNodeLine str =
  TreeNode {mode = m, object = o, name = n}
  where m = fst $ fromJust $ B.readInt $ head w 
        o = fromJust $ readObjStr' $ B.unwords $ take 2 $ drop 1 w
        n = B.unwords $ drop 3 w
        w = B.words str

data Trees = Trees [TreeNode]
  deriving (Show)

readProc :: (ByteString -> a) -> Handle -> IO [a]
readProc f h = do
  eof <- hIsEOF h
  if eof 
    then do hClose h
            return [] 
    else do a <- B.hGetLine h
            m <- readProc f h
            return $ [f a] ++ m 
  
readTree :: GitReader [TreeNode]
readTree = do
  (_,outh,_,_) <- spawnGitProcess cmd
  x <- liftIO $ readProc readTreeNodeLine outh
  return x
  where cmd = makeGitCommand (B.pack "ls-tree") [B.pack "HEAD"]
