{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Lib.HGit.Type 
  ( runGit
  , GitConfig(..)
  , makeGitConfig
  , GitCommand
  , makeGitCommand
  , GitReader
  , createGitProcess'
  , createGitProcess
  , spawnGitProcess
  , ID
  , CommitID(..)
  , BlobID(..)
  , TagID(..)
  , TreeID(..)
  , GitObject(..)
  , Person(..)
  , Commitent(..)
  , TreeNode(..)
  , Trees(..)
  , readObjStr
  , readObjStr'
  , objReader
  , getIdFromObj
  , getStringFromObj) where
import           System.Command
import           System.IO
import           Control.Monad.Reader
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding as E
import           Data.List (find)

data GitConfig  = GitConfig
  { gitCwd   :: FilePath 
  , gitPath  :: Maybe FilePath }
  deriving (Show)

makeGitConfig :: FilePath -> Maybe FilePath -> GitConfig
makeGitConfig cwd' gitPath' = GitConfig {gitCwd = cwd', gitPath = gitPath'}

data GitCommand = GitCommand
  { gitCmd  :: Text 
  , args :: [Text] }
  deriving (Show)
  
makeGitCommand :: Text -> [Text] -> GitCommand 
makeGitCommand cmd args' = GitCommand {gitCmd = cmd, args = args'}

newtype GitReader a = GitReader (ReaderT GitConfig IO a)
  deriving (Monad, MonadIO, MonadReader GitConfig)

createGitProcess' :: GitConfig -> GitCommand -> CreateProcess
createGitProcess' GitConfig  { gitCwd = gitCwd', gitPath = gitPath }
                 GitCommand { gitCmd = cmd, args = args'}
  = (proc (fromMaybe "git" gitPath) args'')
  { cwd = Just gitCwd' 
  , std_in = CreatePipe
  , std_out = CreatePipe 
  , std_err = CreatePipe 
  , close_fds = True }
  where args'' = map T.unpack $ [cmd] ++ args'

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

type ID       = Text 
type CommitID = ID 
type BlobID   = ID 
type TreeID   = ID 
type TagID    = ID

data GitObject = Commit CommitID 
               | Blob   BlobID
               | Tree   TreeID
               | Tag    TagID
  deriving (Show)

data Person = Person 
  { personName  :: Text 
  , personEmail :: Text 
  } deriving (Show)

data Commitent = Commitent
  { ceParents       :: [GitObject]
  , ceTree          :: GitObject 
  , ceAuthor        :: Person
  , ceAuthorTime    :: Text 
  , ceCommitter     :: Person
  , ceCommitterTime :: Text
  , ceCommitMsg     :: Text 
  } deriving (Show)

data TreeNode = TreeNode
  { mode      :: Int
  , object    :: GitObject
  , name      :: Text }
  deriving (Show)

data Trees = Trees [TreeNode]
  deriving (Show)

readObjStr' :: Text -> Maybe GitObject
readObjStr' str = 
  readObjStr s objT 
  where w = T.words str 
        s = head w
        objT = last w

objReader :: [ (Text, ID -> GitObject) ]
objReader = [ (T.pack "commit" , Commit )
            , (T.pack "blob"   , Blob   )
            , (T.pack "tag"    , Tag    )
            , (T.pack "tree"   , Tree   ) ]

getIdFromObj :: GitObject -> ID
getIdFromObj (Commit id) = id
getIdFromObj (Blob   id) = id
getIdFromObj (Tag    id) = id
getIdFromObj (Tree   id) = id

getStringFromObj :: GitObject -> Text 
getStringFromObj (Commit id) = T.unwords $ [T.pack "commit", id]
getStringFromObj (Blob   id) = T.unwords $ [T.pack "blob"  , id]
getStringFromObj (Tag    id) = T.unwords $ [T.pack "tag"   , id]
getStringFromObj (Tree   id) = T.unwords $ [T.pack "tree"  , id]

readObjStr :: Text -> ID -> Maybe GitObject
readObjStr t id = find (\(x,n) -> t == x) objReader >>= \(x,n) -> Just (n id)
