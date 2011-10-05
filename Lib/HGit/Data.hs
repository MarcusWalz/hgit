{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Lib.HGit.Data 
  ( runGit
  , GitConfig(..)
  , GitCommand(..)
  , GitReader
  , createGitProcess
  , spawnGitProcess
  , ID
  , CommitID
  , BlobID
  , TagID
  , TreeID
  , Tag
  , GitObject(..)
  , Person(..)
  , Commitent(..)
  , TreeNode(..)
  , Trees(..)
  , idFromGitObject
  , gitObjectToString
  , makeGitObject
  , readGitObject
  ) where
import           System.Command
import           System.IO
import           Control.Monad.Reader
import           Data.Maybe
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Data.Text.Encoding as E
import           Data.List (find)

data GitConfig  = GitConfig
  { gitCwd   :: FilePath 
  , gitPath  :: Maybe FilePath }
  deriving (Show)

  
newtype GitReader a = GitReader (ReaderT GitConfig IO a)
  deriving (Monad, MonadIO, MonadReader GitConfig)

runGit :: GitConfig -> GitReader t -> IO t
runGit config (GitReader a) = runReaderT a config

data GitCommand = GitCommand
  { gitCmd  :: Text 
  , args :: [Text] }
  deriving (Show)

createGitProcess :: GitCommand -> GitReader CreateProcess 
createGitProcess command = do
  conf <- ask
  return $ createGitProcess' conf command

createGitProcess' :: GitConfig -> GitCommand -> CreateProcess
createGitProcess' GitConfig  { gitCwd = gitCwd', gitPath = gitPath }
                 GitCommand { gitCmd = cmd, args = args'}
  = (proc (fromMaybe "git" gitPath) args'')
  { cwd = Just gitCwd' 
  , std_in = CreatePipe
  , std_out = CreatePipe 
  , std_err = CreatePipe 
  , close_fds = True }
  where args'' = map T.unpack $ cmd : args'

spawnGitProcess :: GitCommand 
                -> GitReader (Handle, Handle, Handle, ProcessHandle)
spawnGitProcess command = do
  proc <- createGitProcess command 
  (Just inh, Just outh, Just errh, pid) <- liftIO $ createProcess proc 
  return (inh, outh, errh, pid)



type ID       = Text 
type CommitID = ID 
type BlobID   = ID 
type TreeID   = ID 
type TagID    = ID

data GitObject = Commit CommitID 
               | Blob   BlobID
               | Tree   TreeID
               | Tag    TagID
  deriving (Show, Eq)

data Person = Person 
  { personName  :: Text 
  , personEmail :: Text 
  } deriving (Show)

data Commitent = Commitent
  { ceParents       :: [CommitID]
  , ceTree          :: TreeID 
  , ceAuthor        :: Person
  , ceAuthorTime    :: Text 
  , ceCommitter     :: Person
  , ceCommitterTime :: Text
  , ceCommitMsg     :: Text 
  } deriving (Show)

data TreeNode = TreeNode
  { mode      :: Int
  , object    :: GitObject
  , name      :: FilePath }
  deriving (Show)

data Tag = FilePath


data Trees = Trees [TreeNode]
  deriving (Show)

readGitObject :: Text -> GitObject
readGitObject str = makeGitObject t id
  where x = T.words str
        t = head x
        id = last x 



idFromGitObject :: GitObject -> ID
idFromGitObject (Commit id) = id
idFromGitObject (Blob   id) = id
idFromGitObject (Tag    id) = id
idFromGitObject (Tree   id) = id

gitObjectToString :: GitObject -> Text 
gitObjectToString (Commit id) = T.unwords [T.pack "commit", id]
gitObjectToString (Blob   id) = T.unwords [T.pack "blob"  , id]
gitObjectToString (Tag    id) = T.unwords [T.pack "tag"   , id]
gitObjectToString (Tree   id) = T.unwords [T.pack "tree"  , id]

objReader :: [ (Text, ID -> GitObject) ]
objReader = [ (T.pack "commit" , Commit )
            , (T.pack "blob"   , Blob   )
            , (T.pack "tag"    , Tag    )
            , (T.pack "tree"   , Tree   ) ]

makeGitObject :: Text -> ID -> GitObject
makeGitObject t id = c id
  where c = snd $ fromJust $ find (\(x,n) -> t == x) objReader
