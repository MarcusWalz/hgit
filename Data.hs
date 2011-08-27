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
  { cwd = Just gitCwd' 
  , std_in = CreatePipe
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

data GitObject = Commit CommitID 
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

getIdFromObj :: GitObject -> ID
getIdFromObj (Commit id) = id
getIdFromObj (Blob   id) = id
getIdFromObj (Tag    id) = id
getIdFromObj (Tree   id) = id

getStringFromObj :: GitObject -> ByteString
getStringFromObj (Commit id) = B.unwords $ [B.pack "commit", id]
getStringFromObj (Blob   id) = B.unwords $ [B.pack "blob"  , id]
getStringFromObj (Tag    id) = B.unwords $ [B.pack "tag"   , id]
getStringFromObj (Tree   id) = B.unwords $ [B.pack "tree"  , id]

readObjStr :: ByteString -> ID -> Maybe GitObject
readObjStr t id = find (\(x,n) -> t == x) objReader >>= \(x,n) -> Just (n id)

readTreeNodeLine :: ByteString -> Maybe TreeNode
readTreeNodeLine str =
  o >>= \x -> Just TreeNode {mode = m, object = x, name = n}
  where m = fst $ fromJust $ B.readInt $ head w 
        o = readObjStr' $ B.unwords $ take 2 $ drop 1 w
        n = B.unwords $ drop 3 w
        w = B.words str

data Trees = Trees [TreeNode]
  deriving (Show)

readProc :: (ByteString -> Maybe a) -> Handle -> IO [a]
readProc f h = do
  eof <- hIsEOF h
  if eof 
    then do hClose h
            return [] 
    else do a <- B.hGetLine h
            m <- readProc f h
            return $ (maybeToList (f a)) ++ m
  
readTree :: GitReader Trees 
readTree = do
  (_,outh,_,_) <- spawnGitProcess cmd
  x <- liftIO $ readProc readTreeNodeLine outh
  return $ Trees x
  where cmd = makeGitCommand (B.pack "ls-tree") [B.pack "HEAD"]
  
readRevListLine :: ID -> Maybe GitObject 
readRevListLine id = Just $ Commit id

revList :: GitReader [GitObject]
revList = do 
  (_,outh,_,_) <- spawnGitProcess cmd
  x <- liftIO $ readProc readRevListLine outh
  return $ x
  where cmd = makeGitCommand (B.pack "rev-list") [B.pack "HEAD"]

catObject :: GitObject -> GitReader (Maybe ByteString)
catObject a = do
  (_,outh,_,_) <- spawnGitProcess cmd
  x <- liftIO $ B.hGetContents outh
  return $ Just x 
  where cmd = makeGitCommand (B.pack "cat-file") (B.words $ getStringFromObj a)

catI :: [GitObject] -> Handle -> Handle -> Handle -> Handle-> IO [Maybe ByteString]
catI [] inh outh inc outc= do 
  mapM hClose [inh, outh, inc, outc]
  return []

--uses --batch-check to make sure object exists and to get size
--then uses --batch to fetch object surprisingly fast

catI (x:xs) inh outh inc outc= do
  B.hPutStrLn inc $ getIdFromObj x
  hFlush inc
  checkStr <- B.hGetLine outc 
  let check = drop 2 $ B.words $ checkStr
  if check == [] then do
              r <- catI xs inh outh inc outc 
              return $ Nothing : r
         else do 
              B.hPutStrLn inh $ getIdFromObj x
              hFlush inh
              a <- B.hGet outh $ fst $ fromJust $ B.readInt $ head $ check 
              r <- catI xs inh outh inc outc 
              return $ (Just a) : r

catObjects :: [GitObject] -> GitReader [Maybe ByteString]
catObjects objs = do
  (inh, outh, _, pid) <- spawnGitProcess cmd
  (inc, outc, _, _)   <- spawnGitProcess cmdChecker
  x <- liftIO $ catI objs inh outh inc outc 
  return x 
  where cmd = makeGitCommand (B.pack "cat-file") [B.pack "--batch"] 
        cmdChecker = makeGitCommand (B.pack "cat-file") [B.pack "--batch-check"]

main = do
  a <- runGit c $ revList 
  runGit c $ catObjects a
  where 
    c = makeGitConfig "../progit" Nothing
