-- | Querys Git 
module Lib.HGit.Readers
  ( gitHeadID
  , gitHeadTree
  , getTag
  , getTags
  , getBranch
  , getBranches
  , gitBlobFromTree
  , gitReadGitProc
  , readTree
  , readTreeString
  , readCommitStr
  , readCommit
  , revList 
  , catObject
  , catObjectUnsafe
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
import           Data.Hex
import           System.Directory
import           System.FilePath ((</>))

import           Lib.HGit.Data


gitHeadID :: GitReader CommitID
gitHeadID = do
  GitConfig {gitCwd = dir} <- ask
  h <- liftIO $ readFile $ dir </> ".git/HEAD"
  c <- liftIO $ T.readFile $ dir </> ".git" </> init ( drop 5 h ) --the commit
  return $ T.init c

gitHeadTree :: GitReader Trees
gitHeadTree = do
  id <- gitHeadID
  gitCatBatch (id, Nothing) gitHeadTree' 

gitHeadTree' :: Maybe Bool -> Maybe ByteString -> Either (ID, Maybe Bool) Trees
gitHeadTree' (Just True) (Just str) = Right $ readTreeString str
gitHeadTree' Nothing     (Just str) = Left (tr $ readCommitStr str, Just True)
  where tr Commitent {ceTree = t} = t

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

gitBlobFromTree :: TreeID -> FilePath -> GitReader (Maybe GitObject)
gitBlobFromTree tree path = gitCatBatch (tree, Just path) traverse  

traverse :: Maybe FilePath
         -> Maybe ByteString 
         -> Either (ID, Maybe FilePath) (Maybe GitObject)
traverse (Just path) tree
    | isNothing tree = return Nothing
    | isNothing fi = Right Nothing
    | p == path = Right $ Just $ obj $ fromJust fi
    | otherwise = Left (idFromGitObject $ obj $ fromJust fi, Just p')
    where (Trees t) = readTreeString $ fromJust tree
          pathMatch TreeNode{name = n} = n == p
          fi = find pathMatch t
          p = takeWhile (/= '/') path
          p' = drop (length p + 1) path
          obj TreeNode{object = o} = o
 
--Useful for reading line by line
readProc :: (Text -> a) -> Handle -> IO [a]
readProc f h = do
  eof <- hIsEOF h
  if eof 
    then do hClose h
            return [] 
    else do a <- T.hGetLine h
            m <- readProc f h
            return $ f a : m


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

readTreeNodeLine :: ByteString -> [TreeNode]
readTreeNodeLine str = 
  TreeNode {mode = m, object = obj, name = file} : next 
  where (a,b) = B.break (\x -> x == c2w '\0') str
        x = T.words $ T.decodeUtf8 a
        mode = head x
        m = read $ T.unpack mode
        file = T.unpack $ last x
        id = T.decodeUtf8 $ hex $ B.take 20 $ B.tail b
        obj = if m == 40000 --TODO: Support SymLinks
              then Tree id
              else Blob id
        next = if (==) B.empty (B.drop 21 b)  
                 then []
                 else readTreeNodeLine (B.drop 21 b)
   
readTreeString :: ByteString -> Trees
readTreeString str = Trees $ readTreeNodeLine str

readTree :: ID -> GitReader Trees 
readTree tree = do
    (Just x) <- catObject (Tree tree) 
    return $ readTreeString x

readRevListLine :: ID -> GitObject 
readRevListLine = Commit

-- | Returns a list of CommitID's in reverse chronological order
-- TODO: options
revList :: GitReader [GitObject]
revList = do 
  (_,outh,_,_) <- spawnGitProcess cmd
  liftIO $ readProc readRevListLine outh
  where cmd = GitCommand (T.pack "rev-list") [T.pack "HEAD"]

-- | cat a git object with some sort of reader function 
gitAbstractCat :: GitObject -> (Handle -> IO a) -> GitReader (Maybe a)
gitAbstractCat a f = do
  (_, outh, errh, pid) <- spawnGitProcess cmd
  a <- liftIO $ T.hGetContents errh
  if a == T.empty 
    then do x <- liftIO $ f outh 
            return $ Just x
    else return Nothing
  where cmd = GitCommand (T.pack "cat-file") (T.words $ gitObjectToString a)

-- | Cat a GitObject 
catObject :: GitObject -> GitReader (Maybe ByteString)
catObject obj = gitAbstractCat obj B.hGetContents

-- | Cat a GitObject
-- Will fail for binary files
catObjectUnsafe :: GitObject -> GitReader (Maybe Text)
catObjectUnsafe obj = gitAbstractCat obj T.hGetContents

-- | Creates a temporary file holding the contents of a GitBlob
unpackFile :: BlobID -> GitReader (Maybe FilePath)
unpackFile blob = do 
    l <- gitReadGitProc cmd
    return $ l >>= Just . T.unpack . T.init
  where cmd = GitCommand (T.pack "unpack-file") [blob]

gitCatBatch :: (ID, Maybe b)
            -> (Maybe b -> Maybe ByteString -> Either (ID, Maybe b) a) 
            -> GitReader a
gitCatBatch x f = do  
  (inh, outh, _, pid) <- spawnGitProcess cmd
  liftIO $ gitCatBatch' inh outh f x 
  where cmd = GitCommand (T.pack "cat-file") [T.pack "--batch"]

gitCatBatch' :: Handle -> Handle 
             -> (Maybe b -> Maybe ByteString -> Either (ID, Maybe b) a)
             -> (ID, Maybe b) 
             -> IO a

gitCatBatch' inh outh f (id', b') = do
  str <- gitCatBatch'' inh outh id'
  case f b' str of 
    Left (id, b) -> gitCatBatch' inh outh f (id, b)
    Right a -> do
     hClose inh
     hClose outh
     return a

gitCatBatch'' :: Handle -> Handle -> ID -> IO (Maybe ByteString)
gitCatBatch'' inh outh id = do 
  T.hPutStrLn inh id 
  hFlush inh
  r <- T.hGetLine outh
  let check = T.unpack $ last $ T.words r 
  if check == "missing"
    then do putStrLn "object not found"
            return Nothing
    else do 
      x <- B.hGet outh $ read check
      hGetLine outh
      return $ Just x


getPersonAndDate :: Text -> (Person, Text)
getPersonAndDate str = (p, date)
  where p = Person { personName = name, personEmail = email }
        name = T.init $ T.takeWhile (/= '<') str
        email = T.tail $ T.takeWhile (/= '>') $ T.dropWhile (/= '<') str
        date = T.drop 2 $ T.dropWhile (/= '>') str 
 
readCommitStr :: ByteString -> Commitent
readCommitStr str = Commitent parents tree auth authDate comitr comitrDate msg
  where l = T.lines $ T.decodeUtf8 str
        tree = T.drop 5 $ head l 
        parents' = takeWhile (\x -> T.take 6 x == T.pack "parent") (tail l)
        parents = map (T.drop 7) parents'
        rest = drop (length parents + 1) l
        (auth, authDate) = getPersonAndDate $ T.drop 7 $ head rest  
        (comitr, comitrDate) = getPersonAndDate $ T.drop 10 $ head $ tail rest
        msg = T.init $ T.unlines $ drop 3 rest

-- Read a single commit
readCommit :: CommitID -> GitReader (Maybe Commitent)
readCommit id = do 
                  obj <- catObject (Commit id) 
                  return $ obj >>= r 
  where r x = Just $ readCommitStr x
