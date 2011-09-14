module Lib.HGit.Readers
  ( getTag
  , getTags
  , getBranch
  , getBranches
  , gitBlobFromTree
  , readGit
  , readTree
  , readTreeString
  , readCommit
  , revList 
  , catObject
  , catObjectUnsafe
  , catObjects
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
import           Data.List (find)
import           Data.Either
import           Data.Hex
import           System.Directory
import           System.FilePath ((</>))

import           Lib.HGit.Data


getDirectory :: FilePath -> GitReader [FilePath]
getDirectory path = do
  GitConfig { gitCwd = c } <- ask
  x <- liftIO $ getDirectoryContents $ c </> path 
  return $ drop 2 x

getFile :: FilePath -> FilePath -> GitReader (CommitID)
getFile path obj = do
    GitConfig { gitCwd = c } <- ask
    x <-liftIO $ T.readFile (c </> path </> obj)
    return $ T.init x --remove \n

getTags :: GitReader [FilePath]
getTags =  getDirectory ".git/refs/tags"
getTag :: FilePath -> GitReader (CommitID)
getTag = getFile ".get/refs/tags" 

getBranches :: GitReader [FilePath] 
getBranches = getDirectory ".git/refs/heads"

getBranch :: FilePath -> GitReader (CommitID)
getBranch = getFile ".git/refs/heads"


catObjects :: [GitObject] -> GitReader [Maybe ByteString]
catObjects objs = gitCatBatch (idFromGitObject $ head objs, Just (tail objs, [])) catObjects'

catObjects' :: Maybe ([GitObject], [Maybe ByteString])
            -> Maybe ByteString
            -> (Either (ID, Maybe ([GitObject], [Maybe ByteString])) [Maybe ByteString])

catObjects' (Just ([], cats)) b = Right (cats ++ [b])
catObjects' (Just (objs, cats)) b = Left (idFromGitObject $ head objs, 
                                    Just (tail objs, cats ++ [b]))

gitBlobFromTree :: TreeID -> FilePath -> GitReader (Maybe GitObject)
gitBlobFromTree tree path = gitCatBatch (tree, Just path) traverse  

traverse :: Maybe FilePath
         -> Maybe ByteString 
         -> (Either (ID, Maybe FilePath) (Maybe GitObject))
traverse (Just path) tree =
  if isNothing tree
    then return Nothing
    else if isNothing fi
            then Right Nothing
            else if p == path 
                    then Right $ Just $ obj $ fromJust $ fi
                    else Left (idFromGitObject $ obj $ fromJust fi, Just p')
  where
    (Trees t) = readTreeString $ fromJust tree 
    pathMatch TreeNode {name = n} = n == p
    fi= find pathMatch t
    p = takeWhile (\x -> x /= '/') path
    p' =  drop (length p + 1) path
    obj TreeNode { object = o } = o

--Useful for reading line by line
readProc :: (Text -> a) -> Handle -> IO [a]
readProc f h = do
  eof <- hIsEOF h
  if eof 
    then do hClose h
            return [] 
    else do a <- T.hGetLine h
            m <- readProc f h
            return $ (f a) : m


readGit :: GitCommand -> GitReader (Maybe Text)
readGit cmd = do  
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
  where (a,b) = B.break (\x -> x == (c2w '\0')) str
        x = T.words $ T.decodeUtf8 a
        mode = head x
        m = read $ T.unpack $ mode
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
readRevListLine id = Commit id

revList :: GitReader [GitObject]
revList = do 
  (_,outh,_,_) <- spawnGitProcess cmd
  liftIO $ readProc readRevListLine outh
  where cmd = makeGitCommand (T.pack "rev-list") [T.pack "HEAD"]


gitAbstractCat :: GitObject -> (Handle -> IO a) -> GitReader (Maybe a)
gitAbstractCat a f = do
  (_, outh, errh, pid) <- spawnGitProcess cmd
  a <- liftIO $ T.hGetContents errh
  if a == T.empty 
    then do x <- liftIO $ f outh 
            return $ Just x
    else return Nothing
  where cmd = makeGitCommand (T.pack "cat-file") (T.words $ gitObjectToString a)

--ByteString in case of binary file / obj like Tree
catObject :: GitObject -> GitReader (Maybe ByteString)
catObject obj = gitAbstractCat obj B.hGetContents

catObjectUnsafe :: GitObject -> GitReader (Maybe Text)
catObjectUnsafe obj = gitAbstractCat obj T.hGetContents

unpackFile :: BlobID -> GitReader (Maybe FilePath)
unpackFile blob = do 
    l <- readGit cmd
    return $ l >>= (\x -> Just $ T.unpack $ T.init $ x)
  where cmd = makeGitCommand (T.pack "unpack-file") [blob]


gitCatBatch :: (ID, Maybe b)
            -> (Maybe b -> Maybe ByteString -> (Either (ID, Maybe b) a)) 
            -> GitReader a
gitCatBatch x f = do  
  (inh, outh, _, pid) <- spawnGitProcess cmd
  liftIO $ gitCatBatch' inh outh f x 
  where cmd = makeGitCommand (T.pack "cat-file") [T.pack "--batch"]

gitCatBatch' :: Handle -> Handle 
             -> (Maybe b -> Maybe ByteString -> (Either (ID, Maybe b) a))
             -> (ID, Maybe b) 
             -> IO a

gitCatBatch' inh outh f (id', b') = do
  str <- gitCatBatch'' inh outh id'
  case (f b' str) of 
    Left (id, b) -> gitCatBatch' inh outh f (id, b)
    Right a -> do
     hClose inh
     hClose outh
     return $ a

gitCatBatch'' :: Handle -> Handle -> ID -> IO (Maybe ByteString)
gitCatBatch'' inh outh id = do 
  T.hPutStrLn inh id 
  hFlush inh
  r <- T.hGetLine outh
  let check = T.unpack $ last $ T.words $ r 
  if check == "missing"
    then do putStrLn "object not found"
            return Nothing
    else do 
      x <- B.hGet outh $ read $ check
      hGetLine outh
      return $ Just x

getParents :: Handle -> IO [CommitID]
getParents h = do
  n <- hLookAhead h 
  if n == 'p' then do
    r <- T.hGetLine h
    p <- getParents h
    return $ (T.drop 7 r) : p

    else do return []

getPersonAndDate :: Text -> (Person, Text)
getPersonAndDate str = (p, date)
  where p = Person { personName = name, personEmail = email }
        name = T.init $ T.takeWhile (\x -> x /= '<') str
        email = T.tail $ T.takeWhile (\x -> x /= '>') $ T.dropWhile (\x -> x /= '<') str
        date = T.drop 2 $ T.dropWhile (\x -> x /= '>') str 
 
readCommit' :: Handle -> IO Commitent
readCommit' h = do
  tr <- T.hGetLine h
  let tree = T.drop 5 tr 
  p  <- getParents h
  al <- T.hGetLine h
  let auth = getPersonAndDate $ T.drop 7 $ al 
  cl <- T.hGetLine h
  let commi = getPersonAndDate $ T.drop 10 $ cl 
  msg <- T.hGetContents h
  return (Commitent { 
    ceParents       = p
  , ceTree          = tree
  , ceAuthor        = fst auth
  , ceAuthorTime    = snd auth
  , ceCommitter     = fst commi
  , ceCommitterTime = snd commi
  , ceCommitMsg     = msg })

readCommit :: CommitID -> GitReader (Maybe Commitent)
readCommit id = gitAbstractCat (Commit id) readCommit'
