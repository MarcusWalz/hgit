import           System.Command
import           System.IO
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)
import           Data.Text.Encoding as E
import           Data.Maybe

import           Lib.HGit.Type


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

readTreeNodeLine :: Text -> TreeNode
readTreeNodeLine str =
  TreeNode {mode = m, object = o, name = n}
  where m = read $ T.unpack $ head w :: Int
        o = readGitObject $ T.unwords $ take 2 $ drop 1 w
        n = T.unwords $ drop 3 w
        w = T.words str 

readTree :: Maybe Text -> GitReader Trees 
readTree tree = do
    (_,outh,_,_) <- spawnGitProcess cmd
    x <- liftIO $ readProc readTreeNodeLine outh
    return $ Trees x
  where cmd = makeGitCommand (T.pack "ls-tree") [fromMaybe (T.pack "HEAD") tree]

readRevListLine :: ID -> GitObject 
readRevListLine id = Commit id

revList :: GitReader [GitObject]
revList = do 
  (_,outh,_,_) <- spawnGitProcess cmd
  liftIO $ readProc readRevListLine outh
  where cmd = makeGitCommand (T.pack "rev-list") [T.pack "HEAD"]


gitAbstractCat :: [Text] -> (Handle -> IO a) -> GitReader (Maybe a)
gitAbstractCat a f = do
  (inh, outh, errh, pid) <- spawnGitProcess cmd
  a <- liftIO $ T.hGetContents errh
  if a == T.empty 
    then do x <- liftIO $ f outh 
            return $ Just x
    else return Nothing
  where cmd = makeGitCommand (T.pack "cat-file") a

--ByteString in case of binary file / obj like Tree
catObject :: GitObject -> GitReader (Maybe ByteString)
catObject obj = gitAbstractCat args B.hGetContents
  where args = T.words $ gitObjectToString obj

unPackFile :: BlobID -> GitReader (Maybe FilePath)
unPackFile blob = do 
    l <- readGit cmd
    return $ l >>= (\x -> Just $ T.unpack $ T.init $ x)
  where cmd = makeGitCommand (T.pack "unpack-file") [blob]

catI :: [GitObject] -> Handle -> Handle-> IO [Maybe Text]
catI [] inh outh = do 
  mapM hClose [inh, outh]
  return []

catI (x:xs) inh outh= do
  T.hPutStrLn inh $ idFromGitObject x
  hFlush inh
  checkStr <- T.hGetLine outh
  let check = T.unpack $ last $ T.words $ checkStr
  if check == "" 
    then do
      r <- catI xs inh outh
      return $ Nothing : r
    else do 
      a <- B.hGet outh $ read $ check 
      B.hGetLine outh --clears buffer
      r <- catI xs inh outh
      return $ (Just $ E.decodeUtf8 a) : r

catObjects :: [GitObject] -> GitReader [Maybe Text]
catObjects objs = do
  (inh, outh, _, pid) <- spawnGitProcess cmd
  x <- liftIO $ catI objs inh outh 
  return x 
  where cmd = makeGitCommand (T.pack "cat-file") [T.pack "--batch"]

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
readCommit id = gitAbstractCat obj readCommit'
  where obj = [T.pack "commit", id]
