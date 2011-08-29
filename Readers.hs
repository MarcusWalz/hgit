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

readProc :: (Text -> Maybe a) -> Handle -> IO [a]
readProc f h = do
  eof <- hIsEOF h
  if eof 
    then do hClose h
            return [] 
    else do a <- T.hGetLine h
            m <- readProc f h
            return $ (maybeToList (f a)) ++ m

readTreeNodeLine :: Text -> Maybe TreeNode
readTreeNodeLine str =
  o >>= \x -> Just TreeNode {mode = m, object = x, name = n}
  where m = read $ T.unpack $ head w :: Int
        o = readObjStr' $ T.unwords $ take 2 $ drop 1 w
        n = T.unwords $ drop 3 w
        w = T.words str 

readTree :: GitReader Trees 
readTree = do
  (_,outh,_,_) <- spawnGitProcess cmd
  x <- liftIO $ readProc readTreeNodeLine outh
  return $ Trees x
  where cmd = makeGitCommand (T.pack "ls-tree") [T.pack "HEAD"]

readRevListLine :: ID -> Maybe GitObject 
readRevListLine id = Just $ Commit id

revList :: GitReader [GitObject]
revList = do 
  (_,outh,_,_) <- spawnGitProcess cmd
  x <- liftIO $ readProc readRevListLine outh
  return $ x
  where cmd = makeGitCommand (T.pack "rev-list") [T.pack "HEAD"]

catObject :: GitObject -> GitReader (Maybe Text)
catObject a = do
  (_,outh,_,_) <- spawnGitProcess cmd
  x <- liftIO $ T.hGetContents outh
  return $ Just x 
  where cmd = makeGitCommand (T.pack "cat-file") (T.words $ getStringFromObj a)

catI :: [GitObject] -> Handle -> Handle-> IO [Maybe Text]
catI [] inh outh = do 
  mapM hClose [inh, outh]
  return []

catI (x:xs) inh outh= do
  T.hPutStrLn inh $ getIdFromObj x
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

getParents :: Handle -> IO [GitObject]
getParents h = do
  n <- hLookAhead h 
  if n == 'p' then do
    r <- T.hGetLine h
    p <- getParents h
    return $ Commit (last $ T.words r) : p

    else do return []

getPersonAndDate :: Text -> Maybe (Person, Text)
getPersonAndDate str = Just (p, date)
  where p = Person { personName = name, personEmail = email }
        name = T.init $ T.takeWhile (\x -> x /= '<') str
        email = T.tail $ T.takeWhile (\x -> x /= '>') $ T.dropWhile (\x -> x /= '<') str
        date = T.drop 2 $ T.dropWhile (\x -> x /= '>') str 
 
readCommit' :: Handle -> IO (Maybe Commitent)
readCommit' h = do
  tr <- T.hGetLine h
  let tree = readObjStr' tr 
  p  <- getParents h
  al <- T.hGetLine h
  let auth = getPersonAndDate $ T.drop 7 $ al 
  cl <- T.hGetLine h
  let commi = getPersonAndDate $ T.drop 10 $ cl 
  msg <- T.hGetContents h
  return (Just Commitent {
    ceParents       = p
  , ceTree          = fromJust $ tree
  , ceAuthor        = fst $ fromJust $ auth
  , ceAuthorTime    = snd $ fromJust $ auth
  , ceCommitter     = fst $ fromJust $ commi
  , ceCommitterTime = snd $ fromJust $ commi
  , ceCommitMsg     = msg })

readCommit :: CommitID -> GitReader (Maybe Commitent)
readCommit id = do
  (_,outh,_,_) <- spawnGitProcess cmd
  a <- liftIO $ readCommit' outh
  return a
  where cmd = makeGitCommand (T.pack "cat-file") [T.pack "commit", id]
