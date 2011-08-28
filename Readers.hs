import           System.Command
import           System.IO
import           Control.Monad.Reader
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe

import           Lib.HGit.Type

readProc :: (ByteString -> Maybe a) -> Handle -> IO [a]
readProc f h = do
  eof <- hIsEOF h
  if eof 
    then do hClose h
            return [] 
    else do a <- B.hGetLine h
            m <- readProc f h
            return $ (maybeToList (f a)) ++ m

readTreeNodeLine :: ByteString -> Maybe TreeNode
readTreeNodeLine str =
  o >>= \x -> Just TreeNode {mode = m, object = x, name = n}
  where m = fst $ fromJust $ B.readInt $ head w 
        o = readObjStr' $ B.unwords $ take 2 $ drop 1 w
        n = B.unwords $ drop 3 w
        w = B.words str --ahh? donde esta

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
