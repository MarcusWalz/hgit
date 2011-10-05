{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Lib.HGit.Batch
  ( GitBatch(..)
  , GitBatchM
  , makeGitBatch
  , runGitBatch
  , runGitBatch'
  , readObject
  ) where

import           System.IO
import           Control.Monad.Reader
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Text.Lazy.Encoding as T
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe

import           Lib.HGit.Data

data GitBatch = GitBatch
  { inh  :: Handle
  , outh :: Handle }
  deriving (Show)

newtype GitBatchM a = GitBatchM (ReaderT GitBatch IO a)
  deriving (Monad, MonadIO, MonadReader GitBatch)

-- | Build a GitBatch from Git Reader
makeGitBatch :: GitReader GitBatch
makeGitBatch = do 
  (inh, outh, _,_) <- spawnGitProcess cmd
  return $ GitBatch inh outh
  where cmd = GitCommand (T.pack "cat-file") [T.pack "--batch"]

--Create a GitReader given a GitBatch reader
runGitBatch :: GitBatchM t -> GitReader t
runGitBatch x = do 
  gb <- makeGitBatch
  t <- liftIO $ runGitBatch' gb x
  liftIO $ destroyGitBatch gb
  return t

runGitBatch' :: GitBatch -> GitBatchM t -> IO t
runGitBatch' h (GitBatchM a) = runReaderT a h

-- | Call once finished with GitBatch
destroyGitBatch :: GitBatch -> IO ()
destroyGitBatch (GitBatch inh outh) = do
  hClose inh
  hClose outh

readObject :: ID -> GitBatchM (Maybe ByteString)
readObject id = do
  GitBatch inh outh <- ask
  liftIO $ do 
    T.hPutStrLn inh id
    hFlush inh
    r <- T.hGetLine outh
    let check = T.unpack $ last $ T.words r 
    if check == "missing"
      then return Nothing
      else do 
        x <- B.hGet outh $ read check
        hGetLine outh --clear \n
        return $ Just x
