import           System.Command
import           System.IO
import           Control.Monad.Reader
import           Data.ByteString.Char8 (ByteString)
import           Data.Maybe
import qualified Data.ByteString.Char8 as B

data GitConfig  = GitConfig
  { gitCwd   :: FilePath 
  , gitPath  :: Maybe FilePath }

makeGitConfig :: FilePath -> Maybe FilePath -> GitConfig
makeGitConfig cwd' gitPath' = GitConfig {gitCwd = cwd', gitPath = gitPath'}

data GitCommand = GitCommand
  { gitCmd  :: ByteString 
  , args :: [ByteString]
  }

makeGitCommand :: ByteString -> [ByteString] -> GitCommand 
makeGitCommand cmd args' = GitCommand {gitCmd = cmd, args = args'}

createGitProcess :: GitConfig -> GitCommand -> CreateProcess
createGitProcess GitConfig  { gitCwd = gitCwd', gitPath = gitPath }
                 GitCommand { gitCmd = cmd, args = args'}
 = (proc (fromMaybe "git" gitPath) args'')
 { std_in = CreatePipe 
 , std_out = CreatePipe 
 , std_err = CreatePipe 
 , close_fds = True }
 where args'' = map B.unpack $ [cmd] ++ args'
