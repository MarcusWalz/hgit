
module Lib.HGit.Parsers
 ( readTreeString
 , readCommitStr
 ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Internal (c2w)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Text.Lazy.Encoding as T
import           Data.Hex

import           Lib.HGit.Data

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
