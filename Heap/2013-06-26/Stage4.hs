module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf

type Term = Text
type Index = Map Term (Set FilePath)

segmentTerms :: Term -> [Term]
segmentTerms contents =
    Text.words . Text.toLower .
    Text.filter (\c -> isSpace c || isAlpha c) $
    contents

indexDocument :: Index -> FilePath -> Term -> Index
indexDocument index docPath contents =
    Map.unionWith Set.union index .
    Map.fromList .
    map (\term -> (term, Set.singleton docPath)) .
    segmentTerms $ contents

listDocuments :: FilePath -> IO [FilePath]
listDocuments directory =
    do contents <- map (directory </>) <$>
                   getDirectoryContents directory
       filterM doesFileExist contents

createIndex :: [FilePath] -> IO Index
createIndex documents =
    foldM addDocument Map.empty documents
    where addDocument index document =
              do contents <- Text.readFile document
                 let index' = indexDocument index document contents
                 seq index' (return index')

intersections :: Ord a => [Set a] -> Set a
intersections [] = Set.empty
intersections xs = foldr1 Set.intersection xs

queryIndex :: Index -> [Term] -> [FilePath]
queryIndex index query =
    Set.toList . intersections . mapMaybe lookupTerm $ query
    where lookupTerm term = Map.lookup (Text.toLower term) index

main :: IO ()
main = do (documentDirectory, query) <- parseArgs <$> getProgName <*> getArgs
          documents <- listDocuments documentDirectory
          index <- createIndex documents
          let results = queryIndex index query
          mapM_ putStrLn results
    where parseArgs prog []  = usage prog
          parseArgs prog [_] = usage prog
          parseArgs _ (dir:query) = (dir, map Text.pack query)
          usage prog = error $ printf "usage: %s DIR TERM.." prog
