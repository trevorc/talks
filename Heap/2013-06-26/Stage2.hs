module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf

type Term = String
type Index = Map Term (Set FilePath)

readFile' :: FilePath -> IO String
readFile' path =
    do contents <- readFile path
       length contents `seq` return contents

segmentTerms :: Term -> [Term]
segmentTerms contents =
    words . map toLower .
    filter (\c -> isSpace c || isAlpha c) $
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
              do contents <- readFile' document
                 return (indexDocument index document contents)

intersections :: Ord a => [Set a] -> Set a
intersections [] = Set.empty
intersections xs = foldr1 Set.intersection xs

queryIndex :: Index -> [Term] -> [FilePath]
queryIndex index query =
    Set.toList . intersections . mapMaybe lookupTerm $ query
    where lookupTerm term = Map.lookup (map toLower term) index

main :: IO ()
main = do (documentDirectory, query) <- parseArgs <$> getProgName <*> getArgs
          documents <- listDocuments documentDirectory
          index <- createIndex documents
          let results = queryIndex index query
          mapM_ putStrLn results
    where parseArgs prog []  = usage prog
          parseArgs prog [_] = usage prog
          parseArgs _ (dir:query) = (dir, query)
          usage prog = error $ printf "usage: %s DIR TERM.." prog
