{-# LANGUAGE ScopedTypeVariables #-}
module Util where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Exception
import           System.IO
import           System.FilePath
import           System.Directory
import           Data.Time.Clock
import           Data.List
import           Network

connectRetry :: Int -> HostName ->  PortNumber -> (Maybe Handle -> IO a) -> IO a
connectRetry delay host port action = go 0
  where
    go n
      | n < (10 :: Int) = do
        tryConnect >>= either (\(_ :: IOException) -> retry n) return
      | otherwise = action Nothing

    tryConnect = try $ bracket connect hClose (action . Just)
    retry n = threadDelay delay >> go (succ n)
    connect = connectTo host $ PortNumber port

getModTime :: IO UTCTime
getModTime = maximum <$> (listFiles >>= mapM getModificationTime)
  where
    listFiles :: IO [FilePath]
    listFiles = filter ((||) <$> isSuffixOf ".hs" <*> isSuffixOf ".lhs") <$> go "."
      where
        go dir = do
          (dirs, files) <- getFilesAndDirectories dir
          (files ++) . concat <$> mapM go (filter (`notElem` exclude) dirs)

        exclude :: [FilePath]
        exclude = ["./.git"]

    getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
    getFilesAndDirectories dir = do
      c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
      (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c
