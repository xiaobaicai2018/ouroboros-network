\subsection{Cardano.BM.Output.Rotator}

%if False
\begin{spec}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | monitor log files for max age and max size

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Output.Rotator
       ( RotationParameters (..)
       , cleanupRotator
       , evalRotator
       , initializeRotator
    --    , latestLogFile
       ) where

import           GHC.Generics
import           Control.Exception.Safe (catchIO)
import           Control.Lens ((^.), makeLenses)
import           Data.Aeson (FromJSON (..), withObject , (.:), (.:?), (.!=))
import           Data.List (sort)
import qualified Data.List.NonEmpty as NE
import           Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime,
                     parseTimeM)
import           Data.Word (Word64)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (listDirectory, removeFile)
import           System.FilePath ((</>), splitFileName, takeFileName, takeDirectory)
import           System.IO (BufferMode (LineBuffering), Handle,
                     IOMode (AppendMode, WriteMode), hFileSize, hSetBuffering, stdout, openFile)

import           Cardano.BM.Output.Internal (FileDescription (..), prtoutException)

#ifdef POSIX
import           System.Directory (createFileLink)
#endif

--  |RotationParameters| one of the two categories used in the
--  logging config, specifying the log rotation parameters.
data RotationParameters = RotationParameters
    { _rpLogLimitBytes :: !Word64  -- ^ max size of file in bytes
    , _rpMaxAgeHours   :: !Word    -- ^ hours
    , _rpKeepFilesNum  :: !Word    -- ^ number of files to keep
    } deriving (Generic, Show, Eq)

instance FromJSON RotationParameters where
    parseJSON = withObject "rotation params" $ \o -> do
        _rpLogLimitBytes  <- o .: "logLimit"
        _rpMaxAgeHours    <- o .:? "maxAge" .!= 24
        _rpKeepFilesNum   <- o .: "keepFiles"
        return RotationParameters{..}

makeLenses ''RotationParameters

-- Because of the System.Directory and System.FilePath
-- {-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

-- format of a timestamp
tsformat :: String
tsformat = "%Y%m%d%H%M%S"

-- get file path to a log file with current time
nameLogFile :: FileDescription -> IO FilePath
nameLogFile fdesc = do
    now <- getCurrentTime
    let tsnow = formatTime defaultTimeLocale tsformat now
    return $ (filePath fdesc) ++ "-" ++ tsnow

-- open a new log file
evalRotator :: RotationParameters -> FileDescription -> IO (Handle, Integer, UTCTime)
evalRotator rotation fdesc = do
    let maxAge   = toInteger $ rotation ^. rpMaxAgeHours
        maxSize  = toInteger $ rotation ^. rpLogLimitBytes

    -- open new log file
    fpath <- nameLogFile fdesc
    hdl <- catchIO (openFile fpath WriteMode) $
               \e -> do
                   prtoutException ("error while opening log: " ++ fpath) e
                   return stdout    -- fallback to standard output in case of exception
    hSetBuffering hdl LineBuffering

#ifdef POSIX
    -- restrict symbolic links only for unix-like OS
    let symLinkPath = filePath fdesc
    let logfilePath = takeFileName fpath
    -- delete a symlink if already exists and create a new
    -- one that points to the correct file.
    (removeFile symLinkPath)
        `catchIO` (prtoutException ("cannot remove symlink: " ++ symLinkPath))
    (createFileLink logfilePath symLinkPath)
        `catchIO` (prtoutException ("cannot create symlink: " ++ symLinkPath))
#endif

    -- compute next rotation time
    now <- getCurrentTime
    let rottime = addUTCTime (fromInteger $ maxAge * 3600) now

    return (hdl, maxSize, rottime)

-- list filenames in prefix dir which match 'filename'
listLogFiles :: FileDescription -> IO (Maybe (NE.NonEmpty FilePath))
listLogFiles fdesc = do
    files <- listDirectory prefixpath
    return $ NE.nonEmpty $ sort $ filter fpredicate files
  where
    (prefixpath, filename) = splitFileName $ filePath fdesc
    tslen = 14  -- length of a timestamp
    fplen = length filename
    fpredicate path = take fplen path == filename
                      && take 1 (drop fplen path) == "-"
                      && length (drop (fplen + 1) path) == tslen

--  latest log file in prefix dir which matches 'filename'
latestLogFile :: FileDescription -> IO (Maybe FilePath)
latestLogFile fdesc =
    listLogFiles fdesc >>= \fs -> return $ latestLogFile' fs
  where
    latestLogFile' :: Maybe (NE.NonEmpty FilePath) -> Maybe FilePath
    latestLogFile' Nothing      = Nothing
    latestLogFile' (Just flist) = Just $ NE.last flist

-- initialize log file at startup
-- may append to existing file
initializeRotator :: RotationParameters -> FileDescription -> IO (Handle, Integer, UTCTime)
initializeRotator rotation fdesc = do
    let maxAge   = toInteger $ rotation ^. rpMaxAgeHours
        maxSize  = toInteger $ rotation ^. rpLogLimitBytes
        file  = filePath fdesc
        fplen = length $ takeFileName file

    latest <- latestLogFile fdesc
    case latest of
        Nothing -> -- no file to append, return new
            evalRotator rotation fdesc
        Just fname -> do
            -- check date
            now <- getCurrentTime
            tsfp <- parseTimeM True defaultTimeLocale tsformat $ drop (fplen + 1) fname
            if (round $ diffUTCTime now tsfp) > (3600 * maxAge)
               then do  -- file is too old, return new
                  evalRotator rotation fdesc
               else do
                  hdl <- catchIO (openFile file AppendMode) $
                             \e -> do
                                 prtoutException fname e
                                 return stdout    -- fallback to standard output in case of exception
                  hSetBuffering hdl LineBuffering
                  cursize <- hFileSize hdl
                  let rottime = addUTCTime (fromInteger $ maxAge * 3600) tsfp
                  return (hdl, (maxSize - cursize), rottime)

-- | remove old files; count them and only keep n (from config)
cleanupRotator :: RotationParameters -> FileDescription -> IO ()
cleanupRotator rotation fdesc = do
    let keepN0 = fromIntegral (rotation ^. rpKeepFilesNum) :: Int
        keepN = max 1 $ min keepN0 99
    listLogFiles fdesc >>= removeOldFiles keepN
  where
    removeOldFiles :: Int -> Maybe (NE.NonEmpty FilePath) -> IO ()
    removeOldFiles _ Nothing = return ()
    removeOldFiles n (Just flist) = do
        removeFiles $ reverse $ NE.drop n $ NE.reverse flist
    removeFiles [] = return ()
    removeFiles (fp : fps) = do
        let bp = takeDirectory $ filePath fdesc
            filepath = bp </> fp
        removeFile filepath   -- destructive
        removeFiles fps

code from Katip.lhs

-- | create a katip scribe for logging to a file
--   and handle file rotation within the katip-invoked logging function
mkFileScribe
    :: RotationParameters
    -> Internal.FileDescription
    -> (forall a . LogItem a => Handle -> Bool -> Verbosity -> Item a -> IO Int)  -- format and output function, returns written bytes
    -> Bool  -- whether the output is colourized
    -> Data.Severity
    -> Verbosity
    -> IO Scribe
mkFileScribe rot fdesc formatter colorize _ v = do
    let prefixDir = Internal.prefixPath fdesc
    (createDirectoryIfMissing True prefixDir)
        `catchIO` (Internal.prtoutException ("cannot log prefix directory: " ++ prefixDir))
    trp <- initializeRotator rot fdesc
    scribestate <- newMVar trp    -- triple of (handle), (bytes remaining), (rotate time)
    -- sporadically remove old log files - every 10 seconds
    cleanup <- mkAutoUpdate defaultUpdateSettings { updateAction = cleanupRotator rot fdesc, updateFreq = 10000000 }
    let finalizer :: IO ()
        finalizer = do
            modifyMVar_ scribestate $ \(hdl, b, t) -> do
                hClose hdl
                return (hdl, b, t)
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item =
        --   when (checkItem s sevfilter item) $
              modifyMVar_ scribestate $ \(hdl, bytes, rottime) -> do
                  byteswritten <- formatter hdl colorize v item
                  -- remove old files
                  cleanup
                  -- detect log file rotation
                  let bytes' = bytes - (toInteger $ byteswritten)
                  let tdiff' = round $ diffUTCTime rottime (_itemTime item)
                  if bytes' < 0 || tdiff' < (0 :: Integer)
                     then do   -- log file rotation
                        hClose hdl
                        (hdl2, bytes2, rottime2) <- evalRotator rot fdesc
                        return (hdl2, bytes2, rottime2)
                     else
                        return (hdl, bytes', rottime)

    return $ Scribe logger finalizer

\end{spec}
