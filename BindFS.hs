module Main where

import Control.Exception
import Foreign.C.Error
import System.Directory ( getDirectoryContents )
import System.IO
import System.Posix.Directory
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

import HFuse

main :: IO ()
main = fuseMain bindFSOps (\e -> print e >> bindExceptionHandler e)

bindExceptionHandler :: Exception -> IO Errno
bindExceptionHandler (IOException ioe)
    | isAlreadyExistsError ioe = return eALREADY
    | isDoesNotExistError  ioe = return eNOENT
    | isAlreadyInUseError  ioe = return eBUSY
    | isFullError          ioe = return eAGAIN
    | isEOFError           ioe = return eIO
    | isIllegalOperation   ioe = return eNOTTY
    | isPermissionError    ioe = return ePERM
    | otherwise                = return eFAULT
bindExceptionHandler e         = return eFAULT

bindFSOps :: FuseOperations
bindFSOps =
    FuseOperations { fuseGetFileStat = bindGetFileStat
                   , fuseReadSymbolicLink = bindReadSymbolicLink
                   , fuseGetDirectoryContents = bindGetDirectoryContents
                   , fuseCreateDevice = bindCreateDevice
                   , fuseCreateDirectory = bindCreateDirectory
                   , fuseRemoveLink = bindRemoveLink
                   , fuseRemoveDirectory = bindRemoveDirectory
                   , fuseCreateSymbolicLink = bindCreateSymbolicLink
                   , fuseRename = bindRename
                   , fuseCreateLink = bindCreateLink
                   , fuseSetFileMode = bindSetFileMode
                   , fuseSetOwnerAndGroup = bindSetOwnerAndGroup
                   , fuseSetFileSize = bindSetFileSize
                   , fuseSetFileTimes = bindSetFileTimes
                   , fuseOpen = bindOpen
                   , fuseRead = bindRead
                   , fuseWrite = bindWrite
                   , fuseGetFileSystemStats = bindGetFileSystemStats
                   , fuseFlush = bindFlush
                   , fuseRelease = bindRelease
                   , fuseSynchronizeFile = bindSynchronizeFile
                   }

fileStatusToEntryType :: FileStatus -> EntryType
fileStatusToEntryType status 
    | isSymbolicLink    status = SymbolicLink
    | isNamedPipe       status = NamedPipe
    | isCharacterDevice status = CharacterSpecial
    | isDirectory       status = Directory
    | isBlockDevice     status = BlockSpecial
    | isRegularFile     status = RegularFile
    | isSocket          status = Socket
    | otherwise                = Unknown
bindGetFileStat :: FilePath -> IO (Either Errno FileStat)
bindGetFileStat path = 
    do status <- getSymbolicLinkStatus path
       return $ Right $ FileStat
                  { statEntryType        = fileStatusToEntryType status
                  , statFileMode         = fileMode status
                  , statLinkCount        = linkCount status
                  , statFileOwner        = fileOwner status
                  , statFileGroup        = fileGroup status
                  , statSpecialDeviceID  = specialDeviceID status
                  , statFileSize         = fileSize status
                  , statBlocks           = fromIntegral
                                               (fileSize status `div` 1024)
                  , statAccessTime       = accessTime status
                  , statModificationTime = modificationTime status
                  , statStatusChangeTime = statusChangeTime status
                  }

bindReadSymbolicLink :: FilePath -> IO (Either Errno FilePath)
bindReadSymbolicLink path =
    do target <- readSymbolicLink path
       return (Right target)

bindGetDirectoryContents :: FilePath -> IO (Either Errno [(FilePath, EntryType)])
bindGetDirectoryContents path =
    do names <- getDirectoryContents path
       mapM pairType names >>= return . Right
    where pairType name =
              do status <- getSymbolicLinkStatus (path ++ "/" ++ name)
                 return (name, fileStatusToEntryType status)

bindCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
bindCreateDevice path entryType mode dev =
    do createDevice path (entryTypeToFileMode entryType `unionFileModes` mode) dev
       return eOK

bindCreateDirectory :: FilePath -> FileMode -> IO Errno
bindCreateDirectory path mode =
    do createDirectory path mode
       return eOK

bindRemoveLink :: FilePath -> IO Errno
bindRemoveLink path =
    do removeLink path
       return eOK

bindRemoveDirectory :: FilePath -> IO Errno
bindRemoveDirectory path =
    do removeDirectory path
       return eOK

bindCreateSymbolicLink :: FilePath -> FilePath -> IO Errno
bindCreateSymbolicLink src dest =
    do createSymbolicLink src dest
       return eOK

bindRename :: FilePath -> FilePath -> IO Errno
bindRename src dest =
    do rename src dest
       return eOK

bindCreateLink :: FilePath -> FilePath -> IO Errno
bindCreateLink src dest =
    do createLink src dest
       return eOK

bindSetFileMode :: FilePath -> FileMode -> IO Errno
bindSetFileMode path mode =
    do setFileMode path mode
       return eOK

bindSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno
bindSetOwnerAndGroup path uid gid =
    do setOwnerAndGroup path uid gid
       return eOK

bindSetFileSize :: FilePath -> FileOffset -> IO Errno
bindSetFileSize path off =
    do setFileSize path off
       return eOK

bindSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno
bindSetFileTimes path accessTime modificationTime =
    do setFileTimes path accessTime modificationTime
       return eOK

bindOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO Errno
bindOpen path mode flags =
    do fd <- openFd path mode Nothing flags
       closeFd fd
       return eOK

bindRead :: FilePath -> ByteCount -> FileOffset
         -> IO (Either Errno (String, ByteCount))
bindRead path count off =
    do fd <- openFd path ReadOnly Nothing defaultFileFlags
       newOff <- fdSeek fd AbsoluteSeek off
       if off /= newOff
          then do closeFd fd
                  return (Left eINVAL)
          else do (content, bytesRead) <- fdRead fd count 
                  return (Right (content, bytesRead))

bindWrite :: FilePath -> String -> FileOffset -> IO (Either Errno ByteCount)
bindWrite path buf off =
    do fd <- openFd path WriteOnly Nothing defaultFileFlags
       newOff <- fdSeek fd AbsoluteSeek off
       if off /= newOff
          then do closeFd fd
                  return (Left eINVAL)
          else do res <- fdWrite fd buf
                  return (Right res)

bindGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
bindGetFileSystemStats _ = return (Left eOK)

bindFlush :: FilePath -> IO Errno
bindFlush _ = return eOK

bindRelease :: FilePath -> Int -> IO ()
bindRelease _ _ = return ()

bindSynchronizeFile :: FilePath -> SyncType -> IO Errno
bindSynchronizeFile _ _ = return eOK
