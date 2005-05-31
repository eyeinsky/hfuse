module Main where

import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import HFuse

main :: IO ()
main = fuseMain helloFSOps defaultExceptionHandler

helloFSOps :: FuseOperations
helloFSOps =
    FuseOperations { fuseGetFileStat = helloGetFileStat
                   , fuseReadSymbolicLink = \_ -> return (Left eNOSYS)
                   , fuseGetDirectoryContents = helloGetDirectoryContents
                   , fuseCreateDevice = \_ _ _ _ ->  return eNOSYS
                   , fuseCreateDirectory = \_ _ -> return eNOSYS
                   , fuseRemoveLink = \_ -> return eNOSYS
                   , fuseRemoveDirectory = \_ -> return eNOSYS
                   , fuseCreateSymbolicLink = \_ _ -> return eNOSYS
                   , fuseRename = \_ _ -> return eNOSYS
                   , fuseCreateLink = \_ _ -> return eNOSYS
                   , fuseSetFileMode = \_ _ -> return eNOSYS
                   , fuseSetOwnerAndGroup = \_ _ _ -> return eNOSYS
                   , fuseSetFileSize = \_ _ -> return eNOSYS
                   , fuseSetFileTimes = \_ _ _ -> return eNOSYS
                   , fuseOpen = helloOpen
                   , fuseRead = helloRead
                   , fuseWrite = \_ _ _ -> return (Left eNOSYS)
                   , fuseGetFileSystemStats = return (Left eNOSYS)
                   , fuseFlush = \_ -> return eOK
                   , fuseRelease = \_ _ -> return ()
                   , fuseSynchronizeFile = \_ _ -> return eNOSYS
                   }

helloString :: String
helloString = "Hello World, HFuse!\n"

helloPath :: FilePath
helloPath = "/hello"

helloGetFileStat :: FilePath -> IO (Either Errno FileStat)
helloGetFileStat "/" = do
    ctx <- getFuseContext
    return $ Right $ FileStat { statEntryType = Directory
                              , statFileMode = foldr1 unionFileModes
                                                 [ ownerReadMode
                                                 , ownerExecuteMode
                                                 , groupReadMode
                                                 , groupExecuteMode
                                                 , otherReadMode
                                                 , otherExecuteMode
                                                 ]
                              , statLinkCount = 2
                              , statFileOwner = fuseCtxUserID ctx
                              , statFileGroup = fuseCtxGroupID ctx
                              , statSpecialDeviceID = 0
                              , statFileSize = 4096
                              , statBlocks = 1
                              , statAccessTime = 0
                              , statModificationTime = 0
                              , statStatusChangeTime = 0
                              }
helloGetFileStat path | path == helloPath = do
    ctx <- getFuseContext
    return $ Right $ FileStat { statEntryType = RegularFile
                              , statFileMode = foldr1 unionFileModes
                                                 [ ownerReadMode
                                                 , groupReadMode
                                                 , otherReadMode
                                                 ]
                              , statLinkCount = 1
                              , statFileOwner = fuseCtxUserID ctx
                              , statFileGroup = fuseCtxGroupID ctx
                              , statSpecialDeviceID = 0
                              , statFileSize = fromIntegral $ length helloString
                              , statBlocks = 1
                              , statAccessTime = 0
                              , statModificationTime = 0
                              , statStatusChangeTime = 0
                              }
helloGetFileStat _ =
    return $ Left eNOENT

helloGetDirectoryContents :: FilePath -> IO (Either Errno [(FilePath, EntryType)])
helloGetDirectoryContents "/" =
    return $ Right [ (".",  Directory)
                   , ("..", Directory)
                   , (tail helloPath, RegularFile) ]
helloGetDirectoryContents _ = return (Left (eNOENT))

helloOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO Errno
helloOpen path mode flags
    | path == helloPath = case mode of
                            ReadOnly -> return eOK
                            _        -> return eACCES
    | otherwise         = return eNOENT


helloRead :: FilePath -> ByteCount -> FileOffset
          -> IO (Either Errno (String, ByteCount))
helloRead path byteCount offset
    | path == helloPath =
        let count = max (length helloString) (fromIntegral byteCount)
         in return $ Right (take count helloString, fromIntegral count)
    | otherwise         = return $ Left eNOENT
