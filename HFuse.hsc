-----------------------------------------------------------------------------
-- |
-- Module      :  HFuse
-- Copyright   :  (c) Jérémy Bobbio
-- License     :  BSD-style
-- 
-- Maintainer  :  jeremy.bobbio@etu.upmc.fr
-- Stability   :  experimental
-- Portability :  GHC 6.4
--
-- HFuse is a binding for the FUSE (Filesystem in USErspace) library.
--
-- See <http://fuse.sourceforge.net/>
--
-- This library allow new filesystem implementation as simple user-land
-- programs.
--
-- The binding tries to follow as much as possible current Haskell POSIX
-- interface in "System.Posix.Files" and "System.Posix.Directory".
-- 
-- FUSE uses POSIX thread, thus Haskell implementation needs to be linked
-- against a threaded runtime system (eg. using the @threaded@ GHC option).
--
-----------------------------------------------------------------------------
module HFuse
    ( -- * Using FUSE

      -- $intro

      module Foreign.C.Error
    , FuseOperations(..)
    , fuseMain -- :: FuseOperations -> (Exception -> IO Errno) -> IO ()
    , defaultExceptionHandler -- :: Exception -> IO Errno
      -- * Operations datatypes
    , FileStat(..)
    , EntryType(..)
    , FileSystemStats(..)
    , SyncType(..)
      -- * FUSE Context
    , getFuseContext -- :: IO FuseContext
    , FuseContext(fuseCtxUserID, fuseCtxGroupID, fuseCtxProcessID)
      -- * File modes
    , entryTypeToFileMode -- :: EntryType -> FileMode
    , OpenMode(..)
    , OpenFileFlags(..)
    , intersectFileModes -- :: FileMode
    , unionFileModes -- :: FileMode
    ) where

import Prelude hiding ( Read )

import Control.Exception ( Exception, handle )
import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.Marshal
import System.Environment ( getProgName, getArgs )
import System.IO ( hPutStrLn, stderr )
import System.Posix.Types
import System.Posix.Files ( accessModes, intersectFileModes, unionFileModes )
import System.Posix.IO ( OpenMode(..), OpenFileFlags(..) )

-- TODO: FileMode -> Permissions
-- TODO: Arguments !
-- TODO: implement binding to fuse_invalidate
-- TODO: bind fuse_*xattr

#include <sys/statfs.h>
#include <dirent.h>
#include <fuse.h>
#include <fcntl.h>

{- $intro
'FuseOperations' contains a field for each filesystem operations that can be called
by FUSE. Think like if you were implementing a file system inside the Linux kernel.

Each actions must return a POSIX error code, also called 'Errno' reflecting
operation relult. For actions not using 'Either', you should return 'eOK' in case
of success.

Read and writes are done with Haskell 'String' type. Even if this representation
is known to have drawbacks, the binding try to be coherent with current
Haskell libraries.

-}

{-  All operations should return the negated error value (-errno) on
      error.
-}

-- | Used by 'fuseGetFileStat'.
data FileStat = FileStat { statEntryType :: EntryType
                         , statFileMode :: FileMode
                         , statLinkCount :: LinkCount
                         , statFileOwner :: UserID
                         , statFileGroup :: GroupID
                         , statSpecialDeviceID :: DeviceID
                         , statFileSize :: FileOffset
                         , statBlocks :: Integer
                         , statAccessTime :: EpochTime
                         , statModificationTime :: EpochTime
                         , statStatusChangeTime :: EpochTime
                         }

{-  getattr() doesn't need to fill in the following fields:
        st_ino
        st_dev
        st_blksize
-}

{-  readlink() should fill the buffer with a null terminated string.  The
    buffer size argument includes the space for the terminating null
    character.  If the linkname is too long to fit in the buffer, it should
    be truncated.  The return value should be 0 for success.
-}

-- | Used by 'fuseGetDirectoryContents' implementation to specify the type of
-- a directory entry.
data EntryType
    = Unknown            -- ^ Unknown entry type
    | NamedPipe
    | CharacterSpecial
    | Directory
    | BlockSpecial
    | RegularFile
    | SymbolicLink
    | Socket

entryTypeToDT :: EntryType -> Int
entryTypeToDT Unknown          = (#const DT_UNKNOWN)
entryTypeToDT NamedPipe        = (#const DT_FIFO)
entryTypeToDT CharacterSpecial = (#const DT_CHR)
entryTypeToDT Directory        = (#const DT_DIR)
entryTypeToDT BlockSpecial     = (#const DT_BLK)
entryTypeToDT RegularFile      = (#const DT_REG)
entryTypeToDT SymbolicLink     = (#const DT_LNK)
entryTypeToDT Socket           = (#const DT_SOCK)

fileTypeModes :: FileMode
fileTypeModes = (#const S_IFMT)

blockSpecialMode :: FileMode
blockSpecialMode = (#const S_IFBLK)

characterSpecialMode :: FileMode
characterSpecialMode = (#const S_IFCHR)

namedPipeMode :: FileMode
namedPipeMode = (#const S_IFIFO)

regularFileMode :: FileMode
regularFileMode = (#const S_IFREG)

directoryMode :: FileMode
directoryMode = (#const S_IFDIR)

symbolicLinkMode :: FileMode
symbolicLinkMode = (#const S_IFLNK)

socketMode :: FileMode
socketMode = (#const S_IFSOCK)

-- | Converts an 'EntryType' into the corresponding POSIX 'FileMode'.
entryTypeToFileMode :: EntryType -> FileMode
entryTypeToFileMode Unknown          = 0
entryTypeToFileMode NamedPipe        = namedPipeMode
entryTypeToFileMode CharacterSpecial = characterSpecialMode
entryTypeToFileMode Directory        = directoryMode
entryTypeToFileMode BlockSpecial     = blockSpecialMode
entryTypeToFileMode RegularFile      = regularFileMode
entryTypeToFileMode SymbolicLink     = symbolicLinkMode
entryTypeToFileMode Socket           = socketMode

fileModeToEntryType :: FileMode -> EntryType
fileModeToEntryType mode
    | fileType == namedPipeMode        = NamedPipe
    | fileType == characterSpecialMode = CharacterSpecial
    | fileType == directoryMode        = Directory
    | fileType == blockSpecialMode     = BlockSpecial
    | fileType == regularFileMode      = RegularFile
    | fileType == symbolicLinkMode     = SymbolicLink
    | fileType == socketMode           = Socket
    where fileType = mode .&. (#const S_IFMT)

{-  getdir() is the opendir(), readdir(), ..., closedir() sequence
    in one call. For each directory entry the filldir parameter should
    be called. 
-}


{-
    There is no create() operation, mknod() will be called for
    creation of all non directory, non symlink nodes.
-}

{-  open() should not return a filehandle, but 0 on success.  No
    creation, or trunctation flags (O_CREAT, O_EXCL, O_TRUNC) will be
    passed to open().  Open should only check if the operation is
    permitted for the given flags.
-}

{-  read(), write() are not passed a filehandle, but rather a
    pathname.  The offset of the read and write is passed as the last
    argument, like the pread() and pwrite() system calls.  (NOTE:
    read() should always return the number of bytes requested, except
    at end of file)

    TODO: String type was used to mimic System.Posix.IO but it should change
          after inclusion of better I/O system in Haskell libraries.
-}
-- | Type used by the 'fuseGetFileSystemStats'.
data FileSystemStats = FileSystemStats
    { fsStatBlockSize :: Integer
      -- ^ Optimal transfer block size. FUSE default is 512.
    , fsStatBlockCount :: Integer
      -- ^ Total data blocks in file system.
    , fsStatBlocksFree :: Integer
      -- ^ Free blocks in file system.
    , fsStatBlocksAvailable :: Integer
      -- ^ Free blocks available to non-superusers.
    , fsStatFileCount :: Integer
      -- ^ Total file nodes in file system.
    , fsStatFilesFree :: Integer
      -- ^ Free file nodes in file system.
    , fsStatMaxNameLength :: Integer
      -- ^ Maximum length of filenames. FUSE default is 255.
    }


{-  release() is called when an open file has:
        1) all file descriptors closed
        2)
all memory mappings unmapped
    This call need only be implemented if this information is required,
    otherwise set this function to NULL.

    TODO: Find out what these "flags" are (Int here).
-}

-- | Used by 'fuseSynchronizeFile'.
data SyncType
    = FullSync
    -- ^ Synchronize all in-core parts of a file to disk: file content and
    -- metadata.
    | DataSync
    -- ^ Synchronize only the file content.
    deriving (Eq, Enum)
{-  fsync() has a boolean 'datasync' parameter which if TRUE then do
    an fdatasync() operation.
-}


-- | Returned by 'getFuseContext'.
data FuseContext = FuseContext
    { fuseCtxUserID :: UserID
    , fuseCtxGroupID :: GroupID
    , fuseCtxProcessID :: ProcessID
    }

-- | Returns the context of the program doing the current FUSE call.
getFuseContext :: IO FuseContext
getFuseContext =
    do pCtx <- fuse_get_context
       userID <- (#peek struct fuse_context, uid) pCtx
       groupID <- (#peek struct fuse_context, gid) pCtx
       processID <- (#peek struct fuse_context, pid) pCtx
       return $ FuseContext { fuseCtxUserID = userID
                            , fuseCtxGroupID = groupID
                            , fuseCtxProcessID = processID
                            }

-- | This record, given to 'fuseMain', binds each required file system operations.
--
-- Each field is named against 'System.Posix' names. Matching Linux system calls
-- are also given as a reference.
--
-- * 'fuseGetFileStat' implements
--   'System.Posix.Files.getSymbolicLinkStatus' operation (POSIX @lstat(2)@).
--
-- * 'fuseReadSymbolicLink' implements
--   'System.Posix.Files.readSymbolicLink' operation (POSIX @readlink(2)@).
--   The returned 'FilePath' might be truncated depending on caller
--   buffer size.
--
-- * 'fuseGetDirectoryContents' implements
--   'System.Directory.getDirectoryContents' (POSIX @readddir(2)@).
--
-- * 'fuseCreateDevice' implements 'System.Posix.Files.createDevice'
--   (POSIX @mknod(2)@).
--   This function will also be called for regular file creation.
--
-- * 'fuseCreateDirectory' implements 'System.Posix.Directory.createDirectory'
--   (POSIX @mkdir(2)@).
--
-- * 'fuseRemoveLink' implements 'System.Posix.Files.removeLink'
--   (POSIX @unlink(2)@).
--
-- * 'fuseRemoveDirectory' implements 'System.Posix.Directory.removeDirectory'
--   (POSIX @rmdir(2)@).
--
-- * 'fuseCreateSymbolicLink' implements
--   'System.Posix.Files.createSymbolicLink' (POSIX @symlink(2)@).
--
-- * 'fuseRename' implements 'System.Posix.Files.rename' (POSIX @rename(2)@).
--
-- * 'fuseCreateLink' implements 'System.Posix.Files.createLink'
--   (POSIX @link(2)@).
--
-- * 'fuseSetFileMode' implements 'System.Posix.Files.setFileMode'
--   (POSIX @chmod(2)@).
--
-- * 'fuseSetOwnerAndGroup' implements 'System.Posix.Files.setOwnerAndGroup'
--   (POSIX @chown(2)@).
--
-- * 'fuseSetFileSize' implements 'System.Posix.Files.setFileSize'
--   (POSIX @truncate(2)@).
--
-- * 'fuseSetFileTimes' implements 'System.Posix.Files.setFileTimes'
--   (POSIX @utime(2)@).
--
-- * 'fuseOpen' implements 'System.Posix.Files.openFd'
--   (POSIX @open(2)@), but this does not actually returns a file handle
--    but 'eOK' if the operation is permitted with the given flags.
--    No creation, exclusive access or truncating flags will be passed.
--
-- * 'fuseRead' implements Unix98 @pread(2)@. It differs
--   from 'System.Posix.Files.fdRead' by the explicit 'FileOffset' argument.
--   
-- * 'fuseWrite' implements Unix98 @pwrite(2)@. It differs
--   from 'System.Posix.Files.fdWrite' by the explicit 'FileOffset' argument.
-- 
-- * 'fuseGetFileSystemStats' implements @statfs(2)@. 
--
-- * 'fuseFlush' is called when @close(2)@ has been called on an open file.
--   Note: this does not mean that the file is released.  This function may be
--   called more than once for each @open(2)@.  The return value is passed on
--   to the @close(2)@ system call.
--
-- * 'fuseRelease' is called when an open file has all file descriptors closed
--   and all memory mappings unmapped.  For every @open@ call there will be
--   exactly one @release@ call with the same flags.  It is possible to have
--   a file opened more than once, in which case only the last release will
--   mean, that no more reads or writes will happen on the file.
-- 
-- * 'fuseSynchronizeFile' implements @fsync(2)@.
--
data FuseOperations = FuseOperations
      { fuseGetFileStat :: FilePath -> IO (Either Errno FileStat)
      , fuseReadSymbolicLink :: FilePath -> IO (Either Errno FilePath)
      , fuseGetDirectoryContents :: FilePath
                                 -> IO (Either Errno [(FilePath, EntryType)])
      , fuseCreateDevice :: FilePath -> EntryType -> FileMode
                         -> DeviceID -> IO Errno
      , fuseCreateDirectory :: FilePath -> FileMode -> IO Errno
      , fuseRemoveLink :: FilePath -> IO Errno
      , fuseRemoveDirectory :: FilePath -> IO Errno
      , fuseCreateSymbolicLink :: FilePath -> FilePath -> IO Errno
      , fuseRename :: FilePath -> FilePath -> IO Errno
      , fuseCreateLink :: FilePath -> FilePath -> IO Errno
      , fuseSetFileMode :: FilePath -> FileMode -> IO Errno
      , fuseSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno
      , fuseSetFileSize :: FilePath -> FileOffset -> IO Errno
      , fuseSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno
      , fuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO Errno
      , fuseRead :: FilePath -> ByteCount -> FileOffset
                 -> IO (Either Errno (String, ByteCount))
      , fuseWrite :: FilePath -> String -> FileOffset
                  -> IO (Either Errno ByteCount)
      , fuseGetFileSystemStats :: IO (Either Errno FileSystemStats)
      , fuseFlush :: FilePath -> IO Errno
      , fuseRelease :: FilePath -> Int -> IO ()
      , fuseSynchronizeFile :: FilePath -> SyncType -> IO Errno
      }

-- | Main function of FUSE.
-- This is all that has to be called from the @main@ function. On top of
-- the 'FuseOperations' record with filesystem implementation, you must give
-- an exception handler converting Haskell exceptions to 'Errno'.
-- 
-- This function does the following:
--
--   * parses command line options (@-d@, @-s@ and @-h@) ;
--
--   * passes all options after @--@ to the fusermount program ;
--
--   * mounts the filesystem by calling @fusermount@ ;
--
--   * installs signal handlers for 'System.Posix.Signals.keyboardSignal',
--     'System.Posix.Signals.lostConnection',
--     'System.Posix.Signals.softwareTermination' and
--     'System.Posix.Signals.openEndedPipe' ;
--
--   * registers an exit handler to unmount the filesystem on program exit ;
--
--   * registers the operations ;
--
--   * calls FUSE event loop.
fuseMain :: FuseOperations -> (Exception -> IO Errno) -> IO ()
fuseMain ops handler =
    allocaBytes (#size struct fuse_operations) $ \ pOps -> do
      mkGetAttr wrapGetAttr   >>= (#poke struct fuse_operations, getattr)  pOps
      mkReadLink wrapReadLink >>= (#poke struct fuse_operations, readlink) pOps 
      mkGetDir wrapGetDir     >>= (#poke struct fuse_operations, getdir)   pOps 
      mkMkNod wrapMkNod       >>= (#poke struct fuse_operations, mknod)    pOps 
      mkMkDir wrapMkDir       >>= (#poke struct fuse_operations, mkdir)    pOps 
      mkUnlink wrapUnlink     >>= (#poke struct fuse_operations, unlink)   pOps 
      mkRmDir wrapRmDir       >>= (#poke struct fuse_operations, rmdir)    pOps 
      mkSymLink wrapSymLink   >>= (#poke struct fuse_operations, symlink)  pOps 
      mkRename wrapRename     >>= (#poke struct fuse_operations, rename)   pOps 
      mkLink wrapLink         >>= (#poke struct fuse_operations, link)     pOps 
      mkChMod wrapChMod       >>= (#poke struct fuse_operations, chmod)    pOps 
      mkChOwn wrapChOwn       >>= (#poke struct fuse_operations, chown)    pOps 
      mkTruncate wrapTruncate >>= (#poke struct fuse_operations, truncate) pOps 
      mkUTime wrapUTime       >>= (#poke struct fuse_operations, utime)    pOps 
      mkOpen wrapOpen         >>= (#poke struct fuse_operations, open)     pOps 
      mkRead wrapRead         >>= (#poke struct fuse_operations, read)     pOps 
      mkWrite wrapWrite       >>= (#poke struct fuse_operations, write)    pOps 
      mkStatFS wrapStatFS     >>= (#poke struct fuse_operations, statfs)   pOps
      mkFlush wrapFlush       >>= (#poke struct fuse_operations, flush)    pOps
      mkRelease wrapRelease   >>= (#poke struct fuse_operations, release)  pOps 
      mkFSync wrapFSync       >>= (#poke struct fuse_operations, fsync)    pOps
      -- TODO: Implement this
      (#poke struct fuse_operations, setxattr)    pOps nullPtr
      (#poke struct fuse_operations, getxattr)    pOps nullPtr
      (#poke struct fuse_operations, listxattr)   pOps nullPtr
      (#poke struct fuse_operations, removexattr) pOps nullPtr
      prog <- getProgName
      args <- getArgs
      let allArgs = (prog:args)
          argc    = length allArgs
      withMany withCString allArgs $ \ pAddrs  ->
          withArray pAddrs $ \ pArgv ->
              fuse_main argc pArgv pOps      
    where fuseHandler :: Exception -> IO CInt
          fuseHandler e = handler e >>= return . unErrno
          wrapGetAttr :: CGetAttr
          wrapGetAttr pFilePath pStat = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 eitherFileStat <- (fuseGetFileStat ops) filePath
                 case eitherFileStat of
                   Left (Errno errno) -> return (- errno)
                   Right stat         ->
                     do (#poke struct stat, st_mode)   pStat
                            (entryTypeToFileMode (statEntryType stat)
                             `unionFileModes`
                             (statFileMode stat `intersectFileModes` accessModes))
                        (#poke struct stat, st_nlink)  pStat (statLinkCount  stat)
                        (#poke struct stat, st_uid)    pStat (statFileOwner  stat)
                        (#poke struct stat, st_gid)    pStat (statFileGroup  stat)
                        (#poke struct stat, st_rdev)   pStat
                            (statSpecialDeviceID stat)
                        (#poke struct stat, st_size)   pStat (statFileSize   stat)
                        (#poke struct stat, st_blocks) pStat
                            (fromIntegral (statBlocks stat) :: (#type blkcnt_t))
                        (#poke struct stat, st_atime)  pStat (statAccessTime stat)
                        (#poke struct stat, st_mtime)  pStat
                            (statModificationTime stat)
                        (#poke struct stat, st_ctime)  pStat
                            (statStatusChangeTime stat)
                        return okErrno
          wrapReadLink :: CReadLink
          wrapReadLink pFilePath pBuf bufSize = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 return (- unErrno eNOSYS)
                 eitherTarget <- (fuseReadSymbolicLink ops) filePath
                 case eitherTarget of
                   Left (Errno errno) -> return (- errno)
                   Right target ->
                     do pokeCStringLen0 (pBuf, (fromIntegral bufSize)) target
                        return okErrno
          wrapGetDir :: CGetDir
          wrapGetDir pFilePath pDirHandle pDirFil = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 let filler (entryFilePath, entryType) =
                         withCString entryFilePath $ \ pEntryFilePath ->
                             (mkDirFil pDirFil) pDirHandle pEntryFilePath
                             (entryTypeToDT entryType) >>= return . Errno
                 eitherContents <- (fuseGetDirectoryContents ops) filePath 
                 case eitherContents of
                   Left (Errno errno) -> return (- errno)
                   Right contents     -> 
                     do mapM_ filler contents
                        return okErrno
          wrapMkNod :: CMkNod
          wrapMkNod pFilePath mode dev = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (Errno errno) <- (fuseCreateDevice ops) filePath
                                      (fileModeToEntryType mode) mode dev
                 return (- errno)
          wrapMkDir :: CMkDir
          wrapMkDir pFilePath mode = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (Errno errno) <- (fuseCreateDirectory ops) filePath mode
                 return (- errno)
          wrapUnlink :: CUnlink
          wrapUnlink pFilePath = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (Errno errno) <- (fuseRemoveLink ops) filePath
                 return (- errno)
          wrapRmDir :: CRmDir
          wrapRmDir pFilePath = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (Errno errno) <- (fuseRemoveDirectory ops) filePath
                 return (- errno)
          wrapSymLink :: CSymLink
          wrapSymLink pSource pDestination = handle fuseHandler $
              do source <- peekCString pSource
                 destination <- peekCString pDestination
                 (Errno errno) <- (fuseCreateSymbolicLink ops) source destination
                 return (- errno)
          wrapRename :: CRename
          wrapRename pOld pNew = handle fuseHandler $
              do old <- peekCString pOld
                 new <- peekCString pNew
                 (Errno errno) <- (fuseRename ops) old new
                 return (- errno)
          wrapLink :: CLink
          wrapLink pSource pDestination = handle fuseHandler $
              do source <- peekCString pSource
                 destination <- peekCString pDestination
                 (Errno errno) <- (fuseCreateLink ops) source destination
                 return (- errno)
          wrapChMod :: CChMod
          wrapChMod pFilePath mode = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (Errno errno) <- (fuseSetFileMode ops) filePath mode
                 return (- errno)
          wrapChOwn :: CChOwn
          wrapChOwn pFilePath uid gid = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (Errno errno) <- (fuseSetOwnerAndGroup ops) filePath uid gid
                 return (- errno)
          wrapTruncate :: CTruncate
          wrapTruncate pFilePath off = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (Errno errno) <- (fuseSetFileSize ops) filePath off
                 return (- errno)
          wrapUTime :: CUTime
          wrapUTime pFilePath pUTimBuf = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 accessTime <- (#peek struct utimbuf, actime) pUTimBuf
                 modificationTime <- (#peek struct utimbuf, modtime) pUTimBuf
                 (Errno errno) <- (fuseSetFileTimes ops) filePath
                                      accessTime modificationTime
                 return (- errno)
          wrapOpen :: COpen
          wrapOpen pFilePath flags = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 let append    = (#const O_APPEND)   .&. flags == (#const O_APPEND)
                     noctty    = (#const O_NOCTTY)   .&. flags == (#const O_NOCTTY)
                     nonBlock  = (#const O_NONBLOCK) .&. flags == (#const O_NONBLOCK)
                     how | (#const O_RDWR) .&. flags == (#const O_RDWR) = ReadWrite
                         | (#const O_WRONLY) .&. flags == (#const O_WRONLY) = WriteOnly
                         | otherwise = ReadOnly
                     openFileFlags = OpenFileFlags { append = append
                                                   , exclusive = False
                                                   , noctty = noctty
                                                   , nonBlock = nonBlock
                                                   , trunc = False
                                                   }
                 (Errno errno) <- (fuseOpen ops) filePath how openFileFlags
                 return (- errno)
          wrapRead :: CRead
          wrapRead pFilePath pBuf bufSiz off = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 eitherRead <- (fuseRead ops) filePath bufSiz off
                 case eitherRead of
                   Left (Errno errno) -> return (- errno)
                   Right (bytes, byteCount)  -> 
                     do pokeCStringLen (pBuf, fromIntegral byteCount) bytes
                        return (fromIntegral byteCount)
          wrapWrite :: CWrite
          wrapWrite pFilePath pBuf bufSiz off = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 buf <- peekCStringLen (pBuf, fromIntegral bufSiz)
                 eitherBytes <- (fuseWrite ops) filePath buf off
                 case eitherBytes of
                   Left  (Errno errno) -> return (- errno)
                   Right bytes         -> return (fromIntegral bytes)
          wrapStatFS :: CStatFS
          wrapStatFS pStatFS = handle fuseHandler $
            do eitherStatFS <- fuseGetFileSystemStats ops
               case eitherStatFS of
                 Left (Errno errno) -> return (- errno)
                 Right stat         ->
                   do (#poke struct statfs, f_bsize) pStatFS
                          (fromIntegral (fsStatBlockSize stat) :: (#type long))
                      (#poke struct statfs, f_blocks) pStatFS
                          (fromIntegral (fsStatBlockCount stat) :: (#type long))
                      (#poke struct statfs, f_bfree) pStatFS
                          (fromIntegral (fsStatBlocksFree stat) :: (#type long))
                      (#poke struct statfs, f_bavail) pStatFS
                          (fromIntegral (fsStatBlocksAvailable
                                             stat) :: (#type long))
                      (#poke struct statfs, f_files) pStatFS
                           (fromIntegral (fsStatFileCount stat) :: (#type long))
                      (#poke struct statfs, f_ffree) pStatFS
                          (fromIntegral (fsStatFilesFree stat) :: (#type long))
                      (#poke struct statfs, f_namelen) pStatFS
                          (fromIntegral (fsStatMaxNameLength stat) :: (#type long))
                      return 0
          wrapFlush :: CFlush
          wrapFlush pFilePath = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (Errno errno) <- (fuseFlush ops) filePath
                 return (- errno)
          wrapRelease :: CRelease
          wrapRelease pFilePath flags = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (fuseRelease ops) filePath flags
                 return 0
          wrapFSync :: CFSync
          wrapFSync pFilePath isFullSync = handle fuseHandler $
              do filePath <- peekCString pFilePath
                 (Errno errno) <- (fuseSynchronizeFile ops)
                                      filePath (toEnum isFullSync)
                 return (- errno)

-- | Default exception handler.
-- Print the exception on error output and returns 'eFAULT'.
defaultExceptionHandler :: (Exception -> IO Errno)
defaultExceptionHandler e = hPutStrLn stderr (show e) >> return eFAULT


-----------------------------------------------------------------------------
-- Miscelaneous utilities

unErrno :: Errno -> CInt
unErrno (Errno errno) = errno

okErrno :: CInt
okErrno = 0

pokeCStringLen :: CStringLen -> String -> IO ()
pokeCStringLen (pBuf, bufSize) src =
    pokeArray pBuf $ take bufSize $ map castCharToCChar src

pokeCStringLen0 :: CStringLen -> String -> IO ()
pokeCStringLen0 (pBuf, bufSize) src =
    pokeArray0 0 pBuf $ take (bufSize - 1) $ map castCharToCChar src

-----------------------------------------------------------------------------
-- C land

---
-- exported C called from Haskell
---  

data CFuseOperations
foreign import ccall threadsafe "fuse.h fuse_main"
    fuse_main :: Int -> Ptr CString -> Ptr CFuseOperations -> IO ()

data StructFuse
foreign import ccall threadsafe "fuse.h fuse_get_context"
    fuse_get_context :: IO (Ptr StructFuse)

---
-- dynamic Haskell called from C
---

data CStat -- struct stat
type CGetAttr = CString -> Ptr CStat -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkGetAttr :: CGetAttr -> IO (FunPtr CGetAttr)

type CReadLink = CString -> CString -> CSize -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkReadLink :: CReadLink -> IO (FunPtr CReadLink)

type CGetDir = CString -> Ptr CDirHandle -> FunPtr CDirFil -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkGetDir :: CGetDir -> IO (FunPtr CGetDir)

type CMkNod = CString -> CMode -> CDev -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkMkNod :: CMkNod -> IO (FunPtr CMkNod)

type CMkDir = CString -> CMode -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkMkDir :: CMkDir -> IO (FunPtr CMkDir)

type CUnlink = CString -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkUnlink :: CUnlink -> IO (FunPtr CUnlink)

type CRmDir = CString -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkRmDir :: CRmDir -> IO (FunPtr CRmDir)

type CSymLink = CString -> CString -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkSymLink :: CSymLink -> IO (FunPtr CSymLink)

type CRename = CString -> CString -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkRename :: CRename -> IO (FunPtr CRename)

type CLink = CString -> CString -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkLink :: CLink -> IO (FunPtr CLink)

type CChMod = CString -> CMode -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkChMod :: CChMod -> IO (FunPtr CChMod)

type CChOwn = CString -> CUid -> CGid -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkChOwn :: CChOwn -> IO (FunPtr CChOwn)

type CTruncate = CString -> COff -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkTruncate :: CTruncate -> IO (FunPtr CTruncate)

data CUTimBuf -- struct utimbuf
type CUTime = CString -> Ptr CUTimBuf -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkUTime :: CUTime -> IO (FunPtr CUTime)

type COpen = CString -> Int -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkOpen :: COpen -> IO (FunPtr COpen)

type CRead = CString -> CString -> CSize -> COff -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkRead :: CRead -> IO (FunPtr CRead)

type CWrite = CString -> CString -> CSize -> COff -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkWrite :: CWrite -> IO (FunPtr CWrite)

data CStructStatFS -- struct fuse_stat_fs
type CStatFS = Ptr CStructStatFS -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkStatFS :: CStatFS -> IO (FunPtr CStatFS)

type CFlush = CString -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkFlush :: CFlush -> IO (FunPtr CFlush)

type CRelease = CString -> Int -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkRelease :: CRelease -> IO (FunPtr CRelease)

type CFSync = CString -> Int -> IO CInt
foreign import ccall threadsafe "wrapper"
    mkFSync :: CFSync -> IO (FunPtr CFSync) 

---
-- dynamic C called from Haskell
---

data CDirHandle -- fuse_dirh_t
type CDirFil = Ptr CDirHandle -> CString -> Int -> IO CInt -- fuse_dirfil_t
foreign import ccall threadsafe "dynamic"
    mkDirFil :: FunPtr CDirFil -> CDirFil

