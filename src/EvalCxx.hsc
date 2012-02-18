{-# LANGUAGE PatternGuards, ViewPatterns, RecordWildCards #-}

{-- Correct ptrace-ing

Our goal is simple: we want to be in control over everything the child does after it execve's. In particular, we want to know which syscall traps are entries and exits (we need this to be able to have ignored syscalls whose entry we replace by entry to getpid, and whose exits we replace with "return 0;"). Since the ptrace interface offers no built-in way to distinguish between the two, this means that we need to keep an in_syscall flag that we toggle at every syscall trap. Correct initialization of this flag is a concern.

How does ptrace-ing commence? The ptrace manual states that once the child has done PTRACE_TRACEME, "all subsequent calls to exec() by this process will cause a SIGTRAP to be sent to it". Unfortunately, this wording seemingly allows for any of the following 6 parent-observable initial sequences of events:

  1. execve entry trap, execve exit trap
  2. execve entry trap
  3. execve exit trap
  4. execve entry trap, separate trap, execve exit trap
  5. execve entry trap, separate trap
  6. separate trap, execve exit trap

Furthermore, different sequences have been observed on different machines (see ptracetest1.c). Worse, we cannot even write a test program to determine which of the sequences happened, since that requires distinguishing between SIGTRAPs caused by entries to syscalls, exits from syscalls, and SIGTRAPs not caused by either entry to or exit from a syscall, but to be able to make that distinction PTRACE_O_TRACESYSGOOD needs to be set, but we cannot reliably set it before the parent's first wait() call (see test2.c). Consequently, execve is just not a solid "first SIGTRAP" to build on.

We therefore abandon execve as ptrace initiator, and instead have the child raise SIGSTOP before it execve's. Now, there are two important aspects here. First, we need to know that the parent will observe the child's SIGSTOP /before/ the child proceeds to do anything else (otherwise anything can happen). This behavior is guaranteed by the glibc manual, which states that:

  If kill is used by a process to send a signal to itself, and the signal is not blocked, then kill delivers at least one signal (which might be some other pending unblocked signal instead of the signal signum) to that process before it returns.

The second important aspect relates to the initialization of the in_syscall flag. We need to know conclusively whether the kill() system call that raise() makes has exited by the time the parent observes the SIGSTOP signal. If it has, the in_syscall flag needs to be initialized to false. If it hasn't, it needs to be initialized to true. So far, on all machines tested it has been the case that the signal is not delivered until after the kill() call has exited, though I can't find documentation that guarantees this. This is currently /the/ big assumption that geordi makes in its ptrace usage. If this behavior turns out to differ between machines, then we may have to resort to something like delaying the initialization of the in_syscall flag until the first execve syscall trap is observed (although that could be tricky, because it would rely on ORIG_EAX still being set to the syscall number on syscall exit traps - behavior that I'm not sure is guaranteed).

-- Secure compilation

Requirements:
  First, since we expose cc1plus/as/ld to malicious data (the code), their execution needs to be ptraced.
  Second, we want to specify the names of the intermediary .s and .o files.

If we try to use a single g++ invocation to compile, assemble, and link, we do not get sufficient control over the names of the intermediary files. We /could/ in theory intercept the exec() calls with ptrace and change the file names, but that's too much trouble.

We could invoke cc1plus/as/ld directly (and in fact previous geordi versions did this), but then we need to pass obscure flags that would otherwise be passed by g++. Previous geordi versions had a separate step in the installation procedure where a script was used to strace g++ to find out these flags and write them to a file which was then read by geordi on startup. It was a kludge.

We currently invoke g++ three times. Once with -S to compile, once with -c to assemble, and once to link. This allows us to specify the intermediary files, and lets g++ add whatever obscure flags it wants. Previous versions of geordi did not use this approach because it seems to require letting g++ vfork and then ptracing the child, which makes things complicated (and we don't want complication in this security-critical code). However, by intercepting g++'s vfork and replacing it with "return 0;", we trick it into thinking it is the newly spawned child process, which causes it to exec() cc1plus/as/ld, replacing itself. This scheme works because g++ (when invoked to do only one thing (e.g. compile/assemble/link)) doesn't have anything useful to do after the exec() anyway.

-- Inherited file descriptors

A newly fork()ed child inherits its parent's file descriptors. In our case, that means that things like the network socket are exposed unless we either close them or set FD_CLOEXEC on them before calling execve().

The most obvious approach is to try and vigilantly set FD_CLOEXEC every time the bot opens an FD. However, that means we immediately have a potential security problem if we forget to do so once, or if Haskell is harboring some FDs we don't know about. We want something more rigorous and fool-proof.

The next most obvious approach is to get a list of open FDs and then close them (except for stdout/stderr) just before calling execve(). (At this point, setting FD_CLOEXEC on them would be equivalent.) Some of the BSD's apparently have closefrom() and/or F_CLOSEM commands for fcnl(), which offers this functionality out of the box. Unfortunately, Linux does not have these, so we'd have to do it manually. Unfortunately, getting a list of open FDs while in our chroot (which has no /proc filesystem) turns out to be hard if not impossible.

The next most obvious approach is to get the highest currently open FD, and then close all FDs (except for stdout/stderr) below it, regardless of whether they're open. FreeBSD apparently has a F_MAXFD fcntl() command that returns the greatest FD currently open by the process, which would be exactly what we need, but unfortunately Linux does not have anything equivalent to it.

The approach we resort to is the following:
- make an estimate N how many files the bot will ever want to have open;
- pick an M well above N;
- setrlimit RLIMIT_NOFILE to M immediately upon program startup to make sure that if our M guess was too low, it will cause the bot to shut down with a file open error rather than expose FDs to the child process;
- close FDs in the range [0, M) just before execve().

In our code, M is close_range_end.

-}

module EvalCxx (evaluator, WithEvaluation, withEvaluation, noEvaluation, EvaluationResult(..), Request(..), CompileConfig(..), Fix(..)) where

import qualified Ptrace
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Flock
import qualified ErrorFilters
import qualified System.Directory
import qualified System.Posix.Process (getProcessID)
import qualified SysCalls
import qualified System.Posix.Internals
import qualified Data.Char as Char

import Paths_geordi (getDataFileName)

import Data.Pointed (Pointed(..))
import Data.Maybe (isNothing)
import Sys (wait, WaitResult(..), strsignal, syscall_off, syscall_ret, fdOfFd, nonblocking_read, chroot, strerror)
import SysCalls (SysCall(..))
import Control.Monad (when, forM_)
import Control.Monad.Fix (fix)
import Foreign (alloca, (.|.))
import System.Environment (getEnvironment)
import System.IO (withFile, IOMode(..), hSetEncoding, utf8, hPutStrLn)
import Foreign.C (CInt, CSize, ePERM, eOK)
import System.Exit (ExitCode(..))
import Data.List ((\\), isPrefixOf)
import Text.Regex (Regex, mkRegex, matchRegex)
import System.Posix.User
  (getGroupEntryForName, getUserEntryForName, setGroupID, setUserID, groupID, userID)
import System.Posix
  (Signal, sigALRM, sigSTOP, sigTRAP, sigKILL, sigSEGV, sigILL, createPipe, setFdOption, executeFile, raiseSignal, ProcessID, openFd, defaultFileFlags, forkProcess, dupTo, stdError, stdOutput, scheduleAlarm, OpenMode(..), exitImmediately, FdOption(..), Resource(..), ResourceLimit(..), ResourceLimits(..), setResourceLimit)
import Clang (Stage(..))
import CompileConfig

#ifdef __x86_64__
import Foreign ((.&.))
#endif

import Prelude hiding ((.))
import Util

#include <sys/reg.h>

data SuperviseResult = Exited ExitCode | DisallowedSyscall SysCall | Signaled Signal | ChildVanished
  deriving Eq

instance Show SuperviseResult where
  show (Exited c) = "Exited: " ++ show c
  show (DisallowedSyscall c) = show c ++ ": " ++ strerror ePERM
  show (Signaled s) = strsignal $ if s == sigALRM then sigKILL else s
    -- We replace sigALRM with sigKILL because the two are caused by two geordi measures with the same function: killing the process if it takes too long. In that sense, sigALRM and sigKILL are both implementation details, but sigKILL has a much nicer strsignal message.
  show ChildVanished = "Child vanished"
    -- This should not actually ever happen, so we don't care about localization.

i386_SYS_exit_group, i386_syscall_instruction :: Num a => a
i386_syscall_instruction = 0x80cd -- "int 0x80"
i386_SYS_exit_group = 252

supervise :: ProcessID → IO SuperviseResult
  -- We assume that the first event observed is the child raising sigSTOP.
supervise pid = alloca $ \wstatp → do
  wait wstatp >>= \s → when (s /= WaitStopped sigSTOP) $ fail $ "first ptraced event not sigSTOP, but " ++ show s
  Ptrace.tracesysgood pid
  Ptrace.syscall pid
  flip fix Nothing $ \sv current_syscall → do
    wstat ← wait wstatp
    case wstat of
      WaitNoChild → return ChildVanished
      WaitExited e → return $ Exited e
      WaitSignaled s → return $ Signaled s
      WaitStopped s | s == sigTRAP → Ptrace.syscall pid >> sv current_syscall
      WaitStopped s | s == (sigTRAP .|. 0x80) →
        case current_syscall of
          Just sc → do
            when (sc `elem` ignored_syscalls) $ Ptrace.pokeuser pid syscall_ret 0
            Ptrace.syscall pid; sv Nothing
          Nothing → do
            #ifdef __x86_64__
            rip ← Ptrace.peekuser pid $ 8 * #const RIP
            instr ← Ptrace.peektext pid (rip - 2)
            if instr .&. 0xffff == i386_syscall_instruction
              then do
                Ptrace.pokeuser pid syscall_off i386_SYS_exit_group
                Ptrace.kill pid
                sv (Just SYS_exit_group)
                return $ Signaled sigKILL -- Not entirely accurate, but it's not worth the hassle to add a new alternative to SuperviseResult.
              else
            #endif
                SysCalls.fromNumber . Ptrace.peekuser pid syscall_off >>= \syscall → case () of
                  ()| syscall `elem` ignored_syscalls → do
                    Ptrace.pokeuser pid syscall_off $ SysCalls.toNumber SYS_getpid
                    Ptrace.syscall pid; sv (Just syscall)
                  ()| syscall `elem` allowed_syscalls → Ptrace.syscall pid >> sv (Just syscall)
                  () → do
                    Ptrace.pokeuser pid syscall_off $ SysCalls.toNumber SYS_exit_group
                    Ptrace.kill pid
                    sv $ Just SYS_exit_group
                    return $ DisallowedSyscall syscall
      WaitStopped sig → Ptrace.kill pid >> sv Nothing >> return (Signaled sig)

-- The documentation for PTRACE_KILL is extremely vague. In supervise above, when we use Ptrace.kill to kill a child attempting to call a disallowed system call (or using a disallowed system call mechanism), it actually restarts the process to finish the system call. That is why we replace the system call with SYS_exit_group, so that one of two things happens: either exit_group succeeds and the next wait returns WaitExited, or it fails and the process is half dead, twitching, and being delivered SIGKILL. Both cases are dealt with adequately by sv.

data Resources = Resources { walltime :: Int, rlimits :: [(Resource, ResourceLimits)], bufsize :: CSize }

close_range_end :: CInt
close_range_end = 25

cap_fds :: IO ()
  -- See section "Inherited file descriptors." in EvalCxx.hsc.
cap_fds = do
  let cre = close_range_end
  setResourceLimit ResourceOpenFiles $
    ResourceLimits (ResourceLimit $ fromIntegral cre) (ResourceLimit $ fromIntegral cre)
  high_fds ← filter (>= cre) . (read .) . (\\ [".", ".."]) . (System.Directory.getDirectoryContents =<< (\s → "/proc/" ++ s ++ "/fd") . show . System.Posix.Process.getProcessID)
  when (high_fds /= []) $ fail $ "fd(s) open >= " ++ show cre ++ ": " ++ show high_fds

data CaptureResult = CaptureResult { supervise_result :: SuperviseResult, output :: String } deriving Eq

capture_restricted :: FilePath → [String] → [(String,String)] → Resources → IO CaptureResult
  -- We assume the program produces UTF-8 encoded text and return it as a proper Unicode String.
capture_restricted a argv env (Resources timeout rlims bs) =
  withResource createPipe $ \(pipe_r, pipe_w) → do
    setFdOption pipe_r NonBlockingRead True
    res ← (=<<) supervise $ forkProcess $ do
      scheduleAlarm timeout
      mapM_ (uncurry setResourceLimit) rlims
      mapM_ (dupTo pipe_w) [stdOutput, stdError]
      forM_ ([0..close_range_end] \\ (fdOfFd . [stdOutput, stdError])) System.Posix.Internals.c_close
      Ptrace.traceme
      raiseSignal sigSTOP
      executeFile a False argv (Just env)
        -- The Haskell implementation of executeFile calls pPrPr_disableITimers, which calls setitimer to disable all interval timers, including ours set a few lines above. However, since by this time we're being ptraced, the setitimer calls are ignored.
      exitImmediately ExitSuccess
    CaptureResult res . UTF8.decode . nonblocking_read pipe_r bs

-- The actual output size is also limited by the pipe buffer.

subst_parseps :: String → String
subst_parseps = f
  where
    f [] = []
    f (c:s) | c == parsep = f s
    f (c:d:s) | Char.isSpace c, d == parsep = c : f s
    f (c:d:s) | d == parsep = c : case f s of
      [] → []
      s'@(',' : _) → s'
      s' → ' ' : s'
    f (c:s) = c : f s

type Line = Int
type Column = Int

data Fix = Fix { fix_begin, fix_end :: (Line, Column), fix_replacement :: String }

data EvaluationResult = EvaluationResult { stage :: Stage, captureResult :: CaptureResult, returnedFix :: Maybe Fix }
  -- The capture result of the last stage attempted.

instance Show EvaluationResult where
  show (EvaluationResult stage (CaptureResult r o) f) = subst_parseps $ ErrorFilters.cleanup_output stage o ++
    if stage == Run
      then case r of
        Exited ExitSuccess → ""
        Signaled s | s ∈ [sigSEGV, sigILL] → parsep : "Undefined behavior detected."
        _ → parsep : show r
      else case r of
        Exited ExitSuccess → if null o then strerror eOK else fixNote
        Exited (ExitFailure _) | not (null o) → fixNote
        _ → parsep : show stage ++ ": " ++ show r
    where fixNote = if isNothing f then "" else " (fix known)"

prog_env :: [(String, String)]
prog_env =
  [ ("GLIBCXX_DEBUG_MESSAGE_LENGTH", "0")
  , ("LD_LIBRARY_PATH", ".")
  , ("LD_PRELOAD", "libtpreload.so.0.0")
  ]

data JailConfig = JailConfig { user, group :: String } deriving Read

jail :: IO ()
jail = do
  cfg ← getDataFileName "jail-config" >>= readTypedFile
  gid ← groupID . getGroupEntryForName (group cfg)
  uid ← userID . getUserEntryForName (user cfg)
  getDataFileName "rt" >>= chroot
  System.Directory.setCurrentDirectory "/"
  setGroupID gid
  setUserID uid

data Request = Request { code :: String, stageOfInterest :: Stage, no_warn :: Bool }

pass_env :: String → Bool
pass_env s = ("LC_" `isPrefixOf` s) || (s `elem` ["PATH", "LD_LIBRARY_PATH"])

capture_success :: CaptureResult
capture_success = CaptureResult (Exited ExitSuccess) ""

{-
  Todo: Analysis.

  let analysis_flags = words "-analyze -analyzer-store=region -analyzer-opt-analyze-nested-blocks -analyzer-check-dead-stores -analyzer-check-objc-mem -analyzer-eagerly-assume -analyzer-check-objc-methodsigs -analyzer-check-objc-unused-ivars -analyzer-check-idempotent-operations"

  ar ← if no_warn req
    then return capture_success
    else capture_restricted "/usr/bin/clang" (basic_flags ++ analysis_flags ++ compileFlags cfg) env (resources Compile)
-}

stagePath :: Stage → String
stagePath Run = "/usr/bin/lli"
stagePath _ = "/usr/bin/clang"

stageArgv :: CompileConfig → Request → Stage → [String]
stageArgv CompileConfig{..} Request{..} s = case s of
  Run → ["-O0", "t.bc", "second", "third", "fourth"]
  Preprocess → words "-cc1 -E -disable-free -main-file-name t.cpp -ferror-limit 1 -fmessage-length 0 -o - -x c++ t.cpp" ++ cf
  Compile → words "-cc1 -emit-llvm-bc -disable-free -disable-llvm-verifier -main-file-name t.cpp -mrelocation-model static -mconstructor-aliases -munwind-tables -momit-leaf-frame-pointer -include preprocessedprelude.hpp -fdiagnostics-parseable-fixits -emit-llvm-bc -ftrapv-handler trapv_handler -ferror-limit 1 -fmessage-length 0 -fcxx-exceptions -fexceptions -o t.bc -x c++ t.cpp" ++ cf
 where cf = ["-w" | no_warn] ++ compileFlags

stageExtraEnv :: Stage → [(String, String)]
stageExtraEnv Run = prog_env
stageExtraEnv _ = []

evaluate :: CompileConfig → Request → IO EvaluationResult
evaluate cfg req = do
  withResource (openFd "lock" ReadOnly Nothing defaultFileFlags) $ \lock_fd → do
  Flock.exclusive lock_fd
  withFile "t.cpp" WriteMode $ \h → hSetEncoding h utf8 >> hPutStrLn h (code req)
    -- Same as utf8-string's System.IO.UTF8.writeFile, but I'm hoping that with GHC's improving UTF-8 support we can eventually drop the dependency on utf8-string altogether.
  baseEnv ← filter (pass_env . fst) . getEnvironment
  let
    runStage :: Stage → Maybe (IO EvaluationResult) → IO EvaluationResult
    runStage stage act = do
      cr ← capture_restricted (stagePath stage) (stageArgv cfg req stage) (baseEnv ++ stageExtraEnv stage) (resources stage)
      case act of
        Just a | cr == CaptureResult (Exited ExitSuccess) "", stage /= stageOfInterest req → a
        _ → return $ EvaluationResult stage cr (findFix $ output cr)
  if stageOfInterest req == Preprocess then runStage Preprocess Nothing else do
  runStage Compile $ Just $ do
  runStage Run Nothing

unescape :: String → String
unescape "" = ""
unescape ('\\':'t':xs) = '\t' : unescape xs
unescape ('\\':'n':xs) = '\n' : unescape xs
unescape ('\\':'"':xs) = '"' : unescape xs
unescape (x:xs) = x : unescape xs

fixitRegex :: Regex
fixitRegex = mkRegex "\nfix-it:\"t.cpp\":\\{([0-9]{1,3}):([0-9]{1,3})-([0-9]{1,3}):([0-9]{1,3})\\}:\"(([^\\]|\\\\(\\\\|n|t|\"))*)\""

findFix :: String → Maybe Fix
findFix (matchRegex fixitRegex → Just [line, col, line', col', s, _, _]) =
  Just $ Fix (read line, read col) (read line', read col') (unescape s)
findFix _ = Nothing
  -- Todo: Find all of them, though that really requires that -Wfatal-errors works reliably, which at the time of this writing it does not.

data WithEvaluation a
  = WithoutEvaluation a
  | WithEvaluation Request (EvaluationResult → a)

instance Functor WithEvaluation where
  fmap f (WithoutEvaluation x) = WithoutEvaluation (f x)
  fmap f (WithEvaluation r g) = WithEvaluation r (f . g)

instance Pointed WithEvaluation where
  point = WithoutEvaluation

-- WithEvaluation is not a monad because it only supports a single evaluation.

withEvaluation :: Request → WithEvaluation EvaluationResult
withEvaluation r = WithEvaluation r id

noEvaluation :: a → WithEvaluation a
noEvaluation = point

evaluator :: IO (WithEvaluation a → IO a, CompileConfig)
evaluator = do
  cap_fds
  cfg ← readCompileConfig
  jail
  return (\we → case we of
      WithoutEvaluation x → return x
      WithEvaluation r g → g . evaluate cfg r
    , cfg)

------------- Config (or at least things that are likely more prone to per-site modification):

-- System calls:

ignored_syscalls, allowed_syscalls :: [SysCall]


ignored_syscalls = -- These are effectively replaced with "return 0;".
  [ SYS_fadvise64, SYS_munmap, SYS_madvise, SYS_umask, SYS_unlink, SYS_rt_sigaction, SYS_rt_sigprocmask, SYS_ioctl, SYS_timer_settime
  #ifdef __x86_64__
    , SYS_fcntl
  #else
    , SYS_fcntl64
  #endif
  ]

allowed_syscalls =
  [ SYS_open, SYS_write, SYS_uname, SYS_brk, SYS_read, SYS_mmap, SYS_exit_group, SYS_getpid, SYS_access, SYS_getrusage, SYS_close, SYS_gettimeofday, SYS_time, SYS_writev, SYS_execve, SYS_mprotect, SYS_getcwd, SYS_times, SYS_readlink, SYS_mremap
  , SYS_set_tid_address, SYS_set_robust_list, SYS_futex, SYS_getrlimit

  -- On x86_64, SYS_times is necessary for clock().

  #ifdef __x86_64__
    , SYS_stat, SYS_fstat, SYS_arch_prctl, SYS_lseek, SYS_lstat, SYS_dup
  #else
    , SYS_fstat64, SYS_lstat64, SYS_stat64, SYS_ugetrlimit, SYS__llseek, SYS_mmap2, SYS_mremap, SYS_set_thread_area, SYS_readlink
  #endif
  ]

-- Resources:

resources :: Stage → Resources
resources s = Resources
    { walltime = t
    , rlimits = (\(r, l) → (r, ResourceLimits (ResourceLimit l) (ResourceLimit l))) .
      [ (ResourceCPUTime, fromIntegral t)
      , (ResourceTotalMemory, 200 * mebi)
      , (ResourceFileSize, 5 * mebi)
        -- Note: We don't add ResourceOpenFiles here, because it is already set as part of the fd closing scheme described in the "Inherited file descriptors" section at the top of this file, and that "global" limit is sufficient.
      ]
    , bufsize = 4 * kibi
    }
  where
    t = case s of
      Preprocess → 8
      Compile → 10
      Run → 4
