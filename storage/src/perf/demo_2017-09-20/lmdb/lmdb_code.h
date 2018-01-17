/** @file mdb.c
 *    @brief Lightning memory-mapped database library
 *
 *    A Btree-based database management library modeled loosely on the
 *    BerkeleyDB API, but much simplified.
 */
/*
 * Copyright 2011-2017 Howard Chu, Symas Corp.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted only as authorized by the OpenLDAP
 * Public License.
 *
 * A copy of this license is available in the file LICENSE in the
 * top-level directory of the distribution or, alternatively, at
 * <http://www.OpenLDAP.org/license.html>.
 *
 * This code is derived from btree.c written by Martin Hedenfalk.
 *
 * Copyright (c) 2009, 2010 Martin Hedenfalk <martin@bzero.se>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#if defined(MDB_VL32) || defined(__WIN64__)
#define _FILE_OFFSET_BITS    64
#endif
#ifdef _WIN32
#include <malloc.h>
#include <windows.h>
#include <wchar.h>                /* get wcscpy() */

/* We use native NT APIs to setup the memory map, so that we can
 * let the DB file grow incrementally instead of always preallocating
 * the full size. These APIs are defined in <wdm.h> and <ntifs.h>
 * but those headers are meant for driver-level development and
 * conflict with the regular user-level headers, so we explicitly
 * declare them here. Using these APIs also means we must link to
 * ntdll.dll, which is not linked by default in user code.
 */
NTSTATUS WINAPI
NtCreateSection(OUT PHANDLE sh, IN ACCESS_MASK acc,
  IN void * oa OPTIONAL,
  IN PLARGE_INTEGER ms OPTIONAL,
  IN ULONG pp, IN ULONG aa, IN HANDLE fh OPTIONAL);

typedef enum _SECTION_INHERIT {
    ViewShare = 1,
    ViewUnmap = 2
} SECTION_INHERIT;

NTSTATUS WINAPI
NtMapViewOfSection(IN PHANDLE sh, IN HANDLE ph,
  IN OUT PVOID *addr, IN ULONG_PTR zbits,
  IN SIZE_T cs, IN OUT PLARGE_INTEGER off OPTIONAL,
  IN OUT PSIZE_T vs, IN SECTION_INHERIT ih,
  IN ULONG at, IN ULONG pp);

NTSTATUS WINAPI
NtClose(HANDLE h);

/** getpid() returns int; MinGW defines pid_t but MinGW64 typedefs it
 *  as int64 which is wrong. MSVC doesn't define it at all, so just
 *  don't use it.
 */
#define MDB_PID_T    int
#define MDB_THR_T    DWORD
#include <sys/types.h>
#include <sys/stat.h>
#ifdef __GNUC__
# include <sys/param.h>
#else
# define LITTLE_ENDIAN    1234
# define BIG_ENDIAN    4321
# define BYTE_ORDER    LITTLE_ENDIAN
# ifndef SSIZE_MAX
#  define SSIZE_MAX    INT_MAX
# endif
#endif
#else
#include <sys/types.h>
#include <sys/stat.h>
#define MDB_PID_T    pid_t
#define MDB_THR_T    pthread_t
#include <sys/param.h>
#include <sys/uio.h>
#include <sys/mman.h>
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#include <fcntl.h>
#endif

#if defined(__mips) && defined(__linux)
/* MIPS has cache coherency issues, requires explicit cache control */
#include <asm/cachectl.h>
extern int cacheflush(char *addr, int nbytes, int cache);
#define CACHEFLUSH(addr, bytes, cache)    cacheflush(addr, bytes, cache)
#else
#define CACHEFLUSH(addr, bytes, cache)
#endif

#if defined(__linux) && !defined(MDB_FDATASYNC_WORKS)
/** fdatasync is broken on ext3/ext4fs on older kernels, see
 *    description in #mdb_env_open2 comments. You can safely
 *    define MDB_FDATASYNC_WORKS if this code will only be run
 *    on kernels 3.6 and newer.
 */
#define    BROKEN_FDATASYNC
#endif

#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _MSC_VER
#include <io.h>
typedef SSIZE_T    ssize_t;
#else
#include <unistd.h>
#endif

#if defined(__sun) || defined(ANDROID)
/* Most platforms have posix_memalign, older may only have memalign */
#define HAVE_MEMALIGN    1
#include <malloc.h>
#endif

#if !(defined(BYTE_ORDER) || defined(__BYTE_ORDER))
#include <netinet/in.h>
#include <resolv.h>    /* defines BYTE_ORDER on HPUX and Solaris */
#endif

#if defined(__APPLE__) || defined (BSD) || defined(__FreeBSD_kernel__)
# if !(defined(MDB_USE_POSIX_MUTEX) || defined(MDB_USE_POSIX_SEM))
# define MDB_USE_SYSV_SEM    1
# endif
# define MDB_FDATASYNC        fsync
#elif defined(ANDROID)
# define MDB_FDATASYNC        fsync
#endif

#ifndef _WIN32
#include <pthread.h>
#include <signal.h>
#ifdef MDB_USE_POSIX_SEM
# define MDB_USE_HASH        1
#include <semaphore.h>
#elif defined(MDB_USE_SYSV_SEM)
#include <sys/ipc.h>
#include <sys/sem.h>
#ifdef _SEM_SEMUN_UNDEFINED
union semun {
    int val;
    struct semid_ds *buf;
    unsigned short *array;
};
#endif /* _SEM_SEMUN_UNDEFINED */
#else
#define MDB_USE_POSIX_MUTEX    1
#endif /* MDB_USE_POSIX_SEM */
#endif /* !_WIN32 */

#if defined(_WIN32) + defined(MDB_USE_POSIX_SEM) + defined(MDB_USE_SYSV_SEM) \
    + defined(MDB_USE_POSIX_MUTEX) != 1
# error "Ambiguous shared-lock implementation"
#endif

#ifdef USE_VALGRIND
#include <valgrind/memcheck.h>
#define VGMEMP_CREATE(h,r,z)    VALGRIND_CREATE_MEMPOOL(h,r,z)
#define VGMEMP_ALLOC(h,a,s) VALGRIND_MEMPOOL_ALLOC(h,a,s)
#define VGMEMP_FREE(h,a) VALGRIND_MEMPOOL_FREE(h,a)
#define VGMEMP_DESTROY(h)    VALGRIND_DESTROY_MEMPOOL(h)
#define VGMEMP_DEFINED(a,s)    VALGRIND_MAKE_MEM_DEFINED(a,s)
#else
#define VGMEMP_CREATE(h,r,z)
#define VGMEMP_ALLOC(h,a,s)
#define VGMEMP_FREE(h,a)
#define VGMEMP_DESTROY(h)
#define VGMEMP_DEFINED(a,s)
#endif

#ifndef BYTE_ORDER
# if (defined(_LITTLE_ENDIAN) || defined(_BIG_ENDIAN)) && !(defined(_LITTLE_ENDIAN) && defined(_BIG_ENDIAN))
/* Solaris just defines one or the other */
#  define LITTLE_ENDIAN    1234
#  define BIG_ENDIAN    4321
#  ifdef _LITTLE_ENDIAN
#   define BYTE_ORDER  LITTLE_ENDIAN
#  else
#   define BYTE_ORDER  BIG_ENDIAN
#  endif
# else
#  define BYTE_ORDER   __BYTE_ORDER
# endif
#endif

#ifndef LITTLE_ENDIAN
#define LITTLE_ENDIAN    __LITTLE_ENDIAN
#endif
#ifndef BIG_ENDIAN
#define BIG_ENDIAN    __BIG_ENDIAN
#endif

#if defined(__i386) || defined(__x86_64) || defined(_M_IX86)
#define MISALIGNED_OK    1
#endif

#include "lmdb.h"
#include "midl.h"

#if (BYTE_ORDER == LITTLE_ENDIAN) == (BYTE_ORDER == BIG_ENDIAN)
# error "Unknown or unsupported endianness (BYTE_ORDER)"
#elif (-6 & 5) || CHAR_BIT!=8 || UINT_MAX!=0xffffffff || MDB_SIZE_MAX%UINT_MAX
# error "Two's complement, reasonably sized integer types, please"
#endif

#ifdef __GNUC__
/** Put infrequently used env functions in separate section */
# ifdef __APPLE__
#  define    ESECT    __attribute__ ((section("__TEXT,text_env")))
# else
#  define    ESECT    __attribute__ ((section("text_env")))
# endif
#else
#define ESECT
#endif

#ifdef _WIN32
#define CALL_CONV WINAPI
#else
#define CALL_CONV
#endif

/** @defgroup internal    LMDB Internals
 *    @{
 */
/** @defgroup compat    Compatibility Macros
 *    A bunch of macros to minimize the amount of platform-specific ifdefs
 *    needed throughout the rest of the code. When the features this library
 *    needs are similar enough to POSIX to be hidden in a one-or-two line
 *    replacement, this macro approach is used.
 *    @{
 */

    /** Features under development */
#ifndef MDB_DEVEL
#define MDB_DEVEL 0
#endif

    /** Wrapper around __func__, which is a C99 feature */
#if __STDC_VERSION__ >= 199901L
# define mdb_func_    __func__
#elif __GNUC__ >= 2 || _MSC_VER >= 1300
# define mdb_func_    __FUNCTION__
#else
/* If a debug message says <mdb_unknown>(), update the #if statements above */
# define mdb_func_    "<mdb_unknown>"
#endif

/* Internal error codes, not exposed outside liblmdb */
#define    MDB_NO_ROOT        (MDB_LAST_ERRCODE + 10)
#ifdef _WIN32
#define MDB_OWNERDEAD    ((int) WAIT_ABANDONED)
#elif defined MDB_USE_SYSV_SEM
#define MDB_OWNERDEAD    (MDB_LAST_ERRCODE + 11)
#elif defined(MDB_USE_POSIX_MUTEX) && defined(EOWNERDEAD)
#define MDB_OWNERDEAD    EOWNERDEAD    /**< #LOCK_MUTEX0() result if dead owner */
#endif

#ifdef __GLIBC__
#define    GLIBC_VER    ((__GLIBC__ << 16 )| __GLIBC_MINOR__)
#endif
/** Some platforms define the EOWNERDEAD error code
 * even though they don't support Robust Mutexes.
 * Compile with -DMDB_USE_ROBUST=0, or use some other
 * mechanism like -DMDB_USE_SYSV_SEM instead of
 * -DMDB_USE_POSIX_MUTEX. (SysV semaphores are
 * also Robust, but some systems don't support them
 * either.)
 */
#ifndef MDB_USE_ROBUST
/* Android currently lacks Robust Mutex support. So does glibc < 2.4. */
# if defined(MDB_USE_POSIX_MUTEX) && (defined(ANDROID) || \
    (defined(__GLIBC__) && GLIBC_VER < 0x020004))
#  define MDB_USE_ROBUST    0
# else
#  define MDB_USE_ROBUST    1
# endif
#endif /* !MDB_USE_ROBUST */

#if defined(MDB_USE_POSIX_MUTEX) && (MDB_USE_ROBUST)
/* glibc < 2.12 only provided _np API */
#  if (defined(__GLIBC__) && GLIBC_VER < 0x02000c) || \
    (defined(PTHREAD_MUTEX_ROBUST_NP) && !defined(PTHREAD_MUTEX_ROBUST))
#   define PTHREAD_MUTEX_ROBUST    PTHREAD_MUTEX_ROBUST_NP
#   define pthread_mutexattr_setrobust(attr, flag)    pthread_mutexattr_setrobust_np(attr, flag)
#   define pthread_mutex_consistent(mutex)    pthread_mutex_consistent_np(mutex)
#  endif
#endif /* MDB_USE_POSIX_MUTEX && MDB_USE_ROBUST */

#if defined(MDB_OWNERDEAD) && (MDB_USE_ROBUST)
#define MDB_ROBUST_SUPPORTED    1
#endif

#ifdef _WIN32
#define MDB_USE_HASH    1
#define MDB_PIDLOCK    0
#define THREAD_RET    DWORD
#define pthread_t    HANDLE
#define pthread_mutex_t    HANDLE
#define pthread_cond_t    HANDLE
typedef HANDLE mdb_mutex_t, mdb_mutexref_t;
#define pthread_key_t    DWORD
#define pthread_self()    GetCurrentThreadId()
#define pthread_key_create(x,y)    \
    ((*(x) = TlsAlloc()) == TLS_OUT_OF_INDEXES ? ErrCode() : 0)
#define pthread_key_delete(x)    TlsFree(x)
#define pthread_getspecific(x)    TlsGetValue(x)
#define pthread_setspecific(x,y)    (TlsSetValue(x,y) ? 0 : ErrCode())
#define pthread_mutex_unlock(x)    ReleaseMutex(*x)
#define pthread_mutex_lock(x)    WaitForSingleObject(*x, INFINITE)
#define pthread_cond_signal(x)    SetEvent(*x)
#define pthread_cond_wait(cond,mutex)    do{SignalObjectAndWait(*mutex, *cond, INFINITE, FALSE); WaitForSingleObject(*mutex, INFINITE);}while(0)
#define THREAD_CREATE(thr,start,arg) \
    (((thr) = CreateThread(NULL, 0, start, arg, 0, NULL)) ? 0 : ErrCode())
#define THREAD_FINISH(thr) \
    (WaitForSingleObject(thr, INFINITE) ? ErrCode() : 0)
#define LOCK_MUTEX0(mutex)        WaitForSingleObject(mutex, INFINITE)
#define UNLOCK_MUTEX(mutex)        ReleaseMutex(mutex)
#define mdb_mutex_consistent(mutex)    0
#define getpid()    GetCurrentProcessId()
#define    MDB_FDATASYNC(fd)    (!FlushFileBuffers(fd))
#define    MDB_MSYNC(addr,len,flags)    (!FlushViewOfFile(addr,len))
#define    ErrCode()    GetLastError()
#define GET_PAGESIZE(x) {SYSTEM_INFO si; GetSystemInfo(&si); (x) = si.dwPageSize;}
#define    close(fd)    (CloseHandle(fd) ? 0 : -1)
#define    munmap(ptr,len)    UnmapViewOfFile(ptr)
#ifdef PROCESS_QUERY_LIMITED_INFORMATION
#define MDB_PROCESS_QUERY_LIMITED_INFORMATION PROCESS_QUERY_LIMITED_INFORMATION
#else
#define MDB_PROCESS_QUERY_LIMITED_INFORMATION 0x1000
#endif
#else
#define THREAD_RET    void *
#define THREAD_CREATE(thr,start,arg)    pthread_create(&thr,NULL,start,arg)
#define THREAD_FINISH(thr)    pthread_join(thr,NULL)

    /** For MDB_LOCK_FORMAT: True if readers take a pid lock in the lockfile */
#define MDB_PIDLOCK            1

#ifdef MDB_USE_POSIX_SEM

typedef sem_t *mdb_mutex_t, *mdb_mutexref_t;
#define LOCK_MUTEX0(mutex)        mdb_sem_wait(mutex)
#define UNLOCK_MUTEX(mutex)        sem_post(mutex)

static int
mdb_sem_wait(sem_t *sem)
{
   int rc;
   while ((rc = sem_wait(sem)) && (rc = errno) == EINTR) ;
   return rc;
}

#elif defined MDB_USE_SYSV_SEM

typedef struct mdb_mutex {
    int semid;
    int semnum;
    int *locked;
} mdb_mutex_t[1], *mdb_mutexref_t;

#define LOCK_MUTEX0(mutex)        mdb_sem_wait(mutex)
#define UNLOCK_MUTEX(mutex)        do { \
    struct sembuf sb = { 0, 1, SEM_UNDO }; \
    sb.sem_num = (mutex)->semnum; \
    *(mutex)->locked = 0; \
    semop((mutex)->semid, &sb, 1); \
} while(0)

static int
mdb_sem_wait(mdb_mutexref_t sem)
{
    int rc, *locked = sem->locked;
    struct sembuf sb = { 0, -1, SEM_UNDO };
    sb.sem_num = sem->semnum;
    do {
        if (!semop(sem->semid, &sb, 1)) {
            rc = *locked ? MDB_OWNERDEAD : MDB_SUCCESS;
            *locked = 1;
            break;
        }
    } while ((rc = errno) == EINTR);
    return rc;
}

#define mdb_mutex_consistent(mutex)    0

#else    /* MDB_USE_POSIX_MUTEX: */
    /** Shared mutex/semaphore as the original is stored.
     *
     *    Not for copies.  Instead it can be assigned to an #mdb_mutexref_t.
     *    When mdb_mutexref_t is a pointer and mdb_mutex_t is not, then it
     *    is array[size 1] so it can be assigned to the pointer.
     */
typedef pthread_mutex_t mdb_mutex_t[1];
    /** Reference to an #mdb_mutex_t */
typedef pthread_mutex_t *mdb_mutexref_t;
    /** Lock the reader or writer mutex.
     *    Returns 0 or a code to give #mdb_mutex_failed(), as in #LOCK_MUTEX().
     */
#define LOCK_MUTEX0(mutex)    pthread_mutex_lock(mutex)
    /** Unlock the reader or writer mutex.
     */
#define UNLOCK_MUTEX(mutex)    pthread_mutex_unlock(mutex)
    /** Mark mutex-protected data as repaired, after death of previous owner.
     */
#define mdb_mutex_consistent(mutex)    pthread_mutex_consistent(mutex)
#endif    /* MDB_USE_POSIX_SEM || MDB_USE_SYSV_SEM */

    /** Get the error code for the last failed system function.
     */
#define    ErrCode()    errno

    /** An abstraction for a file handle.
     *    On POSIX systems file handles are small integers. On Windows
     *    they're opaque pointers.
     */
#define    HANDLE    int

    /**    A value for an invalid file handle.
     *    Mainly used to initialize file variables and signify that they are
     *    unused.
     */
#define INVALID_HANDLE_VALUE    (-1)

    /** Get the size of a memory page for the system.
     *    This is the basic size that the platform's memory manager uses, and is
     *    fundamental to the use of memory-mapped files.
     */
#define    GET_PAGESIZE(x)    ((x) = sysconf(_SC_PAGE_SIZE))
#endif

#define    Z    MDB_FMT_Z    /**< printf/scanf format modifier for size_t */
#define    Yu    MDB_PRIy(u)    /**< printf format for #mdb_size_t */
#define    Yd    MDB_PRIy(d)    /**< printf format for 'signed #mdb_size_t' */

#ifdef MDB_USE_SYSV_SEM
#define MNAME_LEN    (sizeof(int))
#else
#define MNAME_LEN    (sizeof(pthread_mutex_t))
#endif

/** Initial part of #MDB_env.me_mutexname[].
 *    Changes to this code must be reflected in #MDB_LOCK_FORMAT.
 */
#ifdef _WIN32
#define MUTEXNAME_PREFIX        "Global\\MDB"
#elif defined MDB_USE_POSIX_SEM
#define MUTEXNAME_PREFIX        "/MDB"
#endif

/** @} */

#ifdef MDB_ROBUST_SUPPORTED
    /** Lock mutex, handle any error, set rc = result.
     *    Return 0 on success, nonzero (not rc) on error.
     */
#define LOCK_MUTEX(rc, env, mutex) \
    (((rc) = LOCK_MUTEX0(mutex)) && \
     ((rc) = mdb_mutex_failed(env, mutex, rc)))
static int mdb_mutex_failed(MDB_env *env, mdb_mutexref_t mutex, int rc);
#else
#define LOCK_MUTEX(rc, env, mutex) ((rc) = LOCK_MUTEX0(mutex))
#define mdb_mutex_failed(env, mutex, rc) (rc)
#endif

#ifndef _WIN32
/**    A flag for opening a file and requesting synchronous data writes.
 *    This is only used when writing a meta page. It's not strictly needed;
 *    we could just do a normal write and then immediately perform a flush.
 *    But if this flag is available it saves us an extra system call.
 *
 *    @note If O_DSYNC is undefined but exists in /usr/include,
 * preferably set some compiler flag to get the definition.
 */
#ifndef MDB_DSYNC
# ifdef O_DSYNC
# define MDB_DSYNC    O_DSYNC
# else
# define MDB_DSYNC    O_SYNC
# endif
#endif
#endif

/** Function for flushing the data of a file. Define this to fsync
 *    if fdatasync() is not supported.
 */
#ifndef MDB_FDATASYNC
# define MDB_FDATASYNC    fdatasync
#endif

#ifndef MDB_MSYNC
# define MDB_MSYNC(addr,len,flags)    msync(addr,len,flags)
#endif

#ifndef MS_SYNC
#define    MS_SYNC    1
#endif

#ifndef MS_ASYNC
#define    MS_ASYNC    0
#endif

    /** A page number in the database.
     *    Note that 64 bit page numbers are overkill, since pages themselves
     *    already represent 12-13 bits of addressable memory, and the OS will
     *    always limit applications to a maximum of 63 bits of address space.
     *
     *    @note In the #MDB_node structure, we only store 48 bits of this value,
     *    which thus limits us to only 60 bits of addressable data.
     */
typedef MDB_ID    pgno_t;

    /** A transaction ID.
     *    See struct MDB_txn.mt_txnid for details.
     */
typedef MDB_ID    txnid_t;

/** @defgroup debug    Debug Macros
 *    @{
 */
#ifndef MDB_DEBUG
    /**    Enable debug output.  Needs variable argument macros (a C99 feature).
     *    Set this to 1 for copious tracing. Set to 2 to add dumps of all IDLs
     *    read from and written to the database (used for free space management).
     */
#define MDB_DEBUG 0
#endif

#if MDB_DEBUG
static int mdb_debug;
static txnid_t mdb_debug_start;

    /**    Print a debug message with printf formatting.
     *    Requires double parenthesis around 2 or more args.
     */
# define DPRINTF(args) ((void) ((mdb_debug) && DPRINTF0 args))
# define DPRINTF0(fmt, ...) \
    fprintf(stderr, "%s:%d " fmt "\n", mdb_func_, __LINE__, __VA_ARGS__)
#else
# define DPRINTF(args)    ((void) 0)
#endif
    /**    Print a debug string.
     *    The string is printed literally, with no format processing.
     */
#define DPUTS(arg)    DPRINTF(("%s", arg))
    /** Debuging output value of a cursor DBI: Negative in a sub-cursor. */
#define DDBI(mc) \
    (((mc)->mc_flags & C_SUB) ? -(int)(mc)->mc_dbi : (int)(mc)->mc_dbi)
/** @} */

    /**    @brief The maximum size of a database page.
     *
     *    It is 32k or 64k, since value-PAGEBASE must fit in
     *    #MDB_page.%mp_upper.
     *
     *    LMDB will use database pages < OS pages if needed.
     *    That causes more I/O in write transactions: The OS must
     *    know (read) the whole page before writing a partial page.
     *
     *    Note that we don't currently support Huge pages. On Linux,
     *    regular data files cannot use Huge pages, and in general
     *    Huge pages aren't actually pageable. We rely on the OS
     *    demand-pager to read our data and page it out when memory
     *    pressure from other processes is high. So until OSs have
     *    actual paging support for Huge pages, they're not viable.
     */
#define MAX_PAGESIZE     (PAGEBASE ? 0x10000 : 0x8000)

    /** The minimum number of keys required in a database page.
     *    Setting this to a larger value will place a smaller bound on the
     *    maximum size of a data item. Data items larger than this size will
     *    be pushed into overflow pages instead of being stored directly in
     *    the B-tree node. This value used to default to 4. With a page size
     *    of 4096 bytes that meant that any item larger than 1024 bytes would
     *    go into an overflow page. That also meant that on average 2-3KB of
     *    each overflow page was wasted space. The value cannot be lower than
     *    2 because then there would no longer be a tree structure. With this
     *    value, items larger than 2KB will go into overflow pages, and on
     *    average only 1KB will be wasted.
     */
#define MDB_MINKEYS     2

    /**    A stamp that identifies a file as an LMDB file.
     *    There's nothing special about this value other than that it is easily
     *    recognizable, and it will reflect any byte order mismatches.
     */
#define MDB_MAGIC     0xBEEFC0DE

    /**    The version number for a database's datafile format. */
#define MDB_DATA_VERSION     ((MDB_DEVEL) ? 999 : 1)
    /**    The version number for a database's lockfile format. */
#define MDB_LOCK_VERSION     ((MDB_DEVEL) ? 999 : 2)
    /** Number of bits representing #MDB_LOCK_VERSION in #MDB_LOCK_FORMAT.
     *    The remaining bits must leave room for #MDB_lock_desc.
     */
#define MDB_LOCK_VERSION_BITS 12

    /**    @brief The max size of a key we can write, or 0 for computed max.
     *
     *    This macro should normally be left alone or set to 0.
     *    Note that a database with big keys or dupsort data cannot be
     *    reliably modified by a liblmdb which uses a smaller max.
     *    The default is 511 for backwards compat, or 0 when #MDB_DEVEL.
     *
     *    Other values are allowed, for backwards compat.  However:
     *    A value bigger than the computed max can break if you do not
     *    know what you are doing, and liblmdb <= 0.9.10 can break when
     *    modifying a DB with keys/dupsort data bigger than its max.
     *
     *    Data items in an #MDB_DUPSORT database are also limited to
     *    this size, since they're actually keys of a sub-DB.  Keys and
     *    #MDB_DUPSORT data items must fit on a node in a regular page.
     */
#ifndef MDB_MAXKEYSIZE
#define MDB_MAXKEYSIZE     ((MDB_DEVEL) ? 0 : 511)
#endif

    /**    The maximum size of a key we can write to the environment. */
#if MDB_MAXKEYSIZE
#define ENV_MAXKEY(env)    (MDB_MAXKEYSIZE)
#else
#define ENV_MAXKEY(env)    ((env)->me_maxkey)
#endif

    /**    @brief The maximum size of a data item.
     *
     *    We only store a 32 bit value for node sizes.
     */
#define MAXDATASIZE    0xffffffffUL

#if MDB_DEBUG
    /**    Key size which fits in a #DKBUF.
     *    @ingroup debug
     */
#define DKBUF_MAXKEYSIZE ((MDB_MAXKEYSIZE) > 0 ? (MDB_MAXKEYSIZE) : 511)
    /**    A key buffer.
     *    @ingroup debug
     *    This is used for printing a hex dump of a key's contents.
     */
#define DKBUF    char kbuf[DKBUF_MAXKEYSIZE*2+1]
    /**    Display a key in hex.
     *    @ingroup debug
     *    Invoke a function to display a key in hex.
     */
#define    DKEY(x)    mdb_dkey(x, kbuf)
#else
#define    DKBUF
#define DKEY(x)    0
#endif

    /** An invalid page number.
     *    Mainly used to denote an empty tree.
     */
#define P_INVALID     (~(pgno_t)0)

    /** Test if the flags \b f are set in a flag word \b w. */
#define F_ISSET(w, f)     (((w) & (f)) == (f))

    /** Round \b n up to an even number. */
#define EVEN(n)        (((n) + 1U) & -2) /* sign-extending -2 to match n+1U */

    /** Least significant 1-bit of \b n.  n must be of an unsigned type. */
#define LOW_BIT(n)        ((n) & (-(n)))

    /** (log2(\b p2) % \b n), for p2 = power of 2 and 0 < n < 8. */
#define LOG2_MOD(p2, n)    (7 - 86 / ((p2) % ((1U<<(n))-1) + 11))
    /* Explanation: Let p2 = 2**(n*y + x), x<n and M = (1U<<n)-1. Now p2 =
     * (M+1)**y * 2**x = 2**x (mod M). Finally "/" "happens" to return 7-x.
     */

    /** Should be alignment of \b type. Ensure it is a power of 2. */
#define ALIGNOF2(type) \
    LOW_BIT(offsetof(struct { char ch_; type align_; }, align_))

    /**    Used for offsets within a single page.
     *    Since memory pages are typically 4 or 8KB in size, 12-13 bits,
     *    this is plenty.
     */
typedef uint16_t     indx_t;

typedef unsigned long long    mdb_hash_t;

    /**    Default size of memory map.
     *    This is certainly too small for any actual applications. Apps should always set
     *    the size explicitly using #mdb_env_set_mapsize().
     */
#define DEFAULT_MAPSIZE    1048576

/**    @defgroup readers    Reader Lock Table
 *    Readers don't acquire any locks for their data access. Instead, they
 *    simply record their transaction ID in the reader table. The reader
 *    mutex is needed just to find an empty slot in the reader table. The
 *    slot's address is saved in thread-specific data so that subsequent read
 *    transactions started by the same thread need no further locking to proceed.
 *
 *    If #MDB_NOTLS is set, the slot address is not saved in thread-specific data.
 *
 *    No reader table is used if the database is on a read-only filesystem, or
 *    if #MDB_NOLOCK is set.
 *
 *    Since the database uses multi-version concurrency control, readers don't
 *    actually need any locking. This table is used to keep track of which
 *    readers are using data from which old transactions, so that we'll know
 *    when a particular old transaction is no longer in use. Old transactions
 *    that have discarded any data pages can then have those pages reclaimed
 *    for use by a later write transaction.
 *
 *    The lock table is constructed such that reader slots are aligned with the
 *    processor's cache line size. Any slot is only ever used by one thread.
 *    This alignment guarantees that there will be no contention or cache
 *    thrashing as threads update their own slot info, and also eliminates
 *    any need for locking when accessing a slot.
 *
 *    A writer thread will scan every slot in the table to determine the oldest
 *    outstanding reader transaction. Any freed pages older than this will be
 *    reclaimed by the writer. The writer doesn't use any locks when scanning
 *    this table. This means that there's no guarantee that the writer will
 *    see the most up-to-date reader info, but that's not required for correct
 *    operation - all we need is to know the upper bound on the oldest reader,
 *    we don't care at all about the newest reader. So the only consequence of
 *    reading stale information here is that old pages might hang around a
 *    while longer before being reclaimed. That's actually good anyway, because
 *    the longer we delay reclaiming old pages, the more likely it is that a
 *    string of contiguous pages can be found after coalescing old pages from
 *    many old transactions together.
 *    @{
 */
    /**    Number of slots in the reader table.
     *    This value was chosen somewhat arbitrarily. 126 readers plus a
     *    couple mutexes fit exactly into 8KB on my development machine.
     *    Applications should set the table size using #mdb_env_set_maxreaders().
     */
#define DEFAULT_READERS    126

    /**    The size of a CPU cache line in bytes. We want our lock structures
     *    aligned to this size to avoid false cache line sharing in the
     *    lock table.
     *    This value works for most CPUs. For Itanium this should be 128.
     */
#ifndef CACHELINE
#define CACHELINE    64
#endif

    /**    The information we store in a single slot of the reader table.
     *    In addition to a transaction ID, we also record the process and
     *    thread ID that owns a slot, so that we can detect stale information,
     *    e.g. threads or processes that went away without cleaning up.
     *    @note We currently don't check for stale records. We simply re-init
     *    the table when we know that we're the only process opening the
     *    lock file.
     */
typedef struct MDB_rxbody {
    /**    Current Transaction ID when this transaction began, or (txnid_t)-1.
     *    Multiple readers that start at the same time will probably have the
     *    same ID here. Again, it's not important to exclude them from
     *    anything; all we need to know is which version of the DB they
     *    started from so we can avoid overwriting any data used in that
     *    particular version.
     */
    volatile txnid_t        mrb_txnid;
    /** The process ID of the process owning this reader txn. */
    volatile MDB_PID_T    mrb_pid;
    /** The thread ID of the thread owning this txn. */
    volatile MDB_THR_T    mrb_tid;
} MDB_rxbody;

    /** The actual reader record, with cacheline padding. */
typedef struct MDB_reader {
    union {
        MDB_rxbody mrx;
        /** shorthand for mrb_txnid */
#define    mr_txnid    mru.mrx.mrb_txnid
#define    mr_pid    mru.mrx.mrb_pid
#define    mr_tid    mru.mrx.mrb_tid
        /** cache line alignment */
        char pad[(sizeof(MDB_rxbody)+CACHELINE-1) & ~(CACHELINE-1)];
    } mru;
} MDB_reader;

    /** The header for the reader table.
     *    The table resides in a memory-mapped file. (This is a different file
     *    than is used for the main database.)
     *
     *    For POSIX the actual mutexes reside in the shared memory of this
     *    mapped file. On Windows, mutexes are named objects allocated by the
     *    kernel; we store the mutex names in this mapped file so that other
     *    processes can grab them. This same approach is also used on
     *    MacOSX/Darwin (using named semaphores) since MacOSX doesn't support
     *    process-shared POSIX mutexes. For these cases where a named object
     *    is used, the object name is derived from a 64 bit FNV hash of the
     *    environment pathname. As such, naming collisions are extremely
     *    unlikely. If a collision occurs, the results are unpredictable.
     */
typedef struct MDB_txbody {
        /** Stamp identifying this as an LMDB file. It must be set
         *    to #MDB_MAGIC. */
    uint32_t    mtb_magic;
        /** Format of this lock file. Must be set to #MDB_LOCK_FORMAT. */
    uint32_t    mtb_format;
        /**    The ID of the last transaction committed to the database.
         *    This is recorded here only for convenience; the value can always
         *    be determined by reading the main database meta pages.
         */
    volatile txnid_t        mtb_txnid;
        /** The number of slots that have been used in the reader table.
         *    This always records the maximum count, it is not decremented
         *    when readers release their slots.
         */
    volatile unsigned    mtb_numreaders;
#if defined(_WIN32) || defined(MDB_USE_POSIX_SEM)
        /** Binary form of names of the reader/writer locks */
    mdb_hash_t            mtb_mutexid;
#elif defined(MDB_USE_SYSV_SEM)
    int     mtb_semid;
    int        mtb_rlocked;
#else
        /** Mutex protecting access to this table.
         *    This is the reader table lock used with LOCK_MUTEX().
         */
    mdb_mutex_t    mtb_rmutex;
#endif
} MDB_txbody;

    /** The actual reader table definition. */
typedef struct MDB_txninfo {
    union {
        MDB_txbody mtb;
#define mti_magic    mt1.mtb.mtb_magic
#define mti_format    mt1.mtb.mtb_format
#define mti_rmutex    mt1.mtb.mtb_rmutex
#define mti_txnid    mt1.mtb.mtb_txnid
#define mti_numreaders    mt1.mtb.mtb_numreaders
#define mti_mutexid    mt1.mtb.mtb_mutexid
#ifdef MDB_USE_SYSV_SEM
#define    mti_semid    mt1.mtb.mtb_semid
#define    mti_rlocked    mt1.mtb.mtb_rlocked
#endif
        char pad[(sizeof(MDB_txbody)+CACHELINE-1) & ~(CACHELINE-1)];
    } mt1;
#if !(defined(_WIN32) || defined(MDB_USE_POSIX_SEM))
    union {
#ifdef MDB_USE_SYSV_SEM
        int mt2_wlocked;
#define mti_wlocked    mt2.mt2_wlocked
#else
        mdb_mutex_t    mt2_wmutex;
#define mti_wmutex    mt2.mt2_wmutex
#endif
        char pad[(MNAME_LEN+CACHELINE-1) & ~(CACHELINE-1)];
    } mt2;
#endif
    MDB_reader    mti_readers[1];
} MDB_txninfo;

    /** Lockfile format signature: version, features and field layout */
#define MDB_LOCK_FORMAT \
    ((uint32_t)         \
     (((MDB_LOCK_VERSION) % (1U << MDB_LOCK_VERSION_BITS)) \
      + MDB_lock_desc     * (1U << MDB_LOCK_VERSION_BITS)))

    /** Lock type and layout. Values 0-119. _WIN32 implies #MDB_PIDLOCK.
     *    Some low values are reserved for future tweaks.
     */
#ifdef _WIN32
# define MDB_LOCK_TYPE    (0 + ALIGNOF2(mdb_hash_t)/8 % 2)
#elif defined MDB_USE_POSIX_SEM
# define MDB_LOCK_TYPE    (4 + ALIGNOF2(mdb_hash_t)/8 % 2)
#elif defined MDB_USE_SYSV_SEM
# define MDB_LOCK_TYPE    (8)
#elif defined MDB_USE_POSIX_MUTEX
/* We do not know the inside of a POSIX mutex and how to check if mutexes
 * used by two executables are compatible. Just check alignment and size.
 */
# define MDB_LOCK_TYPE    (10 + \
        LOG2_MOD(ALIGNOF2(pthread_mutex_t), 5) + \
        sizeof(pthread_mutex_t) / 4U % 22 * 5)
#endif

enum {
    /** Magic number for lockfile layout and features.
     *
     *  This *attempts* to stop liblmdb variants compiled with conflicting
     *    options from using the lockfile at the same time and thus breaking
     *    it.  It describes locking types, and sizes and sometimes alignment
     *    of the various lockfile items.
     *
     *    The detected ranges are mostly guesswork, or based simply on how
     *    big they could be without using more bits.  So we can tweak them
     *    in good conscience when updating #MDB_LOCK_VERSION.
     */
    MDB_lock_desc =
    /* Default CACHELINE=64 vs. other values (have seen mention of 32-256) */
    (CACHELINE==64 ? 0 : 1 + LOG2_MOD(CACHELINE >> (CACHELINE>64), 5))
    + 6  * (sizeof(MDB_PID_T)/4 % 3)    /* legacy(2) to word(4/8)? */
    + 18 * (sizeof(pthread_t)/4 % 5)    /* can be struct{id, active data} */
    + 90 * (sizeof(MDB_txbody) / CACHELINE % 3)
    + 270 * (MDB_LOCK_TYPE % 120)
    /* The above is < 270*120 < 2**15 */
    + ((sizeof(txnid_t) == 8) << 15)    /* 32bit/64bit */
    + ((sizeof(MDB_reader) > CACHELINE) << 16)
    /* Not really needed - implied by MDB_LOCK_TYPE != (_WIN32 locking) */
    + (((MDB_PIDLOCK) != 0)   << 17)
    /* 18 bits total: Must be <= (32 - MDB_LOCK_VERSION_BITS). */
};
/** @} */

/** Common header for all page types. The page type depends on #mp_flags.
 *
 * #P_BRANCH and #P_LEAF pages have unsorted '#MDB_node's at the end, with
 * sorted #mp_ptrs[] entries referring to them. Exception: #P_LEAF2 pages
 * omit mp_ptrs and pack sorted #MDB_DUPFIXED values after the page header.
 *
 * #P_OVERFLOW records occupy one or more contiguous pages where only the
 * first has a page header. They hold the real data of #F_BIGDATA nodes.
 *
 * #P_SUBP sub-pages are small leaf "pages" with duplicate data.
 * A node with flag #F_DUPDATA but not #F_SUBDATA contains a sub-page.
 * (Duplicate data can also go in sub-databases, which use normal pages.)
 *
 * #P_META pages contain #MDB_meta, the start point of an LMDB snapshot.
 *
 * Each non-metapage up to #MDB_meta.%mm_last_pg is reachable exactly once
 * in the snapshot: Either used by a database or listed in a freeDB record.
 */
typedef struct MDB_page {
#define    mp_pgno    mp_p.p_pgno
#define    mp_next    mp_p.p_next
    union {
        pgno_t        p_pgno;    /**< page number */
        struct MDB_page *p_next; /**< for in-memory list of freed pages */
    } mp_p;
    uint16_t    mp_pad;            /**< key size if this is a LEAF2 page */
/**    @defgroup mdb_page    Page Flags
 *    @ingroup internal
 *    Flags for the page headers.
 *    @{
 */
#define    P_BRANCH     0x01        /**< branch page */
#define    P_LEAF         0x02        /**< leaf page */
#define    P_OVERFLOW     0x04        /**< overflow page */
#define    P_META         0x08        /**< meta page */
#define    P_DIRTY         0x10        /**< dirty page, also set for #P_SUBP pages */
#define    P_LEAF2         0x20        /**< for #MDB_DUPFIXED records */
#define    P_SUBP         0x40        /**< for #MDB_DUPSORT sub-pages */
#define    P_LOOSE         0x4000        /**< page was dirtied then freed, can be reused */
#define    P_KEEP         0x8000        /**< leave this page alone during spill */
/** @} */
    uint16_t    mp_flags;        /**< @ref mdb_page */
#define mp_lower    mp_pb.pb.pb_lower
#define mp_upper    mp_pb.pb.pb_upper
#define mp_pages    mp_pb.pb_pages
    union {
        struct {
            indx_t        pb_lower;        /**< lower bound of free space */
            indx_t        pb_upper;        /**< upper bound of free space */
        } pb;
        uint32_t    pb_pages;    /**< number of overflow pages */
    } mp_pb;
    indx_t        mp_ptrs[1];        /**< dynamic size */
} MDB_page;

    /** Size of the page header, excluding dynamic data at the end */
#define PAGEHDRSZ     ((unsigned) offsetof(MDB_page, mp_ptrs))

    /** Address of first usable data byte in a page, after the header */
#define METADATA(p)     ((void *)((char *)(p) + PAGEHDRSZ))

    /** ITS#7713, change PAGEBASE to handle 65536 byte pages */
#define    PAGEBASE    ((MDB_DEVEL) ? PAGEHDRSZ : 0)

    /** Number of nodes on a page */
#define NUMKEYS(p)     (((p)->mp_lower - (PAGEHDRSZ-PAGEBASE)) >> 1)

    /** The amount of space remaining in the page */
#define SIZELEFT(p)     (indx_t)((p)->mp_upper - (p)->mp_lower)

    /** The percentage of space used in the page, in tenths of a percent. */
#define PAGEFILL(env, p) (1000L * ((env)->me_psize - PAGEHDRSZ - SIZELEFT(p)) / \
                ((env)->me_psize - PAGEHDRSZ))
    /** The minimum page fill factor, in tenths of a percent.
     *    Pages emptier than this are candidates for merging.
     */
#define FILL_THRESHOLD     250

    /** Test if a page is a leaf page */
#define IS_LEAF(p)     F_ISSET((p)->mp_flags, P_LEAF)
    /** Test if a page is a LEAF2 page */
#define IS_LEAF2(p)     F_ISSET((p)->mp_flags, P_LEAF2)
    /** Test if a page is a branch page */
#define IS_BRANCH(p)     F_ISSET((p)->mp_flags, P_BRANCH)
    /** Test if a page is an overflow page */
#define IS_OVERFLOW(p)     F_ISSET((p)->mp_flags, P_OVERFLOW)
    /** Test if a page is a sub page */
#define IS_SUBP(p)     F_ISSET((p)->mp_flags, P_SUBP)

    /** The number of overflow pages needed to store the given size. */
#define OVPAGES(size, psize)    ((PAGEHDRSZ-1 + (size)) / (psize) + 1)

    /** Link in #MDB_txn.%mt_loose_pgs list.
     *  Kept outside the page header, which is needed when reusing the page.
     */
#define NEXT_LOOSE_PAGE(p)        (*(MDB_page **)((p) + 2))

    /** Header for a single key/data pair within a page.
     * Used in pages of type #P_BRANCH and #P_LEAF without #P_LEAF2.
     * We guarantee 2-byte alignment for 'MDB_node's.
     *
     * #mn_lo and #mn_hi are used for data size on leaf nodes, and for child
     * pgno on branch nodes.  On 64 bit platforms, #mn_flags is also used
     * for pgno.  (Branch nodes have no flags).  Lo and hi are in host byte
     * order in case some accesses can be optimized to 32-bit word access.
     *
     * Leaf node flags describe node contents.  #F_BIGDATA says the node's
     * data part is the page number of an overflow page with actual data.
     * #F_DUPDATA and #F_SUBDATA can be combined giving duplicate data in
     * a sub-page/sub-database, and named databases (just #F_SUBDATA).
     */
typedef struct MDB_node {
    /** part of data size or pgno
     *    @{ */
#if BYTE_ORDER == LITTLE_ENDIAN
    unsigned short    mn_lo, mn_hi;
#else
    unsigned short    mn_hi, mn_lo;
#endif
    /** @} */
/** @defgroup mdb_node Node Flags
 *    @ingroup internal
 *    Flags for node headers.
 *    @{
 */
#define F_BIGDATA     0x01            /**< data put on overflow page */
#define F_SUBDATA     0x02            /**< data is a sub-database */
#define F_DUPDATA     0x04            /**< data has duplicates */

/** valid flags for #mdb_node_add() */
#define    NODE_ADD_FLAGS    (F_DUPDATA|F_SUBDATA|MDB_RESERVE|MDB_APPEND)

/** @} */
    unsigned short    mn_flags;        /**< @ref mdb_node */
    unsigned short    mn_ksize;        /**< key size */
    char        mn_data[1];            /**< key and data are appended here */
} MDB_node;

    /** Size of the node header, excluding dynamic data at the end */
#define NODESIZE     offsetof(MDB_node, mn_data)

    /** Bit position of top word in page number, for shifting mn_flags */
#define PGNO_TOPWORD ((pgno_t)-1 > 0xffffffffu ? 32 : 0)

    /** Size of a node in a branch page with a given key.
     *    This is just the node header plus the key, there is no data.
     */
#define INDXSIZE(k)     (NODESIZE + ((k) == NULL ? 0 : (k)->mv_size))

    /** Size of a node in a leaf page with a given key and data.
     *    This is node header plus key plus data size.
     */
#define LEAFSIZE(k, d)     (NODESIZE + (k)->mv_size + (d)->mv_size)

    /** Address of node \b i in page \b p */
#define NODEPTR(p, i)     ((MDB_node *)((char *)(p) + (p)->mp_ptrs[i] + PAGEBASE))

    /** Address of the key for the node */
#define NODEKEY(node)     (void *)((node)->mn_data)

    /** Address of the data for a node */
#define NODEDATA(node)     (void *)((char *)(node)->mn_data + (node)->mn_ksize)

    /** Get the page number pointed to by a branch node */
#define NODEPGNO(node) \
    ((node)->mn_lo | ((pgno_t) (node)->mn_hi << 16) | \
     (PGNO_TOPWORD ? ((pgno_t) (node)->mn_flags << PGNO_TOPWORD) : 0))
    /** Set the page number in a branch node */
#define SETPGNO(node,pgno)    do { \
    (node)->mn_lo = (pgno) & 0xffff; (node)->mn_hi = (pgno) >> 16; \
    if (PGNO_TOPWORD) (node)->mn_flags = (pgno) >> PGNO_TOPWORD; } while(0)

    /** Get the size of the data in a leaf node */
#define NODEDSZ(node)     ((node)->mn_lo | ((unsigned)(node)->mn_hi << 16))
    /** Set the size of the data for a leaf node */
#define SETDSZ(node,size)    do { \
    (node)->mn_lo = (size) & 0xffff; (node)->mn_hi = (size) >> 16;} while(0)
    /** The size of a key in a node */
#define NODEKSZ(node)     ((node)->mn_ksize)

    /** Copy a page number from src to dst */
#ifdef MISALIGNED_OK
#define COPY_PGNO(dst,src)    dst = src
#else
#if MDB_SIZE_MAX > 0xffffffffU
#define COPY_PGNO(dst,src)    do { \
    unsigned short *s, *d;    \
    s = (unsigned short *)&(src);    \
    d = (unsigned short *)&(dst);    \
    *d++ = *s++;    \
    *d++ = *s++;    \
    *d++ = *s++;    \
    *d = *s;    \
} while (0)
#else
#define COPY_PGNO(dst,src)    do { \
    unsigned short *s, *d;    \
    s = (unsigned short *)&(src);    \
    d = (unsigned short *)&(dst);    \
    *d++ = *s++;    \
    *d = *s;    \
} while (0)
#endif
#endif
    /** The address of a key in a LEAF2 page.
     *    LEAF2 pages are used for #MDB_DUPFIXED sorted-duplicate sub-DBs.
     *    There are no node headers, keys are stored contiguously.
     */
#define LEAF2KEY(p, i, ks)    ((char *)(p) + PAGEHDRSZ + ((i)*(ks)))

    /** Set the \b node's key into \b keyptr, if requested. */
#define MDB_GET_KEY(node, keyptr)    { if ((keyptr) != NULL) { \
    (keyptr)->mv_size = NODEKSZ(node); (keyptr)->mv_data = NODEKEY(node); } }

    /** Set the \b node's key into \b key. */
#define MDB_GET_KEY2(node, key)    { key.mv_size = NODEKSZ(node); key.mv_data = NODEKEY(node); }

    /** Information about a single database in the environment. */
typedef struct MDB_db {
    uint32_t    md_pad;        /**< also ksize for LEAF2 pages */
    uint16_t    md_flags;    /**< @ref mdb_dbi_open */
    uint16_t    md_depth;    /**< depth of this tree */
    pgno_t        md_branch_pages;    /**< number of internal pages */
    pgno_t        md_leaf_pages;        /**< number of leaf pages */
    pgno_t        md_overflow_pages;    /**< number of overflow pages */
    mdb_size_t    md_entries;        /**< number of data items */
    pgno_t        md_root;        /**< the root page of this tree */
} MDB_db;

#define MDB_VALID    0x8000        /**< DB handle is valid, for me_dbflags */
#define PERSISTENT_FLAGS    (0xffff & ~(MDB_VALID))
    /** #mdb_dbi_open() flags */
#define VALID_FLAGS    (MDB_REVERSEKEY|MDB_DUPSORT|MDB_INTEGERKEY|MDB_DUPFIXED|\
    MDB_INTEGERDUP|MDB_REVERSEDUP|MDB_CREATE)

    /** Handle for the DB used to track free pages. */
#define    FREE_DBI    0
    /** Handle for the default DB. */
#define    MAIN_DBI    1
    /** Number of DBs in metapage (free and main) - also hardcoded elsewhere */
#define CORE_DBS    2

    /** Number of meta pages - also hardcoded elsewhere */
#define NUM_METAS    2

    /** Meta page content.
     *    A meta page is the start point for accessing a database snapshot.
     *    Pages 0-1 are meta pages. Transaction N writes meta page #(N % 2).
     */
typedef struct MDB_meta {
        /** Stamp identifying this as an LMDB file. It must be set
         *    to #MDB_MAGIC. */
    uint32_t    mm_magic;
        /** Version number of this file. Must be set to #MDB_DATA_VERSION. */
    uint32_t    mm_version;
#ifdef MDB_VL32
    union {        /* always zero since we don't support fixed mapping in MDB_VL32 */
        MDB_ID    mmun_ull;
        void *mmun_address;
    } mm_un;
#define    mm_address mm_un.mmun_address
#else
    void        *mm_address;        /**< address for fixed mapping */
#endif
    mdb_size_t    mm_mapsize;            /**< size of mmap region */
    MDB_db        mm_dbs[CORE_DBS];    /**< first is free space, 2nd is main db */
    /** The size of pages used in this DB */
#define    mm_psize    mm_dbs[FREE_DBI].md_pad
    /** Any persistent environment flags. @ref mdb_env */
#define    mm_flags    mm_dbs[FREE_DBI].md_flags
    /** Last used page in the datafile.
     *    Actually the file may be shorter if the freeDB lists the final pages.
     */
    pgno_t        mm_last_pg;
    volatile txnid_t    mm_txnid;    /**< txnid that committed this page */
} MDB_meta;

    /** Buffer for a stack-allocated meta page.
     *    The members define size and alignment, and silence type
     *    aliasing warnings.  They are not used directly; that could
     *    mean incorrectly using several union members in parallel.
     */
typedef union MDB_metabuf {
    MDB_page    mb_page;
    struct {
        char        mm_pad[PAGEHDRSZ];
        MDB_meta    mm_meta;
    } mb_metabuf;
} MDB_metabuf;

    /** Auxiliary DB info.
     *    The information here is mostly static/read-only. There is
     *    only a single copy of this record in the environment.
     */
typedef struct MDB_dbx {
    MDB_val        md_name;        /**< name of the database */
    MDB_cmp_func    *md_cmp;    /**< function for comparing keys */
    MDB_cmp_func    *md_dcmp;    /**< function for comparing data items */
    MDB_rel_func    *md_rel;    /**< user relocate function */
    void        *md_relctx;        /**< user-provided context for md_rel */
} MDB_dbx;

    /** A database transaction.
     *    Every operation requires a transaction handle.
     */
struct MDB_txn {
    MDB_txn        *mt_parent;        /**< parent of a nested txn */
    /** Nested txn under this txn, set together with flag #MDB_TXN_HAS_CHILD */
    MDB_txn        *mt_child;
    pgno_t        mt_next_pgno;    /**< next unallocated page */
#ifdef MDB_VL32
    pgno_t        mt_last_pgno;    /**< last written page */
#endif
    /** The ID of this transaction. IDs are integers incrementing from 1.
     *    Only committed write transactions increment the ID. If a transaction
     *    aborts, the ID may be re-used by the next writer.
     */
    txnid_t        mt_txnid;
    MDB_env        *mt_env;        /**< the DB environment */
    /** The list of pages that became unused during this transaction.
     */
    MDB_IDL        mt_free_pgs;
    /** The list of loose pages that became unused and may be reused
     *    in this transaction, linked through #NEXT_LOOSE_PAGE(page).
     */
    MDB_page    *mt_loose_pgs;
    /** Number of loose pages (#mt_loose_pgs) */
    int            mt_loose_count;
    /** The sorted list of dirty pages we temporarily wrote to disk
     *    because the dirty list was full. page numbers in here are
     *    shifted left by 1, deleted slots have the LSB set.
     */
    MDB_IDL        mt_spill_pgs;
    union {
        /** For write txns: Modified pages. Sorted when not MDB_WRITEMAP. */
        MDB_ID2L    dirty_list;
        /** For read txns: This thread/txn's reader table slot, or NULL. */
        MDB_reader    *reader;
    } mt_u;
    /** Array of records for each DB known in the environment. */
    MDB_dbx        *mt_dbxs;
    /** Array of MDB_db records for each known DB */
    MDB_db        *mt_dbs;
    /** Array of sequence numbers for each DB handle */
    unsigned int    *mt_dbiseqs;
/** @defgroup mt_dbflag    Transaction DB Flags
 *    @ingroup internal
 * @{
 */
#define DB_DIRTY    0x01        /**< DB was written in this txn */
#define DB_STALE    0x02        /**< Named-DB record is older than txnID */
#define DB_NEW        0x04        /**< Named-DB handle opened in this txn */
#define DB_VALID    0x08        /**< DB handle is valid, see also #MDB_VALID */
#define DB_USRVALID    0x10        /**< As #DB_VALID, but not set for #FREE_DBI */
#define DB_DUPDATA    0x20        /**< DB is #MDB_DUPSORT data */
/** @} */
    /** In write txns, array of cursors for each DB */
    MDB_cursor    **mt_cursors;
    /** Array of flags for each DB */
    unsigned char    *mt_dbflags;
#ifdef MDB_VL32
    /** List of read-only pages (actually chunks) */
    MDB_ID3L    mt_rpages;
    /** We map chunks of 16 pages. Even though Windows uses 4KB pages, all
     * mappings must begin on 64KB boundaries. So we round off all pgnos to
     * a chunk boundary. We do the same on Linux for symmetry, and also to
     * reduce the frequency of mmap/munmap calls.
     */
#define MDB_RPAGE_CHUNK    16
#define MDB_TRPAGE_SIZE    4096    /**< size of #mt_rpages array of chunks */
#define MDB_TRPAGE_MAX    (MDB_TRPAGE_SIZE-1)    /**< maximum chunk index */
    unsigned int mt_rpcheck;    /**< threshold for reclaiming unref'd chunks */
#endif
    /**    Number of DB records in use, or 0 when the txn is finished.
     *    This number only ever increments until the txn finishes; we
     *    don't decrement it when individual DB handles are closed.
     */
    MDB_dbi        mt_numdbs;

/** @defgroup mdb_txn    Transaction Flags
 *    @ingroup internal
 *    @{
 */
    /** #mdb_txn_begin() flags */
#define MDB_TXN_BEGIN_FLAGS    (MDB_NOMETASYNC|MDB_NOSYNC|MDB_RDONLY)
#define MDB_TXN_NOMETASYNC    MDB_NOMETASYNC    /**< don't sync meta for this txn on commit */
#define MDB_TXN_NOSYNC        MDB_NOSYNC    /**< don't sync this txn on commit */
#define MDB_TXN_RDONLY        MDB_RDONLY    /**< read-only transaction */
    /* internal txn flags */
#define MDB_TXN_WRITEMAP    MDB_WRITEMAP    /**< copy of #MDB_env flag in writers */
#define MDB_TXN_FINISHED    0x01        /**< txn is finished or never began */
#define MDB_TXN_ERROR        0x02        /**< txn is unusable after an error */
#define MDB_TXN_DIRTY        0x04        /**< must write, even if dirty list is empty */
#define MDB_TXN_SPILLS        0x08        /**< txn or a parent has spilled pages */
#define MDB_TXN_HAS_CHILD    0x10        /**< txn has an #MDB_txn.%mt_child */
    /** most operations on the txn are currently illegal */
#define MDB_TXN_BLOCKED        (MDB_TXN_FINISHED|MDB_TXN_ERROR|MDB_TXN_HAS_CHILD)
/** @} */
    unsigned int    mt_flags;        /**< @ref mdb_txn */
    /** #dirty_list room: Array size - \#dirty pages visible to this txn.
     *    Includes ancestor txns' dirty pages not hidden by other txns'
     *    dirty/spilled pages. Thus commit(nested txn) has room to merge
     *    dirty_list into mt_parent after freeing hidden mt_parent pages.
     */
    unsigned int    mt_dirty_room;
};

/** Enough space for 2^32 nodes with minimum of 2 keys per node. I.e., plenty.
 * At 4 keys per node, enough for 2^64 nodes, so there's probably no need to
 * raise this on a 64 bit machine.
 */
#define CURSOR_STACK         32

struct MDB_xcursor;

    /** Cursors are used for all DB operations.
     *    A cursor holds a path of (page pointer, key index) from the DB
     *    root to a position in the DB, plus other state. #MDB_DUPSORT
     *    cursors include an xcursor to the current data item. Write txns
     *    track their cursors and keep them up to date when data moves.
     *    Exception: An xcursor's pointer to a #P_SUBP page can be stale.
     *    (A node with #F_DUPDATA but no #F_SUBDATA contains a subpage).
     */
struct MDB_cursor {
    /** Next cursor on this DB in this txn */
    MDB_cursor    *mc_next;
    /** Backup of the original cursor if this cursor is a shadow */
    MDB_cursor    *mc_backup;
    /** Context used for databases with #MDB_DUPSORT, otherwise NULL */
    struct MDB_xcursor    *mc_xcursor;
    /** The transaction that owns this cursor */
    MDB_txn        *mc_txn;
    /** The database handle this cursor operates on */
    MDB_dbi        mc_dbi;
    /** The database record for this cursor */
    MDB_db        *mc_db;
    /** The database auxiliary record for this cursor */
    MDB_dbx        *mc_dbx;
    /** The @ref mt_dbflag for this database */
    unsigned char    *mc_dbflag;
    unsigned short     mc_snum;    /**< number of pushed pages */
    unsigned short    mc_top;        /**< index of top page, normally mc_snum-1 */
/** @defgroup mdb_cursor    Cursor Flags
 *    @ingroup internal
 *    Cursor state flags.
 *    @{
 */
#define C_INITIALIZED    0x01    /**< cursor has been initialized and is valid */
#define C_EOF    0x02            /**< No more data */
#define C_SUB    0x04            /**< Cursor is a sub-cursor */
#define C_DEL    0x08            /**< last op was a cursor_del */
#define C_UNTRACK    0x40        /**< Un-track cursor when closing */
#define C_WRITEMAP    MDB_TXN_WRITEMAP /**< Copy of txn flag */
/** Read-only cursor into the txn's original snapshot in the map.
 *    Set for read-only txns, and in #mdb_page_alloc() for #FREE_DBI when
 *    #MDB_DEVEL & 2. Only implements code which is necessary for this.
 */
#define C_ORIG_RDONLY    MDB_TXN_RDONLY
/** @} */
    unsigned int    mc_flags;    /**< @ref mdb_cursor */
    MDB_page    *mc_pg[CURSOR_STACK];    /**< stack of pushed pages */
    indx_t        mc_ki[CURSOR_STACK];    /**< stack of page indices */
#ifdef MDB_VL32
    MDB_page    *mc_ovpg;        /**< a referenced overflow page */
#    define MC_OVPG(mc)            ((mc)->mc_ovpg)
#    define MC_SET_OVPG(mc, pg)    ((mc)->mc_ovpg = (pg))
#else
#    define MC_OVPG(mc)            ((MDB_page *)0)
#    define MC_SET_OVPG(mc, pg)    ((void)0)
#endif
};

    /** Context for sorted-dup records.
     *    We could have gone to a fully recursive design, with arbitrarily
     *    deep nesting of sub-databases. But for now we only handle these
     *    levels - main DB, optional sub-DB, sorted-duplicate DB.
     */
typedef struct MDB_xcursor {
    /** A sub-cursor for traversing the Dup DB */
    MDB_cursor mx_cursor;
    /** The database record for this Dup DB */
    MDB_db    mx_db;
    /**    The auxiliary DB record for this Dup DB */
    MDB_dbx    mx_dbx;
    /** The @ref mt_dbflag for this Dup DB */
    unsigned char mx_dbflag;
} MDB_xcursor;

    /** Check if there is an inited xcursor, so #XCURSOR_REFRESH() is proper */
#define XCURSOR_INITED(mc) \
    ((mc)->mc_xcursor && ((mc)->mc_xcursor->mx_cursor.mc_flags & C_INITIALIZED))

    /** Update sub-page pointer, if any, in \b mc->mc_xcursor.  Needed
     *    when the node which contains the sub-page may have moved.  Called
     *    with \b mp = mc->mc_pg[mc->mc_top], \b ki = mc->mc_ki[mc->mc_top].
     */
#define XCURSOR_REFRESH(mc, mp, ki) do { \
    MDB_page *xr_pg = (mp); \
    MDB_node *xr_node = NODEPTR(xr_pg, ki); \
    if ((xr_node->mn_flags & (F_DUPDATA|F_SUBDATA)) == F_DUPDATA) \
        (mc)->mc_xcursor->mx_cursor.mc_pg[0] = NODEDATA(xr_node); \
} while (0)

    /** State of FreeDB old pages, stored in the MDB_env */
typedef struct MDB_pgstate {
    pgno_t        *mf_pghead;    /**< Reclaimed freeDB pages, or NULL before use */
    txnid_t        mf_pglast;    /**< ID of last used record, or 0 if !mf_pghead */
} MDB_pgstate;

    /** The database environment. */
struct MDB_env {
    HANDLE        me_fd;        /**< The main data file */
    HANDLE        me_lfd;        /**< The lock file */
    HANDLE        me_mfd;        /**< For writing and syncing the meta pages */
#if defined(MDB_VL32) && defined(_WIN32)
    HANDLE        me_fmh;        /**< File Mapping handle */
#endif
    /** Failed to update the meta page. Probably an I/O error. */
#define    MDB_FATAL_ERROR    0x80000000U
    /** Some fields are initialized. */
#define    MDB_ENV_ACTIVE    0x20000000U
    /** me_txkey is set */
#define    MDB_ENV_TXKEY    0x10000000U
    /** fdatasync is unreliable */
#define    MDB_FSYNCONLY    0x08000000U
    uint32_t     me_flags;        /**< @ref mdb_env */
    unsigned int    me_psize;    /**< DB page size, inited from me_os_psize */
    unsigned int    me_os_psize;    /**< OS page size, from #GET_PAGESIZE */
    unsigned int    me_maxreaders;    /**< size of the reader table */
    /** Max #MDB_txninfo.%mti_numreaders of interest to #mdb_env_close() */
    volatile int    me_close_readers;
    MDB_dbi        me_numdbs;        /**< number of DBs opened */
    MDB_dbi        me_maxdbs;        /**< size of the DB table */
    MDB_PID_T    me_pid;        /**< process ID of this env */
    char        *me_path;        /**< path to the DB files */
    char        *me_map;        /**< the memory map of the data file */
    MDB_txninfo    *me_txns;        /**< the memory map of the lock file or NULL */
    MDB_meta    *me_metas[NUM_METAS];    /**< pointers to the two meta pages */
    void        *me_pbuf;        /**< scratch area for DUPSORT put() */
    MDB_txn        *me_txn;        /**< current write transaction */
    MDB_txn        *me_txn0;        /**< prealloc'd write transaction */
    mdb_size_t    me_mapsize;        /**< size of the data memory map */
    off_t        me_size;        /**< current file size */
    pgno_t        me_maxpg;        /**< me_mapsize / me_psize */
    MDB_dbx        *me_dbxs;        /**< array of static DB info */
    uint16_t    *me_dbflags;    /**< array of flags from MDB_db.md_flags */
    unsigned int    *me_dbiseqs;    /**< array of dbi sequence numbers */
    pthread_key_t    me_txkey;    /**< thread-key for readers */
    txnid_t        me_pgoldest;    /**< ID of oldest reader last time we looked */
    MDB_pgstate    me_pgstate;        /**< state of old pages from freeDB */
#    define        me_pglast    me_pgstate.mf_pglast
#    define        me_pghead    me_pgstate.mf_pghead
    MDB_page    *me_dpages;        /**< list of malloc'd blocks for re-use */
    /** IDL of pages that became unused in a write txn */
    MDB_IDL        me_free_pgs;
    /** ID2L of pages written during a write txn. Length MDB_IDL_UM_SIZE. */
    MDB_ID2L    me_dirty_list;
    /** Max number of freelist items that can fit in a single overflow page */
    int            me_maxfree_1pg;
    /** Max size of a node on a page */
    unsigned int    me_nodemax;
#if !(MDB_MAXKEYSIZE)
    unsigned int    me_maxkey;    /**< max size of a key */
#endif
    int        me_live_reader;        /**< have liveness lock in reader table */
#ifdef _WIN32
    int        me_pidquery;        /**< Used in OpenProcess */
#endif
#ifdef MDB_USE_POSIX_MUTEX    /* Posix mutexes reside in shared mem */
#    define        me_rmutex    me_txns->mti_rmutex /**< Shared reader lock */
#    define        me_wmutex    me_txns->mti_wmutex /**< Shared writer lock */
#else
    mdb_mutex_t    me_rmutex;
    mdb_mutex_t    me_wmutex;
# if defined(_WIN32) || defined(MDB_USE_POSIX_SEM)
    /** Half-initialized name of mutexes, to be completed by #MUTEXNAME() */
    char        me_mutexname[sizeof(MUTEXNAME_PREFIX) + 11];
# endif
#endif
#ifdef MDB_VL32
    MDB_ID3L    me_rpages;    /**< like #mt_rpages, but global to env */
    pthread_mutex_t    me_rpmutex;    /**< control access to #me_rpages */
#define MDB_ERPAGE_SIZE    16384
#define MDB_ERPAGE_MAX    (MDB_ERPAGE_SIZE-1)
    unsigned int me_rpcheck;
#endif
    void        *me_userctx;     /**< User-settable context */
    MDB_assert_func *me_assert_func; /**< Callback for assertion failures */
};

    /** Nested transaction */
typedef struct MDB_ntxn {
    MDB_txn        mnt_txn;        /**< the transaction */
    MDB_pgstate    mnt_pgstate;    /**< parent transaction's saved freestate */
} MDB_ntxn;

    /** max number of pages to commit in one writev() call */
#define MDB_COMMIT_PAGES     64
#if defined(IOV_MAX) && IOV_MAX < MDB_COMMIT_PAGES
#undef MDB_COMMIT_PAGES
#define MDB_COMMIT_PAGES    IOV_MAX
#endif

    /** max bytes to write in one call */
#define MAX_WRITE        (0x40000000U >> (sizeof(ssize_t) == 4))

    /** Check \b txn and \b dbi arguments to a function */
#define TXN_DBI_EXIST(txn, dbi, validity) \
    ((txn) && (dbi)<(txn)->mt_numdbs && ((txn)->mt_dbflags[dbi] & (validity)))

    /** Check for misused \b dbi handles */
#define TXN_DBI_CHANGED(txn, dbi) \
    ((txn)->mt_dbiseqs[dbi] != (txn)->mt_env->me_dbiseqs[dbi])

static int  mdb_page_alloc(MDB_cursor *mc, int num, MDB_page **mp);
static int  mdb_page_new(MDB_cursor *mc, uint32_t flags, int num, MDB_page **mp);
static int  mdb_page_touch(MDB_cursor *mc);

#define MDB_END_NAMES {"committed", "empty-commit", "abort", "reset", \
    "reset-tmp", "fail-begin", "fail-beginchild"}
enum {
    /* mdb_txn_end operation number, for logging */
    MDB_END_COMMITTED, MDB_END_EMPTY_COMMIT, MDB_END_ABORT, MDB_END_RESET,
    MDB_END_RESET_TMP, MDB_END_FAIL_BEGIN, MDB_END_FAIL_BEGINCHILD
};
#define MDB_END_OPMASK    0x0F    /**< mask for #mdb_txn_end() operation number */
#define MDB_END_UPDATE    0x10    /**< update env state (DBIs) */
#define MDB_END_FREE    0x20    /**< free txn unless it is #MDB_env.%me_txn0 */
#define MDB_END_SLOT MDB_NOTLS    /**< release any reader slot if #MDB_NOTLS */
static void mdb_txn_end(MDB_txn *txn, unsigned mode);

static int  mdb_page_get(MDB_cursor *mc, pgno_t pgno, MDB_page **mp, int *lvl);
static int  mdb_page_search_root(MDB_cursor *mc,
                MDB_val *key, int modify);
#define MDB_PS_MODIFY    1
#define MDB_PS_ROOTONLY    2
#define MDB_PS_FIRST    4
#define MDB_PS_LAST        8
static int  mdb_page_search(MDB_cursor *mc,
                MDB_val *key, int flags);
static int    mdb_page_merge(MDB_cursor *csrc, MDB_cursor *cdst);

#define MDB_SPLIT_REPLACE    MDB_APPENDDUP    /**< newkey is not new */
static int    mdb_page_split(MDB_cursor *mc, MDB_val *newkey, MDB_val *newdata,
                pgno_t newpgno, unsigned int nflags);

static int  mdb_env_read_header(MDB_env *env, int prev, MDB_meta *meta);
static MDB_meta *mdb_env_pick_meta(const MDB_env *env);
static int  mdb_env_write_meta(MDB_txn *txn);
#ifdef MDB_USE_POSIX_MUTEX /* Drop unused excl arg */
# define mdb_env_close0(env, excl) mdb_env_close1(env)
#endif
static void mdb_env_close0(MDB_env *env, int excl);

static MDB_node *mdb_node_search(MDB_cursor *mc, MDB_val *key, int *exactp);
static int  mdb_node_add(MDB_cursor *mc, indx_t indx,
                MDB_val *key, MDB_val *data, pgno_t pgno, unsigned int flags);
static void mdb_node_del(MDB_cursor *mc, int ksize);
static void mdb_node_shrink(MDB_page *mp, indx_t indx);
static int    mdb_node_move(MDB_cursor *csrc, MDB_cursor *cdst, int fromleft);
static int  mdb_node_read(MDB_cursor *mc, MDB_node *leaf, MDB_val *data);
static size_t    mdb_leaf_size(MDB_env *env, MDB_val *key, MDB_val *data);
static size_t    mdb_branch_size(MDB_env *env, MDB_val *key);

static int    mdb_rebalance(MDB_cursor *mc);
static int    mdb_update_key(MDB_cursor *mc, MDB_val *key);

static void    mdb_cursor_pop(MDB_cursor *mc);
static int    mdb_cursor_push(MDB_cursor *mc, MDB_page *mp);

static int    mdb_cursor_del0(MDB_cursor *mc);
static int    mdb_del0(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data, unsigned flags);
static int    mdb_cursor_sibling(MDB_cursor *mc, int move_right);
static int    mdb_cursor_next(MDB_cursor *mc, MDB_val *key, MDB_val *data, MDB_cursor_op op);
static int    mdb_cursor_prev(MDB_cursor *mc, MDB_val *key, MDB_val *data, MDB_cursor_op op);
static int    mdb_cursor_set(MDB_cursor *mc, MDB_val *key, MDB_val *data, MDB_cursor_op op,
                int *exactp);
static int    mdb_cursor_first(MDB_cursor *mc, MDB_val *key, MDB_val *data);
static int    mdb_cursor_last(MDB_cursor *mc, MDB_val *key, MDB_val *data);

static void    mdb_cursor_init(MDB_cursor *mc, MDB_txn *txn, MDB_dbi dbi, MDB_xcursor *mx);
static void    mdb_xcursor_init0(MDB_cursor *mc);
static void    mdb_xcursor_init1(MDB_cursor *mc, MDB_node *node);
static void    mdb_xcursor_init2(MDB_cursor *mc, MDB_xcursor *src_mx, int force);

static int    mdb_drop0(MDB_cursor *mc, int subs);
static void mdb_default_cmp(MDB_txn *txn, MDB_dbi dbi);
static int mdb_reader_check0(MDB_env *env, int rlocked, int *dead);

/** @cond */
static MDB_cmp_func    mdb_cmp_memn, mdb_cmp_memnr, mdb_cmp_int, mdb_cmp_cint, mdb_cmp_long;
/** @endcond */

/** Compare two items pointing at '#mdb_size_t's of unknown alignment. */
#ifdef MISALIGNED_OK
# define mdb_cmp_clong mdb_cmp_long
#else
# define mdb_cmp_clong mdb_cmp_cint
#endif

/** True if we need #mdb_cmp_clong() instead of \b cmp for #MDB_INTEGERDUP */
#define NEED_CMP_CLONG(cmp, ksize) \
    (UINT_MAX < MDB_SIZE_MAX && \
     (cmp) == mdb_cmp_int && (ksize) == sizeof(mdb_size_t))

#ifdef _WIN32
static SECURITY_DESCRIPTOR mdb_null_sd;
static SECURITY_ATTRIBUTES mdb_all_sa;
static int mdb_sec_inited;

struct MDB_name;
static int utf8_to_utf16(const char *src, struct MDB_name *dst, int xtra);
#endif

/** Return the library version info. */
/*
Griff comments out because it is also defined in liblmdb.a(mdb.o)
char * ESECT
mdb_version(int *major, int *minor, int *patch)
{
    if (major) *major = MDB_VERSION_MAJOR;
    if (minor) *minor = MDB_VERSION_MINOR;
    if (patch) *patch = MDB_VERSION_PATCH;
    return MDB_VERSION_STRING;
}
*/

/** Table of descriptions for LMDB @ref errors */
static char *const mdb_errstr[] = {
    "MDB_KEYEXIST: Key/data pair already exists",
    "MDB_NOTFOUND: No matching key/data pair found",
    "MDB_PAGE_NOTFOUND: Requested page not found",
    "MDB_CORRUPTED: Located page was wrong type",
    "MDB_PANIC: Update of meta page failed or environment had fatal error",
    "MDB_VERSION_MISMATCH: Database environment version mismatch",
    "MDB_INVALID: File is not an LMDB file",
    "MDB_MAP_FULL: Environment mapsize limit reached",
    "MDB_DBS_FULL: Environment maxdbs limit reached",
    "MDB_READERS_FULL: Environment maxreaders limit reached",
    "MDB_TLS_FULL: Thread-local storage keys full - too many environments open",
    "MDB_TXN_FULL: Transaction has too many dirty pages - transaction too big",
    "MDB_CURSOR_FULL: Internal error - cursor stack limit reached",
    "MDB_PAGE_FULL: Internal error - page has no more space",
    "MDB_MAP_RESIZED: Database contents grew beyond environment mapsize",
    "MDB_INCOMPATIBLE: Operation and DB incompatible, or DB flags changed",
    "MDB_BAD_RSLOT: Invalid reuse of reader locktable slot",
    "MDB_BAD_TXN: Transaction must abort, has a child, or is invalid",
    "MDB_BAD_VALSIZE: Unsupported size of key/DB name/data, or wrong DUPFIXED size",
    "MDB_BAD_DBI: The specified DBI handle was closed/changed unexpectedly",
    "MDB_PROBLEM: Unexpected problem - txn should abort",
};

/* Griff comments out because it is also defined in liblmdb.a(mdb.o)
char *
mdb_strerror(int err)
{
#ifdef _WIN32
    /** HACK: pad 4KB on stack over the buf. Return system msgs in buf.
     *    This works as long as no function between the call to mdb_strerror
     *    and the actual use of the message uses more than 4K of stack.
     * /
#define MSGSIZE    1024
#define PADSIZE    4096
    char buf[MSGSIZE+PADSIZE], *ptr = buf;
#endif
    int i;
    if (!err)
        return ("Successful return: 0");

    if (err >= MDB_KEYEXIST && err <= MDB_LAST_ERRCODE) {
        i = err - MDB_KEYEXIST;
        return mdb_errstr[i];
    }

#ifdef _WIN32
    /* These are the C-runtime error codes we use. The comment indicates
     * their numeric value, and the Win32 error they would correspond to
     * if the error actually came from a Win32 API. A major mess, we should
     * have used LMDB-specific error codes for everything.
     * /
    switch(err) {
    case ENOENT:    /* 2, FILE_NOT_FOUND * /
    case EIO:        /* 5, ACCESS_DENIED * /
    case ENOMEM:    /* 12, INVALID_ACCESS * /
    case EACCES:    /* 13, INVALID_DATA * /
    case EBUSY:        /* 16, CURRENT_DIRECTORY * /
    case EINVAL:    /* 22, BAD_COMMAND * /
    case ENOSPC:    /* 28, OUT_OF_PAPER * /
        return strerror(err);
    default:
        ;
    }
    buf[0] = 0;
    FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL, err, 0, ptr, MSGSIZE, (va_list *)buf+MSGSIZE);
    return ptr;
#else
    return strerror(err);
#endif
}
*/

/** assert(3) variant in cursor context */
#define mdb_cassert(mc, expr)    mdb_assert0((mc)->mc_txn->mt_env, expr, #expr)
/** assert(3) variant in transaction context */
#define mdb_tassert(txn, expr)    mdb_assert0((txn)->mt_env, expr, #expr)
/** assert(3) variant in environment context */
#define mdb_eassert(env, expr)    mdb_assert0(env, expr, #expr)

#ifndef NDEBUG
# define mdb_assert0(env, expr, expr_txt) ((expr) ? (void)0 : \
        mdb_assert_fail(env, expr_txt, mdb_func_, __FILE__, __LINE__))

static void ESECT
mdb_assert_fail(MDB_env *env, const char *expr_txt,
    const char *func, const char *file, int line)
{
    char buf[400];
    sprintf(buf, "%.100s:%d: Assertion '%.200s' failed in %.40s()",
        file, line, expr_txt, func);
    if (env->me_assert_func)
        env->me_assert_func(env, buf);
    fprintf(stderr, "%s\n", buf);
    abort();
}
#else
# define mdb_assert0(env, expr, expr_txt) ((void) 0)
#endif /* NDEBUG */


#include <math.h>
#include <sys/time.h>
#include "assert.h"
/*
#define E(expr) CHECK((rc = (expr)) == MDB_SUCCESS, #expr)
#define RES(err, expr) ((rc = expr) == (err) || (CHECK(!rc, #expr), 0))
#define CHECK(test, msg) ((test) ? (void)0 : ((void)fprintf(stderr, \
	"%s:%d: %s: %s\n", __FILE__, __LINE__, msg, mdb_strerror(rc)), abort()))
*/

