/*
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 2000, 2001, 2002, 2004 Ben Wing.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not really in FSF. */

#ifndef INCLUDED_sysfile_h_
#define INCLUDED_sysfile_h_

/* The anonymous voice of the past says:
   Must come before sysfile.h

   So instead we just put it here. --ben */
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include <errno.h>

#ifndef WIN32_NATIVE
# include <sys/errno.h>		/* <errno.h> does not always imply this */
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef INCLUDED_FCNTL
# define INCLUDED_FCNTL
# include <fcntl.h>
#endif /* INCLUDED_FCNTL */

/* The anonymous voice of the past says:
   In some systems loading it twice is suicidal.  */
#ifndef INCLUDED_SYS_TYPES
# define INCLUDED_SYS_TYPES
# include <sys/types.h>		/* some typedefs are used in sys/file.h */
#endif /* INCLUDED_SYS_TYPES */

#ifndef WIN32_NATIVE
# include <sys/file.h>
#endif

/* Some systems (SCO 3.2v5) do #define stat ... in this header.  So it
   _must_ be included before any use of struct stat.  Most emacs files
   should include sysfile.h.  The unex*.c include <sys/stat.h> directly. */
#include <sys/stat.h>

#ifdef WIN32_ANY
# include <io.h>
#endif

#ifdef WIN32_NATIVE
# include <direct.h>
#else
/* Some configuration files' definitions for the LOAD_AVE_CVT macro
   (like sparc.h's) use macros like FSCALE, defined here. */
# ifdef HAVE_GTK
/* I hate GTK */
#  undef MIN
#  undef MAX
# endif /* HAVE_GTK */
# include <sys/param.h>
/* As per Martin's recommendation, we do not include this.  There was
   a comment stating that stuff from here was needed on NeXT, Cygwin,
   and sunplay.c.  However, Cygwin includes this automatically from
   fcntl.h, and Martin says that a "conforming" system should never
   need this.  We will put it back if necessary on systems requiring it. */
/* # include <sys/fcntl.h> */
#endif /* WIN32_NATIVE */

#ifndef	STDERR_FILENO
#define	STDIN_FILENO	0
#define	STDOUT_FILENO	1
#define	STDERR_FILENO	2
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

#ifndef O_RDWR
#define O_RDWR 2
#endif

/* file opening defaults */
#ifndef OPEN_BINARY
#ifdef O_BINARY
#define OPEN_BINARY	O_BINARY
#else
#define OPEN_BINARY	(0)
#endif
#endif

#ifndef OPEN_TEXT
#ifdef O_TEXT
#define OPEN_TEXT	O_TEXT
#else
#define OPEN_TEXT	(0)
#endif
#endif

#ifndef CREAT_MODE
#ifdef WIN32_NATIVE
#define CREAT_MODE	(S_IREAD | S_IWRITE)
#else
#define CREAT_MODE	(0666)
#endif
#endif

#ifndef READ_TEXT
#ifdef O_TEXT
#define READ_TEXT "rt"
#else
#define READ_TEXT "r"
#endif
#endif

#ifndef READ_BINARY
#ifdef O_BINARY
#define READ_BINARY "rb"
#else
#define READ_BINARY "r"
#endif
#endif

#ifndef READ_PLUS_TEXT
#ifdef O_TEXT
#define READ_PLUS_TEXT "r+t"
#else
#define READ_PLUS_TEXT "r+"
#endif
#endif

#ifndef READ_PLUS_BINARY
#ifdef O_BINARY
#define READ_PLUS_BINARY "r+b"
#else
#define READ_PLUS_BINARY "r+"
#endif
#endif

#ifndef WRITE_TEXT
#ifdef O_TEXT
#define WRITE_TEXT "wt"
#else
#define WRITE_TEXT "w"
#endif
#endif

#ifndef WRITE_BINARY
#ifdef O_BINARY
#define WRITE_BINARY "wb"
#else
#define WRITE_BINARY "w"
#endif
#endif

#ifndef APPEND_TEXT
#ifdef O_TEXT
#define APPEND_TEXT "at"
#else
#define APPEND_TEXT "a"
#endif
#endif

#ifndef APPEND_BINARY
#ifdef O_BINARY
#define APPEND_BINARY "ab"
#else
#define APPEND_BINARY "a"
#endif
#endif

#ifndef O_NONBLOCK
#ifdef O_NDELAY
#define O_NONBLOCK O_NDELAY
#else
#define O_NONBLOCK 04000
#endif
#endif

#if !S_IRUSR
# if S_IREAD
#  define S_IRUSR S_IREAD
# else
#  define S_IRUSR 00400
# endif
#endif

#if !S_IWUSR
# if S_IWRITE
#  define S_IWUSR S_IWRITE
# else
#  define S_IWUSR 00200
# endif
#endif

#if !S_IXUSR
# if S_IEXEC
#  define S_IXUSR S_IEXEC
# else
#  define S_IXUSR 00100
# endif
#endif

#ifdef STAT_MACROS_BROKEN
#undef S_ISBLK
#undef S_ISCHR
#undef S_ISDIR
#undef S_ISFIFO
#undef S_ISLNK
#undef S_ISMPB
#undef S_ISMPC
#undef S_ISNWK
#undef S_ISREG
#undef S_ISSOCK
#endif /* STAT_MACROS_BROKEN.  */

#ifdef WIN32_NATIVE
/* This is the standard value for S_IFLNK.  All of the S_... flags that
   exist in the MSVCRT have standard values, so their bit tests will
   magically work. */
#define S_IFLNK 0120000
#endif

#if !defined(S_ISBLK) && defined(S_IFBLK)
#define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)
#endif
#if !defined(S_ISCHR) && defined(S_IFCHR)
#define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)
#endif
#if !defined(S_ISDIR) && defined(S_IFDIR)
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif
#if !defined(S_ISREG) && defined(S_IFREG)
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif
#if !defined(S_ISFIFO) && defined(S_IFIFO)
#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#endif
#if !defined(S_ISLNK) && defined(S_IFLNK)
#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif
#if !defined(S_ISSOCK) && defined(S_IFSOCK)
#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
#endif
#if !defined(S_ISMPB) && defined(S_IFMPB) /* V7 */
#define S_ISMPB(m) (((m) & S_IFMT) == S_IFMPB)
#define S_ISMPC(m) (((m) & S_IFMT) == S_IFMPC)
#endif
#if !defined(S_ISNWK) && defined(S_IFNWK) /* HP/UX */
#define S_ISNWK(m) (((m) & S_IFMT) == S_IFNWK)
#endif

/* Determining the maximum pathname length.

   NOTE: IN GENERAL, YOU SHOULD NOT USE THIS.
   If at all possible, avoid using fixed-length buffers of any sort.
   You cannot guarantee on many systems that pathnames won't exceed
   these limits for one reason or another.

   Unfortunately, there is no universal agreement over whether these
   values should include a final null-terminator or not.  Even recent
   versions of Linux (circa 2002) are switching from the not-including-
   terminator kind to the including-terminator kind.  So we assume that
   the including-terminator kind will be even (usually is), and round
   up as necessary. */
    
#define ROUND_UP_TO_EVEN_NUMBER(val) (((val + 1) >> 1) << 1)
#ifdef PATH_MAX
# define QXE_PATH_MAX ROUND_UP_TO_EVEN_NUMBER (PATH_MAX)
#elif defined (_MAX_PATH)
/* MS Win -- and preferable to _POSIX_PATH_MAX, which is also defined */
# define QXE_PATH_MAX ROUND_UP_TO_EVEN_NUMBER (_MAX_PATH)
#elif defined (_POSIX_PATH_MAX)
# define QXE_PATH_MAX ROUND_UP_TO_EVEN_NUMBER (_POSIX_PATH_MAX)
#elif defined (MAXPATHLEN)
# define QXE_PATH_MAX ROUND_UP_TO_EVEN_NUMBER (MAXPATHLEN)
#else
# define QXE_PATH_MAX 1024
#endif

/* Client .c files should use PATH_MAX_INTERNAL or PATH_MAX_EXTERNAL
   if they must use either one at all. */

/* Use for internally formatted text, which can potentially have up to
   four bytes per character */
#define PATH_MAX_INTERNAL (QXE_PATH_MAX * MAX_ICHAR_LEN)
/* Use for externally formatted text. */
#define PATH_MAX_EXTERNAL (QXE_PATH_MAX * MAX_XETCHAR_SIZE)

/* The following definitions are needed under Windows, at least */
#ifndef X_OK
# define X_OK 1
#endif

#ifndef R_OK
# define R_OK 4
#endif

#ifndef D_OK
# define D_OK 8
#endif

#ifndef W_OK
# define W_OK 2
#endif

#ifndef F_OK
# define F_OK 0
#endif

#ifndef FD_CLOEXEC
# define FD_CLOEXEC 1
#endif

#ifdef emacs

/* Emacs needs to use its own definitions of certain system calls on
   some systems (like SunOS 4.1 and USG systems, where the read system
   call is interruptible but Emacs expects it not to be; and under
   MULE, where all filenames need to be converted to external format).

   We used to play preprocessor games, but in the long run that just leads
   you to ruin.  So we explicitly put in the new calls, even if the source
   gets marginally less pretty.

   Current files where we don't use retry_ or qxe_ versions:

   -- all sound files except ntplay.c (includes esd.c libsst.[ch] libst.h
                                       linuxplay.c sgiplay.c sunplay.c
				       hpplay.c nas.c)
   -- all unex* files
   -- hftctl.c
   -- lib-src files
   */

ssize_t retry_read (int, void *, size_t);
ssize_t retry_write (int, const void *, size_t);
int retry_open (const Extbyte *path, int oflag, ...);
int qxe_open (const Ibyte *path, int oflag, ...);
int qxe_interruptible_open (const Ibyte *path, int oflag, int mode);
int retry_close (int);
Bytecount read_allowing_quit (int fildes, void *buf, Bytecount size);
Bytecount write_allowing_quit (int fildes, const void *buf,
				  Bytecount size);

/* Now the stdio versions ... */

size_t retry_fread (void *, size_t, size_t, FILE *);
size_t retry_fwrite (const void *, size_t, size_t, FILE *);
FILE *retry_fopen (const Extbyte *path, const Ascbyte *mode);
FILE *qxe_fopen (const Ibyte *path, const Ascbyte *mode);
int retry_fclose (FILE *);

/* encapsulations: file-information calls */

int qxe_access (const Ibyte *path, int mode);
int qxe_eaccess (const Ibyte *path, int mode);
int qxe_lstat (const Ibyte *path, struct stat *buf);
int qxe_readlink (const Ibyte *path, Ibyte *buf, size_t bufsiz);
int qxe_fstat (int fd, struct stat *buf);
int qxe_stat (const Ibyte *path, struct stat *buf);
Ibyte *qxe_realpath (const Ibyte *path, Ibyte resolved_path [],
		     Boolint links_only);

/* encapsulations: file-manipulation calls */

int qxe_chmod (const Ibyte *path, mode_t mode);

#if defined (HAVE_LINK)
int qxe_link (const Ibyte *existing, const Ibyte *new_);
#endif /* defined (HAVE_LINK) */

int qxe_rename (const Ibyte *old, const Ibyte *new_);

#if defined (HAVE_SYMLINK)
int qxe_symlink (const Ibyte *name1, const Ibyte *name2);
#endif /* defined (HAVE_SYMLINK) */

int qxe_unlink (const Ibyte *path);

/* definition in filemode.c
   must be declared here to ensure that struct stat is properly formed
   on systems like SCO 3.2v5 */
void filemodestring (struct stat *, char *);

#ifdef WIN32_ANY
extern int mswindows_shortcuts_are_symlinks;
#endif

#endif /* emacs */


#ifndef HAVE_H_ERRNO
extern int h_errno;
#endif

#ifndef HAVE_DUP2
int dup2 (int oldd, int newd);
#endif

#ifndef HAVE_STRERROR
/* X11R6 defines strerror as a macro */
# ifdef strerror
# undef strerror
# endif
const char *strerror (int);
#endif



/* 
   DEFAULT_DIRECTORY_SEP is the default value of Vdirectory_sep_char.
   DIRECTORY_SEP is the currently preferred separator between elements
     of a path, when paths are canonicalized.
   DEVICE_SEP is the separator between devices and paths (might not
     be defined).
   SEPCHAR is the separator between paths in a path search string
     (e.g. the PATH environment variable).
   IS_DIRECTORY_SEP() returns true if the character is any directory
     separator (there might be more than one allowed on a system.).
   IS_DEVICE_SEP() returns true if the character is a device separator.
   IS_ANY_SEP() returns true if the character is a directory or device
     separator.
*/

/* We used to put some of this stuff in the s+m files for the various
   types of MS Windows, but that's disingenuous.  The various definitions
   above were specifically created for MS Windows, and the "if not, then
   let's define the defaults" stuff (formerly in lisp.h) specifically knows
   about what is going to get redefined and how, and code all over the
   place that works with filenames has to conditionalize on WIN32_NATIVE
   anyway.  It's much clearer if we put all related definitions in one
   place. (In fact, I discovered a number of bugs in the process.)

   S+M files should be used for simple on-off or multiple-choice settings,
   or possibly string settings.  Anything that gets to the level of
   programming should be elsewhere, and anything that ends up having
   lots of complicated interactions scattered around in many files should
   be consolidated. */

#ifdef WIN32_NATIVE

#define SEPCHAR ';'
#define DEFAULT_DIRECTORY_SEP '\\'

#ifdef emacs
DECLARE_INLINE_HEADER (Ibyte sysfile_get_directory_sep (void))
{
  if (!CHARP (Vdirectory_sep_char)
      || (XCHAR (Vdirectory_sep_char) != '/'
	  && XCHAR (Vdirectory_sep_char) != '\\'))
    {
      warn_when_safe
	(Qfile_name, Qerror,
	 "`directory-sep-char' set to invalid %s: resetting to %c.",
	 DEFAULT_DIRECTORY_SEP);
      Vdirectory_sep_char = make_char (DEFAULT_DIRECTORY_SEP);
    }

  return XCHAR (Vdirectory_sep_char);
}
#define DIRECTORY_SEP sysfile_get_directory_sep()

#else /* emacs */

/* The above Lisp variables are not available to make-docfile, etc. */
#define DIRECTORY_SEP DEFAULT_DIRECTORY_SEP

#endif /* emacs */

#else /* not WIN32_NATIVE */

#define SEPCHAR ':'
#define DEFAULT_DIRECTORY_SEP '/'
#define DIRECTORY_SEP '/'

#endif /* WIN32_NATIVE */


#ifdef WIN32_ANY

#define DEVICE_SEP ':'

#define IS_DEVICE_SEP(c) ((c) == DEVICE_SEP)

#ifdef emacs

DECLARE_INLINE_HEADER (int IS_DIRECTORY_SEP (Ichar c))
{
  return (c == '/' || c == '\\');
}

DECLARE_INLINE_HEADER (int IS_ANY_SEP (Ichar c))
{
  return (c == '/' || c == '\\' || c == ':');
}

#else /* emacs */

/* The Ichar typedef is not available to make-docfile, etc. */

DECLARE_INLINE_HEADER (int IS_DIRECTORY_SEP (int c))
{
  return (c == '/' || c == '\\');
}

DECLARE_INLINE_HEADER (int IS_ANY_SEP (int c))
{
  return (c == '/' || c == '\\' || c == ':');
}

#endif

#else /* not WIN32_ANY */

#define IS_DEVICE_SEP(c) 0
#define IS_DIRECTORY_SEP(c) ((c) == DIRECTORY_SEP)
#define IS_ANY_SEP(c) IS_DIRECTORY_SEP (c)

#endif /* WIN32_ANY */

/* How long can a source filename be in DOC (including "\037S" at the start
   and "\n" at the end) ? */
#define DOC_MAX_FILENAME_LENGTH 2048

#ifdef emacs

#if defined (WIN32_NATIVE)
#define PATHNAME_RESOLVE_LINKS(path, pathout)		\
do							\
{							\
  if (mswindows_shortcuts_are_symlinks)			\
    {							\
      Ibyte *_prl_path_ = (Ibyte *) (path);		\
      Ibyte _prl_path2_[PATH_MAX_INTERNAL];		\
							\
      if (!qxe_realpath (_prl_path_, _prl_path2_, 1))	\
	(pathout) = _prl_path_;				\
      else						\
	IBYTE_STRING_TO_ALLOCA (_prl_path2_, pathout);	\
    }							\
  else (pathout) = (Ibyte *) (path);			\
} while (0)
#else
#define PATHNAME_RESOLVE_LINKS(path, pathout) ((pathout) = (Ibyte *) (path))
#endif

#define LISP_PATHNAME_RESOLVE_LINKS(path, pathout) \
  PATHNAME_RESOLVE_LINKS (XSTRING_DATA (path), pathout)

/* The documentation in VC++ claims that the pathname library functions
   accept strings in the current locale-specific encoding, but that's
   false, because they just call the native Win32 routines directly, which
   always use the system-default encoding (which is what Qmswindows_tstr
   will give us when not XEUNICODE_P). */
#ifdef WIN32_NATIVE
# define PATHNAME_CONVERT_OUT(path, pathout)	\
do						\
{						\
  const Ibyte *_pco_path_;			\
  PATHNAME_RESOLVE_LINKS (path, _pco_path_);	\
  C_STRING_TO_TSTR (_pco_path_, pathout);	\
} while (0)
#else
# define PATHNAME_CONVERT_OUT(path, pathout) \
  C_STRING_TO_EXTERNAL (path, pathout, Qfile_name)
#endif

#define LISP_PATHNAME_CONVERT_OUT(path, pathout) \
  PATHNAME_CONVERT_OUT (XSTRING_DATA (path), pathout)

#endif /* emacs */

#endif /* INCLUDED_sysfile_h_ */
