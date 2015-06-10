/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.in by autoheader.  */
/* Define this if SYSV IPC is available (e.g. msgget, etc.) */
#define HAVE_SYSVIPC 1

/* Define this if internet-domain sockets are available */
#define HAVE_INTERNET_DOMAIN_SOCKETS 1

/* Define this if berkeley unix-domain sockets are available */
#define HAVE_UNIX_DOMAIN_SOCKETS 1

/* Define this if have sun_len member in struct sockaddr_un */
/* #undef HAVE_SOCKADDR_SUN_LEN */

/* Define this if using Xauth authentication */
#define HAVE_XAUTH 1

/* #undef socklen_t */

#define HAVE_X_WINDOWS 1

/* #undef _GNU_SOURCE */

/* Define to 1 if you have the <bsd/sgtty.h> header file. */
/* #undef HAVE_BSD_SGTTY_H */

/* Define to 1 if you have the declaration of `sys_siglist', and to 0 if you
   don't. */
#define HAVE_DECL_SYS_SIGLIST 1

/* Define to 1 if you have the `getcwd' function. */
#define HAVE_GETCWD 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `intl' library (-lintl). */
#define HAVE_LIBINTL 1

/* Define to 1 if you have the <libintl.h> header file. */
#define HAVE_LIBINTL_H 1

/* Define to 1 if you have the `nsl' library (-lnsl). */
/* #undef HAVE_LIBNSL */

/* Define to 1 if you have the `resolv' library (-lresolv). */
/* #undef HAVE_LIBRESOLV */

/* Define to 1 if you have the `socket' library (-lsocket). */
/* #undef HAVE_LIBSOCKET */

/* Define to 1 if you have the `memcmp' function. */
#define HAVE_MEMCMP 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `msgget' function. */
#define HAVE_MSGGET 1

/* Define to 1 if you have the <sgtty.h> header file. */
/* #undef HAVE_SGTTY_H */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#define HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/un.h> header file. */
#define HAVE_SYS_UN_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME ""

/* Define to the full name and version of this package. */
#define PACKAGE_STRING ""

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME ""

/* Define to the version of this package. */
#define PACKAGE_VERSION ""

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */
#define CONST const

#if (defined(linux) && defined(HAVE_BSD_SGTTY)) || (!defined(linux) && defined(HAVE_SGTTY))
#define USE_LITOUT
#else
#define DONT_USE_LITOUT
#endif

/* From XEmacs config.h.in */
#ifndef BITS_PER_CHAR
#define BITS_PER_CHAR 8
#endif
#define LONGBITS (SIZEOF_LONG * BITS_PER_CHAR)

/* Define the return type of signal handlers if the s/xxx.h file
   did not already do so. */
#define RETSIGTYPE void

#ifndef XCDECL
#define XCDECL
#endif

/* SIGTYPE is the macro we actually use. */
#ifndef SIGTYPE
#define SIGTYPE RETSIGTYPE XCDECL
#define SIGRETURN return
#endif

