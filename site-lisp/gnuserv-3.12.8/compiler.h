/* Compiler-specific definitions for XEmacs.
   Copyright (C) 1998-1999, 2003 Free Software Foundation, Inc.
   Copyright (C) 1994 Richard Mlynarik.
   Copyright (C) 1995, 1996, 2000-2004 Ben Wing.

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

/* Synched up with: not in FSF. */

/* Authorship:

   NOT_REACHED, DOESNT_RETURN, PRINTF_ARGS by Richard Mlynarik, c. 1994.
   RETURN_SANS_WARNING by Martin buchholz, 1998 or 1999.
   Many changes and improvements by Jerry James, 2003.
     Split out of lisp.h, reorganized, and modernized.
     {BEGIN,END}_C_DECLS, NEED_GCC, GCC_VERSION
     ATTRIBUTE_MALLOC, ATTRIBUTE_CONST, ATTRIBUTE_PURE, UNUSED
*/

#ifndef INCLUDED_compiler_h
#define INCLUDED_compiler_h

/* Define min() and max(). (Some compilers put them in strange places that
   won't be referenced by include files used by XEmacs, such as `macros.h'
   under Solaris.) */

#ifndef min
# define min(a,b) (((a) <= (b)) ? (a) : (b))
#endif
#ifndef max
# define max(a,b) (((a) > (b)) ? (a) : (b))
#endif

/* Regular C complains about possible clobbering of local vars NOT declared
   as volatile if there's a longjmp() in a function.  C++ complains if such
   vars ARE volatile; or more correctly, sans volatile no problem even when
   you longjmp, avec volatile you get unfixable compile errors like

/src/xemacs/lilfix/src/process-unix.c: In function `void
   unix_send_process(Lisp_Object, lstream*)':
/src/xemacs/lilfix/src/process-unix.c:1577: no matching function for call to `
   Lisp_Object::Lisp_Object(volatile Lisp_Object&)'
/src/xemacs/lilfix/src/lisp-union.h:32: candidates are:
   Lisp_Object::Lisp_Object(const Lisp_Object&)
*/

#ifdef __cplusplus
# define VOLATILE_IF_NOT_CPP
#else
# define VOLATILE_IF_NOT_CPP volatile
#endif

/* Avoid indentation problems when XEmacs sees the curly braces */
#ifndef BEGIN_C_DECLS
# ifdef __cplusplus
#  define BEGIN_C_DECLS extern "C" {
#  define END_C_DECLS }
# else
#  define BEGIN_C_DECLS
#  define END_C_DECLS
# endif
#endif

/* Guard against older gccs that did not define all of these symbols */
#ifdef __GNUC__
# ifndef __GNUC_MINOR__
#  define __GNUC_MINOR__      0
# endif
# ifndef __GNUC_PATCHLEVEL__
#  define __GNUC_PATCHLEVEL__ 0
# endif
#endif /* __GNUC__ */

/* Simplify testing for specific GCC versions.  For non-GNU compilers,
   GCC_VERSION evaluates to zero. */
#ifndef NEED_GCC
# define NEED_GCC(major,minor,patch) (major * 1000000 + minor * 1000 + patch)
#endif /* NEED_GCC */
#ifndef GCC_VERSION
# ifdef __GNUC__
#  define GCC_VERSION NEED_GCC (__GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__)
# else
#  define GCC_VERSION 0
# endif /* __GNUC__ */
#endif /* GCC_VERSION */

#ifdef _MSC_VER
#define MSC_VERSION _MSC_VER
#else
#define MSC_VERSION 0
#endif

/* GCC < 2.6.0 could only declare one attribute per function.  In that case,
   we define DOESNT_RETURN in preference to PRINTF_ARGS, which is only used
   for checking args against the string spec. */
#ifndef PRINTF_ARGS
# if (GCC_VERSION >= NEED_GCC (2, 6, 0))
#  define PRINTF_ARGS(string_index,first_to_check) \
          __attribute__ ((format (printf, string_index, first_to_check)))
# else
#  define PRINTF_ARGS(string_index,first_to_check)
# endif /* GNUC */
#endif

#ifndef DOESNT_RETURN_TYPE
# if (GCC_VERSION > NEED_GCC (0, 0, 0))
#  if (GCC_VERSION >= NEED_GCC (2, 5, 0))
#   ifndef __INTEL_COMPILER
#    define RETURN_NOT_REACHED(value) DO_NOTHING
#   endif
#   define DOESNT_RETURN_TYPE(rettype) rettype
#   define DECLARE_DOESNT_RETURN_TYPE(rettype,decl) rettype decl \
	   __attribute__ ((noreturn))
#  else /* GCC_VERSION < NEED_GCC (2, 5, 0) */
#   define DOESNT_RETURN_TYPE(rettype) rettype volatile
#   define DECLARE_DOESNT_RETURN_TYPE(rettype,decl) rettype volatile decl
#  endif /* GCC_VERSION >= NEED_GCC (2, 5, 0) */
# elif (MSC_VERSION >= 1200)
/* MSVC 6.0 has a mechanism to declare functions which never return */
#  define DOESNT_RETURN_TYPE(rettype) __declspec(noreturn) rettype
#  define DECLARE_DOESNT_RETURN_TYPE(rettype,decl) \
  __declspec(noreturn) rettype XCDECL decl
#  if (MSC_VERSION >= 1300)
/* VC++ 7 issues warnings about return statements in __declspec(noreturn)
   functions; this problem didn't exist under VC++ 6 */
#   define RETURN_NOT_REACHED(value) DO_NOTHING
#  endif
# else /* not gcc, VC++ */
#  define DOESNT_RETURN_TYPE(rettype) rettype
#  define DECLARE_DOESNT_RETURN_TYPE(rettype,decl) rettype decl
# endif /* GCC_VERSION > NEED_GCC (0, 0, 0) */
#endif /* DOESNT_RETURN_TYPE */
#ifndef DOESNT_RETURN
# define DOESNT_RETURN DOESNT_RETURN_TYPE (void)
# define DECLARE_DOESNT_RETURN(decl) DECLARE_DOESNT_RETURN_TYPE (void, decl)
#endif /* DOESNT_RETURN */

/* Another try to fix SunPro C compiler warnings */
/* "end-of-loop code not reached" */
/* "statement not reached */
#if defined __SUNPRO_C || defined __USLC__
# define RETURN_SANS_WARNINGS if (1) return
# define RETURN_NOT_REACHED(value) DO_NOTHING
#endif

/* More ways to shut up compiler.  This works in Fcommand_loop_1(),
   where there's an infinite loop in a function returning a Lisp object.
*/
#if (defined (_MSC_VER) && MSC_VERSION < 1300) || defined (__SUNPRO_C) || \
  defined (__SUNPRO_CC) || (defined (DEC_ALPHA) && defined (OSF1))
# define DO_NOTHING_DISABLING_NO_RETURN_WARNINGS if (0) return Qnil
#else
# define DO_NOTHING_DISABLING_NO_RETURN_WARNINGS DO_NOTHING
#endif

#ifndef RETURN_NOT_REACHED
# define RETURN_NOT_REACHED(value) return (value)
#endif

#ifndef RETURN_SANS_WARNINGS
# define RETURN_SANS_WARNINGS return
#endif

#ifndef DO_NOTHING
# define DO_NOTHING do {} while (0)
#endif

#ifndef DECLARE_NOTHING
# define DECLARE_NOTHING struct nosuchstruct
#endif

#ifndef ATTRIBUTE_MALLOC
# if (GCC_VERSION >= NEED_GCC (2, 96, 0))
#  define ATTRIBUTE_MALLOC __attribute__ ((__malloc__))
# else
#  define ATTRIBUTE_MALLOC
# endif /* GCC_VERSION >= NEED_GCC (2, 96, 0) */
#endif /* ATTRIBUTE_MALLOC */

#ifndef ATTRIBUTE_PURE
# if (GCC_VERSION >= NEED_GCC (2, 96, 0))
#  define ATTRIBUTE_PURE __attribute__ ((pure))
# else
#  define ATTRIBUTE_PURE
# endif /* GCC_VERSION >= NEED_GCC (2, 96, 0) */
#endif /* ATTRIBUTE_PURE */

#ifndef ATTRIBUTE_CONST
# if (GCC_VERSION >= NEED_GCC (2, 5, 0))
#  define ATTRIBUTE_CONST __attribute__ ((const))
#  define CONST_FUNC
# else
#  define ATTRIBUTE_CONST
#  define CONST_FUNC const
# endif /* GCC_VERSION >= NEED_GCC (2, 5, 0) */
#endif /* ATTRIBUTE_CONST */

/* Unused declarations; g++ and icc do not support this. */
/*
   NOTE:  These macros MUST be named UNUSED (exactly) or something
   prefixed with USED_IF_, or DEFUN docstrings will be parsed incorrectly.
   See comments in make_docfile.c (write_c_args).  You'd think that this
   wouldn't happen, but unfortunately we do indeed have some arguments
   of DEFUNs unused for GNU compatibility or because features are missing.
*/
#ifndef UNUSED_ARG
# define UNUSED_ARG(decl) unused_##decl
#endif
#ifndef UNUSED
# if defined(__GNUC__) && !defined(__cplusplus) && !defined(__INTEL_COMPILER)
#  define ATTRIBUTE_UNUSED __attribute__ ((unused))
# else
#  define ATTRIBUTE_UNUSED
# endif
# define UNUSED(decl) UNUSED_ARG (decl) ATTRIBUTE_UNUSED
# ifdef MULE
#  define USED_IF_MULE(decl) decl
# else
#  define USED_IF_MULE(decl) UNUSED (decl)
# endif
# if defined (MULE) || defined (ERROR_CHECK_TEXT)
#  define USED_IF_MULE_OR_CHECK_TEXT(decl) decl
# else
#  define USED_IF_MULE_OR_CHECK_TEXT(decl) UNUSED (decl)
# endif
# ifdef USE_XFT
#  define USED_IF_XFT(decl) decl
# else
#  define USED_IF_XFT(decl) UNUSED (decl)
# endif
#endif /* UNUSED */

#ifdef DEBUG_XEMACS
# define REGISTER
# define register
#else
# define REGISTER register
#endif

#if defined(HAVE_MS_WINDOWS) && defined(HAVE_SHLIB)
# ifdef EMACS_MODULE
#  define MODULE_API __declspec(dllimport)
# else
#  define MODULE_API __declspec(dllexport)
# endif
#else
# define MODULE_API
#endif

/* Under "strict-aliasing" assumptions, you're not necessarily allowed to
   access the same memory address as two different types.  The proper way
   around that is with a union.  The macros below help out, e.g. the
   definition of XE_MAKEPOINTS(val) is

   ANSI_ALIASING_TYPEDEF (POINTS, POINTS);
   #define XE_MAKEPOINTS(l)       ANSI_ALIASING_CAST (POINTS, l)

   replacing

   BAD!!! #define XE_MAKEPOINTS(l)       (* (POINTS *) &(l))

   On the other hand, if you are just casting from one pointer to the other
   in order to pass a pointer to another function, it's probably OK to just
   trick GCC by inserting an intermediate cast to (void *), to avoid
   warnings about "dereferencing type-punned pointer".  #### I don't know
   how kosher this is, but do strict-aliasing rules really apply across
   functions?

   Note that the input to e.g. VOIDP_CAST must be an lvalue (i.e. not
   &(something)), but the value of the macro is also an lvalue, so in place
   of `(void **) &foo' you could write `& VOIDP_CAST (foo)' if you are
   subsequently dereferencing the value or don't feel comfortable doing a
   trick like `(void **) (void *) &foo'.

   Unfortunately, it does not work to just define the union type on the fly in
   the cast -- otherwise, we could avoid the need for a typedef.  Or rather,
   it does work under gcc but not under Visual C++.

   --ben
 */

#define ANSI_ALIASING_TYPEDEF(name, type) typedef union { char c; type p; } *ANSI_ALIASING_##name
#define ANSI_ALIASING_CAST(name, val) (((ANSI_ALIASING_##name) &(val))->p)
ANSI_ALIASING_TYPEDEF (voidp, void *);
/* VOIDP_CAST: Cast an lvalue to (void *) in a way that is ANSI-aliasing
   safe and will not result in GCC warnings.  The result is still an
   lvalue, so you can assign to it or take its address. */
#define VOIDP_CAST(l)  ANSI_ALIASING_CAST (voidp, l)

#endif /* INCLUDED_compiler_h */
