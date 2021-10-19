AC_INIT([LoLA], 2.0, lola@service-technology.org, lola, http://service-technology.org/lola)

AC_MSG_NOTICE([ _          _        _    ])
AC_MSG_NOTICE([| |    ___ | |      / \   ])
AC_MSG_NOTICE([| |   / _ \| |     / _ \  ])
AC_MSG_NOTICE([| |__| (_) | |___ / ___ \ ])
AC_MSG_NOTICE([|_____\\___/|_____/_/   \_\\])
AC_MSG_NOTICE([a low-level analyzer])
AC_MSG_NOTICE([http://service-technology.org/lola])
AC_MSG_NOTICE([version ${PACKAGE_VERSION}])
AC_MSG_NOTICE()

AC_CONFIG_SRCDIR(src/Core/main.cc)
AC_CONFIG_HEADER(src/config.h)
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([gnits subdir-objects])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# do not use "-g -O2" as default flags (see http://stackoverflow.com/a/16672757/266378)
: ${CFLAGS=""}
: ${CXXFLAGS="-D NDEBUG"}

# we want to use POSIX extensions
AC_USE_SYSTEM_EXTENSIONS

AC_ARG_ENABLE(
  [debug],
  [AS_HELP_STRING(
    [--enable-debug],
    [switch off optimizations and enable assertions (warning: this makes LoLA runs 10-20 times slower)]
  )],
  [
    AC_MSG_CHECKING([whether to compile in debug mode])
    AC_MSG_RESULT([yes])
    CFLAGS+=" -g -ggdb -O0" CXXFLAGS+=" -g -ggdb -O0"
    CFLAGS +="-O0"
    CFLAGS -=" -O1"
    CFLAGS -=" -O2"
    CFLAGS -=" -O3"
    CXXFLAGS +=" -O0"
    CXXFLAGS -=" -O1"
    CXXFLAGS -=" -O2"
    CXXFLAGS -=" -O3"
    AX_CHECK_COMPILE_FLAG([-g -O0], [CFLAGS+=" -g -O0" CXXFLAGS+=" -g -O0"])
  ],
  [
    AC_MSG_CHECKING([whether to compile in debug mode])
    AC_MSG_RESULT([no])
    AC_DEFINE(NDEBUG)
    AC_DEFINE(
      [USE_PERFORMANCE],
      1,
      [whether LoLA was built with optimization (configured with `--disable-debug')]
    )
    CFLAGS+=" -O4" CXXFLAGS+=" -O4"
    AX_CHECK_COMPILE_FLAG([-Ofast], [CFLAGS+=" -Ofast" CXXFLAGS+=" -Ofast"])
  ]
)

# check for standard programs needed to compile
AC_PROG_AWK
AC_PROG_CXX
AM_PROG_CC_C_O
AC_PROG_LEX
AC_PROG_YACC
AC_PROG_RANLIB
AC_PROG_LN_S

# use the service-technology.org macros
AC_ST_BASIC
AC_ST_AUTOTEST
AC_ST_VERBOSE

# look for Kimwitu++
AM_MISSING_PROG(KC, kc++)

# check for POSIX threads
AX_PTHREAD()
LIBS="$PTHREAD_LIBS $LIBS"
CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
CXXFLAGS="$CXXFLAGS $PTHREAD_CFLAGS"
# CC="$PTHREAD_CC"

# Check host system to set compiler option for float precision  
AC_CANONICAL_HOST
case $host_os in
    *x86*linux*)
        # is set to avoid false precision
        CXXFLAGS="$CXXFLAGS -ffloat-store"
        ;;
    *darwin*)        
        # is set to avoid kimwitu++ related warnings on a mac os x
        CXXFLAGS="$CXXFLAGS -Wno-parentheses-equality"
        ;;
    *)
        ;;
esac

# search for optional tools needed by "make check"
AC_PATH_PROG(VALGRIND, [valgrind], not found)

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT8_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T

AC_CHECK_SIZEOF(void*)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(int64_t)

AC_LANG_PUSH(C++)
AC_CHECK_SIZEOF(arrayindex_t,, [#define _CONFIGURING
#include "$srcdir/src/Core/Dimensions.h"])
AC_CHECK_SIZEOF(mult_t,, [#define _CONFIGURING
#include "$srcdir/src/Core/Dimensions.h"])
AC_CHECK_SIZEOF(capacity_t,, [#define _CONFIGURING
#include "$srcdir/src/Core/Dimensions.h"])
AC_CHECK_SIZEOF(cardbit_t,, [#define _CONFIGURING
#include "$srcdir/src/Core/Dimensions.h"])
AC_CHECK_SIZEOF(hash_t,, [#define _CONFIGURING
#include "$srcdir/src/Core/Dimensions.h"])
AC_CHECK_SIZEOF(fairnessAssumption_t,, [#define _CONFIGURING
#include "$srcdir/src/Core/Dimensions.h"])
AC_CHECK_SIZEOF(vectordata_t,, [#define _CONFIGURING
#include "$srcdir/src/Core/Dimensions.h"])
AC_CHECK_SIZEOF(statenumber_t,, [#define _CONFIGURING
#include "$srcdir/src/Core/Dimensions.h"])
AC_CHECK_SIZEOF(sockaddr_in,, [#include <sys/types.h>
#ifdef WIN32
#include <winsock.h>
#include <windows.h>
#include <stdint.h>
typedef uint32_t socklen_t;
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#endif

#include <sys/types.h>
#include <cstddef>])

AC_CHECK_SIZEOF(bool)
AC_LANG_POP(C++)

# Checks for header files.
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([arpa/inet.h inttypes.h libintl.h malloc.h netinet/in.h stddef.h stdlib.h string.h sys/socket.h unistd.h limits.h netdb.h sys/time.h fcntl.h])

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_VPRINTF

AC_CHECK_FUNCS([asprintf atexit gethostbyname getline gettimeofday inet_ntoa kill memmove memset pow signal snprintf socket strchr strdup strerror strsignal strtol strtoul vasprintf vsnprintf])

# Collect required libraries if libraries are not present.
AC_CHECK_FUNC(gethostent, , AC_CHECK_LIB(nsl, gethostent))
AC_CHECK_FUNC(setsockopt, , AC_CHECK_LIB(socket, setsockopt))

# write files
AC_CONFIG_FILES([
	Makefile src/Makefile utils/Makefile doc/Doxyfile doc/Doxydetails doc/Makefile
  tests/Makefile tests/package.m4])
AC_CONFIG_FILES([tests/cover.sh], [chmod +x tests/cover.sh])
AC_OUTPUT

echo ""
echo "============================================================"
echo "  Successfully configured $PACKAGE_STRING."
echo "  -> compile LoLA with \`make'."
echo "============================================================"
echo ""
