AC_INIT([anankasm], [1])
AC_CHECK_PROG([CHECK_MP3GAIN], [mp3gain], [yes])
if test "$CHECK_MP3GAIN" != "yes"; then
   AC_MSG_ERROR([Please install mp3gain])
fi

AC_PATH_PROG([RACKET], [racket])
AC_PATH_PROG([MP3GAIN], [mp3gain])

AC_SUBST([RACKET])
AC_CONFIG_FILES([anankasm.mk dist.scm])
AC_OUTPUT
