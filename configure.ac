AC_INIT([anankasm], [1])

AC_DEFUN(AMOE_PATH_REQUIRED, [
  AC_PATH_PROG($1, $2)
  AS_IF([test -z "$$1"], [AC_MSG_ERROR([Please install $2])])
])

AMOE_PATH_REQUIRED([RACKET], [racket])
AMOE_PATH_REQUIRED([VORBISGAIN], [vorbisgain])
AMOE_PATH_REQUIRED([MP3GAIN], [mp3gain])


AC_SUBST([RACKET])
AC_CONFIG_FILES([anankasm.mk dist.scm])
AC_OUTPUT
