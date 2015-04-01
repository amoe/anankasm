AC_INIT([anankasm], [1])

AC_DEFUN(AMOE_PATH_REQUIRED, [
  AC_PATH_PROG($1, $2)
  AS_IF([test -z "$$1"], [AC_MSG_ERROR([Please install $2])])
])



AMOE_PATH_REQUIRED([RACKET], [racket])
AMOE_PATH_REQUIRED([REPLAYGAIN], [replaygain])
#AMOE_PATH_REQUIRED([VORBISGAIN], [vorbisgain])
#AMOE_PATH_REQUIRED([MP3GAIN], [mp3gain])
AMOE_PATH_REQUIRED([EYED3], [eyeD3])

AC_CHECK_LIB([tag_c], [taglib_tag_free_strings], [],
  [AC_MSG_ERROR([The TagLib C binding is required to use anankasm])])

AC_SUBST([RACKET])
AC_CONFIG_FILES([anankasm.mk dist.scm])
AC_OUTPUT
