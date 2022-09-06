AC_INIT(cbits/conv.c)

AC_DISABLE_OPTION_CHECKING

AC_CONFIG_HEADER(cbits/config.h)

AC_CHECK_HEADERS(xlocale.h)

AC_CHECK_FUNCS(strptime_l)
AC_CHECK_FUNCS(timegm)
AC_CHECK_DECLS([_mkgmtime], [], [], [[#include <time.h>]])
AC_CHECK_FUNCS(_get_current_locale)
AC_CHECK_FUNCS(_create_locale)
AC_CHECK_FUNCS(strtol_l)
AC_CHECK_FUNCS(strtoll_l)
AC_CHECK_FUNCS(_isspace_l)
AC_CHECK_FUNCS(_isupper_l)
AC_CHECK_FUNCS(_isdigit_l)
AC_CHECK_FUNCS(_isblank_l)

host=`uname -a`
case $host in
  Linux*)
	LINUX=1
	;;
  *)
	LINUX=0
	;;
esac

AC_DEFINE_UNQUOTED(IS_LINUX,$LINUX,"Is Linux")

case $host in
  MINGW??_NT-6.1*)
	NT61=1
	;;
  *)
	NT61=0
	;;
esac

AC_DEFINE_UNQUOTED(IS_NT61,$NT61,"Is Windows NT 6.1")

AC_OUTPUT
