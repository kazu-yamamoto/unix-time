AC_INIT(cbits/conv.c)

AC_DISABLE_OPTION_CHECKING

AC_CONFIG_HEADER(cbits/config.h)

AC_CHECK_HEADERS(xlocale.h)

AC_CHECK_FUNCS(strptime_l)
AC_CHECK_FUNCS(timegm)
AC_CHECK_FUNCS(_mkgmtime)
AC_CHECK_FUNCS(_get_current_locale)
AC_CHECK_FUNCS(strtol_l)
AC_CHECK_FUNCS(strtoll_l)

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
AC_OUTPUT
