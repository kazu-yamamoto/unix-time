AC_INIT(cbits/conv.c)

AC_DISABLE_OPTION_CHECKING

AC_CONFIG_HEADER(cbits/config.h)

AC_CHECK_FUNCS(strptime_l)

host=`uname -a`
case $host in
  Linux*)
	LINUX=1
	;;
  *)
	LINUX=0
	;;
esac

AC_DEFINE_UNQUOTED(IS_LINUX,$LINUX)
AC_OUTPUT


