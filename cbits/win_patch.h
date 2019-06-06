#ifndef UNIX_TIME_WIN_PATCH_H
#define UNIX_TIME_WIN_PATCH_H

#include "config.h"

#include <sys/cdefs.h>

#include <time.h>
#include <ctype.h>
#include <errno.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#if defined(_WIN32)
#include <Windows.h>
#endif

#if !defined(TM_YEAR_BASE)
#define TM_YEAR_BASE 1900
#endif

#if !defined(TM_SUNDAY)
#define TM_SUNDAY   0
#endif

#if !defined(TM_MONDAY)
#define TM_MONDAY   1
#endif

#if !defined(DAYSPERLYEAR)
#define DAYSPERLYEAR    366
#endif

#if !defined(SECSPERMIN)
#define SECSPERMIN  60
#endif

#if !defined(MINSPERHOUR)
#define MINSPERHOUR 60
#endif

#if !defined(HOURSPERDAY)
#define HOURSPERDAY 24
#endif

#if !defined(TM_YEAR_BASE)
#define TM_YEAR_BASE    1900
#endif

#if !defined(MONSPERYEAR)
#define MONSPERYEAR 12
#endif

#if !defined(DAYSPERWEEK)
#define DAYSPERWEEK 7
#endif

#if !defined(DAYSPERNYEAR)
#define DAYSPERNYEAR    365
#endif

#ifndef TYPE_BIT
#define TYPE_BIT(type)  (sizeof (type) * CHAR_BIT)
#endif /* !defined TYPE_BIT */

#ifndef TYPE_SIGNED
#define TYPE_SIGNED(type) (((type) -1) < 0)
#endif /* !defined TYPE_SIGNED */

#ifndef INT_STRLEN_MAXIMUM
/*
** 302 / 1000 is log10(2.0) rounded up.
** Subtract one for the sign bit if the type is signed;
** add one for integer division truncation;
** add one more for a minus sign if the type is signed.
*/
#define INT_STRLEN_MAXIMUM(type) \
    ((TYPE_BIT(type) - TYPE_SIGNED(type)) * 302 / 1000 + \
    1 + TYPE_SIGNED(type))
#endif /* !defined INT_STRLEN_MAXIMUM */

#if HAVE__ISSPACE_L
#define isspace_l _isspace_l
#else
#define isspace_l(ch, locale) isspace(ch)
#endif

#if HAVE__ISUPPER_L
#define isupper_l _isupper_l
#else
#define isupper_l(ch, locale) isupper(ch)
#endif

#if HAVE__ISDIGIT_L
#define isdigit_l _isdigit_l
#else
#define isdigit_l(ch, locale) isdigit(ch)
#endif

#if !HAVE_STRTOL_L
long strtol_l(const char *nptr, char **endptr, int base, _locale_t locale);
#endif

#if !HAVE_STRTOLL_L
long long strtoll_l(const char *nptr, char **endptr, int base, _locale_t locale);
#endif

#define isleap(y) (((y) % 4) == 0 && (((y) % 100) != 0 || ((y) % 400) == 0))

#ifndef isleap_sum
/*
** See tzfile.h for details on isleap_sum.
*/
#define isleap_sum(a, b)    isleap((a) % 400 + (b) % 400)
#endif /* !defined isleap_sum */

#if !HAVE__ISBLANK_L
int isblank_l( int c, _locale_t _loc);
#endif

int strncasecmp_l(const char *s1, const char *s2, int len, _locale_t _loc);

struct tm *gmtime_r(const time_t *_time_t, struct tm *_tm);

struct tm *localtime_r(const time_t *_time_t, struct tm *_tm);

#if HAVE__MKGMTIME
#define timegm _mkgmtime
#define HAVE_TIMEGM 1
#endif

#define fprintf_l(fp, loc, ...) fprintf(fp, ##__VA_ARGS__)
#define sprintf_l(buf, loc, ...) sprintf(buf, ##__VA_ARGS__)

struct lc_time_T {
    const char  *mon[12];
    const char  *month[12];
    const char  *wday[7];
    const char  *weekday[7];
    const char  *X_fmt;
    const char  *x_fmt;
    const char  *c_fmt;
    const char  *am;
    const char  *pm;
    const char  *date_fmt;
    const char  *alt_month[12];
    const char  *md_order;
    const char  *ampm_fmt;
};

extern const struct lc_time_T   _C_time_locale;

int _patch_setenv(const char *var, const char *val, int ovr);

int _patch_unsetenv(const char *name);

size_t _patch_strftime(char * __restrict s, size_t maxsize, const char * __restrict format,
    const struct tm * __restrict t);

size_t _patch_strftime_l(char * __restrict s, size_t maxsize, const char * __restrict format,
    const struct tm * __restrict t, _locale_t loc);

char *strptime_l(const char * __restrict buf, const char * __restrict fmt,
    struct tm * __restrict tm, _locale_t loc);

char *strptime(const char * __restrict buf, const char * __restrict fmt,
    struct tm * __restrict tm);

#endif // UNIX_TIME_WIN_PATCH_H
