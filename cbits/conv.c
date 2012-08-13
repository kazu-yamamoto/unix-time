#include "config.h"

#if IS_LINUX
/* Linux cheats AC_CHECK_FUNCS(strptime_l), sigh. */
#define THREAD_SAFE 0
#define _XOPEN_SOURCE
#define _BSD_SOURCE
#elif HAVE_STRPTIME_L
#define THREAD_SAFE 1
#else
#define THREAD_SAFE 0
#endif

#include <time.h>
#include <locale.h>

#if THREAD_SAFE
#include <xlocale.h>

locale_t c_locale = NULL;

void init_locale() {
    if (c_locale == NULL) c_locale = newlocale(LC_TIME_MASK, NULL, NULL);
}
#else
void init_locale() {
    static int initialized = 0;
    if (initialized == 0) {
        setlocale(LC_TIME, "C");
	initialized == 1;
    }
}
#endif

time_t c_parse_unix_time(char *fmt, char *src) {
    struct tm dst;
    init_locale();
#if THREAD_SAFE
    strptime_l(src, fmt, &dst, c_locale);
#else
    strptime(src, fmt, &dst);
#endif
    return mktime(&dst);
}

time_t c_parse_unix_time_gmt(char *fmt, char *src) {
    struct tm dst;
    init_locale();
#if THREAD_SAFE
    strptime_l(src, fmt, &dst, c_locale);
#else
    strptime(src, fmt, &dst);
#endif
    return timegm(&dst);
}

void c_format_unix_time(char *fmt, time_t src, char* dst, int siz) {
    struct tm tim;
    init_locale();
    localtime_r(&src, &tim);
#if THREAD_SAFE
    strftime_l(dst, siz, fmt, &tim, c_locale);
#else
    strftime(dst, siz, fmt, &tim);
#endif
}

void c_format_unix_time_gmt(char *fmt, time_t src, char* dst, int siz) {
    struct tm tim;
    init_locale();
    gmtime_r(&src, &tim);
#if THREAD_SAFE
    strftime_l(dst, siz, fmt, &tim, c_locale);
#else
    strftime(dst, siz, fmt, &tim);
#endif
}
