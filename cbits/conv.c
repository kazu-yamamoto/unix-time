#include "config.h"

#if HAVE_STRPTIME_L && HAVE_STRFTIME_L
#define THREAD_SAFE 1
#define _GNU_SOURCE
#else
#define THREAD_SAFE 0
#if !defined(_WIN32)
#define _XOPEN_SOURCE
#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#endif
#endif

#include <limits.h>
#include <string.h>
#include <time.h>
#include <locale.h>
#include <stdlib.h>
#include <stdio.h>

#if defined(_WIN32)
#include "win_patch.h"
#endif // _WIN32

/*
 * c_init_locale() must be called once before the other functions.
 * The Haskell side guarantees this.
 */
#if THREAD_SAFE
#if HAVE_XLOCALE_H
#include <xlocale.h>
#endif

static locale_t c_locale = NULL;

void c_init_locale(void) {
    c_locale = newlocale(LC_TIME_MASK, "C", NULL);
}

#define STRPTIME(src, fmt, tm) strptime_l(src, fmt, tm, c_locale)
#define STRFTIME(dst, siz, fmt, tm) strftime_l(dst, siz, fmt, tm, c_locale)
#else
void c_init_locale(void) {
    setlocale(LC_TIME, "C");
}

#define STRPTIME(src, fmt, tm) strptime(src, fmt, tm)
#if defined(_WIN32)
#define STRFTIME(dst, siz, fmt, tm) _patch_strftime(dst, siz, fmt, tm)
#else
#define STRFTIME(dst, siz, fmt, tm) strftime(dst, siz, fmt, tm)
#endif // _WIN32
#endif

#if defined(_WIN32)
/* Unlike strptime(), this does not convert the result to local time. */
#define STRPTIME_UTC(src, fmt, tm) _patch_strptime_utc(src, fmt, tm)
#else
#define STRPTIME_UTC(src, fmt, tm) STRPTIME(src, fmt, tm)
#endif

#if !defined(HAVE_TIMEGM)
/* For systems without timegm(), e.g. Solaris. */
static time_t unix_time_timegm(struct tm *tm) {
    /* Normalize the month first, then count days from 1970-01-01. */
    long long y = (long long)tm->tm_year + 1900 + tm->tm_mon / 12;
    int m = tm->tm_mon % 12;
    long long era, days;
    unsigned yoe, doy, doe;
    if (m < 0) { m += 12; y--; }
    /* days_from_civil() by Howard Hinnant (March-based years). */
    if (m < 2) y--;
    era = (y >= 0 ? y : y - 399) / 400;
    yoe = (unsigned)(y - era * 400);
    doy = (153 * (m + (m < 2 ? 10 : -2)) + 2) / 5;
    doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    days = era * 146097 + (long long)doe - 719468 + tm->tm_mday - 1;
    return (time_t)(((days * 24 + tm->tm_hour) * 60 + tm->tm_min) * 60
                    + tm->tm_sec);
}
#define timegm unix_time_timegm
#endif /* HAVE_TIMEGM */

/*
 * Parsing.
 * None of the functions below touch the process-global TZ.
 */

#define NO_GMTOFF LONG_MIN

/* Returns 1 if the entire src matches fmt. */
static int parse(int utc, const char *fmt, const char *src, struct tm *tm) {
    char *end;
    memset(tm, 0, sizeof(struct tm));
    /* Let mktime() decide whether DST is in effect. */
    tm->tm_isdst = -1;
#if HAVE_STRUCT_TM_TM_GMTOFF
    tm->tm_gmtoff = NO_GMTOFF;
#endif
    end = utc ? STRPTIME_UTC(src, fmt, tm) : STRPTIME(src, fmt, tm);
    return end != NULL && *end == '\0';
}

/*
 * Seconds since the epoch of the parsed time. Fields without an
 * explicit zone are local time if utc is 0, or UTC otherwise.
 */
static time_t resolve(int utc, struct tm *tm) {
#if HAVE_STRUCT_TM_TM_GMTOFF && HAVE_STRUCT_TM_TM_ZONE
    /* BSD strptime() converts %s, %z and GMT to local time. */
    if (tm->tm_zone != NULL) return mktime(tm);
    /* glibc strptime() stores %z in tm_gmtoff only. */
    if (tm->tm_gmtoff != NO_GMTOFF) {
        long off = tm->tm_gmtoff;
        tm->tm_gmtoff = 0;
        return timegm(tm) - off;
    }
    tm->tm_gmtoff = 0;
#endif
    return utc ? timegm(tm) : mktime(tm);
}

time_t c_parse_unix_time(char *fmt, char *src, int *ok) {
    struct tm tm;
    *ok = parse(0, fmt, src, &tm);
    return resolve(0, &tm);
}

time_t c_parse_unix_time_gmt(char *fmt, char *src, int *ok) {
    struct tm tm;
    *ok = parse(1, fmt, src, &tm);
    return resolve(1, &tm);
}

/*
 * Formatting.
 */

size_t c_format_unix_time(char *fmt, time_t src, char *dst, int siz) {
    struct tm tim;
    if (localtime_r(&src, &tim) == NULL) return 0;
    return STRFTIME(dst, siz, fmt, &tim);
}

/*
 * strftime() may compute %s and %z from the local time zone
 * (e.g. macOS), so expand them here. So does %Z on Windows.
 */
static char *expand_gmt_format(const char *fmt, time_t src) {
    /* Each 2-byte directive expands to at most 21 bytes. */
    char *buf = malloc(strlen(fmt) * 11 + 1);
    char *p = buf;
    if (buf == NULL) return NULL;
    while (*fmt) {
        if (fmt[0] != '%' || fmt[1] == '\0') {
            *p++ = *fmt++;
            continue;
        }
        switch (fmt[1]) {
        case 's':
            p += sprintf(p, "%lld", (long long)src);
            break;
        case 'z':
            p += sprintf(p, "+0000");
            break;
#if defined(_WIN32)
        case 'Z':
            p += sprintf(p, "UTC");
            break;
#endif
        default:
            *p++ = fmt[0];
            *p++ = fmt[1];
            break;
        }
        fmt += 2;
    }
    *p = '\0';
    return buf;
}

size_t c_format_unix_time_gmt(char *fmt, time_t src, char *dst, int siz) {
    struct tm tim;
    char *gmt_fmt;
    size_t len;
    if (gmtime_r(&src, &tim) == NULL) return 0;
    gmt_fmt = expand_gmt_format(fmt, src);
    if (gmt_fmt == NULL) return 0;
    len = STRFTIME(dst, siz, gmt_fmt, &tim);
    free(gmt_fmt);
    return len;
}
