#define _GNU_SOURCE
#include <time.h>
#include <locale.h>
#include <xlocale.h>

locale_t c_locale = NULL;

void init_locale() {
    if (c_locale == NULL) {
        c_locale = newlocale(LC_ALL_MASK, NULL, NULL);
        uselocale(c_locale);
    }
}

time_t c_parse_unix_time(char *fmt, char *src) {
    struct tm dst;
    init_locale();
    strptime(src, fmt, &dst);
    return mktime(&dst);
}

time_t c_parse_unix_time_gmt(char *fmt, char *src) {
    struct tm dst;
    init_locale();
    strptime(src, fmt, &dst);
    return timegm(&dst);
}

void c_format_unix_time(char *fmt, time_t src, char* dst, int siz) {
    struct tm tim;
    init_locale();
    localtime_r(&src, &tim);
    strftime(dst, siz, fmt, &tim);
}

void c_format_unix_time_gmt(char *fmt, time_t src, char* dst, int siz) {
    struct tm tim;
    init_locale();
    gmtime_r(&src, &tim);
    strftime(dst, siz, fmt, &tim);
}
