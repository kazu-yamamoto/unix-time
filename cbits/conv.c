/* from snap-core */

#include <time.h>
#include <locale.h>

time_t c_parse_unix_time(char *fmt, char *src) {
    struct tm dst;
    setlocale(LC_TIME, "C");
    strptime(src, fmt, &dst);
    return timegm(&dst);
}

void c_format_unix_time(char *fmt, time_t src, char* dst, int siz) {
    struct tm tim;
    setlocale(LC_TIME, "C");
    gmtime_r(&src, &tim);
    strftime(dst, siz, fmt, &tim);
}
