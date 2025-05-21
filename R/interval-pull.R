#' @importFrom tsibble interval_pull gcd_interval new_interval
#' @method interval_pull yeartrimester
#' @export
interval_pull.yeartrimester <- function(x) {
    tri   <- as.double(x)
    ntris <- tsibble::gcd_interval(tri)
    tsibble::new_interval(.others = list("T" = ntris))
}

#' @importMethodsFrom lubridate as.period
methods::setMethod("as.period", "interval", function(x, ...) {
    period(
        year   = x$year,
        month  = (
            x$quarter   * 3 +
            x$trimester * 4 +
            x$month
        ),
        week   = x$week,
        day    = x$day,
        hour   = x$hour,
        minute = x$minute,
        second = (
            x$second +
            x$millisecond / 1e3 +
            x$microsecond / 1e6 +
            x$nanosecond  / 1e9
        )
    )
})

#' @importMethodsFrom lubridate as.duration
#' @importFrom methods setMethod
methods::setMethod("as.duration", "interval", function(x, ...) {
    as.duration(as.period(x))
})
