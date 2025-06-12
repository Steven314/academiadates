# This comes from:
#   https://github.com/tidyverts/tsibble/blob/main/R/yearmonth.R#L363
#
# A copy is included here to avoid a calling an un-exported function.
# There are slight modifications to include trimesters as a valid unit of time.

seq_date <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
    if (missing(from)) {
        stop("'from' must be specified")
    }
    if (!inherits(from, "Date")) {
        stop("'from' must be a \"Date\" object")
    }
    if (length(as.Date(from)) != 1L) {
        stop("'from' must be of length 1")
    }
    if (!missing(to)) {
        if (!inherits(to, "Date")) {
            stop("'to' must be a \"Date\" object")
        }
        if (length(as.Date(to)) != 1L) {
            stop("'to' must be of length 1")
        }
    }
    if (!is.null(along.with)) {
        # !missing(along.with) in seq.Date
        length.out <- length(along.with)
    } else if (!is.null(length.out)) {
        if (length(length.out) != 1L) {
            stop("'length.out' must be of length 1")
        }
        length.out <- ceiling(length.out)
    }
    status <- c(!missing(to), !missing(by), !is.null(length.out))
    if (sum(status) != 2L) {
        stop(
            "exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified"
        )
    }
    if (missing(by)) {
        from <- unclass(as.Date(from))
        to <- unclass(as.Date(to))
        res <- as.double(seq.int(from, to, length.out = length.out))
        return(new_date(res))
    }
    if (length(by) != 1L) {
        stop("'by' must be of length 1")
    }
    valid <- 0L
    if (inherits(by, "difftime")) {
        by <- switch(
            attr(by, "units"),
            secs = 1 / 86400,
            mins = 1 / 1440,
            hours = 1 / 24,
            days = 1,
            weeks = 7
        ) *
            unclass(by)
    } else if (is.character(by)) {
        by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
        if (length(by2) > 2L || length(by2) < 1L) {
            stop("invalid 'by' string")
        }
        valid <- pmatch(
            by2[length(by2)],
            c("days", "weeks", "months", "quarters", "trimesters", "years")
        )
        if (is.na(valid)) {
            stop("invalid string for 'by'")
        }
        if (valid <= 2L) {
            by <- c(1, 7)[valid]
            if (length(by2) == 2L) {
                by <- by * as.integer(by2[1L])
            }
        } else {
            by <- if (length(by2) == 2L) {
                as.integer(by2[1L])
            } else {
                1
            }
        }
    } else if (!is.numeric(by)) {
        stop("invalid mode for 'by'")
    }
    if (is.na(by)) {
        stop("'by' is NA")
    }
    if (valid <= 2L) {
        # days and weeks
        from <- unclass(as.Date(from))
        if (!is.null(length.out)) {
            res <- seq.int(from, by = by, length.out = length.out)
        } else {
            to0 <- unclass(as.Date(to))
            res <- seq.int(0, to0 - from, by) + from
        }
    } else {
        r1 <- as.POSIXlt(from)
        if (valid == 6L) {
            # years
            if (missing(to)) {
                yr <- seq.int(r1$year, by = by, length.out = length.out)
            } else {
                to0 <- as.POSIXlt(to)
                yr <- seq.int(r1$year, to0$year, by)
            }
            r1$year <- yr
            res <- as.Date(r1)
        } else {
            if (valid == 4L) {
                # quarters
                by <- by * 3
            }
            if (valid == 5L) {
                # trimesters
                by <- by * 4
            }
            # months
            if (missing(to)) {
                mon <- seq.int(r1$mon, by = by, length.out = length.out)
            } else {
                to0 <- as.POSIXlt(to)
                mon <- seq.int(
                    r1$mon,
                    12 * (to0$year - r1$year) + to0$mon,
                    by
                )
            }
            r1$mon <- mon
            res <- as.Date(r1)
        }
    }
    if (!missing(to)) {
        to <- as.Date(to)
        res <- if (by > 0) {
            res[res <= to]
        } else {
            res[res >= to]
        }
    }
    new_date(as.double(res))
}
