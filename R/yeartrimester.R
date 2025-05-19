#' Represent year-trimester
#'
#' @param x A date.
#' @param academic_start A numeric indicating the starting month of the academic
#'   year.
#'
#' @description Create or coerce using `yeartrimester()`.
#'
#' @section Display:
#' Use [`format()`] to display `yeartrimester` objects in desired format.
#'
#' @return A year-trimester (`yeartrimester`) object.
#'
#' @rdname year-trimester
#'
#' @export
yeartrimester <- function(x, academic_start = 1) {
    UseMethod("yeartrimester")
}


#' @rdname year-trimester
#' @param year,trimester A vector of numerics which give year and trimester
#'   values.
#' @export
make_yeartrimester <- function(year = 1970L, trimester = 1L, academic_start = 1) {
    lst <- vctrs::vec_recycle_common(year = year, trimester = trimester)

    if (any(lst$trimester > 3 | lst$trimster < 1)) {
        abort("Trimesters must be an integer within the interval [1, 3].")
    }

    yeartrimester(3 * (lst$year - 1970) + lst$trimester - 1, academic_start)
}


#' @export
yeartrimester.default <- function(x, academic_start = 1) {
    abort(
        sprintf(
            "`yeartrimester` does not know how to handle the %s class yet.",
            class(x)
        )
    )
}

#' @export
yeartrimester.NULL <- function(x, academic_start = 1) {
    new_yeartrimester(double(), academic_start = 1)
}

#' @export
yeartrimester.logical <- function(x, ...) {
    if (is.logical(x) && all(is.na(x))) {
        new_yeartrimester(0) + NA_real_
    } else {
        abort("`yeartrimester` does not know how to handle logicals fully.")
    }
}

#' @export
yeartrimester.POSIXct <- function(x, academic_start = 1) {
    yr <- lubridate::year(x)
    mth <- academic_start + (lubridate::month(x) - academic_start) %/% 4 * 4
    mth0 <- mth == 0
    mth1 <- mth == -1
    mth[mth0] <- 12
    mth[mth1] <- 11
    lgl <- mth0 | mth1
    vctrs::vec_slice(yr, lgl) <- vctrs::vec_slice(yr, lgl) - 1
    new_yeartrimester(lubridate::make_date(yr, mth), academic_start)
}

#' @export
yeartrimester.POSIXlt <- yeartrimester.POSIXct

#' @export
yeartrimester.Date <- yeartrimester.POSIXct

#' @export
yeartrimester.character <- function(x, academic_start = 1) {
    # It would be cool if this understood things like Fall, Spring, and Summer.

    # extract the characters from the rest of the value.
    key_words <- regmatches(x, gregexpr("[[:alpha:]]+", x))

    # check that all of the text is what we are looking for.
    if (all(grepl("^(t|tri|trimester)$", key_words, ignore.case = TRUE))) {
        # take the numeric parts
        yr_tri <- regmatches(x, gregexpr("[[:digit:]]+", x))

        # ensure they are the complete and valid.
        digits_lgl <- purrr::map_lgl(yr_tri, function(.x) !rlang::has_length(.x, 2))
        digits_len <- purrr::map_int(yr_tri, function(.x) sum(nchar(.x)))
        if (any(digits_lgl) || any(digits_len != 5)) {
            abort("Character strings are not in a standard unambiguous format.")
        }

        # determine which part is the year.
        yr_lgl <- purrr::map(yr_tri, function(.x) grepl("[[:digit:]]{4}", .x))

        # extract the year part as an integer.
        yr <- as.integer(purrr::map2_chr(yr_tri, yr_lgl, function(.x, .y) .x[.y]))

        # extract the trimester part as an integer as well.
        tri <- as.integer(purrr::map2_chr(yr_tri, yr_lgl, function(.x, .y) .x[!.y]))

        # check that the trimester is valid.
        if (any(tri > 3 | tri < 1)) {
            abort("Trimesters can't be less than 1 or greater than 3.")
        }

        # compose the output
        yeartrimester(3 * (yr - 1970) + tri - 1, academic_start)
    } else {
        # if there was a weird value, try to mush them into date objects and try
        # again.
        assertDate(x)
        yeartrimester(anydate(x), academic_start)
    }
}

#' @export
yeartrimester.yearweek <- yeartrimester.POSIXct

#' @export
yeartrimester.yearmonth <- yeartrimester.POSIXct

#' @export
yeartrimester.yeartrimester <- function(x, academic_start = attr(x, "academic_start")) {
    # This doesn't seem right to me, but that's have the `tsibble` equivalents
    # work.

    as <- academic_start(x)
    mth <- academic_start - as
    new_yeartrimester(
        new_date(x) + lubridate::period(
            year  = -(as == 1) + (academic_start == 1),
            month = mth
        ),
        academic_start
    )
}

#' @export
yeartrimester.numeric <- function(x, academic_start = 1) {
    # define the base of the index
    date0 <- lubridate::make_date(
        year  = 1969 + as.integer(academic_start == 1),
        month = academic_start
    )

    new_yeartrimester(
        x              = date0 + lubridate::period(month = x * 4),
        academic_start = academic_start
    )
}

new_yeartrimester <- function(x = double(), academic_start = 1) {
    if (!rlang::has_length(academic_start, 1)) {
        abort("`academic_start` must be of length 1.")
    }
    if (academic_start < 1 || academic_start > 12) {
        abort("`academic_start` only accepts a value between 1 and 12.")
    }
    vctrs::new_vctr(x, academic_start = academic_start, class = "yeartrimester")
}

academic_start <- function(x) {
    attr(x, "academic_start") %||% 1
}

#' @rdname year-trimester
#' @export
is_yeartrimester <- function(x) {
    inherits(x, "yeartrimester")
}

#' @export
is.numeric.yeartrimester <- function(x) {
    FALSE
}

#' @export
tz.yeartrimester <- function(x) {
    "UTC"
}

# vec_cast ----

#' @method vec_cast yeartrimester
#' @export
vec_cast.yeartrimester <- function(x, to, ...) {
    UseMethod("vec_cast.yeartrimester")
}

#' @export
vec_cast.Date.yeartrimester <- function(x, to, ...) {
    new_date(x)
}

#' @export
vec_cast.POSIXct.yeartrimester <- function(x, to, ...) {
    as.POSIXct(new_date(x), ...)
}

#' @export
vec_cast.double.yeartrimester <- function(x, to, ...) {
    base <- yeartrimester(0, academic_start(x))
    4 *
        (year(x) - year(base)) +
        as.numeric(trimester(x,    type = "{trimester}")) -
        as.numeric(trimester(base, type = "{trimester}"))
}

#' @export
vec_cast.POSIXlt.yeartrimester <- function(x, to, ...) {
    as.POSIXlt(new_date(x), ...)
}

#' @export
vec_cast.yeartrimester.yeartrimester <- function(x, to, ...) {
    yeartrimester(x, academic_start(to))
}

#' @export
vec_cast.character.yeartrimester <- function(x, to, ...) {
    format(x)
}

# vec_ptype2 ----

#' @method vec_ptype2 yeartrimester
#' @export
vec_ptype2.yeartrimester <- function(x, y, ...) {
    UseMethod("vec_ptype2.yeartrimester", y)
}

#' @export
vec_ptype2.yeartrimester.POSIXct <- function(x, y, ...) {
    new_datetime()
}

#' @export
vec_ptype2.POSIXct.yeartrimester <- function(x, y, ...) {
    new_datetime()
}

#' @export
vec_ptype2.yeartrimester.Date <- function(x, y, ...) {
    new_date()
}

#' @export
vec_ptype2.yeartrimester.yeartrimester <- function(x, y, ...) {
    if (academic_start(x) != academic_start(y)) {
        abort("Can't combine <yeartrimester> with different `academic_start`.")
    }
    new_yeartrimester(academic_start = academic_start(x))
}

#' @export
vec_ptype2.Date.yeartrimester <- function(x, y, ...) {
    new_date()
}

# vec_arith ----

#' @rdname academiadates-vctrs
#' @method vec_arith yeartrimester
#' @export
vec_arith.yeartrimester <- function(op, x, y, ...) {
    UseMethod("vec_arith.yeartrimester", y)
}

#' @method vec_arith.yeartrimester default
#' @export
vec_arith.yeartrimester.default <- function(op, x, y, ...) {
    vctrs::stop_incompatible_op(op, x, y)
}

#' @method vec_arith.yeartrimester numeric
#' @export
vec_arith.yeartrimester.numeric <- function(op, x, y, ...) {
    if (op == "+") {
        new_yeartrimester(as_date(x) + period(months = y * 4), academic_start(x))
    } else if (op == "-") {
        new_yeartrimester(as_date(x) - period(months = y * 4), academic_start(x))
    } else {
        vctrs::stop_incompatible_op(op, x, y)
    }
}

#' @method vec_arith.yeartrimester yeartrimester
#' @export
vec_arith.yeartrimester.yeartrimester <- function(op, x, y, ...) {
    if (op == "-") {
        as.double(x) - as.double(y)
    } else {
        vctrs::stop_incompatible_op(op, x, y)
    }
}

#' @method vec_arith.numeric yeartrimester
#' @export
vec_arith.numeric.yeartrimester <- function(op, x, y, ...) {
    if (op == "+") {
        yeartrimester(
            period(months = x * 4) + as_date(y),
            academic_start(y)
        )
    } else {
        vctrs::stop_incompatible_op(op, x, y)
    }
}

#' @method vec_arith.yeartrimester MISSING
#' @export
vec_arith.yeartrimester.MISSING <- function(op, x, y, ...) {
    switch(
        op,
        `-` = x,
        `+` = x,
        vctrs::stop_incompatible_op(op, x, y)
    )
}

# Formatting and Labels ----

#' @export
format.yeartrimester <- function(x, format = "%Y T%q", ...) {
    as <- academic_start(x)

    yrtri <- x |>
        trimester(
            type = "{ay_year}.{trimester}",
            academic_start = as
        ) |>
        as.numeric()

    yr  <- trunc(yrtri)
    tri <- round(yrtri %% 1 * 10)

    tri_sub <- purrr::map_chr(formatC(tri), function(z) gsub("%q", z, x = format))
    tri_sub[is.na(tri_sub)] <- "-" # NA formats cause errors
    format.Date(make_date(yr, tri * 4 - 2), format = tri_sub)
}

#' @rdname academiadates-vctrs
#' @export
obj_print_data.yeartrimester <- function(x, ...) {
    if (length(x) == 0) return()
    print(format(x))
}

#' @export
obj_print_footer.yeartrimester <- function(x, ...) {
    cli::cat_line("# Academic year starts in ", month.name[academic_start(x)], ".")
}

fmt_month <- function(x) {
    month.name[x]
}

#' @export
vec_ptype_abbr.yeartrimester <- function(x, ...) {
    "tri"
}

# Sequences ----

#' @export
seq.yeartrimester <- function(
    from,
    to,
    by, # must be numeric
    length.out = NULL,
    along.with = NULL,
    ...
) {
    as <- academic_start(from)
    from <- vec_cast(from, new_date())

    if (!is_missing(to)) {
        to <- vec_cast(to, new_date())
    }

    if (is_missing(by)) {
        new_yeartrimester(
            seq_date(
                from = from,
                to   = to,
                length.out = length.out,
                along.with = along.with,
                ...
            ),
            as
        )
    } else {
        # See `bad_by(by)` in tsibble/R/yearmonth.R (line 356 to 360).
        if (!rlang::is_bare_numeric(by, n = 1)) {
            abort("`by` only takes a numeric.")
        }
        by_tri <- paste(by * 4, "month")
        new_yeartrimester(
            seq_date(
                from = from,
                to   = to,
                by   = by_tri,
                length.out = length.out,
                along.with = along.with,
                ...
            ),
            as
        )
    }
}

#' @export
seq.ordered <- function(from, to, by, ...) {
    if (!rlang::is_bare_numeric(by, n = 1)) {
        abort("`by` only takes a numeric.")
    }

    lvls     <- levels(from)
    idx_from <- which(lvls %in% from)
    idx_to   <- which(lvls %in% to)
    idx      <- seq.int(idx_from, idx_to, by = by)
    ordered(lvls[idx], levels = lvls)
}

#' @rdname year-trimester
#' @export
#' @examples
#'
#' # `academic_year()` helps to extract academic year
#' y <- yeartrimester(as.Date("2020-06-01"), academic_start = 6)
#' academic_year(y)
#' lubridate::year(y) # calendar years
academic_year <- function(x) {
    stopifnot(is_yeartrimester(x))
    trunc(as.numeric(trimester(
        x,
        type = '{ay_year}.{trimester}',
        academic_start = academic_start(x)
    )))
}

#' @importFrom generics union
#' @export
generics::union

#' @importFrom generics intersect
#' @export
generics::intersect

#' @importFrom generics setdiff
#' @export
generics::setdiff

set_ops <- function(class = "yeartrimester", op = "intersect") {
    # see tsibble/R/yearmonth.R (lines 332-345)
    force(class)
    force(op)
    fun <- switch(
        op,
        "union"     = function(x, y, ...) vec_unique(vec_c(x, y)),
        "intersect" = function(x, y, ...) vec_slice(x, vec_in(x, y)),
        "setdiff"   = function(x, y, ...) {
            vec_unique(
                if (length(x) || length(y))
                    x[is.na(vec_match(x, y))]
                else x
            )
        }
    )
    function(x, y, ...) {
        # see tsibble/R/error.R (lines 1-6)
        if (!inherits(y, class)) {
            abort(
                sprintf(
                    "`%s` must be of class \"%s\".", deparse(substitute(y)),
                    class
                )
            )
        }
        fun(x, y, ...)
    }
}

#' @export
union.yeartrimester <- set_ops("yeartrimester", op = "union")

#' @export
intersect.yeartrimester <- set_ops("yeartrimester", op = "intersect")

#' @export
setdiff.yeartrimester <- set_ops("yeartrimester", op = "setdiff")
