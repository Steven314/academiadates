
#' academiadates scales for ggplot2
#'
#' Defines ggplot2 scales for the [`yeartrimester`] index.
#'
#' @param ... Arguments passed to [`ggplot2::scale_x_date()`].
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @name academiadates-scales
NULL

pkg_not_available <- function(pkg, min_version = NULL) {
    pkg_lgl <- requireNamespace(pkg, quietly = TRUE)
    if (!pkg_lgl) {
        abort(
            sprintf(
                "Package `%s` required.\nPlease install and try again.",
                pkg
            )
        )
    } else if (pkg_lgl && is_null(min_version)) {
        return()
    } else if (utils::packageVersion(pkg) < min_version) {
        abort(
            sprintf(
                "Package `%s` (>= v%s) required.\nPlease install and try again.",
                pkg,
                min_version
            )
        )
    }
}

scale_fun_pkg_check <- function() {
    pkg_not_available("ggplot2", "3.3.0")
    pkg_not_available("scales",  "1.1.0")
}

scale_type.yeartrimester <- function(x) c("yeartrimester", "date", "continuous")

yeartrimester_trans <- function() {
    scales::trans_new(
        "yeartrimester",
        transform = function(x) {
            scales::date_trans()$transform(
                as_date(x) + months(academic_start(x) - 1)
            )
        },
        inverse = function(x) {
            yeartrimester(scales::date_trans()$inverse(x))
        },
        breaks = yeartrimester_breaks()
    )
}

yeartrimester_get_breaks <- function(self, limits = self$get_limits()) {
    breaks <- ggplot2::ggproto_parent(
        ggplot2::ScaleContinuous,
        self
    )$get_breaks(limits)
    # (non-)redundant censoring because of non-invertibility of transforms
    scales::censor(breaks, limits, only.finite = FALSE)
}

yeartrimester_breaks <- function(n = 5) {
    force(n)
    function(x) {
        yeartrimester(scales::breaks_pretty(n)(as_date(x)))
    }
}

fullseq.yeartrimester <- function(range, size, ...) {
    scales::fullseq(as_date(range), size = size, ...)
}

#' @rdname academiadates-scales
#' @export
scale_x_yeartrimester <- function(...) {
    scale_fun_pkg_check()
    ggplot2::ggproto(
        "ScaleContinuousYeartrimester",
        ggplot2::scale_x_date(...),
        trans      = yeartrimester_trans(),
        get_breaks = yeartrimester_get_breaks
    )
}

#' @rdname academiadates-scales
#' @export
scale_y_yeartrimester <- function(...) {
    scale_fun_pkg_check()
    ggplot2::ggproto(
        "ScaleContinuousYeartrimester",
        ggplot2::scale_y_date(...),
        trans      = yeartrimester_trans(),
        get_breaks = yeartrimester_get_breaks
    )
}

#' @rdname academiadates-scales
#' @export
scale_colour_yeartrimester <- function(...) {
    scale_fun_pkg_check()
    ggplot2::ggproto(
        "ScaleContinuousYeartrimester",
        ggplot2::scale_colour_date(...),
        trans      = yeartrimester_trans(),
        get_breaks = yeartrimester_get_breaks
    )
}

#' @rdname academiadates-scales
#' @export
scale_color_yeartrimester <- scale_colour_yeartrimester

#' @rdname academiadates-scales
#' @export
scale_alpha_yeartrimester <- function(...) {
    scale_fun_pkg_check()
    ggplot2::ggproto(
        "ScaleContinuousYeartrimester",
        ggplot2::scale_alpha_date(...),
        trans      = yeartrimester_trans(),
        get_breaks = yeartrimester_get_breaks
    )
}

#' @rdname academiadates-scales
#' @export
scale_fill_yeartrimester <- function(...) {
    scale_fun_pkg_check()
    ggplot2::ggproto(
        "ScaleContinuousYeartrimester",
        ggplot2::scale_fill_date(...),
        trans      = yeartrimester_trans(),
        get_breaks = yeartrimester_get_breaks
    )
}

#' @rdname academiadates-scales
#' @export
scale_size_yeartrimester <- function(...) {
    scale_fun_pkg_check()
    ggplot2::ggproto(
        "ScaleContinuousYeartrimester",
        ggplot2::scale_size_date(...),
        trans      = yeartrimester_trans(),
        get_breaks = yeartrimester_get_breaks
    )
}
