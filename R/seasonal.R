#' Convert a Seasonal Name to year-trimester
#'
#' It is common to refer to an academic term by its associated season: fall,
#' spring, or summer. This function is built to take out the step of converting
#' the term name to a date and then to a `yeartrimester` object.
#'
#' @param x A character vector of terms.
#' @param start_term The name of the term which starts the academic year. Must
#'   be `"Fall"`, `"Spring"`, or `"Summer"` (the default).
#'
#' @returns A `yeartrimester` object.
#' @export
#'
#' @examples
#' seasonal_trimester("Fall 2025")
#'
#' seasonal_trimester("Fall 2025", start_term = "Fall")
#'
seasonal_trimester <- function(
    x = character(),
    start_term = c("Summer", "Fall", "Spring")
) {
    stopifnot(is.character(x))

    start_term <- rlang::arg_match(start_term)

    # The start terms are associated the following months:
    # - Summer: 5
    # - Fall: 9
    # - Spring: 1

    parts <- regmatches(
        x,
        gregexpr(
            "[[:digit:]]+|Fall|Spring|Summer",
            x,
            ignore.case = TRUE
        )
    )

    yr_lgl <- purrr::map(parts, function(.x) grepl("[[:digit:]]{4}", .x))

    yr <- as.integer(purrr::map2_chr(parts, yr_lgl, function(.x, .y) .x[.y]))

    season <- purrr::map2_chr(parts, yr_lgl, function(.x, .y) tolower(.x[!.y]))

    season_numeric <- purrr::map_int(
        season,
        function(.x) {
            switch(
                .x,
                "fall" = 9,
                "spring" = 1,
                "summer" = 5
            )
        }
    )

    start_term_numeric <- switch(
        start_term,
        "Fall" = 9,
        "Spring" = 1,
        "Summer" = 5
    )

    yeartrimester(
        x = make_date(
            year = yr,
            month = season_numeric
        ),
        academic_start = start_term_numeric
    )
}
