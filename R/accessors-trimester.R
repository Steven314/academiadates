#' Get the academic trimester of a date-time
#'
#' Trimesters divide the year into thirds. This is largely based on
#' [`lubridate::quarter`].
#'
#' The type argument has a number of possibilities.
#' - `ay_year trimester`: The default. Academic year followed by a "T" and the trimester number.
#' - `trimester`: A **numeric** value of the trimester.
#' - `ay_year.trimester`: Academic year with trimester in **numeric form**.
#' - `year_start/end`: The calendar years which the academic year spans.
#' - `date_last`: Date on which the term ends, rounded down to the month.
#' - `date_first`: Date on which the term begins, rounded down to the month.
#' - `season`: The season of the term. This is a **factor**.
#' - `season cal_year`: The season with the calendar year.
#'
#' @param x A date-time object.
#' @param type The format to be returned for the trimester. See details. They
#'   are similar to the options for [`lubridate::quarter`].
#' @param academic_start A integer from 1 to 12 indicating the month for the
#'   start of the academic year.
#'
#' @returns A numeric, character, or factor depending on `type`.
#' @importFrom lubridate today as_date month year add_with_rollback make_date
#' @export
#'
#' @examples
#' trimester(lubridate::as_date("2024-08-24"), academic_start = 5)
#'
#' trimester(lubridate::today(), academic_start = 5)
#'
#' trimester(lubridate::today(), type = "trimester", academic_start = 5)
trimester <- function(x, type = "ay_year trimester", academic_start = 1) {
    if (length(academic_start) > 1) {
        stop("`academic_start` must have a length of one.")
    }
    if (academic_start < 1 | academic_start > 12) {
        stop("`academic_start` must be between 1 and 12.")
    }

    # month of the year right before the academic year starts
    as <- (academic_start - 1) %% 12

    # a 12-month sequence offset to the academic start month
    shifted <- seq(as, 11 + as) %% 12 + 1

    # get the month from the given date
    month <- lubridate::month(x)

    # sequence for the trimester of each month
    trimesters <- rep(1:3, each = 4)

    # find the trimester of the date
    s <- match(month, shifted)
    trimester <- trimesters[s]

    nxt_year_months <- if (as != 0) (as + 1):12
    ay_year <- lubridate::year(x) + (month %in% nxt_year_months)

    cal_year <- lubridate::year(x)

    starting_months <- shifted[seq(1, length(shifted), 4)]

    final_years <- lubridate::year(x) - (starting_months[trimester] > month)

    trimester_starting_dates <- lubridate::make_date(
        year = final_years,
        month = starting_months[trimester],
        day = 1L
    )

    date_first <- trimester_starting_dates
    date_last <- lubridate::add_with_rollback(
        trimester_starting_dates,
        months(4)
    ) -
        lubridate::days(1)

    # This might come out weird at times, so some experimentation is probably
    # needed.
    season <- sapply(
        (starting_months[trimester] %/% 4) + 1,
        \(x) {
            switch(
                x,
                "1" = "Spring",
                "2" = "Summer",
                "3" = "Fall"
            )
        },
        USE.NAMES = FALSE
    )

    # Assigning seasons, which have four per year, doesn't make sense for all
    # `academic_start` values. When it does make sense, it's useful. Perhaps a
    # dynamic system with Winter as well, or letting the user supply their own
    # break points would be better in the long term.

    # 1, 5, 9 make the most sense to me. Other combinations get a little fuzzy
    # between what should be called which season.

    # This probably is misaligned with the `season` above, so it comes out weird
    # at times.
    if (starting_months[1] < 4) {
        season_order <- c("Spring", "Summer", "Fall")
    } else if (starting_months[1] < 8) {
        season_order <- c("Summer", "Fall", "Spring")
    } else {
        season_order <- c("Fall", "Spring", "Summer")
    }

    switch(
        type,
        "trimester" = trimester,
        "season" = ordered(season, levels = season_order),
        "season cal_year" = sprintf(
            "%s %s",
            season,
            cal_year
        ),
        "ay_year trimester" = sprintf(
            "%d T%d",
            ay_year,
            trimester
        ),
        "year_start/end" = sprintf(
            "%d/%d T%d",
            final_years - 1,
            final_years %% 100,
            trimester
        ),
        "ay_year.trimester" = ay_year + (trimester / 10),
        "date_first" = date_first,
        "date_last" = date_last,
        stop("Unsuported type ", type)
    )
}
