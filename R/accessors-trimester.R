#' Get the academic trimester of a date-time
#'
#' Trimesters divide the year into thirds. This is largely based on
#' [`lubridate::quarter`].
#'
#' @param x A date-time object.
#' @param type A [`glue::glue`] specification containing any of: `ay_year`,
#'   `cal_year`, `trimester`, `date_last`, `date_first`. The default behavior is
#'   to return the academic year with the trimester number following a 'T'.
#' @param academic_start A integer from 1 to 12 indicating the month for the
#'   start of the academic year.
#'
#' @returns A character string.
#' @importFrom glue glue
#' @importFrom lubridate today as_date month year add_with_rollback make_date
#' @export
#'
#' @examples
#' trimester(lubridate::as_date("2024-08-24"), academic_start = 5)
#'
#' trimester(lubridate::today(), academic_start = 5)
#'
#' # Notice that this returns a string, not an integer.
#' trimester(lubridate::today(), type = "{trimester}", academic_start = 5)
trimester <- function(x, type = "{ay_year} T{trimester}", academic_start = 1) {
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
    date_last  <- lubridate::add_with_rollback(
        trimester_starting_dates,
        months(4)
    ) - lubridate::days(1)

    return(
        sapply(
            X = glue::glue(type),
            FUN = \(.x) if (.x == "NA") NA else as.character(.x),
            USE.NAMES = FALSE
        )
    )
}
