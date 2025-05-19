#' @importFrom tsibble interval_pull gcd_interval new_interval
#' @method interval_pull yeartrimester
#' @export
interval_pull.yeartrimester <- function(x) {
    tri   <- as.double(x)
    ntris <- tsibble::gcd_interval(tri)
    tsibble::new_interval(.others = list("T" = ntris))
}
