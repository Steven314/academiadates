.onLoad <- function(...) {
    s3_register("ggplot2::scale_type", "yeartrimester")
    s3_register("scales::fullseq", "yeartrimester")
}
