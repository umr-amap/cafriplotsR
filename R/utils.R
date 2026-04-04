#' @importFrom magrittr %>%
#' @importFrom stats as.formula na.omit setNames
#' @importFrom utils capture.output head object.size read.csv setTxtProgressBar txtProgressBar write.csv
NULL

#' Conditional rclipboard button
#'
#' Returns an rclipboard copy button when the rclipboard package is available,
#' or NULL otherwise.  Drop-in replacement for rclipboard::rclipButton().
#'
#' @param ... Arguments passed to rclipboard::rclipButton()
#' @return A Shiny tag or NULL
#' @keywords internal
#' @noRd
.rclip_button <- function(...) {
  if (!requireNamespace("rclipboard", quietly = TRUE)) return(NULL)
  rclipboard::rclipButton(...)
}

#' Conditional rclipboard setup
#'
#' Returns rclipboard JS/CSS dependencies when available, or NULL.
#'
#' @return A Shiny dependency tag or NULL
#' @keywords internal
#' @noRd
.rclipboard_setup <- function() {
  if (!requireNamespace("rclipboard", quietly = TRUE)) return(NULL)
  rclipboard::rclipboardSetup()
}
