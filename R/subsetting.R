#' #' @export
#' `$.listbl` <- function(x, i) {
#'   attr(x, "listbl") |>
#'     filter(name == i) |>
#'     pull(value) |>
#'     first()
#' }
#'
#' #' @export
#' `[[.listbl` <- function(x, i, ...) {
#'   ellipsis::check_dots_empty()
#'   if (is.numeric(i)) {
#'     attr(x, "listbl") |>
#'       slice(i) |>
#'       pull(value) |>
#'       first()
#'   } else {
#'     `$.listbl`(x, i)
#'   }
#' }
