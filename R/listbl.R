#' @export
listbl <- function(name, label, value) {
  attr_listbl <- new_data_frame(list(name = name,
                                     label = label,
                                     value = value),
                                class = "tbl") |>
    arrange(name)
  res <- attr_listbl$value |>
    purrr::modify(~ {
      res <- unclass(.x)
      attr(res, "listbl") <- NULL
      res
    }) |>
    rapply(function(x) NULL,
           how = "replace")
  class(res) <- c("listbl", class(res))
  names(res) <- attr_listbl$name
  attr(res, "listbl") <- attr_listbl
  res
}

#' @export
as_listbl <- function(x) {
  UseMethod("as_listbl")
}

#' @export
as_listbl.data.frame <- function(x) {
  gv <- group_vars(x)
  if (is_scalar_character(gv)) {
    listbl(name = x[[gv]],
           label = vec_rep(factor(gv), vec_size(x)),
           value = as.list(x[[setdiff(names2(x), gv)]]))
  } else {
    x |>
      ungroup() |>
      nest_by(across(gv[1])) |>
      mutate(data = data |>
               group_by(across(gv[-1])) |>
               as_listbl() |>
               list()) |>
      as_listbl()
  }
}

#' @export
as.list.listbl <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr_listbl <- attr(x, "listbl")
  res <- attr_listbl |>
    pull(value) |>
    purrr::map(~ {
      if (inherits(.x, "listbl")) {
        as.list.listbl(.x)
      } else {
        .x
      }
    })
  names(res) <- paste(attr_listbl$label, attr_listbl$name,
                      sep = ".")
  res
}

#' @export
as.data.frame.listbl <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "listbl") |>
    as.data.frame()
}

#' @export
as_tibble.listbl <- function(x, ...) {
  ellipsis::check_dots_empty()
  as.data.frame.listbl(x) |>
    as_tibble()
}

#' @export
c.listbl <- function(...) {
  res <- list2(...) |>
    purrr::modify(unclass) |>
    flatten()
  class(res) <- "listbl"
  attr(res, "listbl") <- list2(...) |>
    purrr::modify(~ attr(.x, "listbl")) |>
    bind_rows()
  res
}

# printing ----------------------------------------------------------------

#' @export
format.listbl <- function(x, ...) {
  x |>
    as.list() |>
    format(...)
}

#' @export
print.listbl <- function(x, ...) {
  x |>
    as.list() |>
    print(...)
  invisible(x)
}


# subsetting --------------------------------------------------------------

#' @export
`$.listbl` <- function(x, i) {
  attr(x, "listbl") |>
    filter(name == i) |>
    pull(value) |>
    first()
}

#' @export
`[[.listbl` <- function(x, i, ...) {
  ellipsis::check_dots_empty()
  if (is.numeric(i)) {
    attr(x, "listbl") |>
      slice(i) |>
      pull(value) |>
      first()
  } else {
    `$.listbl`(x, i)
  }
}

