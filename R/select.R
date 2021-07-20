# TODO
#' @export
select.listbl <- function(.data, ...) {
  attr_listbl <- attr(.data, "listbl")

  call <- exprs(...)
  attr_listbl_child <- attr_listbl |>
    pull(value) |>
    purrr::keep(~ inherits(., "listbl")) |>
    purrr::modify(~ {
      res <- .x |>
        select.listbl(!!!call)
      attr(res, "listbl") |>
        list()
    }) |>
    bind_rows()

  col_names <- attr_listbl |>
    pull(label) |>
    vec_unique()
  x <- vec_recycle(list(NULL),
                    size = vec_size(col_names))
  names(x) <- col_names
  df <- new_data_frame(x)

  col_names <- tidyselect::eval_select(expr(c(...)), df,
                                       strict = F) |>
    names2()
  attr_listbl <- attr_listbl |>
    filter(label %in% col_names)

  res <- listbl(name = attr_listbl$name,
                label = attr_listbl$label,
                value = attr_listbl$value)

  if (!vec_is_empty(attr_listbl_child)) {
    res <- res |>
      c(listbl(name = attr_listbl_child$name,
               label = attr_listbl_child$label,
               value = attr_listbl_child$value))
  }

  res
}
