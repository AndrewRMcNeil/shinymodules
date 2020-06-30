#' Generate reactable from tibble
#'
#' @param .tbl (tbl_df) Data to display in table
#' @param .col_names (character) Vector of column names
#' @param .col_sortable (character) Vector of column names to be sortable
#'
#' @return (reactable) htmlwidget table
#' @export
gen_reactable <- function(
  .tbl,
  .col_names = NULL,
  .col_sortable = NULL,
  .selection = NULL,
  .searchable = FALSE
) {

  assert_tibble(.tbl)
  assert_col_names(.col_names, .len = length(.tbl))
  assert_col_sortable(.col_sortable)

  col_defs <- gen_col_defs(
    .tbl = .tbl,
    .col_names = .col_names,
    .col_sortable = .col_sortable
  )

  reactable(
    data = .tbl,
    columns = col_defs,
    showSortable = TRUE,
    selection = .selection,
    searchable = .searchable,
    defaultPageSize = 6
  )

}

assert_col_sortable <- function(.col_sortable) {

  if (not_null(.col_sortable)) {

    assert_character(
      .col_sortable,
      any.missing = FALSE,
      min.len = 1
    )

  } else {

    assert_null(.col_sortable)

  }

  TRUE

}

assert_col_names <- function(.col_names,
                             .len) {

  if (not_null(.col_names)) {

    assert_character(
      .col_names,
      any.missing = FALSE,
      len = .len
    )

  } else {

    assert_null(.col_names)

  }

  TRUE

}

gen_col_defs <- function(.tbl,
                         .col_names,
                         .col_sortable) {

  col_names_tbl <- colnames(.tbl)

  if (is.null(.col_names)) {
    col_names <- col_names_tbl
  } else {
    col_names <- .col_names
  }

  col_sortable_lgl <- map_lgl(
    col_names_tbl,
    ~.x %in% .col_sortable
  )

  arg_tibble <- tibble(
    name = col_names,
    sortable = col_sortable_lgl
  )

  arg_tibble %>%
    pmap(colDef) %>%
    set_names(col_names_tbl)

}
