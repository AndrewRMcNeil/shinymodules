#' @export
mod_editable_reactable_ui <- function(id, .title) {

  ns <- NS(id)

  bs4Box(
    width = 12,
    title = .title,

    mod_reactable_buttons_ui(
      id = ns("reactable_buttons")
    ),

    mod_reactable_ui(
      id = ns("reactable")
    )

  )

}

#' @export
mod_editable_reactable_server <- function(
  input,
  output,
  session,
  .tbl,
  .col_names = NULL,
  .col_sortable = colnames(.tbl),
  .searchable = TRUE
) {

  tbl_state <- reactiveVal(.tbl)

  buttons <- callModule(
    module = mod_reactable_buttons_server,
    id = "reactable_buttons",
    .row_index = selected_row$index
  )

  selected_row <- callModule(
    module = mod_reactable_server,
    id = "reactable",
    .tbl = tbl_state,
    .col_names = .col_names,
    .col_sortable = .col_sortable,
    .searchable = .searchable
  )

  tbl_with_added_row <- callModule(
    module = mod_add_row_server,
    id = "add_row",
    .tbl = tbl_state,
    .add_row = buttons$add_row
  )

  tbl_with_edited_row <- callModule(
    module = mod_edit_row_server,
    id = "edit_row",
    .tbl = tbl_state,
    .edit_row = buttons$edit_row,
    .row_index = selected_row$index
  )

  tbl_with_deleted_row <- callModule(
    module = mod_delete_row_server,
    id = "delete_row",
    .tbl = tbl_state,
    .delete_row = buttons$delete_row,
    .row_index = selected_row$index
  )

  observeEvent(
    tbl_with_added_row(), {

    req(tbl_with_added_row())

    tbl_state(tbl_with_added_row())

  })

  observeEvent(
    tbl_with_edited_row(), {

    req(tbl_with_edited_row())

    tbl_state(tbl_with_edited_row())

  })

  observeEvent(
    tbl_with_deleted_row(), {

    req(tbl_with_deleted_row())

    tbl_state(tbl_with_deleted_row())

  })

  return(tbl_state)

}
