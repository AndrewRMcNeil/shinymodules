mod_delete_row_server <- function(
  input,
  output,
  session,
  .tbl,
  .delete_row,
  .row_index
) {

  tbl_with_deleted_row <- eventReactive(
    .delete_row(), {

    req(
      .tbl(),
      .delete_row(),
      .row_index()
    )

    .tbl() %>% slice(-.row_index())

  })

  return(tbl_with_deleted_row)

}
