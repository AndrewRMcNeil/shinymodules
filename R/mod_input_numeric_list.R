#' @export
mod_input_numeric_list_ui <- function(id) {

  ns <- NS(id)

  uiOutput(
    outputId = ns("input_numeric_list")
  )

}

#' @export
mod_input_numeric_list_server <- function(
  input,
  output,
  session,
  .input_id,
  .label,
  .unit = rep(NA, length(.input_id)),
  .value = rep(0, length(.input_id)),
  .min = rep(NA, length(.input_id)),
  .max = rep(NA, length(.input_id)),
  .step = rep(NA, length(.input_id)),
  .width = rep("250px", length(.input_id))
) {

  ns <- session$ns

  args_tbl <- tibble(
    id = map_chr(.input_id, ns),
    .label = .label,
    .unit = .unit,
    .value = .value,
    .min = .min,
    .max = .max,
    .step = .step,
    .width = .width
  )

  input_widgets <- pmap(
    .l = args_tbl,
    .f = mod_input_numeric_ui
  )

  output$input_numeric_list <- renderUI({

    input_widgets

  })

  widget_outputs <- map(
    .x = .input_id,
    .f = ~callModule(
      module = mod_input_numeric_server,
      id = .x
    )
  ) %>%
    set_names(.input_id)

  return(widget_outputs)

}
