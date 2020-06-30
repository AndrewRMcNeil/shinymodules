mod_reactable_buttons_ui <- function(id) {

  ns <- NS(id)

  fluidRow(

    column(
      width = 1,

      actionLink(
        inputId = ns("add_row"),
        label = "Add",
        icon = icon("plus"),
        style = "color: #007bff;"
      ),

    ),

    column(
      width = 1,

      actionLink(
        inputId = ns("edit_row"),
        label = "Edit",
        icon = icon("gear"),
        style = "color: #403734;"
      ),

    ),

    column(
      width = 1,

      actionLink(
        inputId = ns("delete_row"),
        label = "Delete",
        icon = icon("minus"),
        style = "color: #dc3545;"
      )

    )

  )

}

mod_reactable_buttons_server <- function(
  input,
  output,
  session,
  .row_index
) {

  observeEvent(
    input$edit_row, {

    if (is.null(.row_index())) {

      showModal(
        modalDialog(
          p("Select a row to edit..."),
          easyClose = TRUE
        )
      )

    }

  })

  observeEvent(
    input$delete_row, {

    if (is.null(.row_index())) {

      showModal(
        modalDialog(
          p("Select a row to delete..."),
          easyClose = TRUE
        )
      )

    }

  })

  buttons_output <- reactiveValues(
    add_row = reactive({ input$add_row }),
    edit_row = reactive({ input$edit_row }),
    delete_row = reactive({ input$delete_row })
  )

  return(buttons_output)

}
