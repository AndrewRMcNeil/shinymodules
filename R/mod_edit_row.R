mod_edit_row_server <- function(
  input,
  output,
  session,
  .tbl,
  .edit_row,
  .row_index
) {

  ns <- session$ns

  col_names <- reactive({

    req(.tbl())

    colnames(.tbl())

  })

  tbl_row <- reactive({

    req(
      .tbl(),
      .row_index()
    )

    .tbl() %>% slice(.row_index())

  })

  input_widgets <- reactive({

    req(tbl_row(), col_names())

    map2(
      .x = tbl_row(),
      .y = col_names(),
      .f = function(.col, .name) {

        if (is.character(.col)) {

          textInput(
            inputId = ns(.name),
            label = .name,
            value = .col,
            width = "100%"
          )

        } else if (is.numeric(.col)) {

          numericInput(
            inputId = ns(.name),
            label = .name,
            value = .col,
            width = "100%"
          )

        } else {

          stop("Invalid column type")

        }

      }
    )

  })

  widget_outputs <- reactive({

    req(col_names())

    col_names() %>%
      map(~input[[.x]]) %>%
      set_names(col_names())

  })

  modal_ui <- reactive({

    modalDialog(
      title = "Edit values then click 'Submit' or 'Cancel'",
      footer = NULL,
      easyClose = TRUE,

      input_widgets(),

      br(),

      fluidRow(

        column(
          width = 5,

          div(
            class = "float-right",

            actionLink(
              inputId = ns("submit_edit_row"),
              label = "Submit",
              icon = icon("ok", lib = "glyphicon"),
              style = "color: #007bff;"
            )

          )

        ),

        column(
          width = 5,
          offset = 2,

          div(
            class = "float-left",

            actionLink(
              inputId = ns("cancel_edit_row"),
              label = "Cancel",
              icon = icon("remove", lib = "glyphicon"),
              style = "color: #403734;"
            )

          )

        )

      )

    )

  })

  observeEvent(
    input$submit_edit_row, {

    req(input$submit_edit_row)

    removeModal()

  })

  observeEvent(
    input$cancel_edit_row, {

    req(input$cancel_edit_row)

    removeModal()

  })

  observeEvent(
    .edit_row(), {

    req(
      .edit_row(),
      modal_ui()
    )

    showModal(
      modal_ui()
    )

  })

  tbl_with_edited_rows <- eventReactive(
    input$submit_edit_row, {

    req(
      input$submit_edit_row > 0,
      widget_outputs()
    )

    new_row <- widget_outputs()

    .tbl() %>%
      slice(-.row_index()) %>%
      add_row(!!!new_row, .before = 1)

  })

  return(tbl_with_edited_rows)

}
