#' @export
mod_input_numeric_ui <- function(id,
                                 .label = NULL,
                                 .value = 0,
                                 .min = 0,
                                 .max = NA,
                                 .step = NA,
                                 .width = "250px",
                                 .unit = NA) {

  ns <- NS(id)

  if (is.null(.label)) {

    div_label <- NULL

  } else {

    div_label <- div(
      style = "
        height: calc(2.25rem + 2px);
        margin-bottom: 1rem;
        padding-top: 0.375rem;
        padding-right: 1.25rem;
        padding-bottom: 0.375rem;
        padding-left: 0.75rem;
      ",

      tags$label(str_glue("{.label}: "))

    )

  }

  if (is.na(.unit)) {

    div_unit <- NULL

  } else {

    div_unit <- div(
      style = "
        height: calc(2.25rem + 2px);
        margin-bottom: 1rem;
        margin-right: 0.75rem;
        padding-top: 0.375rem;
        padding-right: 0.75rem;
        padding-bottom: 0.375rem;
        padding-left: 0.75rem;
        background-color: #f2f2f2;
        border: 1px solid #ced4da;
        border-radius: .25rem;
      ",

      tags$label(.unit)

    )

  }

  tagList(

    div(
      style = "
        display: flex;
        justify-content: flex-end;
      ",

      div_label,

      div(
        style = str_glue("
          display: flex;
          width: {.width};
        "),

        div(
          style = "
            flex-grow: 1;
          ",

          numericInput(
            inputId = ns("numeric"),
            label = NULL,
            value = .value,
            min = .min,
            max = .max,
            step = .step,
            width = "100%"
          )

        ),

        div_unit

      )

    ),

    uiOutput(
      outputId = ns("invalid_message")
    )

  )

}

#' @export
mod_input_numeric_server <- function(input,
                                     output,
                                     session,
                                     .min = 0,
                                     .max = NULL) {

  ns <- session$ns

  value <- reactive({
    input$numeric
  })

  output$invalid_message <- renderUI({
    req(value())

    if (
      (not_null(.min) && (value() < .min)) ||
      (not_null(.max) && (value() > .max))
    ) {

      gt_text <- str_glue(
        "Input must {.min} or greater"
      )

      lt_text <- str_glue(
        "Input must be {.max} or less"
      )

      if (value() < .min) {

        invalid_text <- gt_text

      } else if (value() > .max) {

        invalid_text <- lt_text

      } else {

        stop()

      }

      div(
        style = "
          color: #d9534f;
          font-weight: bold;
          margin-bottom: 20px;
        ",

        p(invalid_text)

      )

    }

  })

  return(
    reactive({
      value()
    })
  )

}

