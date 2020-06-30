mod_reactable_ui <- function(id) {

  ns <- NS(id)

  fluidRow(

    column(
      width = 12,

      reactableOutput(
        outputId = ns("reactable")
      )

    )

  )

}

mod_reactable_server <- function(
  input,
  output,
  session,
  .tbl,
  .col_names,
  .col_sortable,
  .searchable
) {

  observeEvent(
    .tbl(), {

    req(.tbl())

    updateReactable(
      outputId = "reactable",
      selected = NA
    )

  })

  output$reactable <- renderReactable({

    req(.tbl())

    gen_reactable(
      .tbl = .tbl(),
      .col_names = .col_names,
      .col_sortable = .col_sortable,
      .selection = "single",
      .searchable = .searchable
    )

  })

  index <- reactive({

    getReactableState(
      session = session,
      outputId = "reactable",
      name = "selected"
    )

  })

  value <- reactive({

    req(index())

    .tbl %>% slice(index())

  })

  return(
    list(
      index = index,
      value = value
    )
  )

}
