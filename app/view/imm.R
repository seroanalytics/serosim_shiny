box::use(
  echarts4r,
  shiny[div, moduleServer, NS, numericInput, reactive, renderUI, uiOutput, plotOutput, renderPlot, reactiveValues, actionButton, observeEvent, textInput, selectInput, tagList, h3, h4],
  dplyr[mutate, select], tidyr,
  ggplot2[ggplot, stat_ecdf, aes, labs, theme_minimal, geom_segment, theme, element_blank, geom_line],
  DT[datatable, renderDT, DTOutput],
  serosim[plot_biomarker_mediated_protection]
)
box::use(
  app/logic/chart_utils[label_formatter],
  app/logic/abkin_funcs[kinetics_power_function],
)

# Define the UI for numeric inputs
#' @export
ui_inputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box",
    uiOutput(ns("dynamicTitle_imm")),
    numericInput(ns("max_T"), "Max biomarker value:", value = 14),
    numericInput(ns("biomarker_prot_midpoint"), "Midpiont of protection:", value = 7),
    numericInput(ns("biomarker_prot_width"), "Variance:", value = 1)
  )
}

# Define the UI for displaying the output plot
#' @export
ui_outputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box",
    plotOutput(ns("plot_cop"))
  )
}

# Define the server logic for the inputs
#' @export
server_inputs <- function(id, bioexp_outputs) {
  moduleServer(id, function(input, output, session) {

    output$dynamicTitle_imm <- renderUI({
      value <- unique(bioexp_outputs()$biomarker_map_original[, 2])
      h4(value)#])  # Update title panel based on input
    })

    reactive({
        list(
          max_T = input$max_T,
          biomarker_prot_midpoint = input$biomarker_prot_midpoint,
          biomarker_prot_width = input$biomarker_prot_width
        )
      }
    )

  })
}

# Define the server logic for the outputs
#' @export
server_outputs <- function(id, inputs) {
  moduleServer(id, function(input, output, session) {
    # Render the UI elements dynamically based on dropdown selection
   
    output$plot_cop <- renderPlot({
      inputs <- inputs()
      p1 <- plot_biomarker_mediated_protection(0:inputs$max_T, biomarker_prot_midpoint=inputs$biomarker_prot_midpoint, biomarker_prot_width=inputs$biomarker_prot_width)
      p1 
      })
  })
}