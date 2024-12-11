box::use(
  echarts4r,
  shiny[div, moduleServer, NS, numericInput, reactive, renderUI, uiOutput, plotOutput, renderPlot, reactiveValues, actionButton, observeEvent, textInput, selectInput, tagList, h3, h4, h5],
  dplyr[mutate, select], tidyr,
  ggplot2[ggplot, stat_ecdf, aes, labs, theme_minimal, geom_segment, theme, element_blank, geom_line, geom_point],
  DT[datatable, renderDT, DTOutput],
  serosim[observation_model_continuous_noise, observation_model_continuous_bounded_noise],
  purrr[map_df]
)
box::use(
  app/logic/chart_utils[label_formatter]
)

# Define the UI for numeric inputs
#' @export
ui_inputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box",
    uiOutput(ns("dynamicTitle_obs")),
    selectInput(ns("dropdown_obs"),
    h5("Select a observation function:"), choices = c("Continuous-bounded", "Continuous-unbounded")),
    h5("Select the parameters"),
    uiOutput(ns("text_inputs_obs")),
    h5("Number if bleeds"),
    numericInput(ns("no_bleeds"), "Per person over study", value = 3)
  )
}

# Define the UI for displaying the output plot
#' @export
ui_outputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box",
    plotOutput(ns("plot_obs"))
  )
}

# Define the server logic for the inputs
#' @export
server_inputs <- function(id, inputs_demo, bioexp_outputs) {
  moduleServer(id, function(input, output, session) {

    output$dynamicTitle_obs <- renderUI({
      value <- unique(bioexp_outputs()$biomarker_map_original[, 2])
      h4(value)#])  # Update title panel based on input
    })

    output$text_inputs_obs <- renderUI({
      if (input$dropdown_obs == "Continuous-bounded") {
        # If "Teunis 2016" is chosen, display numeric inputs
        tagList(
           div(class = "flex-container",
            div(class = "input-pars-obs", numericInput(session$ns("sigma"), "Error", value = 1)),
            div(class = "input-pars-obs", numericInput(session$ns("lower_bound"), "Lower bound", value = 1)),
            div(class = "input-pars-obs", numericInput(session$ns("upper_bound"), "Upper bound", value = 1000)),
           )
        )
      } else if (input$dropdown_obs == "Continuous-unbounded") {
        # If "two" is chosen, display text inputs
        tagList(
           div(class = "flex-container",
            div(class = "input-pars-obs", numericInput(session$ns("sigma"), "Error", value = 1)),
           )
       )
      }
    })

    reactive({
        inputs_demo <- inputs_demo()
        N <- inputs_demo$N
        t_max <- inputs_demo$t_max
        observation_times <- map_df(1:N, 
          function(i) {
            
            data.frame(
              id = i, 
              t = sample(1:t_max, input$no_bleeds),
              b = 1
            )
          }
        )
      if (input$dropdown_obs == "Continuous-bounded") {
        bounds <- data.frame(
          biomarker_id = c(1, 1),
          name = c("lower_bound", "upper_bound"),
          value = c(input$lower_bound,input$upper_bound)
        )

        list(
          func = observation_model_continuous_bounded_noise,
          dropdown = input$dropdown_obs,
          bounds = bounds,
          no_bleeds = input$no_bleeds,
          observation_times = observation_times
        )
      } else {
         bounds <- data.frame(
          biomarker_id = c(1, 1),
          name = c("lower_bound", "upper_bound"),
          value = c(input$lower_bound,input$upper_bound)
        )

        list(
          func = observation_model_continuous_noise,
          dropdown = input$dropdown_obs,
          bounds = bounds,
          no_bleeds = input$no_bleeds,
          observation_times = observation_times
        )
      }
    })

  })
}

# Define the server logic for the outputs
#' @export
server_outputs <- function(id, inputs) {
  moduleServer(id, function(input, output, session) {
    # Render the UI elements dynamically based on dropdown selection

     output$plot_obs <- renderPlot({ 

        inputs <- inputs()
        p1 <- ggplot(inputs$observation_times) + 
          geom_point(aes(y = id, x = t), color = "red")
        p1
      })
  })
}