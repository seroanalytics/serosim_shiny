
box::use(
    echarts4r,
    shiny[div, moduleServer, NS, numericInput, reactive, renderUI, uiOutput, plotOutput, renderPlot, reactiveValues],
    dplyr[mutate, select], tidyr, 
    ggplot2[ggplot, stat_ecdf, aes, labs, theme_minimal, geom_segment, theme, element_blank],
    serosim[generate_pop_demography]
)
box::use(
  app/logic/chart_utils[label_formatter],
)


# Define the UI for numeric inputs
#' @export
ui_inputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box",
    numericInput(ns("N"), "Number of individuals:", value = 100, min = 1),
    numericInput(ns("t_max"), "Time frame of the simulation:", value = 100, min = 1),
    numericInput(ns("prob_removal"), "Probability of removal:", value = 0.0, min = 0, max = 1)
  )
}

# Define the UI for displaying the output plot
#' @export
ui_outputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box",
    plotOutput(ns("demographics_plot"))
  )
}


# Define the server logic for the inputs
#' @export
server_inputs <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
        list(
            N = input$N,
            t_max = input$t_max,
            prob_removal = input$prob_removal
        )
        })
    })
}

# Define the server logic for the outputs
#' @export
server_outputs <- function(id, inputs) {
  moduleServer(id, function(input, output, session) {

    simulationDemo <- reactive({
    params <- inputs()

      N <- params$N
      t_max <- params$t_max
      prob_removal <- params$prob_removal

      birth_times <- rep(1, N)

      # Placeholder for serosim::generate_pop_demography function
      demography <- generate_pop_demography(N, times = 1:t_max, birth_times = birth_times, removal_min=1, removal_max=t_max, prob_removal=prob_removal) 
#      demo ->
  #      mutate(i = as.character(i)) |>
     #   mutate(i = factor(i, levels = 1:N))
      return(demography)
    })

    simulationData <- reactive({
        params <- inputs()

        N <- params$N
        t_max <- params$t_max
        prob_removal <- params$prob_removal
        
        # Initialize population status (1 means present, 0 means removed)
        population <- rep(1, N)
        data <- data.frame(time = numeric(), individuals = numeric())
        
        # Run the simulation over the time frame
        for (t in 1:t_max) {
            removal <- runif(N) < prob_removal
            population <- population * (1 - removal)
            data <- rbind(data, data.frame(time = t, individuals = sum(population)))
        }
        
        return(data)
    })

    output$demographics_plot <- renderPlot({
        data <- simulationDemo()
        


        p2 <- data |>
            ggplot() + 
                geom_segment(aes(y = i, yend = i, x = birth, xend = removal - 1), size = 2, alpha = 0.2, color = "gray85") + 
                theme_minimal() + labs(x = "Time in study", y = "Individuals", title = "Individuals in study over time")  + 
                theme(axis.text.y = element_blank())

        p2  + patchwork::plot_layout()
    })

    reactive({
      demo = simulationDemo()

      params <- inputs()
      simulation_settings <- list("t_start"=1,"t_end"=max(params$t_max))

      N <- params$N
      t_max <- params$t_max
      list(
        simulation_settings = simulation_settings,
        N = N,
        t_max = t_max,
        demography = demo
      )
    })

  })
}
