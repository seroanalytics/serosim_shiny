box::use(
  echarts4r,
  shiny[div, moduleServer, titlePanel, NS, numericInput, reactive, renderUI, uiOutput, plotOutput, renderPlot, reactiveValues, actionButton, observeEvent, textInput, selectInput, tagList, h5, h4, h3],
  dplyr[mutate, select, bind_rows], tidyr,
  ggplot2[ggplot, stat_ecdf, aes, labs, theme_minimal, geom_segment, theme, element_blank, geom_line],
  DT[datatable, renderDT, DTOutput],
  serosim[antibody_model_biphasic, plot_antibody_model, draw_parameters_random_fx, draw_parameters_random_fx_biomarker_dep]
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
    uiOutput(ns("dynamicTitleUI")),
    selectInput(ns("dropdown_abs"), 
    h5("Select a kinetic function:"), choices = c("Biphasic decay", "Teunis 2016")),
    h5("Select the parameters"),
    uiOutput(ns("text_inputs"))
  )
}

# Define the UI for displaying the output plot
#' @export
ui_outputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box",
    plotOutput(ns("plot_ab"))
  )
}

# Define the server logic for the inputs
#' @export
server_inputs <- function(id, bioexp_outputs, i) {
  i <- i


  moduleServer(id, function(input, output, session) {

    output$dynamicTitleUI <- renderUI({
      value <- paste0(bioexp_outputs()$biomarker_map_original[i, 1], ", ", bioexp_outputs()$biomarker_map_original[i, 2] )
      h4(value)#])  # Update title panel based on input
    })

    output$text_inputs <- renderUI({
      if (input$dropdown_abs == "Teunis 2016") {
        # If "Teunis 2016" is chosen, display numeric inputs
        tagList(
           div(class = "flex-container",
            div(class = "input-pars-ab", numericInput(session$ns("t_peak"), "t_peak", value = 14)),
            div(class = "input-pars-ab", numericInput(session$ns("peak"), "peak", value = 4)),
            div(class = "input-pars-ab", numericInput(session$ns("k"), "k", value = 0)),
            div(class = "input-pars-ab", numericInput(session$ns("v"), "v", value = 0.001)),
            div(class = "input-pars-ab", numericInput(session$ns("r"), "r", value = 2))
           )
        )
      } else if (input$dropdown_abs == "Biphasic decay") {
        # If "two" is chosen, display text inputs
          tagList(
                    div(class = "flex-container",
                      div(class = "input-pars-ab", numericInput(session$ns("boost_long"), "boost_long", value = 14)),
                      div(class = "input-pars-ab", numericInput(session$ns("boost_short"), "boost_short", value = 4)),
                      div(class = "input-pars-ab", numericInput(session$ns("wane_long"), "wane_long", value = 0)),
                      div(class = "input-pars-ab", numericInput(session$ns("wane_short"), "wane_short", value = 0)),
                    )
                  )
                }
              })

    reactive({

      bioexp <- bioexp_outputs()
      bioexp_i <- bioexp$biomarker_map[i, ]

      if (input$dropdown_abs == "Teunis 2016") {
        pars <- c("t_peak" = input$t_peak,
                  "peak" = input$peak,
                  "k" = input$k,
                  "v" = input$v,
                  "r" = input$r,
                  "y0" = 1)
        list(
          func = kinetics_power_function,
          dropdown = input$dropdown_abs,
          pars = pars
        )
      } else if (input$dropdown_abs == "Biphasic decay") {
        pars <- data.frame(
            exposure_id = rep(bioexp_i$exposure_id, 4),
            biomarker_id =  rep(bioexp_i$biomarker_id, 4),
            name = c("boost_long", "boost_short", "wane_long", "wane_short"),
            mean = c(input$boost_long, input$boost_short, input$wane_long, input$wane_short),
            sd = NA,
            distribution = NA
        )

        list(
          func = antibody_model_biphasic,
          draw_parameters = draw_parameters_random_fx,
          dropdown = input$dropdown_abs,
          pars = pars
        )
      }
    })

  })
}

# Define the server logic for the outputs
#' @export
server_outputs <- function(id, inputs_ab, outputs_exp) {
  moduleServer(id, function(input, output, session) {
    # Render the UI elements dynamically based on dropdown selection
   
  
    output$plot_ab <- renderPlot({

        outputs_exp <- outputs_exp()

        inputs <- list(inputs_ab[[1]](), inputs_ab[[2]]())
        ab_func <- inputs[[1]]$func
        draw_parameters <- inputs[[1]]$draw_parameters
        model_pars <- bind_rows(inputs[[1]]$pars, inputs[[2]]$pars)
        model_pars <- bind_rows(model_pars, data.frame(exposure_id = NA, biomarker_id = 1, name = "obs_sd", mean = NA, sd = 1, distribution = "normal"))

        biomarker_map <- outputs_exp$biomarker_map
        plot_antibody_model(ab_func, N=100, model_pars=model_pars, draw_parameters_fn = draw_parameters, biomarker_map=biomarker_map)

      })
  })
}