
box::use(
    echarts4r,
    shiny[div, moduleServer, NS, numericInput, reactive, renderUI, uiOutput, downloadHandler, plotOutput, downloadLink, renderPlot, reactiveValues, downloadButton, actionButton, observeEvent, textInput],
    dplyr[mutate, select, bind_rows, filter], tidyr, 
    ggplot2[ggplot, stat_ecdf, aes, labs, theme_minimal, geom_segment, theme, element_blank],
    DT[datatable, renderDT, DTOutput],
    serosim[immunity_model_vacc_ifxn_biomarker_prot, plot_subset_individuals_history, plot_biomarker_quantity, plot_immune_histories],
    utils[str, write.csv, zip]
)
box::use(
  app/logic/chart_utils[label_formatter],
)


# Define the UI for numeric inputs
#' @export
ui_inputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box-top",
      textInput(ns("user_input_name"), "Name of simulation: ", value = "Simulation A"),
      actionButton(ns("run_btn"), "Run model")
  )
}

# Define the UI for displaying the output plot
#' @export
ui_outputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box-top",
    plotOutput(ns("plot_ind_1")),
    plotOutput(ns("plot_ind_2")),
    plotOutput(ns("plot_ind_3")),
    downloadButton(ns("download_data"), "Download Data")
  )
}


# Define the server logic for the inputs
#' @export
server_inputs <- function(id, runput_demo, runput_bioexp, inputs_abkin, inputs_imm, inputs_obs) {
  moduleServer(id, function(input, output, session) {
    fit <- reactiveValues(res = 0)

    observeEvent(input$run_btn, {
        inputs <- list(inputs_abkin[[1]](), inputs_abkin[[2]]())
        ab_func <- inputs[[1]]$func
        draw_parameters <- inputs[[1]]$draw_parameters
        model_pars <- bind_rows(inputs[[1]]$pars, inputs[[2]]$pars)
        model_pars <- bind_rows(model_pars, data.frame(exposure_id = NA, biomarker_id = 1, name = "obs_sd", mean = NA, sd = 1, distribution = "normal"))


        model_pars <- bind_rows(model_pars, 
          data.frame(exposure_id = 1, biomarker_id = 1, name = "biomarker_prot_midpoint", mean =  inputs_imm()$biomarker_prot_midpoint, sd = NA, distribution = ""),
          data.frame(exposure_id = 1, biomarker_id = 1, name = "biomarker_prot_width", mean =  inputs_imm()$biomarker_prot_midpoint, sd = NA, distribution = ""))


        print("simulation_settings")
        print(runput_demo()$simulation_settings)
        print("demography")
        print(runput_demo()$demography)
        print("observation_times")
        print(inputs_obs()$observation_times)
        print("foe_pars")
        print(runput_bioexp()$foe_pars)
        print("biomarker_map")
        print(runput_bioexp()$biomarker_map)
        print("model_pars")
        print(model_pars)
        print("exposure_model")
        print(runput_bioexp()$exposure_model)
        print("immunity_model")
        print(immunity_model_vacc_ifxn_biomarker_prot)
        print("antibody_model")
        print(ab_func)
        print("observation_model")
        print(inputs_obs()$func)
        print("draw_parameters")
        print(draw_parameters)
        print("bounds")
        print(inputs_obs()$bounds) 


        fit$res <- serosim::runserosim(
            simulation_settings = runput_demo()$simulation_settings,
            demography = runput_demo()$demography,
            observation_times = inputs_obs()$observation_times, 
            foe_pars = runput_bioexp()$foe_pars,
            biomarker_map = runput_bioexp()$biomarker_map,
            model_pars = model_pars,
            exposure_model = runput_bioexp()$exposure_model,
            immunity_model = immunity_model_vacc_ifxn_biomarker_prot,
            antibody_model = ab_func,
            observation_model = inputs_obs()$func,
            draw_parameters = draw_parameters,

            ## Specify other arguments needed
            VERBOSE = 10,
            bounds = inputs_obs()$bounds,
            max_events = c(1, 1),
            vacc_exposures = 2,
            vacc_age = c(NA, 9),
            sensitivity = 1,
            specificity = 1
            )
          
    })

    reactive({
      list(res = fit$res, name = input$user_input_name)
    })
    
    })
}

# Define the server logic for the outputs: inputs_demo, inputs_bioexp, inputs_abkin, inputs_imm, inputs_obs
#' @export
server_outputs <- function(id, inputs, runput_demo) {
  moduleServer(id, function(input, output, session) {


    # Render the data table
    output$plot_ind_1 <- renderPlot({
        res <- inputs()$res
        demography <- runput_demo()$demography
        plot_subset_individuals_history(res$biomarker_states, res$immune_histories_long, subset=10, demography)
    })

    output$plot_ind_2 <- renderPlot({
        res <- inputs()$res
        demography <- runput_demo()$demography
        plot_immune_histories(res$immune_histories_long)
    })

    output$plot_ind_3 <- renderPlot({
        res <- inputs()$res
        demography <- runput_demo()$demography
        plot_biomarker_quantity(res$biomarker_states)
    })

    my_data <- reactive({
      res <- inputs()$res
    # You can replace this with any R object you want to make downloadable
      res
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        # Define the name of the file to be downloaded
        paste0(inputs()$name, ".zip")
      },
      
      content = function(file) {
        res <- my_data()
  
        sero_data <- select(res$observed_biomarker_states, id = i, day = t, biomarker = b, value, observed)
        inf_data <- select(filter(res$immune_histories_long, value == 1) , id = i, day = t, exposure_type = x)

        # Write the R object to a file
        saveRDS(res, file = paste0( "res_output.RDS"))
        write.csv(sero_data, file =  paste0( "sero_data.csv"), row.names = FALSE)
        write.csv(inf_data, file = paste0("inf_data.csv"), row.names = FALSE)
        direct <- c(paste0("res_output.RDS"), paste0("sero_data.csv"), paste0("inf_data.csv") )
        # Create a ZIP file
        zip(zipfile = file, files = direct)
      }
    )

  })
}
