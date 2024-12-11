
box::use(
    echarts4r,
    shiny[div, moduleServer, NS, numericInput, reactive, renderUI, uiOutput, plotOutput, renderPlot, reactiveValues, actionButton, observeEvent, textInput],
    dplyr[mutate, select], tidyr,
    ggplot2[ggplot, stat_ecdf, aes, labs, theme_minimal, geom_segment, theme, element_blank],
    DT[datatable, renderDT, DTOutput],
    serosim[reformat_biomarker_map, exposure_model_simple_FOE]
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
      textInput(ns("user_input"), "Enter exposure type and biomarker in format (exposure_type, biomarker): ", value = ""),
      actionButton(ns("add_btn"), "Add")
  )
}

# Define the UI for displaying the output plot
#' @export
ui_outputs <- function(id) {
  ns <- NS(id)

  div(
    class = "component-box",
    DTOutput(ns("data_table"))
  )
}


# Define the server logic for the inputs
#' @export
server_inputs <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
        list(
            user_input = input$user_input,
            add_btn = input$add_btn        )
        })
    })
}

# Define the server logic for the outputs
#' @export
server_outputs <- function(id, inputs, inputs_demo) {
  moduleServer(id, function(input, output, session) {
    input_list <- reactiveValues(data = data.frame(exposure_id = c("Delta", "Vax"), biomarker_id = c("IgG", "IgG"), FOE = c(0.02, 0.04), stringsAsFactors = FALSE))

    observeEvent(inputs()$add_btn, {
        pars <- inputs()
        new_input <- pars$user_input
        new_input_split <- strsplit(new_input, ",")[[1]]
        if (new_input == "") {
          input_list$data
        } else {
          input_list$data <- rbind(input_list$data, data.frame(exposure_id = new_input_split[1], biomarker_id = new_input_split[2], FOE = new_input_split[3], stringsAsFactors = FALSE))
        }
        
        # Add the new input to the list
    })
    
    # Render the data table
    output$data_table <- renderDT({
        datatable(input_list$data)
    })

    calFOEpar <- reactive({
        params <- inputs()
        para_demo <- inputs_demo()

        exposure_id_l <- length(unique(input_list$data[1, ]))

        foe_pars <- array(0, dim=c(1, para_demo$t_max, exposure_id_l))

        ## Specify the force of exposure for exposure ID 1 which represents measles natural infection
        foe_pars[,,1] <- 0.02

        ## Specify the force of exposure for exposure ID 2 which represents measles vaccination
        foe_pars[,,2] <- 0.04

        ## Specify a simple exposure model which calculates the probability of exposure directly from the force of exposure at that time step
        foe_pars
    })

    getBiomarkerMap <- reactive({
      biomarker_map_original <- as.data.frame(input_list$data[, 1:2])

      ## Reformat biomarker_map for runserosim
      biomarker_map <-reformat_biomarker_map(biomarker_map_original)
      biomarker_map
    })
   

    reactive({
      foe_pars = calFOEpar()
      biomarker_map <- getBiomarkerMap()
      biomarker_map_original <- as.data.frame(input_list$data[, 1:2])

      list(
          foe_pars = foe_pars,
          biomarker_map = biomarker_map,
          biomarker_map_original = biomarker_map_original,
          biomarker_n = nrow(biomarker_map),
          exposure_model = exposure_model_simple_FOE
      )
    })

  })
}
