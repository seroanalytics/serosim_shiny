# Description: Main module for the Rhino app
box::use(
  shiny[bootstrapPage,reactive, observe, div, h1, h2, icon, moduleServer, NS, tags, tabsetPanel, textInput, sidebarLayout, sliderInput, sidebarPanel, tabPanel], # d
  shinythemes[shinytheme],
  rhino[rhinos],
)

# Load modules in the view folder
box::use(
  app/view/demo,
  app/view/bioexp,
  app/view/abkin,
  app/view/imm,
  app/view/obs,
  app/view/run,


)

# How to combine the modules
#' @export
ui <- function(id) {
  ns <- NS(id)

bootstrapPage(
    theme = shinytheme("cerulean"),
    h1("SEROSIM DASHBOARD"),

      tabsetPanel(
        tabPanel("Vignette 1 (Measles)",
      h2("Define demography"),

    
        div(
          class = "components-container",
          div(class = "input-panel", demo$ui_inputs(ns("inputs"))),
          div(class = "output-panel", demo$ui_outputs(ns("outputs")))
        ),


      h2("Define biomarkers and exposure types"),
      div(
        class = "components-container",
        div(class = "input-panel", bioexp$ui_inputs(ns("inputs"))),
        div(class = "output-panel", bioexp$ui_outputs(ns("outputs")))
      ),

      tags$button(
        id = "help-button",
        icon("question"),
        onclick = "App.showHelp()"
      ),

      h2("Define antibody kinetics"),
        div(
          class = "components-container",
          div(class = "input-panel", abkin$ui_inputs(ns(paste0("inputs_", 1)))),
          div(class = "input-panel", abkin$ui_inputs(ns(paste0("inputs_", 2))))
        ),
        div(class = "output-panel", abkin$ui_outputs(ns("outputs"))),


      h2("Define immunity model"),
      div(
        class = "components-container",
        div(class = "input-panel", imm$ui_inputs(ns("inputs"))),
        div(class = "output-panel", imm$ui_outputs(ns("outputs")))
      ),

      h2("Define observational model"),
      div(
        class = "components-container",
        div(class = "input-panel", obs$ui_inputs(ns("inputs"))),
        div(class = "output-panel", obs$ui_outputs(ns("outputs")))
      ),

      h2("Run model"),
      div(
        class = "components-container-top",
          div(class = "input-panel", run$ui_inputs(ns("inputs")))
        ),
      h2("Visualise simulated data"),
      div(
        class = "components-container-top",
          div(class = "output-panel", run$ui_outputs(ns("outputs")))
        )
        )
      )
  )
}

# Main server
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {


    inputs_demo <- demo$server_inputs("inputs")
    runput_demo <- demo$server_outputs("outputs", inputs_demo)
  
    inputs_bioexp <- bioexp$server_inputs("inputs")
    runput_bioexp <- bioexp$server_outputs("outputs", inputs_bioexp, inputs_demo)
    exposure_bio_length <- reactive({
      # This can be based on any user input, for example, an input slider
      nrow(runput_bioexp()$biomarker_map)  # Assuming num_inputs is a user input slider or numeric input
    })


    observe({
      L <- exposure_bio_length()

      inputs_abkin <- list()
      for (i in 1:L) {
        # Create dynamic input name
        input_name <- paste0("inputs_", i)
        # Call the server_inputs function with the dynamic input name
        inputs_abkin[[i]] <- abkin$server_inputs(input_name, runput_bioexp, i)
        # Create dynamic output name
        # Call the server_outputs function with the dynamic input object
      }


   # output_name <- "outputs"
    abkin$server_outputs("outputs", inputs_abkin, runput_bioexp)

    inputs_imm <- imm$server_inputs("inputs", runput_bioexp)
    imm$server_outputs("outputs", inputs_imm)

    inputs_obs <- obs$server_inputs("inputs", inputs_demo, runput_bioexp)
    outputs_obs <- obs$server_outputs("outputs", inputs_obs)

    inputs_run <- run$server_inputs("inputs", runput_demo, runput_bioexp, inputs_abkin, inputs_imm, inputs_obs)
    outputs_run <- run$server_outputs("outputs", inputs_run, runput_demo)
         })
  })
}