# QAQC app UI and Server logic.  Opens vrl/vdat files, extracts summary info and produces table of output, interactive figure of integrated tag operation


# shiny app UI function
QAQC_ui <- function(){

  `div` = NULL # due to NSE notes in R CMD check
  
  # Shiny UI
  shiny::fluidPage(

    # title
    shiny::titlePanel("Receiver QAQC"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 2,
        shiny::textInput("recorder", "Data recorder", placeholder = "Bob Smith"),
        shiny::textInput("study_code", "Study code", placeholder = "HECWL"),
        shiny::selectInput("tz", "Time Zone", c("", "Eastern", "Central")),
        shiny::textInput("test_tag", "Full test tag ID", placeholder = "A69-9002-1234"),
        shiny::textInput("VUE", "VUE version", placeholder = "3.1.2"),             
        shiny::radioButtons(
          inputId = "dtype",
          label = "action:",
          choices = list("Download" = "download",
            "Initialize" = "initialize"),
          inline = TRUE),
        shiny::radioButtons(
          inputId = "battR",
          label = "battery replaced?",
          choices = list("Yes" = "yes",
            "No" = "no"),
          inline = TRUE),
        shiny::fileInput(inputId = "file1", label = "Choose vrl or vdat file",  multiple = TRUE, accept = c(".vdat", ".vrl")),
        shiny::div(
          style = "display: flex; flex-direction: column; align-items: stretch; width: 100%;",

          # make download button fit the entire sidepanel
          shiny::downloadButton("downloadData", "Download", style = "width: 100%;"),

          # make spacer
          shiny::div(style = "margin-bottom: 50px;"),
          
          shiny::actionButton("close_button", "Exit session", class = "btn-danger", style = "width: 100%;")
        )
      ),
  
      shiny::mainPanel(width = 10,

        shiny::tabsetPanel(type = "tabs",
          shiny::tabPanel("Receivers",
            DT::DTOutput('data.table1') |>
              shinycssloaders::withSpinner(color="#0dc5c1", size = 1.5, type = 3, color.background = "white")),
          shiny::tabPanel("Metadata",
            DT::DTOutput('clk') |>
              shinycssloaders::withSpinner(color = "#0dc5c1", size = 1.5, type = 3, color.background = "white"), width = "50%", height = "50%"),

          shiny::tabPanel("Integrated Tag",
            echarts4r::echarts4rOutput("I_tag", width = "100%", height = "80vh") |>
              shinycssloaders::withSpinner(color = "#0dc5c1", size = 1.5, type = 3, color.background = "white"), width = "50%", height = "50%")
        )
      )
    )
  )
}


##--------------------server-----------------------

# Shiny app server function
# revised 2026-06-25.  code bits from google!
QAQC_server <- function(input, output, session) {

  foo = observeEvent = stopApp = NULL # due to NSE notes in R CMD check
  
  # create vdat call
  vdat_call <- normalizePath(vdat_check())

  metadata <- shiny::reactive({
    clk(
      recorder = input$recorder,
      code = input$study_code,
      tz = input$tz,
      tag = input$test_tag,
      vue = input$VUE)
  }
  )
  
  # create reativeVal object to hold data on each iteration for appending
  foo1 <- shiny::reactiveVal(NULL)

  # create reactiveVal object to hold data for shiny input on each iteration
  server_input <- shiny::reactiveVal(NULL)
  
  # Observable event triggered on upload of files
  # output is a list of vdat S7 objects (one object for each receiver file)
  # each object in list is a upload event.
  shiny::observeEvent(input$file1, {
    shiny::req(input$file1)

    # creates vdat object for each file. S7 constructor validates each object and takes care of any missing stuff as well as enforces data types
    df <- vdat(shiny_in = input$file1, schema = vdat_schema(), battery = input$battR, action = input$dtype, vdat_pth = vdat_call)
    
    # creates and updates list each time a batch of receivers are submitted
    current <- foo1()
   
    # combines old and new data into a new class that consist of a list of vdat objects
    current <- vdat_c(current, df)

    # same current_list to reactive value for safe keeping
    foo1(current)
    
  })

  # display output summary table
  output$data.table1 <- DT::renderDT({
    shiny::req(input$file1)
    
    QAQC(tableize(foo1()))},
    option = list(paging = FALSE), escape = FALSE, server = FALSE)

  # create an interactive plot of integrated tag info
  output$I_tag <- echarts4r::renderEcharts4r({
    shiny::req(foo1())
    e_plot(x = foo1())
  })

  # display metadata 
  output$clk <- DT::renderDT({
    QAQC(metadata())},
    escape = FALSE, server = TRUE)
  
  # https://shiny.rstudio.com/reference/shiny/1.0.5/downloadHandler.html
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste0(input$study_code, "_", input$dtype, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      temp_dir <- tempfile()
      dir.create(temp_dir)
      file.copy(foo1()@meta_all$og_datapath, file.path(temp_dir, foo1()@meta_all$name))
      l <- excel_class(lst = list(Receivers = tableize(foo1()), Metadata = tableize(metadata())))
      QAQC(x = l, output = file.path(temp_dir, paste0(input$study_code, "_", input$dtype, format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".xlsx")))
      htmlwidgets::saveWidget(e_plot(foo1()), file = file.path(temp_dir, paste0(input$study_code, "_", input$dtype, format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".html")), selfcontained = TRUE)
            
      zip_em <- dir(temp_dir, full.names = TRUE)
      zip::zip(zipfile = file,
        files = zip_em,
        root = temp_dir,
        mode = "cherry-pick")
      
    }
  )

 
  # close session when done.
  observeEvent(input$close_button, {
    session$close()
    Sys.sleep(0.5)
    stopApp()
  })
  
}


