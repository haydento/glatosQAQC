
# code borrowed from this: https://stackoverflow.com/questions/56301940/import-file-and-append-to-previously-loaded-file-in-r-shiny
# https://shiny.rstudio.com/articles/generating-reports.html
# https://shiny.rstudio.com/gallery/file-download.html
# https://community.rstudio.com/t/color-cells-in-dt-datatable-in-shiny/2288/3
# https://deanattali.com/201library(shiny)
# https://stackoverflow.com/questions/31464111/r-shiny-data-table-content-with-html-tags
# https://rstudio.github.io/DT/shiny.html
library(glatosQAQC)
library(DT)
library(dplyr)
library(data.table)


options(shiny.maxRequestSize = 20*1024^2) 


vdat_call <- normalizePath(check_vdat())


# Shiny #
########################################################################
# start shiny app
ui <- fluidPage(

  # title
  titlePanel("Receiver QAQC"),
  sidebarLayout(
    sidebarPanel(width = 2,
                 textInput("recorder", "Data recorder", placeholder = "Bob Smith"),
                 textInput("study_code", "Study code", placeholder = "HECWL"),
                 selectInput("tz", "Time Zone", c("", "Eastern", "Central")),
                 textInput("test_tag", "Full test tag ID", placeholder = "A69-9002-1234"),
                 textInput("VUE", "VUE version", placeholder = "3.1.2"),             
                 radioButtons(
                   inputId = "dtype",
                   label = "action:",
                   choices = list("Download" = "download",
                                  "Initialize" = "initialize"),
                   inline = TRUE),                 
                 fileInput(inputId = "file1", label = "Choose vrl file",  multiple = TRUE, accept = c(".vdat", ".vrl")),
                 downloadButton("downloadData", "Download")
                 
                 ),
  
    mainPanel(width = 10,

              tabsetPanel(type = "tabs",
                          tabPanel("rec data",
                                  #tableOutput('data.table1') %>%
                                   DT::DTOutput('data.table1') %>%
                                     shinycssloaders::withSpinner(color="#0dc5c1", size = 1.5, type = 3, color.background = "white")),
                          tabPanel("metadata",
                                   DT::DTOutput('clk') %>%
                                     shinycssloaders::withSpinner(color = "#0dc5c1", size = 1.5, type = 3, color.background = "white"), width = "50%", height = "50%")
                          )
              )
  )
)


##--------------------server-----------------------

# code borrowed from this: https://stackoverflow.com/questions/56301940/import-file-and-append-to-previously-loaded-file-in-r-shiny
# https://shiny.rstudio.com/articles/generating-reports.html
# https://shiny.rstudio.com/gallery/file-download.html
# https://community.rstudio.com/t/color-cells-in-dt-datatable-in-shiny/2288/3
# https://deanattali.com/2015/04/21/r-package-shiny-app/

# https://towardsdatascience.com/create-interactive-map-applications-in-r-and-r-shiny-for-exploring-geospatial-data-96b0f9692f0f
# https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table
# https://stackoverflow.com/questions/59449396/change-pagetype-in-dt-pagination-in-r-shiny-module
server <- function(input, output, session) {

  metadata <- reactive({
    data.table(field = c("Data recorder",
                 "Study code",
                 "Time Zone",
                 "Full test tag ID",
                 "VUE version"),
               value = c(input$recorder,
                         input$study_code,
                         input$tz,
                         input$test_tag,
                         input$VUE))
  }
  )
  
  
  
  # kill R session when browser or tab are closed. 
  session$onSessionEnded(stopApp)
  df_all <- reactiveVal(NULL)
 
#output$data.table1 <- DT::renderDT({
  foo <-reactive({
    tst <- isolate(input$dtype)
    req(input$file1)
    olddf <- isolate(df_all())
    if(is.null(input$file1)) return(NULL)
#    print(glimpse(input$file1$datapath))
#    print(glimpse(input$file1$name))
   # print(glimpse(input$file1))
#    df <- glatosQAQC::process_table(fls = input$file1$datapath, nme = input$file1, action = tst, mrk_params = vdat_call, work_dir = tempdir())
    df <- glatosQAQC::process_table(fls = input$file1, action = tst, vdat_pth = vdat_call)
 #   print(glimpse(olddf))
    #  print(glimpse(df))
   # print(glimpse(input$file1))
    df <- dplyr::bind_rows(olddf, df)
    isolate(df_all(df))
    return(df)
  })

  #output$data.table1 <- renderTable({QAQC(foo())})
  
  output$data.table1 <- DT::renderDT({QAQC(foo())},
                                     option = list(paging = FALSE), escape = FALSE, server = FALSE)
  
  # https://shiny.rstudio.com/reference/shiny/1.0.5/downloadHandler.html
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$study_code, "_", input$dtype, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      l <- list(detections = foo(), metadata = clk())
      excel_QAQC(input = l, output = file)  
    }
  )

  # clock comparison
  clk <- reactive({
    rbind(time_compare(), metadata())
  })
  
  output$clk <- DT::renderDT({clk()}, escape = FALSE, server = TRUE)
}

shinyApp(ui, server)


