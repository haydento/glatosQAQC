
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


# determine OS and set vdat call
os <- Sys.info()['sysname']

if(os == "Linux"){
  vdat_call <- system.file("exec/vdat_linux", package = "glatosQAQC")}

# if vdat is installed on windows machine via FATHOM, then use it instead of older version bundled with package
if(os == "Windows"){
  pths <- c("C:/Program Files/Innovasea/Fathom/vdat.exe", "vdat.exe", "vdat")
  pths <- Sys.which(pths)
  vdat_call <- pths[pths != ""][1]

  if(!is.na(vdat_call)){message("using FATHOM vdat.exe")}
  if(is.na(vdat_call)){message("using packaged vdat.exe")}

  if(is.na(vdat_call)){
    vdat_call <- system.file("exec/vdat.exe", package = "glatosQAQC")
  }

}

 

if(os == "Darwin"){
  vdat_call <- system.file("exec/vdat_mac", package = "glatosQAQC")}

if(!is.null(vdat_call)){
    vdat_call <-  normalizePath(vdat_call)
}


# Shiny #
########################################################################
# start shiny app
ui <- fluidPage(

  # title
  titlePanel("Receiver QAQC"),
  sidebarLayout(
    sidebarPanel(width = 2,
                 radioButtons(
                   inputId = "dtype",
                   label = "action:",
                   choices = list("Download" = "down",
                                  "Initialize" = "init"),
                   inline = TRUE),                 
                 fileInput(inputId = "file1", label = "Choose vrl file",  multiple = TRUE, accept = ".vrl"),
                 downloadButton("downloadData", "Download")
                 
                 ),
  
    mainPanel(width = 10,

              tabsetPanel(type = "tabs",
                          tabPanel("rec data",
                                  #tableOutput('data.table1') %>%
                                   DT::DTOutput('data.table1') %>%
                                     shinycssloaders::withSpinner(color="#0dc5c1", size = 1.5, type = 3, color.background = "white")),
                          tabPanel("computer time sync",
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
    print(glimpse(input$file1))
    df <- glatosQAQC::process_table(fls = input$file1$datapath, nme = input$file1, action = tst, mrk_params = vdat_call, work_dir = tempdir())
    
 #   print(glimpse(olddf))
 #   print(glimpse(df))
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
      paste0("receiver_download_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".csv")
    },
    content = function(file) {
            write.csv(foo(), file)
    }
  )

  # clock comparison
  clk <- reactive({
    time_compare()
  })
  
  
  output$clk <- DT::renderDT({clk()}, escape = FALSE, server = TRUE)
}

shinyApp(ui, server)


