# main file for Shiny brain tumour classifier

library(shiny)
library(shinyjs)

options(shiny.maxRequestSize=30*1024^2) #  for file uploading size
source('scripts/functions.R', echo=TRUE)

updata_folder <<- "up_data/"

ui <- fluidPage(
  useShinyjs(),
  # tags$head(tags$style(HTML("pre { white-space: pre-wrap; }"))),
  
  titlePanel("Brain tumor classifier"),
  helpText("DNA methylation-based classification of central nervous system tumors. 
           For the scientific background and interpretation of the data, please see 
           Capper D, Jones DTW, Sill M, Hovestadt V et al., Nature. 2018 Mar 22; 555(7697):469-474."),
  
  # left panel
  sidebarPanel(
    h4("1 Upload your data files (red and grn chanells) in .idat or .idat.gz format"),
    # Input: Select a file ----
    fileInput("filein", "Choose .idat files",
              multiple = TRUE,
              accept = c(".idat", ".idat.gz")),
    
    disabled(actionButton("clear", "Clear frame")),
    
    # Horizontal line ----
    tags$hr(),
    
    radioButtons("radio_gen","Gender", 
                 c("Male" = "Male", "Female" = "Female","Other/not given" = "Other/not given"), 
                 selected = "Other/not given", inline=T),
    radioButtons("radio_mt","Matherial type", 
                 c("FFPE" = "FFPE", "Frozen" = "Frozen"), inline=T),
   
    tags$hr(), # Horizontal line ----
    h4("2 Validate your datafile"),
    disabled(actionButton("validate", "Validate")),
    br(),
    textOutput("val_text"),
    #textOutput(test), #tmp for debug/mc
    #textOutput("test1"),
    
    # Horizontal line ----
    tags$hr(),
    h4("3 Run classifier and wait for results"),
    disabled(actionButton("run", "Run")),
    
    width = 4
  ),
  
  mainPanel(
    # Output: Report ----
    htmlOutput("report_html"),
    
    hidden(downloadButton("download", "Download"))
  )
)

server <- function(input, output, session) {
 
  sentrix_id <<- NULL # non-reactive global var (will created at infile action)
  check_res <<- NULL
  # text after validation button
  rval_text <- reactiveVal(" ")
  output$val_text = renderText({rval_text() })

  filepath <- reactive({
    #' return path to ONE file + resave with correct names 
    infile <- input$filein
    # resave with correct name - temp filepath doesn works in read.metharray
    if(!dir.exists(updata_folder)){
     dir.create(updata_folder)
    } 
    file.copy(input$filein$datapath, paste0(updata_folder, input$filein$name))
    cat("Copied:",file.exists(paste0(updata_folder, input$filein$name)), "\n")
    cat(paste0(updata_folder, input$filein$name), "\n")
    if (is.null(infile)){
      return(NULL)      
    }
    # return one filename (including _Grn.idat or _Red.idat) for read.metharray()
    paste0(updata_folder, input$filein$name[1])
  })
  
  ### TMP meta data for test output ###----
  output$test <- renderText(({filepath()}))
  #output$test1 <- renderText(({input$filein$name}))
  ###----
  
  # Action after file was uploaded: enable validate and get sentrix id (first)
  observeEvent(input$filein$datapath,
               {
                 enable("validate")
                 enable("clear")
                 fpath <<- filepath()
                 sentrix_id <<- gsub(updata_folder,"",fpath)
                 sentrix_id <<- gsub("_Grn.idat.*|_Red.idat.*","",sentrix_id)
                 cat("Sentrix_id:", sentrix_id, "\n")
               })
  
  # Action for Clear frame button
  observeEvent(input$clear, {
    cat("Event Clear form\n")
    clean_dir(dir = updata_folder)
    reset('filein')
    #removeUI(selector = "#report_html") always
    hide("report_html")
    disable("clear")
    disable("validate") 
    disable("run")
    rval_text(character())
    shinyjs::hide("download")
  })
  
  # Action for Validate button
  observeEvent(input$validate, {
    
    check_res <<- validate_files()
    # return warning messages if need
    val_warn_text <- character()
    if(check_res$Red == T & check_res$Grn == T){
      if(check_res$sentrix_id == F){
        # set new value to reactiveVal
        val_warn_text <- paste0(val_warn_text, "Files must have the same sentrix_id.\n
                                                  Please upload the correct files.")
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Files must have the same sentrix_id!')
        
      } else{
        val_warn_text <- paste0(val_warn_text, "Files verification passed successfully.")
        enable("run")
      }
    } else{
      if(check_res$Red == F){
        val_warn_text <- paste0(val_warn_text, "_Red.idat file not found!\n")
      }
      if(check_res$Grn == F){
        val_warn_text <- paste0(val_warn_text, "_Grn.idat file not found!\n")
      }
      val_warn_text <- paste0(val_warn_text, "Please upload the correct files.")
    }
    rval_text(val_warn_text)
  })
  
  getPage <- function() {
    return(includeHTML("report/report.html"))
  }
  
  observeEvent(input$run, {
    # if not validated - out (second run click f example)
    if(is.null(check_res)){
      # ..text?
      return()
    } # else .. revalidate? - solved by disabling Run
    
    withProgress(message = 'Run classifier..', value = 0, {
      test_pipline(path = fpath)
      
      render_simp_report(path = fpath, sentrix_id = sentrix_id,
                         gender = input$radio_gen, material_type = input$radio_mt)
      
      # run classifier here
      # add output here
      #...
      
      output$report_html <- renderUI({getPage()})
      shinyjs::show("report_html")
    })
    
    # convert final report to pdf
    pagedown:: chrome_print("report/report.html", 
                            output = paste0("report/report_", sentrix_id,".pdf"))
    
    shinyjs::show("download")
    
    cat("Reset app to be ready for next iteration\n")
    clean_dir(updata_folder)
    disable("clean")
    reset('filein')
    disable("validate")
    disable("run")
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("report/report_", sentrix_id,".pdf")
    },
    content = function(file) {
      file.copy(paste0("report/report_", sentrix_id,".pdf"), file)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)