# main file for Shiny brain tumour classifier

library(shiny)
library(shinyjs)

options(shiny.maxRequestSize=30*1024^2) #  for file uploading size
source('scripts/functions.R', echo=TRUE)

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
   
    # Horizontal line ----
    tags$hr(),
    
    radioButtons("radio_gen","Gender", c("Male" = "Male", "Female" = "Female",
                                         "Other" = "Other"), inline=T),
    radioButtons("radio_mt","Matherial type", c("FFPE" = "FFPE", "Frozen" = "Frozen",
                                         "Other" = "Other"), inline=T),
   
    tags$hr(), # Horizontal line ----
    h4("2 Validate your datafile"),
    disabled(actionButton("validate", "Validate")),
    br(),
    textOutput("val_text"),
    #textOutput(test),//
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
    
    disabled(downloadButton("download", "Download"))
  )
)

server <- function(input, output, session) {
  
  #disable(input$validate)
  #disable(input$run)  

  # text after validation button
  rval_text <- reactiveVal(" ")
  output$val_text = renderText({rval_text() })

  filepath <- reactive({
    infile <- input$filein
    # resave with correct name - temp filepath doesn works in read.metharray
    if(!dir.exists("up_data/")){
     dir.create("up_data")
    } 
    file.copy(input$filein$datapath, paste0("up_data/", input$filein$name))
    cat("Copied:",file.exists(paste0("up_data/", input$filein$name)))
    cat(paste0("up_data/", input$filein$name))
    if (is.null(infile)){
      return(NULL)      
    }
    # return one filename (including _Grn.idat or _Red.idat) for read.metharray()
    paste0("up_data/", input$filein$name[1])
  })
  
  ### meta data for test output ###----
  output$test <- renderText(({filepath()}))
  #output$test1 <- renderText(({input$filein$name}))
  ###----
  
  # enable Validate button after file was uploaded
  observeEvent(input$filein$datapath,
               {
                 enable("validate")
               })
  
  # Action for Validate button
  observeEvent(input$validate, {
    
    fpath <<- filepath()
    check_res <- validate_files()
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
    
    withProgress(message = 'Run classifier..', value = 0, {
      test_pipline(path = fpath)
      
      render_simp_report(path = fpath, gender = input$radio_gen, 
                         material_type = input$radio_mt)
      
      # run classifier here
      # add output here
      #...
      
      output$report_html <- renderUI({getPage()})
      
    })
    
    # convert final report to pdf
    pagedown:: chrome_print("report/report.html", output = "report/report_final.pdf")
    
    enable("download")
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "report.pdf"
    },
    content = function(file) {
      file.copy("report/report_final.pdf", file)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)