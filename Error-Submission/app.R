library(shiny)
library(base64enc)
library(DT)

# which fields are text
fieldsText <- c("description", "os_type", "os_version")

# which fields are images
fieldsImages <- c("file1")

# which fields are mandatory
fieldsMandatory <- c("description", "os_type", "os_version", "file1")

# directory where responses get stored
responsesDir <- file.path("responses")
errorDir <- file.path("error-images")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the text responses to a csv file
saveTextData <- function(data) {
  textFileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, textFileName),
            row.names = FALSE, quote = TRUE)
}

# upload all previous text responses submitted into a data frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  textData <- lapply(files, read.csv, stringsAsFactors = FALSE)
  textData <- do.call(rbind, textData)
  textData
}

# upload all previous image responses submitted
errorData <- list.files(errorDir, pattern = ".jpg", full.names = TRUE)

# convert all previous images to b64 so they can later be displayed in a data table

errorb64 <- vapply(errorData, function(img){
  dataURI(mime = "image/jpg", file = img)
}, character(1L))

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "Errors in R - Submission Form",
    sidebarLayout(
      sidebarPanel(
               h3("Error Submission Form"),
               textInput("description", labelMandatory("Description"), ""),
               selectInput("os_type",  labelMandatory("Operating System"),
                           c("",  "Windows", "Mac", "Linux")),
               textInput("os_version", labelMandatory("Operating System Version"), ""),
               fileInput(
                 inputId = "file1",
                 labelMandatory("Upload Screenshot of Error Message"),
                 accept = c('image/jpeg','image/jpg')
               ),
               tags$div(
                 HTML(paste(tags$span(style="color:red", "Please ensure your screenshot is of your entire screen and shows both the error message and line of code to which the error refers to"), sep = ""))
               ),
               br(),
               actionButton("submit", "Submit", class = "btn-primary"),
             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("Thanks, your response was submitted successfully!"),
                 actionLink("submit_another", "Submit another response"),
               )
             )
             ),
             
      mainPanel(
             h3("Previous Errors Submitted"),
             tags$strong("Errors up to July 13th, 2022 have been reviewed"),
             br(),
             br(),
             DT::dataTableOutput("responsesTable"),
          )
        )
  ),
  
  server = function(input, output, session) {
    
    # Only enable the submit button when all mandatory fields are filled out
   
     observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]])
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    # Gather all text response inputs and add timestamp
    formData <- reactive({
      fdata <- sapply(fieldsText, function(x) input[[x]])
      fdata <- c(fdata, timestamp = epochTime())
      fdata <- t(fdata)
      fdata
    })    
    
    # When the Submit button is clicked
      # save the image responses locally
    
    observeEvent(input$submit, {
      
        inFileName1 <- sprintf("%s_%s.jpg",
                              humanTime(),
                              digest::digest(data))
      
        inFile1 <- input$file1
        if (is.null(inFile1))
          return()
        file.copy(inFile1$datapath, file.path(errorDir, inFileName1))
      
      # User experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the text response data and show an error message in case of error
      tryCatch({
        saveTextData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
    # Show the responses in the output table

    output$responsesTable <- DT::renderDataTable({
      
      # Load text response data and add human friendly timestamp 
      data <- loadData()
      data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
      
      # Create a data frame for the submitted screenshots (in b64) of the error 
      errorDataFrame <- data.frame(error_image = sprintf('<img src=%s height="300""></img>', errorb64))
      
      # Combine the images data frames with the text responses
      errorDataFrame <- cbind(errorDataFrame, data)
      
      # Reorder columns 
      errorDataFrame <- errorDataFrame[,c(5,2,3,4,1)]
      
      # Display final response table
      datatable(errorDataFrame, escape=FALSE)
    
    })
  
    # Allow user to download responses
    output$downloadBtn <- downloadHandler(
      filename = function() { 
        sprintf("mimic-google-form_%s.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )    
  }
)