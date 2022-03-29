library(shiny)
library(shinymanager)
library(dplyr)
library(rmarkdown)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)

logfile <- "log.csv"
data.folder = "www/data"
choices.site = c("a","b","c")

# for recording the their treatments
# data headers: site, screeningID, age, sex, randomizationID, Treatment, Date
addPID <- function(site, screenID, age, sex, randID, treatment, file, timestamp){
  
  tmpdat <- rbind(read.csv(file.path(data.folder, file)),
                  data.frame(site = site, 
                             screeningID = screenID, 
                             age = age, sex = sex, 
                             randomizationID = randID, 
                             Treatment = treatment, 
                             Date = timestamp))
  
  write.csv(tmpdat, file = file.path(data.folder, file), row.names = F)
  
  return(tmpdat)
}

# check if the patient has been recorded in the data file already
# return F if not and return the data if recorded
existQ <- function(site, pid, age, sex){
  data.file <- paste0("data-",site,".csv")
  tmp.dat <- read.csv(file.path(data.folder,data.file))
  tmp <- tmp.dat[tmp.dat$screeningID == pid & tmp.dat$age == age & tmp.dat$sex == sex,]
  tmp2 <- tmp.dat[tmp.dat$screeningID == pid & tmp.dat$age != age | tmp.dat$sex != sex,]
  outlist <- list()
  if(nrow(tmp)==1){ 
    outlist$exist <- TRUE
    outlist$data <- tmp
  }else{
    outlist$exist <- FALSE
    outlist$data <- NULL
  }
  
  # for checking if the entered PID exist but age and sex are not matched.
  if(nrow(tmp2==1)){
    outlist$exist2 <- TRUE
  }else{
    outlist$exist2 <- FALSE
  }
  
  return(outlist)
}

## get treatment (Subject No)
# rand headers : site, randomization, Treatment
getTreatment <- function(site, PID){
  rand.file <- paste0("rand-",site,".csv")
  data.file <- paste0("data-",site,".csv")
  
  output <- list()
  
  # how many patients have been added?
  tmp.data <- read.csv(file.path(data.folder,data.file))
  is.assigned <- PID %in% tmp.data[,2]
  output$Assigned <- is.assigned
  
  npid <- nrow(tmp.data)
  tmp.rand <-read.csv(file.path(data.folder,rand.file))
  n.rand <- nrow(tmp.rand)
  
  # get the new treatment
  if ((is.assigned == F) && (npid+1 <= n.rand)){
    output$site <- site
    output$screeningID <- PID
    output$randomizationID <- tmp.rand[npid+1,2]
    output$Treatment <- tmp.rand[npid + 1, 3]
  } else {
    output$Treatment <- NULL
  }
  return(output)
}

## check screening ID is in correct format
check_screening_ID <- function(id){
  if(is.na(id)) return (FALSE)
  if(!is.numeric(id)) return (FALSE)
  xx = unlist(strsplit(as.character(id), split = ''))
  if(!xx[1] == '1') return (FALSE)
  if(!length(xx) == 5) return (FALSE)
  return (TRUE)
}

## check age is in thre correct format
check_age <- function(age){
  if(is.null(age)) return(FALSE)
  if(is.na(age)) return(FALSE)
  if(!is.numeric(age)) return(FALSE)
  return(TRUE)
}


# the main UI
ui <- fluidPage(
  titlePanel(tagList(
    tags$h1("Random"),
    pickerInput(
      inputId = "site",
      label = "Site : ",
      choices = choices.site
    )
  )),  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      # numericInput("PID","Screening ID (1XXXX)",value = NULL),
      # radioButtons("sex","Sex:",c("Male","Female","Other"),inline=T),
      # numericInput("age","Age:",value = NULL,min=18,max = 50),
      uiOutput('reset_input'),
      actionButton("validate.button",label = "Validate"),
      useShinyalert()
    ),
    mainPanel(
      # textOutput("treatment"),
      dataTableOutput("site.data")
    )
  )
)### end UI



# server side
server <- function(input, output, session){
  
  
  
  values <- reactiveValues()
  
  observe({
    if(is.null(input$sex)){
      shinyjs::disable("validate.button")
    }else{
      shinyjs::enable("validate.button")
    }
  })
  
  output$reset_input <- renderUI({
    div(
      numericInput("PID","Screening ID (1XXXX)",value = NULL),
      radioButtons("sex","Sex:",c("Male","Female","Other"),inline=T),
      numericInput("age","Age:",value = NULL,min=18,max = 50)
    )
    
  })
  
  #check if validate.button is clicked
  observeEvent(input$validate.button,{
    values$site <- input$site

    
    values$site.file <- paste0("rand-",values$site,".csv")
    values$data.file <- paste0("data-",values$site,".csv")
    values$PID <- input$PID
    
    values$age <- input$age
    values$sex <- input$sex
    
    ## check if the inputs are ok
    values$check.screenID <- check_screening_ID(input$PID)
    values$check.age <- check_age(input$age)
    
    if(values$check.screenID == T && values$check.age == T && between(values$age,18,50) ==T){  
      ask_confirmation(
        inputId = "validate.confirm",
        type = "warning",
        title = "Please confirm your input!",
        text = tags$div(

          tags$p(),tags$b("Screening ID: PLT-"),values$site,"-",values$PID,
          tags$p(),tags$b("Age: "),values$age,tags$b("Sex: "),values$sex,
          tags$p(),
          # tags$p(),tags$b("Randomization ID: PLT-"),values$exist.data$site,"-",
          #        sprintf("%03d", as.numeric(values$exist.data$randomizationID)),
          # tags$p(),tags$b("Treatment: "),values$exist.data$Treatment,
          # tags$p(),tags$b("Date and Time: "),values$exist.data$Date,tags$p(),tags$p(),
          "Click of OK to proceed or click Cancel to go back."
        ),
        btn_labels = c("Cancel", "OK"),
        btn_colors = c("#FE642E", "#04B404"),
        html = TRUE
      )
    }else{
      ask_confirmation(
        inputId = "checking.input",
        type = "error",
        title = "Please check your inputs!",
        text = "Screeing ID must be 5 digits and Age must be between 18 and 50 years.",
        btn_labels = c(NULL,"OK")
      )
    }
    
  })
  
  
  # check if the OK/Cancel button in validate window is clicked 
  observeEvent(input$validate.confirm,{
    
    values$validate.confirm <- input$validate.confirm
    
    ## click ok after in the validate windows
    if(values$validate.confirm == TRUE){
      # check if patient has been randomized
      # matching pid/age/sex/site
      exist.check <- existQ(pid = values$PID,
                            age = values$age,
                            sex = values$sex,
                            site = values$site)
      
      values$exist.data <- exist.check$data
      values$exist.check <- exist.check$exist
      values$exist2.check <- exist.check$exist2
      
      treatment.lst <- getTreatment(values$site,values$PID)
      values$assigned <- treatment.lst$Assigned
      values$screeningID <- treatment.lst$screeningID
      values$randomizationID <- treatment.lst$randomizationID
      
      values$treatment <- treatment.lst$Treatment
      values$timestamp <- date()
      
      
      # if all inputs are matched with the existed one then show the information as well as the download button
      if(values$exist.check == TRUE){
        ask_confirmation(
          inputId = "exist.info",
          type = "success",
          title = "This patient has been randomized!",
          text = tags$div(
            tags$b("Registered by: User"),
            tags$p(),tags$b("Screening ID: PLT-"),values$exist.data$site,"-",values$exist.data$screeningID,
            tags$p(),tags$b("Age: "),values$exist.data$age,tags$b("Sex: "),values$exist.data$sex,
            tags$p(),
            tags$p(),tags$b("Subject No: PLT-"),values$exist.data$site,"-",
            sprintf("%03d", as.numeric(values$exist.data$randomizationID)),
            tags$p(),tags$b("Treatment: "),values$exist.data$Treatment,
            tags$p(),tags$b("Date and Time: "),values$exist.data$Date,tags$p(),
            tagList(downloadButton("downloadPDF","Download as PDF"))
          ),
          btn_labels = c(NULL, "OK"),
          btn_colors = c("#04B404", "#04B404"),
          html = TRUE
        )
      }else{
        # only PID that match with the existed one
        if(values$exist2.check == TRUE && values$assigned == TRUE){
          
          ask_confirmation(
            inputId = "exist2.info",
            type = "warning",
            title = "This patient has been randomized but the sex or age does not match with the existing record.",
            text = "Please check your inputs.",
            btn_labels = c(NULL, "OK"),
            btn_colors = c("#FE642E", "#04B404")
          )
        }else{
          # if PID is new then add
          if(values$assigned == FALSE){
            
            values$newdata <- addPID(site = values$site, 
                                     screenID = values$PID,
                                     age = values$age,
                                     sex = values$sex, 
                                     randID = values$randomizationID,
                                     treatment = values$treatment, 
                                     file = values$data.file, 
                                     timestamp = values$timestamp
            )
            
            ask_confirmation(
              inputId = "add.new",
              type = "success",
              title = values$treatment,
              text = tags$div(
                tags$p(),tags$b("Screening ID: PLT-"),values$site,"-",values$PID,
                tags$p(),tags$b("Age: "),values$age,tags$b("Sex: "),values$sex,
                tags$p(),
                tags$p(),tags$b("Subject No: PLT-"),values$site,"-",
                sprintf("%03d", as.numeric(values$randomizationID)),
                tags$p(),tags$b("Treatment: "),values$treatment,
                tags$p(),tags$b("Date and Time: "),values$timestamp,tags$p(),
                tagList(downloadButton("downloadPDF2","Download as PDF"))
              ),
              btn_labels = c(NULL, "OK"),
              btn_colors = c("#FE642E", "#04B404"),
              html = TRUE
            )
            
          }
          
        }
        
        
      }## else
      
      
    }
  })
  
  # for reporting the existed
  output$downloadPDF <- downloadHandler(
    
    filename = function() {
      paste0('PLT-',values$site,'-',values$PID,'.pdf',sep='')
      #format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".pdf")
    },
    #content in pdf file look more details in report.Rmd
    content = function(file) {
      #Use template form report-pdf.Rmd
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')
      
      #render pdf doc by using report-pdf.Rmd template and latex engine is xelatex
      outpdf <- render(input = 'report.Rmd',
                       output_format = pdf_document(latex_engine = "xelatex") 
      )
      #rename file
      file.rename(outpdf, file)
    }
  )
  
  # for reporting the new PID
  output$downloadPDF2 <- downloadHandler(
    
    filename = function() {
      paste0('PLT-',values$site,'-',values$PID,'.pdf',sep='')
      #format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".pdf")
    },
    #content in pdf file look more details in report.Rmd
    content = function(file) {
      #Use template form report-pdf.Rmd
      src <- normalizePath('report2.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report2.Rmd')
      
      #render pdf doc by using report-pdf.Rmd template and latex engine is xelatex
      outpdf <- render(input = 'report2.Rmd',
                       output_format = pdf_document(latex_engine = "xelatex") 
      )
      #rename file
      file.rename(outpdf, file)
    }
  )
  
  
  ### clear the input fields
  observeEvent(input$exist.info,{
    
    values$exist.info <- input$exist.info
    
    if(values$exist.info == FALSE){
      output$reset_input <- renderUI({
        div(
          numericInput("PID","Screening ID (1XXXX)",value = NULL),
          radioButtons("sex","Sex:",c("Male","Female","Other"),inline=T),
          numericInput("age","Age:",value = NULL,min=18,max = 50)
        )
      })
    }
    
  })
  
  ### clear the input fields
  observeEvent(input$exist2.info,{
    
    values$exist2.info <- input$exist2.info
    
    if(values$exist2.info == FALSE){
      output$reset_input <- renderUI({
        div(
          numericInput("PID","Screening ID (1XXXX)",value = NULL),
          radioButtons("sex","Sex:",c("Male","Female","Other"),inline=T),
          numericInput("age","Age:",value = NULL,min=18,max = 50)
        )
      })
    }
    
  })
  
  
  ### clear the input fields
  observeEvent(input$add.new,{
    
    values$add.new <- input$add.new
    
    if(values$add.new == FALSE){
      output$reset_input <- renderUI({
        div(
          numericInput("PID","Screening ID (1XXXX)",value = NULL),
          radioButtons("sex","Sex:",c("Male","Female","Other"),inline=T),
          numericInput("age","Age:",value = NULL,min=18,max = 50)
        )
      })
    }
    
  })
  
  data.table <- reactivePoll(                              1000, session,
                             # This function returns the time that log_file was last modified
                             checkFunc = function() {
                               data_name <- paste0("data-",input$site,".csv")
                               if (file.exists(file.path(data.folder, data_name)))
                                 file.info(file.path(data.folder, data_name))$mtime[1]
                               else
                                 ""
                             },
                             # This function returns the content of log_file
                             valueFunc = function() {
                               data_name <- paste0("data-",input$site,".csv")
  read.csv(file.path(data.folder, data_name))
                             }
  )

  output$site.data <- renderDataTable({
    data.table()
  })
  
  
}### end SERVER


# run the app
shinyApp(ui, server)
