### Libraries
library(shiny)
options(shiny.maxRequestSize = 500*1024^2)
library(shinydashboard)
library(plyr)
library(vroom)
library(caret)
library("readxl")
library(esquisse)
library(tidyverse)
library(DT)
library(data.table)
library("Hmisc")


### Import table 
c <- read_excel("table_def_1.xlsx")

### Import Model
a <-read.csv("mdl_dat_18.csv")
mdl_glm <- readRDS("mdl_glm_18.rds")
a[sapply(a, is.character)] <- lapply(a[sapply(a, is.character)], as.factor)




####################################### Server ################################

server <- function(input, output){ 
  
  
  ####################### Acceuil ####################
  
  output$table1 <- renderTable({
    
    c
    
  })
  
  ##################### Fin Acceuil ###################
  
  
  
  
  
  ##################### Visualisation ####################
  
  
  ### tab_base
  
  b <- reactive({
    
    fich <- input$explore
    k <- read.table(fich$datapath, sep = ",", header = T)
    
  })
  
  observeEvent(input$explore, 
               isolate(output$table <- DT::renderDT({
                 b()
               }, options = list(pageLength = 8, scrollX = TRUE, length))
               )
  )
  
  
  ### Fin tab_base
  
  
  ### tab_visual
  
  data_r <- reactiveValues(data = iris, name = "iris")
  
  observeEvent(input$explore, {
    
    data_r$data <- b()
    esquisse_server(
      
      id = "esquisse",
      data_rv = data_r
      
    )
  })
  
  
  ### Fin tab_visual
  
  
  
  #################### Fin Visualisation ###################
  
  
  
  
  
  ##################### Prediction ####################
  
  
  ###################### tab base
  
  ### user Data cleaning and predicting
  
  b2 <- reactive({
    
    fich <- input$predire
    b <- read.table(fich$datapath, sep = ",", header = T)
    
    ### data cleaning
    
    b[sapply(b, is.character)] <- lapply(b[sapply(b, is.character)], as.factor)
    
    unk_TYP_DIPL <- which(!(b$TYP_DIPL %in% levels(a$TYP_DIPL)))
    unk_CURSUS_LMD <- which(!(b$CURSUS_LMD %in% levels(a$CURSUS_LMD)))
    
    # Etud_sup <- length(unk_COMPOS)+length(unk_TYP_DIPL) ### nombre etudiant supprimÃ©
    
    b$TYP_DIPL[unk_TYP_DIPL] <- NA
    b$CURSUS_LMD [unk_CURSUS_LMD ] <- NA
    
    b <- b[!is.na(b$CURSUS_LMD),]
    b <- b[!is.na(b$TYP_DIPL),]
    b <- b[!is.na(b$DEGETU),]
    
    
    # Predict
    
    pred_glm <- predict(mdl_glm, b)
    
    
    # Export Base
    
    b$RES_Pred <- pred_glm
    b
    
  })### Fin Prediction
  
  observeEvent(input$predire, 
               output$table2 <- DT::renderDT({
                 b2()
               }, options = list(pageLength = 8, scrollX = TRUE, length))
  )
  
  
  ##################### tab_visual
  
  data_r <- reactiveValues(data = iris, name = "iris")
  
  observeEvent(input$predire, {
    
    data_r$data <- b2()
    esquisse_server(
      
      id = "esquisse2",
      data_rv = data_r
      
    )
  })
  
  
  ##################### tab_download
  
  observeEvent(input$predire, 
               output$summ <- renderPrint({
                 summary(b2())
               })
  )
  
  output$dbase <- downloadHandler(
    
    filename = function(){
      
      paste("pred","csv", sep = ".")
    },
    
    content = function(file){
      
      vroom::vroom_write(b2(),file)
    })
  
  
  
  
  
}### Fin Server