########################################

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
library("rstudioapi")


setwd(dirname(getActiveDocumentContext()$path))   



### Import table 
c <- read_excel("table_def_1.xlsx")

### Import Model
a <-read.csv("mdl_dat_18.csv")
mdl_glm <- readRDS("mdl_glm_18.rds")
a[sapply(a, is.character)] <- lapply(a[sapply(a, is.character)], as.factor)


#------------------------------------------------------------------------------------------------#

###################################### Dashboard Interface

ui  <- dashboardPage( skin = "yellow",
                      
                      ### Header                  
                      dashboardHeader(title = "Prédiction de la réussite" , titleWidth = 250
                                      
                      ), 
                      
                      
#------------------------------------------------------------------------------------------------#
                      

                      ### Menu
                      dashboardSidebar( width = 250,                                             
                                        
                                        sidebarMenu(
                                          
                                          menuItem("Acceuil", tabName = "home", icon = icon("home")),
                                          menuItem("Visualisation de la base", tabName = "visual", icon = icon("poll")),
                                          menuItem("Prédiction", tabName = "pred", icon = icon("brain")),
                                          menuItem("Contact", tabName = "contact", icon = icon("phone"))
                                          
                                          
                                        )
                      ), 
                      
                      
#------------------------------------------------------------------------------------------------#
                      
                      
                      ### Page
                      dashboardBody(
                        
                        ### SubPage
                        
                        tabItems(
                          
                          
                          ### Page Acceuil
                          
                          tabItem( tabName = "home",img(src = "Logo_color_30.png", 
                                                        width = 210, height = 75),
                                   h1(em("Bienvenue !"), align = "center", style = "color:orange"),
                                   br(),
                                   
                                   p("Bienvenue à l'interface de l'application de prédiction de la réussite."),
                                   
                                   h4("Utilisation Prévue", style = "color:navy"),
                                   
                                   p("Cette application est concue afin de donner des prédictions 
                   sur le résultat final des étudiants de l'université de Paris 
                   Saclay, en se basant sur leur situation académique et social 
                   antérieure enregistrés sur les bases d'inscription."),
                                   
                                   
                                   h4("Description de l'interface", style = "color:navy"),
                                   
                                   p("Le menu Accueil donne une description générale de 
                           l'application.", code("Un guide d'utilisation est 
                           fourni dans la section 'Comment utiliser?")),
                                   p("Sur le menu ' Visualisation ', 
                                     l’application donne la possibilité 
                                     d’explorer la base importée. 
                                     Elle fournit également un outil pour 
                                     la data-visualisation."),
                                   
                                   p("Sur le menu 'Prédiction', 
                  l'application donne la possibilité de :"),
                                   
                                   tags$ul(
                                     tags$li("Charger une base de donnée d'inscription 
                   d'étudiants (format csv)."), 
                                     tags$li("Avoir une data-visualisation 
                                             de la base après l’ajout d’une colonne de prédiction"), 
                                     tags$li("Télécharger la base de donnée après l’ajout 
                                     d’une colonne de prédiction (en format csv).")
                                   ),
                                   
                                   h4("Comment utiliser? (Important)", style = "color:navy"),
                                   
                                   tags$ul(
                                     tags$li("La base d'inscription doit contenir impérativement les 
                   colonnes suivantes :")
                                   ),
                                   fluidRow(box(tableOutput("table1"), status = "primary")),
                                   
                                   tags$ul(
                                     tags$li("Il faut que le nom et le type des variables correspond 
                   exactement à celui indiqué dans le tableau ci-dessus"),
                                     tags$li("La base d'inscription doit étre nettoyé de valeurs manquantes 
                   sur les colonnes indiqué ci-dessus avant son importation.")
                                   ), 
                                   
                                   h4("Remarque", style = "color:red"),
                                   
                                   tags$ul(
                                     tags$li("La base d'inscription peut contenir d'autres colonnes 
                   en plus des colonnes nécessaires mentionnées en dessus."), 
                                     tags$li("Si un étudiant à un type de diplôme diffèrent de la 
                   liste ci-dessus, la prédiction ne peut pas être faite.")
                                   )
                                   
                          ),
                          
                          
           #---------------------------------------------------------------------#    
                          
                          ### Page Visualisation
                          
                          tabItem( tabName = "visual", img(src = "Logo_color_30.png",width = 210, height = 75),
                                   h1(em("Explorer votre base de données"), align = "center", style = "color:orange"),
                                   br(),
                                   br(),
                                   tabBox(width=12,
                                          tabPanel(title = "Base de donnée",
                                                   fileInput("explore", "Charger la base de donnée a visualiser",
                                                             buttonLabel = "Importer"), 
                                                   br(),
                                                   DT::dataTableOutput(outputId = "table")
                                          ),
                                          tabPanel(title = "Visualisation",
                                                   tags$div(
                                                     esquisse_ui(
                                                       id = "esquisse", 
                                                       header = FALSE, # dont display gadget title
                                                       container = esquisseContainer(height = "700px", width = "850px")
                                                       
                                                     )
                                                   )
                                                   
                                          )
                                   )
                          ),
                          
           
             #------------------------------------------------------------------------------#
                          
   
                          ### Page Prediction
                          
                          tabItem( tabName = "pred", img(src = "Logo_color_30.png",
                                                         width = 210, height = 75),
                                   h1(em("Faire vos prédictions !"), align = "center", style = "color:orange"),
                                   br(),
                                   br(),
                                   
                                   
                                   
                                   tabBox(width=12,
                                          tabPanel(title = "Base de donnée", style = "orange",
                                                   fileInput("predire", "Charger la base de donnée à prédire",
                                                             buttonLabel = "Importer"),
                                                   br(),
                                                   DT::dataTableOutput(outputId = "table2")
                                          ),
                                          tabPanel(title = "Visualisation",
                                                   esquisse_ui(
                                                     id = "esquisse2",
                                                     header = FALSE, # dont display gadget title
                                                     container = esquisseContainer(height = "700px", width = "850px")
                                                   )
                                          ),
                                          tabPanel(title = "Infos et Télechargement",
                                                   downloadButton("dbase", 
                                                                  label = "Telecharger la base de donnée predi",
                                                                  icon = shiny::icon("download")),
                                                   br(),
                                                   verbatimTextOutput("summ")
                                          )
                                   ),
                                           
                          ),
                         
            
                   #------------------------------------------------------------------------------#
                          
                          ### Page Contact
           
                          tabItem( tabName = "contact", img(src = "Logo_color_30.png",width = 210, height = 75),
                                   h1(em("Contactez nous !"), align = "center", style = "color:orange"),
                                   br(),
                                   br()
                          )
    
           
                        )### Fin SubPage
                      )### Fin DashboardBody                                                                     
)### Fin Dashboard Interface


####################################### end interface #########################





              #------------------------------------------------------------------------------#






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
    
    # Etud_sup <- length(unk_COMPOS)+length(unk_TYP_DIPL) ### nombre etudiant supprimé
    
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


# Run the application 
shinyApp(ui = ui, server = server)