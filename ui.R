### Libraries
library(shiny)
options(shiny.maxRequestSize = 500*1024^2)
library(shinydashboard)
library("readxl")
library(esquisse)
library(vroom)
library(DT)
library(data.table)






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
                                     l'application donne la possibilité 
                                     d'explorer la base importée. 
                                     Elle fournit également un outil pour 
                                     la data-visualisation."),
                                   
                                   p("Sur le menu 'Prédiction', 
                  l'application donne la possibilité de :"),
                                   
                                   tags$ul(
                                     tags$li("Charger une base de donnée d'inscription 
                   d'étudiants (format csv)."), 
                                     tags$li("Avoir une data-visualisation 
                                             de la base après l'ajout d'une colonne de prédiction"), 
                                     tags$li("Télécharger la base de donnée après l'ajout 
                                     d'une colonne de prédiction (en format csv).")
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
                                     tags$li("Si un étudiant à un type de diplÃ´me diffèrent de la 
                   liste ci-dessus, la prédiction ne peut pas Ãªtre faite.")
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