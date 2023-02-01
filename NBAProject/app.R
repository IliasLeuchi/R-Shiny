library(shiny)
library(DT)
library(dplyr)
library(shinydashboard)
library(car)
library(Nba)
#-------------------------------------PAGE---------------------------------------#

joueur =liste_valeurs("player_name")
year = liste_valeurs("season")
team = liste_valeurs("team_abbreviation")
key <- c("Points", "rebonds", "Passes décisives","taille","poids")
value <- c("pts","reb","ast","player_height","player_weight")
my_list2=setNames(value,key)

#-------------------------------------PAGE---------------------------------------#

ui <- dashboardPage( skin = 'black',
header<-dashboardHeader(title= "NBA PROJECT"),

sidebar<-dashboardSidebar(
  
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      menuItem("Donnée", tabName = "données", icon = icon("table")),
      menuItem("Age", tabName = "age", icon = icon("bar-chart-o")),
      menuItem("Joueur", tabName = "joueur", icon = icon("person-running")),
      menuItem("Equipe", tabName = "IE", icon = icon("people-pulling")),
      menuItem("Croisement", tabName = "NDP", icon = icon("basketball"))
     
    )
  ), 

body<-dashboardBody(
    tabItems(
#-------------------------------------PAGE---------------------------------------#
      tabItem(tabName = "donnée",
              fluidRow(
                h3("Voici l'ensemble du jeu de donnée"),
                dataTableOutput("table", width = 1000)
              )    
      ),
      
#-------------------------------------PAGE---------------------------------------#
      
           tabItem(tabName = "age",
              fluidRow(
                box(  title = "Histogramme selon la saison", solidHeader = T,
                      collapsible = T,status="primary",
                      
                      plotOutput("histoAge"),
                
                     
                  selectInput(inputId = "annee", label = "Choisir une annee",
                              choices = year, multiple = FALSE), 
                  
                  sliderInput("bins",
                              "Nombre de découpage:",
                              min = 2,
                              max = 30,
                              value = 15)
                  
                ),
                box( title = "Histogramme selon l'équipe", solidHeader = TRUE,
                     collapsible = TRUE,status="primary",
                     
                     plotOutput("histoAge2"),
                     
                     selectInput(inputId = "team", label = "Choisir une équipe",
                                 choices = team, multiple = FALSE), 
                     
                     sliderInput("bins2",
                                 "Nombre de découpage:",
                                 min = 2,
                                 max = 30,
                                 value = 15),
                     
                ),
                
              )),
      
      
#-------------------------------------PAGE---------------------------------------#
      tabItem(tabName = "joueur",
            fluidRow( 
             column(12,
              box(
                title = "Choix du joueur", 
                solidHeader = TRUE,
                collapsible = TRUE,status="primary",
                
                selectInput(inputId = "joueur", label = "Choisir un joueur",
                             choices = joueur, multiple = FALSE),
              ),
             
           
              box (
                title = "Nombre de saisons entre 1996 et 2022", 
                solidHeader = TRUE,
                collapsible = TRUE,status="primary",
                textOutput("NbSaison")
                ),
             ),
             
             column(12, 
              box (
                title = "Taille et poids", 
                solidHeader = TRUE,
                collapsible = TRUE,status="primary",
                textOutput("TaillePoids")
              ),
             
              box (
                title = "Poits, rebonds, passe D", 
                solidHeader = TRUE,
                collapsible = TRUE,status="primary",
                textOutput("PRPD")
              ),
             )
              
              #tags$img(src="logoBW.png",width="200px",height="200px",alt ="logo NBA"),

            )
            
      ),
#-------------------------------------PAGE---------------------------------------#

tabItem(tabName = "IE",
        fluidRow(
          box(
            title = "Parametres", 
            solidHeader = TRUE,
            collapsible = TRUE,status="primary",
            
            selectInput(inputId = "annee2", label = "Choisir une annee",
                        choices = year, multiple = FALSE), 
            
            selectInput(inputId = "team2", label = "Choisir une équipe",
                        choices = team, multiple = FALSE), 
          ),
            box(
              title = "Info équipe", 
              solidHeader = TRUE,
              collapsible = TRUE,status="primary",
            
            textOutput("statEquipe")
          ),
          
        )    
),

#-------------------------------------PAGE---------------------------------------#

          tabItem(tabName = "NDP",
                  fluidRow(
                    
                    box(
                      title = "Paramètre", 
                      solidHeader = TRUE,
                      collapsible = TRUE,status="primary",
                      
                      selectInput(inputId = "team3", label = "Choisir une équipe",
                                  choices = team,  multiple = F),
                      
                      selectInput(inputId = "annee3", label = "Choisir une annee",
                                  choices = year, multiple = F),

                      checkboxGroupInput("SelecetedVars",
                                         "Choisir deux variables à coiser:",
                                         my_list2, inline = TRUE, selected = c("pts","reb")),
                      
                      
                      actionButton("valide","Go !" ,icon = icon("basketball"))
                    ),
                
                    
                    box(  title = "Nuage de points", 
                          solidHeader = TRUE,
                          collapsible = TRUE,status="primary",
                          plotOutput("NDP"),
                          textOutput("NoData"),
                          
                          ),
                    

                  )
                  
          )
          


#-------------------------------------PAGE---------------------------------------#

   )
  )
#------------------------------------PAGE----------------------------------------#
)


##############################################################################################################################################################################
#SEPARATION ULTIME
##############################################################################################################################################################################


nba = Nba::nba

server <- function(input, output, session) {
    
    
    output$table<- renderDataTable(nba,
    options = list(scrollX = TRUE))

#----------------------------------------------------------------------------------#    
    output$histoAge<- renderPlot({
      age = filter(nba,nba$season==input$annee)[[4]] #les données des age pour une années choisis
      hist(age, breaks = input$bins, xlim = c(15,50), ylim = c(0,200),
           col = '#C9082A', border = F, ylab = "effectifs", cex.main = 1,
           main =paste("Histogramme de l'age des joueurs de la saisosn",input$annee))
 
    })

#----------------------------------------------------------------------------------#    
    table = reactive ({
      filter(nba,nba$player_name==input$joueur)
      
    })
    
    output$NbSaison<- renderText({
      paste(input$joueur,"a joué",length(table()$player_name),"saisons en NBA."
      ,"Il s'agit des saisons",toString(table()$season),".")
      
      })
    
    output$TaillePoids<- renderText({
      paste(input$joueur,"mesure",round(mean(table()$player_height),2),"centimetres et"
            ,"pèse",round(mean(table()$player_weight),2),"kilogrammes.")
      
    })
    
    output$PRPD <- renderText({
      paste(input$joueur,"a marqué en moyenne",round(mean(table()$pts),2),"points,",
            "a obtenu en moyenne",round(mean(table()$reb),2),"rebonds",
            "et a délivré en moyenne",round(mean(table()$ast),2),"passe décisives.")
    })
    
#----------------------------------------------------------------------------------#  
    output$histoAge2<- renderPlot({
      age = filter(nba,nba$team_abbreviation==input$team)[[4]]
      hist(age, breaks = input$bins2, xlim = c(15,50), ylim = c(0,200),
           col = '#C9082A', border = F, ylab = "effectifs", cex.main = 1,
           main =paste("Histogramme de l'age des joueurs pour l'équipe :",input$team))

    })

#----------------------------------------------------------------------------------#     
    # output$joueur2=renderText({ 
    #   
    #   table1 = filter(nba,nba$season==input$annee2 &
    #                     nba$team_abbreviation==input$team2)
    #   
    #   levels(factor(table1$player_name))
    #   })
    
    output$statEquipe = renderText({
      table1 = filter(nba,nba$season==input$annee2 &
                        nba$team_abbreviation==input$team2)
      
      moyReb = round(mean(table1$reb),2)
      moyPts = round(mean(table1$pts),2)
      moyAst = round(mean(table1$ast),2)
      
      
      
      if (dim(table1)[1] == 0){
        paste(input$team2,"n'était pas en NBA lors de la saison",input$annee2,".")
      }
      else {
      paste("Les joueurs de ",input$team2,"ont marqué en moyenne",moyPts,"points, 
             ont obtenu en moyenne", moyReb, "rebonds et ont fait en moyenne",moyAst,
            "passes décisives, pendant la saison",input$annee2,".")
      }
      
    })
#----------------------------------------------------------------------------------#
    
    observe({
      if(length(input$SelecetedVars) > 2){
        updateCheckboxGroupInput(session, "SelecetedVars", selected= tail(input$SelecetedVars,2))
      }
      if(length(input$SelecetedVars) < 1){
        updateCheckboxGroupInput(session, "SelecetedVars")
      }
      
      
    })


    table2= reactive ({
      filter(nba,nba$season==input$annee3 &
            nba$team_abbreviation==input$team3)
      
    })
    
    output$NoData = renderText({
      input$valide
      isolate({
      if (dim(table2())[1] == 0){
        paste(input$team3,"n'était pas en NBA lors de la saison",input$annee3,".")
      }
      
    })
    })
    
    
      output$NDP =renderPlot({
        
       input$valide
        isolate({
          if (dim(table2())[1]>0){
      scatterplot(as.formula(paste0(input$SelecetedVars[1],"~",input$SelecetedVars[2])),
              data = table2(),
              regLine =F, grid =F,smooth = FALSE, legend = FALSE,
              main =paste("Croisement entre les",input$SelecetedVars[1],
                          "et les",input$SelecetedVars[2],
                          '\n',"de l'équipe",input$team3,'\n',
                          "pendant la saison",input$annee3), 
              cex.main = 1,
              boxplot= F,
              col=c("#C9082A"),pch= 20,cex=2,bg = "blue", lwd=2)
          }

    }) 
      
      })
    
    
#----------------------------------------------------------------------------------#  
}

# Run the application 
shinyApp(ui = ui, server = server)
