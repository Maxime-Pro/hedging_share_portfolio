#library(tidyr)
#library(dplyr)
library(readr)
library(tidyverse)
library(caret)
library(leaps)
library(quantmod)
library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(DT)
library(shinycssloaders)
library(stringi) 

options(max.print=999999)
options(DT.options = list(pageLength = 7))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------SOMMAIRE (environ)------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------I) PRE-FONCTION-------------------------50/180-----------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------II) PARTIE GRAPHIQUE---------------------180/560----------------------------------------------------------------------
#------------------------------------------------------------------a) PRESENTATION-----------------------210/245---------------------------------------------------------------------
#------------------------------------------------------------------b) MON PORTEFEUILLE-------------------245/280---------------------------------------------------------------------
#------------------------------------------------------------------c) COUVERTURE DE MON PORTEFEUILLE-----280/3600--------------------------------------------------------------------
#------------------------------------------------------------------d) EVOLUTION DU COURS DE BOURSE-------360/470---------------------------------------------------------------------
#------------------------------------------------------------------e) SIMULATION DE COUVERTURE-----------470/550---------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------III) PARTIE SERVEUR-----------------------560/1240---------------------------------------------------------------------
#------------------------------------------------------------------a) AUTRE------------------------------565/585---------------------------------------------------------------------
#------------------------------------------------------------------b) MON PORTEFEUILLE-------------------585/720---------------------------------------------------------------------
#------------------------------------------------------------------c) COUVERTURE DE MON PORTEFEUILLE-----720/890---------------------------------------------------------------------
#------------------------------------------------------------------d) EVOLUTION DU COURS DE BOURSE-------890/1105--------------------------------------------------------------------
#------------------------------------------------------------------e) SIMULATION DE COUVERTURE-----------1105/1240-------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------- PARTIE PRE-FONCTION --------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------Permet de vider le dossier comportant les .csv des portefeuilles ou de creer le dossier s'il n'est pas cree
if(dir.exists("portfolio1")){
  responsesDir <- file.path("portfolio1")
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  for(i in 1:length(files)){
    Loc <- files[i] 
    file.remove(Loc)
  } 
}else{
  dir.create("portfolio1")
}

#------------Lancement de la session
session <- rsconnect::setAccountInfo(name='myportfoliomanagement',
                                     token='50D3FA3D5C6DE49A41E31E9EACD0A205',
                                     secret='LruTPq1urdouHZbyVRhGiC10GOteDRRDvGhcom16')

#-------------Tickers des actions a utiliser
ticker <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
            "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
            "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
            "Hrmes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
            "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
            "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
            "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
            "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")

tickers_name <- function(data){
  
  if(data=="AC.PA"){
    new_data="Accor SA"
  }
  else if(data=="AI.PA"){
    new_data="Air Liquide SA"
  }
  else if(data=="AIR.PA"){
    new_data="Airbus SE"
  }
  else if(data=="MT.AS"){
    new_data="Arcelor Mittal SA"
  }
  else if(data=="ATO.PA"){
    new_data="Atos SE"
  }
  else if(data=="CS.PA"){
    new_data="AXA SA"
  }
  else if(data=="BNP.PA"){
    new_data="BNP Paribas SA"
  }
  else if(data=="EN.PA"){
    new_data="Bouygues SA"
  }
  else if(data=="CAP.PA"){
    new_data="Capgemini SE"
  }
  else if(data=="CA.PA"){
    new_data="Carrefour SA"
  }
  else if(data=="ACA.PA"){
    new_data="Credit Agricole SA"
  }
  else if(data=="BN.PA"){
    new_data="Danone SA"
  }
  else if(data=="DSY.PA"){
    new_data="Dassault Systemes SA"
  }
  else if(data=="ENGI.PA"){
    new_data="ENGIE SA"
  }
  else if(data=="EL.PA"){
    new_data="EssilorLuxottica SA"
  }
  else if(data=="RMS.PA"){
    new_data="Hermes"
  }
  else if(data=="KER.PA"){
    new_data="Kering SA"
  }
  else if(data=="LR.PA"){
    new_data="Legrand SA"
  }
  else if(data=="MC.PA"){
    new_data="LVMH SE"
  }
  else if(data=="ML.PA"){
    new_data="Michelin SA"
  }
  else if(data=="ORA.PA"){
    new_data="Orange SA"
  }
  else if(data=="OR.PA"){
    new_data="L'Oreal SA"
  }
  else if(data=="RI.PA"){
    new_data="Pernod Ricard SA"
  }
  else if(data=="UG.PA"){
    new_data="Peugeot SA"
  }
  else if(data=="PUB.PA"){
    new_data="Publicis Groupe SA"
  }
  else if(data=="RNO.PA"){
    new_data="Renault"
  }
  else if(data=="SAF.PA"){
    new_data="Safran"
  }
  else if(data=="SGO.PA"){
    new_data="Saint-Gobain SA"
  }
  else if(data=="SAN.PA"){
    new_data="Sanofi"
  }
  else if(data=="SU.PA"){
    new_data="Schneider Electric SE"
  }
  else if(data=="GLE.PA"){
    new_data="Societe Generale SA"
  }
  else if(data=="SW.PA"){
    new_data="Sodexo SA"
  }
  else if(data=="STM.PA"){
    new_data="StMicroElectronics"
  }
  else if(data=="HO.PA"){
    new_data="Thales SA"
  }
  else if(data=="FP.PA"){
    new_data="TOTAL SA"
  }
  else if(data=="URW.AS"){
    new_data="Unibail Rodamco Wes"
  }
  else if(data=="VIE.PA"){
    new_data="Veolia Environnement SA"
  }
  else if(data=="DG.PA"){
    new_data="Vinci SA"
  }
  else if(data=="VIV.PA"){
    new_data="Vivendi SA"
  }
  else if(data=="WLN.PA"){
    new_data="Worldline SA"
  }
  else if(data=="EEEE"){
    new_data="EE"
  }else{
    new_data="Unknown"
  }
  
  new_data
  
}

#-----------------------------Fonction permettant de recuperer un tableau de rendement de toutes les actions du CAC40------------------------
get_symbol_regression <- function(symbol, start, end){
  ticker <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
              "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
              "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
              "Hermes" = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
              "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
              "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
              "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
              "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")

  
  portfolioRegression <- NULL
  if(symbol=="Portfolio's active"){
    tableau <- loadData()
    tableau2<-tableau[,1]
    ticker_variable_explicative <- ticker
    
    for(m in 1:length(tableau2)){
      ticker_variable_explicative <- ticker_variable_explicative [ticker_variable_explicative !=tableau2[m]]
    }
    i = 1
  }else if(symbol=="Aucun"){
    
    ticker_variable_explicative <- ticker
    i = 1
  }  else{

    portfolioRegression <- getSymbols(symbol, from = start, to = end, src = "yahoo", periodicity = "daily", auto.assign=FALSE)[,4]
    names(portfolioRegression)[1] = c("Var_exp")
    
    ticker_variable_explicative <- ticker [ticker != symbol]
    i = 2
  }
  
  data = ""
  for (data in ticker_variable_explicative){
    portfolioRegression <- cbind(portfolioRegression, getSymbols(data, from = start, to = end, src = "yahoo", periodicity = "daily", auto.assign=FALSE)[,4])
    
    names(portfolioRegression)[i] <- c(data)
    i = i + 1
  }
  portfolioRegression = as.data.frame(portfolioRegression)
  
  j = 2 #lignes
  while (j <= length(portfolioRegression[,1])){
    rendement =  portfolioRegression[j,1:length( portfolioRegression)]/ portfolioRegression[(j-1),1:length( portfolioRegression)]
    log_rendement=log(rendement)
    portfolioRegression[(j-1),1:length(portfolioRegression)]=log(rendement)
    j = j + 1 
  }
  
  database <- NULL
  for(i in 1:(length(portfolioRegression[,1])-1)){
    database <- rbind(database, portfolioRegression[i,1:length(portfolioRegression)])
  }
  database
}

#-----------------------------Fonction permettant de recuperer le tableau avec les differents coefficients-----------------------
coef_regression <- function(modele, number, symbol, portfolioPrices_regression){
  
  if(symbol=="Portfolio's active"){
    volume <- loadData()[,3]  
    tickers_portfolio <-loadData()[,1] 
    total_actif=0
    prix_actuel <- portfolioPrices_regression[length(portfolioPrices_regression[,1]), tickers_portfolio]
    for(l in 1:length(prix_actuel)){
      total_actif=as.numeric(volume[l]*prix_actuel[l]+total_actif)
    }
  }else{
    volume=as.numeric(number)
    total_actif=0
    prix_actuel <- portfolioPrices_regression[length(portfolioPrices_regression[,1]), symbol]
    total_actif=volume*prix_actuel
  }
  
  
  res.sum <- summary(modele)
  Adj.R2 = which.max(res.sum$adjr2)
  coeff <- coef(modele,Adj.R2)
  
  i=1
  tableau <- NULL
  total = 0
  while(i <= length(coeff) ){
    naa <-names(which(res.sum$which[Adj.R2,]==TRUE))[i]

    if(coeff[i]<0){
      total = total - round(abs((coeff[i]*total_actif)/portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]),0)*portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]
      tableau <- rbind(tableau, cbind(tickers_name(naa),round(abs((coeff[i]*total_actif)/portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]),0), round(portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa],2), "Buy", paste(round(round(abs((coeff[i]*total_actif)/portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]),0)*as.numeric(portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]),2),"€")))
      i=i+1
    }else{
      total = total + round(abs((coeff[i]*total_actif)/portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]),0)*portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]
      tableau <- rbind(tableau, cbind(tickers_name(naa),round(abs((coeff[i]*total_actif)/portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]),0), round(portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa],2), "Sell",paste(round(round(abs((coeff[i]*total_actif)/portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]),0)*as.numeric(portfolioPrices_regression[length(portfolioPrices_regression[,1]),naa]),2),"€")))
      i=i+1
    }
  }
  
  tableau <- rbind(tableau, cbind("Total","-","-","-", paste(round(total,2),"€")))
  tableau <- as.data.frame(tableau)
  names(tableau) <- c("Company","Volume", "Price", "Share", "Investment")
  tableau
}


#-----------------------------Fonction permettant de recuperer un tableau des tableaux .csv dans le dossier portfolio1------------------------
loadData <- function() {
  responsesDir <- file.path("portfolio1")
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- bind_rows(data)
  if(length(data)!=0){
    names(data)[2] <- "Cost Price"
  }
  
  data
}


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------- PARTIE GRAPHIQUE -----------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------Partie Header
header <- dashboardHeader(

  title = "My Portfolio"
)

#----------------------------Partie SideBar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebar",
    menuItem("Overview",
             tabName = "Presentation", icon = icon("fas fa-tasks")),
    menuItem("My portfolio",
             tabName = 'Portfolio', icon = icon("fas fa-book")),
    menuItem("Portfolio hedging",
             tabName ="Couverture", icon = icon("fas fa-umbrella")),
    menuItem("Stock price evolution",
             tabName = "Courbe", icon = icon("fas fa-chart-line")),
    menuItem("Hedging simulation",
             tabName = "Modele", icon = icon("fas fa-umbrella"))
  )
)

#---------------------------Partie Body
body <- dashboardBody(
  
  tabItems(
    #------------------------------------------------------- Onglet PRESENTATION ---------------------------------------------------
    tabItem(tabName = "Presentation",
        tags$style(HTML(".box-header{text-align:center;}")),
        
        #Box des etudiants
        fluidRow(
          box(title = tags$b("DEVELOPERS"),
            widgetUserBox(
              title = "Alexandre Belzacq Paillassoux",
              subtitle = "Student Master 1 - IFS",
              type = NULL,
              src = "https://media-exp1.licdn.com/dms/image/C5603AQH_NtVHG5OfEA/profile-displayphoto-shrink_200_200/0?e=1596672000&v=beta&t=pFOEB_fQg1O5Bogu2fgRgQhapcl6cGqF2IA1mYthbbE",
              background = TRUE,
              backgroundUrl = "https://images.unsplash.com/photo-1515772667704-f61e807b77e3?ixlib=rb-1.2.1&auto=format&fit=crop&w=634&q=80",
              shiny::actionButton(inputId='ab1', label="", 
                                  onclick ="window.open('https://www.linkedin.com/in/alexandre-belzacq-paillassoux-668858137/', '_blank')", icon = icon("fab fa-linkedin")),
              collapsible = FALSE,
              height = 172,
              width = 3
            ),
            widgetUserBox(
              title = "Maxime Desaniau",
              subtitle = "Student Master 1 - IFS",
              type = NULL,
              src = "https://media-exp1.licdn.com/dms/image/C4D03AQHKVXmvzIBgJQ/profile-displayphoto-shrink_200_200/0?e=1596672000&v=beta&t=qDuOmOTKqSiw8QsLvdTXXdYOBoAZuQicz5ny3thJmKQ",
              background = TRUE,
              backgroundUrl = "https://images.unsplash.com/photo-1513398200893-5b194f3e9fc1?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDAxNX0&auto=format&fit=crop&w=634&q=80",
              shiny::actionButton(inputId='ab2', label="", 
                                  onclick ="window.open('https://www.linkedin.com/in/maxime-desaniau-813336138/', '_blank')",icon = icon("fab fa-linkedin")),
              collapsible = FALSE,
              height = 172,
              width = 3
            ),
            widgetUserBox(
              title = "Quentin Fons",
              subtitle = "Student Master 1 - IFS",
              type = NULL,
              src = "https://media-exp1.licdn.com/dms/image/C5603AQHj6-8bWqDd1g/profile-displayphoto-shrink_200_200/0?e=1596672000&v=beta&t=gefGAtRr9Av5Z7_Ey0oQtTxqIo3j_tGrXNcHUPwLyMY",
              background = TRUE,
              backgroundUrl = "https://images.unsplash.com/photo-1512488746415-c0ad46cc26c1?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1350&q=80",
              shiny::actionButton(inputId='ab3', label="", 
                                  onclick ="window.open('https://www.linkedin.com/in/quentin-fons-2aa10a143/', '_blank')",icon = icon("fab fa-linkedin")),
              collapsible = FALSE,
              height = 172,
              width = 3
            ),
            widgetUserBox(
              title = "Bastien Giugliano",
              subtitle = "Student Master 1 - IFS",
              type = NULL,
              src = "https://media-exp1.licdn.com/dms/image/C4E03AQFJp8mC6Y710w/profile-displayphoto-shrink_200_200/0?e=1597881600&v=beta&t=Gpbo85bH59dz93owq-eyWGXWtf0KaUgACTGaeJq0pFc",
              background = TRUE,
              backgroundUrl = "https://images.unsplash.com/photo-1513209427138-5162573f1ca9?ixlib=rb-1.2.1&auto=format&fit=crop&w=634&q=80",
              shiny::actionButton(inputId='ab4', label="", 
                                  onclick ="window.open('https://www.linkedin.com/in/bastien-giugliano-42bb9b137/', '_blank')", icon = icon("fab fa-linkedin")),
              collapsible = FALSE,
              height = 172,
              width = 3
            ),
            solidHeader = TRUE,
            status = "primary",
            height = 268,
            width = 12
          )
        ),
        br(),br(),
        fluidRow(
          
          #Box descriptives du projet
          column(width = 6, valueBox("Application", "Web application with shiny to get the optimal coverage of a portflio.", icon = icon("fas fa-code"), color= "blue", width = NULL)),
          column(width = 6, valueBox("Supervisor", "Romain Menier _ Ph.D - Quantitative Analyst - IA Associate Actuary", icon = icon("fas fa-user-tie"), color= "blue", width = NULL)),
        ),
        br(),br(),
        fluidRow(
          
          #Box pour faire afficher les videos des tutoriels des onglets
          box(title = tags$b("WELCOME"),
            flipBox(id = 1,
                    main_img = "https://yt3.ggpht.com/OTkul3mi_ANaAOyv5MvUisRFY0Dl7fufifJBja_OkHoRwmJ-wGxHifNqX3pGVurlc97l5dkrU0V6=s561-c-fcrop64=1,0ebd0000f142ffff-nd",
                    header_img = "https://images.unsplash.com/photo-1586021280718-53fbadcb65a7?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1800&q=80",
                    front_title = "ABOUT",
                    front_btn_text = "MORE INFORMATION",
                    back_btn_text = "BACK TO MAIN",
                    back_title = "TUTORIALS",
                    "The objective of this project is to design an application on the R software (Shiny) that will determine the optimal coverage of an action.",
                    back_content = tagList(
                    fluidRow(
                      box(title = "Application Excel",
                          status = "success",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          tags$iframe(width="665", height="275", src="https://www.youtube.com/embed/EyBrYGhEYBs", allowfullscreen = "0", frameborder = "0", allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"),
                          tags$iframe(width="665", height="275", src="https://www.youtube.com/embed/eeAiZhz4hkg", allowfullscreen = "0", frameborder = "0", allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"),
                          width = 6
                      ),
                      box(title = "Portfolio Hedging",
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          tags$iframe(width="665", height="275", src="https://www.youtube.com/embed/7FcvbWKE5sc", allowfullscreen = "0", frameborder = "0", allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"),
                          width = 6
                      )
                    ),
                    fluidRow(
                      box(title = "Stock Price Evolution",
                          status = "info",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          tags$iframe(width="665", height="275", src="https://www.youtube.com/embed/ZktJtg1rs3E", allowfullscreen = "0", frameborder = "0", allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"),
                          width = 6
                      ),
                      box(title = "Hedging Simulation",
                          status = "danger",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          tags$iframe(width="665", height="275", src="https://www.youtube.com/embed/JWk9S5cSy60", allowfullscreen = "0", frameborder = "0", allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"),
                          width = 6
                      )
                    )
                )
            ),
            solidHeader = TRUE,
            height = 570,
            status = "primary",
            width = 12
          )
        )
    ),
    #------------------------------------------------------- Onglet MON PORTFOLIO --------------------------------------------------
    tabItem(tabName = "Portfolio",
            fluidRow(
                #Box pour importer un portefeuille et telecharger la feuille de creation de portefeuille Excel
                box(title = "Portfolio caracteristics", 
                    HTML('<h4>Please download the "Management Portfolio" application to build your equity portfolio : </h4> 
                         <a href="https://drive.google.com/file/d/1QlhDNKU4Ion36zJAJnxXkGUeWX5Lt0UZ/view?usp=sharing" download="app_management_portfolio.xlsm">Download Application : Management Portfolio</a>'),
                    br(),br(),
                    fileInput('import_file', "Import a portfolio"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 5
                ),
                

              
              
              #Box pour afficher son portefeuille/le supprimer/le telecharger
              box(title = "Portfolio", 
                  useShinyalert(),
                  actionButton("supp", "Reset portofio", class = "btn-primary"),
                  br(),
                  br(),
                  withSpinner(DT::dataTableOutput("portfolio")), 
                  status = "primary",
                  solidHeader = TRUE,
                  width = 7
              ),
              
              
              
                
            ), 
            
            #Onglet pour faire afficher les graphiques des action composant le modele
           fluidRow(
             #Box pour le choix de la periode pour le graphique
             box(title = "Period", dateRangeInput("datees", "Period",
                                                  start = "2020-01-01",
                                                  end = as.character(Sys.Date())),
                 useShinyalert(),
                 actionButton("do_comparaison", "Compare", class = "btn-primary"),
                 solidHeader = TRUE,
                 status = "primary",
                 width = 12
             ),
             
            
            br(),
            
            #Box pour faire afficher les graphiques de comparaison des rendements antre le portefeuille et le CAC40
            box(title = "Graphic", 
               withSpinner(plotlyOutput("plot_diff_portfolio")), 
               solidHeader = TRUE,
               status = "primary", 
               width = 12)
           ),
    ),
    #------------------------------------------------------- Onglet COUVERTURE DE MON PORTEFEUILLE ---------------------------------------------------
    tabItem(tabName = "Couverture",
            fluidRow(
              
              #Box pour parametrer son calcul des modeles
              box(title = "Model's variable", 
                  selectInput("action_ref_portfolio", "Stock to be hedged from our portfolio",
                              choices = "Portfolio's active",
                              multiple = FALSE),
                  #textInput("nbre_action_portfolio", "Number of shares:", 1),
                 dateRangeInput("datesss", "Period",
                                 start = "2020-01-01",
                                 end = as.character(Sys.Date())),
                  useShinyalert(),
                  actionButton("do_regression_portfolio", "Calculate", class = "btn-primary"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 6
              ),
              
              #Box pour faire afficher le R2 ajuster le plus grand et le nombre d'actions dans le modele
              box(title = "Information on the optimal model",
                  infoBoxOutput("information_R2_portfolio", width = 5),
                  infoBoxOutput("information_model_portfolio", width = 7),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 6),
            ),
      
        #Onglet des resultats de la regression lineaire et du modele    
        tabsetPanel(type = "tabs", id = "tabs_port",
                    
                    #Onglet du tableau recapitulatif du modele et du graphique des meilleurs combinaisons
                    tabPanel("Optimal model", id = "Modele_portfolio",
                             fluidRow(
                               br(),
                               
                               #Box pour afficher le tableau recapitulatif du modele
                               box(title = "Result of the hedging of our portfolio",
                                   withSpinner(DT::dataTableOutput("modele_optimal_portfolio")), 
                                   actionButton("do_plot_portfolio", "Display coverage curves", class = "btn-primary"),
                                   status = "primary",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = 6),
                              
                               #Box pour afficher le graphique des meilleurs combinaisons
                               box(title = "Hedging comparison",
                                   withSpinner(plotlyOutput("compare_hedging_portfolio")), 
                                   status = "primary",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = 6)
                             )
                    ),      
                    
                    #Onglet pour faire afficher les graphiques des action composant le modele
                    tabPanel(title = "Graphic portfolio", id = "Graphique_portfolio",
                                                       fluidRow(
                                                         br(),
                                                         box(title = "Graphic", 
                                                             withSpinner(plotlyOutput("plot_regression_portfolio")), 
                                                             solidHeader = TRUE,
                                                             status = "primary", 
                                                             width = 12)),
                    ),
                    
                    #Onglet pour faire afficher un tableau des meilleurs combinaisons
                    tabPanel("Further information", id = "Infos_portfolio",
                             fluidRow(
                               br(),
                               box(title = "Most optimal models",
                                   withSpinner(plotOutput("modele_plot_portfolio")), 
                                   status = "primary",
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width = 12)
                             )
                    )
        )
    ),
    
    
    #------------------------------------------------------- Onglet EVOLUTION DES COURS DE BOURSE ---------------------------------------------------
    tabItem(tabName = "Courbe",
            
            #Fluid pour parametrage de l'affichage des graphiques
            fluidRow(
              
              #Box pour le choix des actions a faire afficher
              box( title = "Shares", 
                   selectInput("global_indices_input", "", 
                               choices = ticker,
                               multiple = TRUE,
                               selected = "AC.PA"), 
                   solidHeader = TRUE,
                   status = "primary",
                   width = 6),
              
              #Box pour le choix de la periode pour le graphique
              box(title = "Period", dateRangeInput("dates", "",
                                                     start = "2020-01-01",
                             end = as.character(Sys.Date())),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 6
              )
            ),
            
            #Fluid de l'affichage du graphique
            fluidRow(
              box(title = "Graphic", 
                  withSpinner(plotlyOutput("plot")),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12),
            ),
            
            #Fluid des differents rendements
            fluidRow(
              
              #Box pour faire afficher le dernier rendement journalier
              box(title = "Last daily yield",
                  withSpinner(DT::dataTableOutput("infoj")), 
                  status = "primary",
                  colidHeader = TRUE,
                  collapsible = TRUE,
                  width = 4),
              
              #Box pour faire afficher le dernier rendement mensuel
              box(title = "Last monthly yield",
                  withSpinner(DT::dataTableOutput("infom")), 
                  status = "primary",
                  colidHeader = TRUE,
                  collapsible = TRUE,
                  width = 4),
              
              #Box pour faire afficher le dernier rendement annuel
              box(title = "Last yearly yield",
                  withSpinner(DT::dataTableOutput("infoa")), 
                  status = "primary",
                  colidHeader = TRUE,
                  collapsible = TRUE,
                  width = 4),
            ),
            
            #Onglet des details des differents rendement
            tabsetPanel(type = "tabs", id = "tab",
                        
                        #Onglet du details des rendements journaliers
                        tabPanel("Daily yield", id = "tcj",
                                 fluidRow(
                                   br(),
                                   box(title="Further information : Daily yield",
                                       DT::dataTableOutput("tauxdecroissancej"), 
                                       status = "primary",
                                       colidHeader = TRUE,
                                       collapsible = TRUE,
                                       width = 12, collapsed = TRUE),
                                 )
                        ),
                        
                        #Onglet du details des rendements mensuels
                        tabPanel(title = "Monthly yield", id = "tcm",
                                 fluidRow(
                                   br(),
                                   box(title="Further information : Monthly yield",
                                       DT::dataTableOutput("tauxdecroissancem"), 
                                       status = "primary",
                                       colidHeader = TRUE,
                                       collapsible = TRUE,
                                       width = 12, collapsed = FALSE),
                                 ),
                        ),
                        
                        #Onglet du details des rendements annuels
                        tabPanel("Yearly yield", id = "tca",
                                 fluidRow(
                                   br(),
                                   box(title="Further information : Yearly yield",
                                       DT::dataTableOutput("tauxdecroissancea"), 
                                       status = "primary",
                                       colidHeader = TRUE,
                                       collapsible = TRUE,
                                       width = 12, collapsed = FALSE)
                                 )
                        )
                        
            )
            
    ),
    
    
    #------------------------------------------------------- Onglet SIMULATION DE COUVERTURE ---------------------------------------------------
    tabItem(tabName = "Modele",
            
            fluidRow(
              
              #Box pour parametrer son calcul des modeles
              box(title = "Model's variable", 
                    selectInput("action_ref", "Share (variable to be explained)",
                              choices = ticker,
                              multiple = FALSE),
                    textInput("actions", "Number of shares:", 1000),
                    dateRangeInput("datess", "Period",
                                 start = "2020-01-01",
                                 end = as.character(Sys.Date())),
                    actionButton("do_regression", "Calculate", class = "btn-primary"),
                solidHeader = TRUE,
                status = "primary",
                width = 6
              ),
              
              #Box pour faire afficher le R2 ajuster le plus grand et le nombre d'actions dans le modele
              box(title = "Information on the optimal model",
                  infoBoxOutput("information_R2", width = 5),
                  infoBoxOutput("information_modele", width = 7),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 6)
            ),
            
            #Onglet des resultats de la regression lineaire et du modele  
            tabsetPanel(type = "tabs", id = "tabs",
                        
                        #Onglet du tableau recapitulatif du modele et du graphique des meilleurs combinaisons
                        tabPanel("Optimal model", id = "Modele",
                                 fluidRow(
                                 br(),
                                 
                                 #Box pour afficher le tableau recapitulatif du modele
                                 box(title = "Result of hedging simulation",
                                          withSpinner(DT::dataTableOutput("modele_optimal")), 
                                          actionButton("do_plot", "Show curves", class = "btn-primary"),
                                          status = "primary",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          width = 6),
                                 
                                 #Box pour afficher le graphique des meilleurs combinaisons
                                 box(title = "Hedging comparison",
                                          withSpinner(plotlyOutput("compare_hedging")), 
                                          status = "primary",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          width = 6)
                                )
                        ),
                        
                        #Onglet pour faire afficher les graphiques des action composant le modele
                        tabPanel(title = "Graphic", id = "Graphique",
                                 fluidRow(
                                   br(),
                                   box(title = "Graphic", 
                                       withSpinner(plotlyOutput("plot_regression")), 
                                       solidHeader = TRUE,
                                       status = "primary", 
                                       width = 12)
                                   ),
                          ),
                        
                        #Onglet pour faire afficher un tableau des meilleurs combinaisons
                        tabPanel("Further information", id = "Infos",
                                 fluidRow(
                                   br(),
                                   #Box pour afficher le graphique des meilleurs combinaisons
                                   box(title = "Most optimal models",
                                       withSpinner(plotOutput("modele_plot")), 
                                       status = "primary",
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       width = 12)
                                  ),
                                 
                        )
            )
      )
  )
)

ui <- dashboardPage(
  
  skin = "black",
  header,
  sidebar,
  body
)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------- PARTIE SERVEUR -------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session){
                                                                               #-----#
#------------------------------------------------------------------------------ AUTRE ----------------------------------------------------------------------------------------------- 
                                                                               #-----#
  
  #---- Permet de vider le dossier comportant les .csv des portefeuilles ou de creer le dossier s'il n'est pas cree ----
  if(dir.exists("portfolio1")){
    responsesDir <- file.path("portfolio1")
    files <- list.files(file.path(responsesDir), full.names = TRUE)
    for(i in 1:length(files)){
      Loc <- files[i] 
      file.remove(Loc)
    } 
  }else{
    dir.create("portfolio1")
  }

  #-----Permet de calculer et de mettre sur un format unique le temps actuel pour l'ajouter au nom du portefeuille lors du telechargement---
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
   
                                                                          #----------------#
#------------------------------------------------------------------------- MON PORTEFEUILLE ----------------------------------------------------------------------------------------- 
                                                                          #----------------#
  
  #---- Permet de recuperer touts les Close_Prices des actions du CAC40  ----
  portfolioPrices_compare_portfolio<-reactive({
    ticker <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
                "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
                "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
                "Hermes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
                "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
                "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
                "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
                "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")
    
    portfolioPricesss <- NULL
    
    i = 1
    for (symbol in ticker){
      portfolioPricesss <- cbind(portfolioPricesss, getSymbols(symbol, from = input$datees[1], to = input$datees[2], src = "yahoo", periodicity = "daily", auto.assign=FALSE)[,4])
      names(portfolioPricesss)[i] <- c(symbol)
      i = i + 1
    }
    
    portfolioPricesss = as.data.frame(portfolioPricesss)
    portfolioPricesss = cbind(as.Date(rownames(portfolioPricesss)), portfolioPricesss)
    names(portfolioPricesss)[1] = 'Date'
    
    portfolioPricesss
  })
  
  #---- Permet d'enregistrer l'action ajouter au portefeuille dans un fichier .csv ----
  saveData <- function(data) {
    fileName <- sprintf("%s.csv",
                        digest::digest(data))
    responsesDir <- file.path("portfolio1")
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  #---- Permet de copier le portefeuille que l'on veut importer dans le dossier "portfolio1" regroupant les autres actions du portefeuilles ----
  import_portfolio <- reactive({
    inFile <- input$import_file
    if (is.null(inFile))
      return(NULL)
    file.copy(inFile$datapath, "portfolio1", overwrite = TRUE)
    df <- read.csv(inFile$datapath, header = TRUE,sep = ",")
    return(df)
  })
  

  #---- Permet de recuperer le portefeuille importer et de d'actualiser le tableau du portefeuille ----
  observeEvent(input$import_file,{
    import_portfolio()
    output$portfolio <- DT::renderDataTable({
      loadData()
    },rownames = FALSE)
  })
  
  #---- Permet de supprimer tout le portefeuille et d'actualiser le tableau ----
  observeEvent(input$supp,{
    if(length(loadData())==0){
      shinyalert("Attention!", "Votre portefeuille est vide, voulez-vous en creer un?", type = "warning", showConfirmButton = TRUE,
                 showCancelButton = TRUE,
                 confirmButtonText = "OUI", cancelButtonText = "NON", inputId = "reset", 
                 callbackR = 
                   function(x) {
                     if(x==TRUE){
                       updateTabItems(session, "sidebar", "Portfolio")
                     }else{
                       updateTabItems(session, "sidebar", "Presentation")
                     }
                   }
                 ,)
    }else{
      shinyalert("Attention!", "Votre portefeuille va etre supprime, voulez-vous continuer?", type = "warning", showConfirmButton = TRUE,
                 showCancelButton = TRUE,
                 confirmButtonText = "OUI", cancelButtonText = "NON", inputId = "reset", 
                 callbackR = 
                   function(x) {
                     if(x==TRUE){
                       responsesDir <- file.path("portfolio1")
                       files <- list.files(file.path(responsesDir), full.names = TRUE)
                       for(i in 1:length(files)){
                         Loc <- files[i] # le chemin de ton dossier
                         file.remove(Loc)
                       }
                       output$portfolio <- DT::renderDataTable({
                         loadData()
                       },rownames = FALSE)
                     }
                   }
                 ,)
    }
  })
  
  #---- Permet d'actualiser le tableau du portefeuille selon l'interaction avec l'onglet ----
  output$portfolio <- DT::renderDataTable({
    if(input$supp | !(is.null(input$import_file)) ){
      loadData()
    }else{
      loadData()
    }
  },rownames = FALSE)
  
  #---- Permet de recuperer les rendements journaliers du CAC40 ----
  rendements_CAC40 <- reactive({

    portfolioRegression <- NULL
    portfolioRegression <- cbind(portfolioRegression, getSymbols("^FCHI", from = input$datees[1], to = input$datees[2], src = "yahoo", periodicity = "daily", auto.assign=FALSE)[,4])
    names(portfolioRegression)[1] <- c("^FCHI")
    
    portfolioRegression
    for (i in 1:length(portfolioRegression[,1])){
      if(is.na(portfolioRegression[i,1])){
        portfolioRegression[i,1] <- portfolioRegression[i-1,1]
      }
    }
    portfolioRegression <- as.data.frame(portfolioRegression)
    tableau4 <- portfolioRegression
    
    j = 2 #lignes
    while (j <= length(portfolioRegression[,1])){
      rendement =  log(portfolioRegression[j,1:length( portfolioRegression)])- log(portfolioRegression[(j-1),1:length( portfolioRegression)])
      
      portfolioRegression[(j-1),length(portfolioRegression)]=rendement*100
      j = j + 1 
    }
    portfolioRegression <- as.data.frame(portfolioRegression[-length(portfolioRegression[,1]),1])
    

    #Daily_returns <- NULL
   
    #  Daily_returns <- cbind(Daily_returns,periodReturn(portfolioRegression[,"^FCHI"],period='daily',subset=NULL,type='log',leading=TRUE))
     # names(Daily_returns)[1] <- c("FCHI")
      
    #Daily_returns <- as.data.frame(round(Daily_returns*100,2))
    
    Daily_returns <- cbind(as.Date(rownames(tableau4)[-1]),portfolioRegression)

    names(Daily_returns)[1] <- 'Date'
    names(Daily_returns)[2] <- 'FCHI'
    Daily_returns <- as.data.frame(Daily_returns[,c("Date","FCHI")])
    names(Daily_returns)[1]<- c("Date")
    
    #if (length(Daily_returns) != 1){
     # Daily_returns<-Daily_returns[-1,]
    #}
    Daily_returns

    base100 <- as.data.frame(100)

    j=2
    
    while (j<=length(Daily_returns[,1])){

      dayli100=Daily_returns[(j-1),2]/100
      if(!is.na(dayli100)){
        calcul_base_100=base100[(j-1),1]*(1+dayli100)
      }else{
        calcul_base_100=base100[(j-1),1]
      }
      calcul_base_100=base100[(j-1),1]*(1+dayli100)
      base100 <- as.data.frame(rbind(base100, calcul_base_100))
      j=j+1
    }
    
    base100 <- as.data.frame(cbind(Daily_returns[,1], base100))
    
  })
  
  
  

  #---- Permet de creer l'actif representatif du portefeuille ----
  representative_compare <- function(tab)({

    if(length(loadData())==0){
      tableau3 <- NULL
    }else{
      
      tableau <- loadData()
      
      tableau2<-tableau[,1] 
      volume <- as.data.frame(tableau[,3])
      
      
      tableau3<- as.data.frame(tab)[, tableau2]
      tableau8 <- tableau3
      
      
      
      #Recuperer prix au lieu de rendement 
      
      actif_portfolio<-NULL
      PNL=0
      total=0
      i=1
      j=1
      
      while(i <= length(tableau3[,1])){
        total=0
        j=1
        
        while(j<=length(tableau3)){
          total=tableau3[i,j]*volume[j,]+total
          j=j+1
        }
        PNL=total
        actif_portfolio<-rbind(actif_portfolio, PNL)
        i=i+1
      }
      
      tableau3 <- cbind(tableau3,actif_portfolio)

      tableau3<-as.data.frame(tableau3[1:(length(tableau3[,1])),length(tableau3)])
      
      j = 2 #lignes
      while (j <= length(tableau3[,1])){
        rendement =  log(tableau3[j,1:length( tableau3)])- log(tableau3[(j-1),1:length( tableau3)])
        
        tableau3[(j-1),length(tableau3)]=rendement
        j = j + 1 
      }
      tableau3 <- as.data.frame(tableau3[-length(tableau3[,1]),1])
      
      base100 <- as.data.frame(100)

      
      h=2
      
      while (h<=length(tableau3[,1])){
        dayli100=tableau3[(h-1),1]
        if(!is.na(dayli100)){
          calcul_base_100=base100[(h-1),1]*(1+dayli100)
        }else{
          calcul_base_100=base100[(h-1),1]
        }
       
        base100 <- as.data.frame(rbind(base100, calcul_base_100))
        h=h+1
      }
      
      base100 <- as.data.frame(cbind(rownames(tableau8)[-1],base100))
      
    }
  })
  
  
  #---- Recupere le tableau des redements du CAC40 et du portefeuille dans le tab Graphique ----
  mydataplot_rendement_CAC40 <- eventReactive(input$do_comparaison,{
    if(length(loadData())==0){
      # Show a modal when the button is pressed
      shinyalert("Oops!", "Vous devez posseder un portefeuille.", type = "error", showConfirmButton = TRUE,
                 showCancelButton = TRUE,
                 confirmButtonText = "CREER UN PORTEFEUILLE", cancelButtonText = "ANNULER", inputId = "calcul", 
                 callbackR = 
                   function(x) {
                     if(x==TRUE){
                       updateTabItems(session, "sidebar", "Portfolio")
                     }
                   })
      table <- as.data.frame(cbind("Date", "Tickers", "Prices"))
      names(table)[1] <- "Date"
      names(table)[2] <- "Tickers"
      names(table)[3] <- "Prices"
      table
     
      
    }else{
      CAC40 <-rendements_CAC40()
      CAC40 <- as.data.frame(CAC40[,-1])
      if(length(rendements_CAC40()[,1])!= length(representative_compare(portfolioPrices_compare_portfolio())[,2])){
        diff=abs(length(rendements_CAC40()[,1])- length(representative_compare(portfolioPrices_compare_portfolio())[,2]))
        for (i in 1:diff){
          CAC40 <- as.data.frame(rbind(100, CAC40))
        }
      }
      
      CAC40 <- as.data.frame(cbind(representative_compare(portfolioPrices_compare_portfolio())[,1],CAC40))

      table <- as.data.frame(cbind(CAC40, representative_compare(portfolioPrices_compare_portfolio())[,2]))
      names(table)[1]="Date"
      names(table)[2]="CAC40"
      names(table)[3]="Portfolio"
      table <- pivot_longer(data = table, cols = -Date, names_to = "Tickers", values_to = "Prices")
      table
     # r_table <- as.data.frame(cbind("CAC40", "Portfolio()"))
      filter(Tickers =="Portfolio" || Tickers =="CAC40", .data =  table)
    }

    
  })
  
  
  #---- Fais afficher les rendements du CAC40 et le portefeuille dans le Graphique ----
  output$plot_diff_portfolio <- renderPlotly({
    plot <- plot_ly(
      mydataplot_rendement_CAC40(), 
      x = ~Date, 
      y = ~Prices, 
      name = ~Tickers,
      type="scatter", 
      mode="lines"
    )
  })
  
                                                                   #------------------------------#
#------------------------------------------------------------------ COUVERTURE DE MON PORTEFEUILLE ----------------------------------------------------------------------------------  
                                                                   #------------------------------#
  
  #---- Permet de recuperer touts les Close_Prices des actions du CAC40  ----
  portfolioPrices_regression_portfolio<-reactive({
    ticker <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
                "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
                "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
                "Hermes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
                "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
                "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
                "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
                "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")
    
    portfolioPricesss <- NULL
    
    i = 1
    for (symbol in ticker){
      portfolioPricesss <- cbind(portfolioPricesss, getSymbols(symbol, from = input$datesss[1], to = input$datesss[2], src = "yahoo", periodicity = "daily", auto.assign=FALSE)[,4])
      names(portfolioPricesss)[i] <- c(symbol)
      i = i + 1
    }
    
    portfolioPricesss = as.data.frame(portfolioPricesss)
    portfolioPricesss = cbind(as.Date(rownames(portfolioPricesss)), portfolioPricesss)
    names(portfolioPricesss)[1] = 'Date'
    
    portfolioPricesss
  })
  
  #---- Lorsque le bouton "Calculer" est lance, cela recupere le tableau des rendements et verifie l'existence d'un portefeuille ----
  mydataregression_portfolio <- eventReactive(input$do_regression_portfolio, {
    if(length(loadData())==0){
      # Show a modal when the button is pressed
      shinyalert("Oops!", "Vous devez posseder un portefeuille.", type = "error", showConfirmButton = TRUE,
                 showCancelButton = TRUE,
                 confirmButtonText = "CREER UN PORTEFEUILLE", cancelButtonText = "ANNULER", inputId = "calcul", 
                 callbackR = 
                   function(x) {
                     if(x==TRUE){
                       updateTabItems(session, "sidebar", "Portfolio")
                     }
                   })
      
    }
    get_symbol_regression(input$action_ref_portfolio, input$datesss[1], input$datesss[2])
  })
  
  #---- Permet de creer l'actif representatif du portefeuille ----
  representative_portfolio <- eventReactive(input$do_regression_portfolio,{
    
    tableau_portfolio <- loadData()
    
    tickers_portfolio<-tableau_portfolio[,1] 
    volume <- as.data.frame(tableau_portfolio[,3])
  
    actions_portfolio<- as.data.frame(portfolioPrices_regression_portfolio())[, tickers_portfolio]
    
   

    actif_portfolio<-NULL
    i=1
    j=1

    #Calculer la PNL du portefeuille
    while(i <= length(actions_portfolio[,1])){
      PNL_t=0
      j=1
      
      while(j<=length(actions_portfolio)){
        PNL_t=actions_portfolio[i,j]*volume[j,]+PNL_t
        j=j+1
      }
      actif_portfolio<-rbind(actif_portfolio, PNL_t)
      i=i+1
    }
    
    actions_portfolio <- cbind(actions_portfolio,actif_portfolio)
    
    PNL_portfolio<-as.data.frame(actions_portfolio[1:(length(actions_portfolio[,1])),length(actions_portfolio)])

    #Calculer les rendements du portefeuille
    j = 2
    while (j <= length(PNL_portfolio[,1])){
      rendement =  log(PNL_portfolio[j,1:length(PNL_portfolio)])- log(PNL_portfolio[(j-1),1:length(PNL_portfolio)])
     
      PNL_portfolio[(j-1),length(PNL_portfolio)]=rendement
      j = j + 1 
    }
    
    rendement_portfolio <- as.data.frame(PNL_portfolio[-length(PNL_portfolio[,1]),1])
    #ncol/nrow
    
  })
  

  #---- Calcul les modeles ----
  modele_regression_portfolio <- reactive({
    n <- length(ticker)
    mod_regression <- regsubsets(data = data_portfolio(), Var_exp ~ ., nvmax = n, force.in=NULL, force.out=NULL, nbest = 1, intercept = FALSE)
  })
  
  #---- Permet de changer le nom de la premiere colonne afin de faire les modeles ----
  data_portfolio <- reactive({
    data <- mydataregression_portfolio()
    data_portfolio <- cbind(representative_portfolio(), data)
    names(data_portfolio)[1]="Var_exp"
    data_portfolio
  })
  
  #---- Fais afficher le meilleur R2 ----
  output$information_R2_portfolio <- renderInfoBox({ 
    res.sum <- summary(modele_regression_portfolio())
    Adj.R2 = which.max(res.sum$adjr2)
    
    infoBox("Max R2 Adjusted", round(res.sum$adjr2[Adj.R2],5), color = "blue")
  })
  
  
  #---- Fais afficher le nombre d'action composant le meilleur modele ----
  output$information_model_portfolio <- renderInfoBox({ 
    res.sum <- summary(modele_regression_portfolio())
    Adj.R2 = which.max(res.sum$adjr2)
    
    infoBox("Number of shares in the modele", paste(Adj.R2,"shares"), color = "blue")
  })
  
  
  #---- Fais afficher le tableau recapitulatif avec les coefficients ----
  output$modele_optimal_portfolio <- DT::renderDataTable({
    #table <- coef_regression(modele_regression_portfolio(),input$nbre_action_portfolio, input$action_ref_portfolio, portfolioPrices_regression())
    table <- portfolio_hedging()
    
    datatable(table)%>%formatStyle("Share",backgroundColor=styleEqual("Sell", "#99e699"))%>%formatStyle("Share",backgroundColor=styleEqual("Buy", "#ff8080" ))%>% formatStyle(0, cursor = 'pointer')
  })
  
  plot_regression_portfolio <- reactive({
    models <- regsubsets(data = data_portfolio(), Var_exp ~ ., nvmax = 39, force.in=NULL, force.out=NULL, nbest = 1, intercept = FALSE)
    plot(models, scale = "adjr2", ylim = 0.99999)
  })
  
  #---- Fais afficher le graphique des meilleurs combinaisons ----
  output$modele_plot_portfolio <- renderPlot({
   # plot_regression(data_portfolio())
    plot_regression_portfolio()
   
    
  })
  
  #---- Lorsque le bouton "Afficher les graphiques" est appuye, ca charge le tab Graphique ---- 
  observeEvent(input$do_plot_portfolio,{
    updateTabsetPanel(session, "tabs_port", "Graphic portfolio")
  })
  
  
  #---- Recupere le tableau des actions selectionner dans le tab Graphique ----
  mydataplot_regression_portfolio <- reactive({
    table <- coef_regression(modele_regression_portfolio(),1, input$action_ref_portfolio, portfolioPrices_regression())
    portfolioPrices_regression_2()
    filter(Tickers %in% rownames(table[input$modele_optimal_portfolio_rows_selected,0]), .data =  portfolioPrices_regression_2())
  })
  
  
  #---- Fais afficher les actions selectionner dans le tab Graphique ----
  output$plot_regression_portfolio <- renderPlotly({
    plot <- plot_ly(
      mydataplot_regression_portfolio(), 
      x = ~Date, 
      y = ~Prices, 
      name = ~Tickers,
      type="scatter", 
      mode="lines"
    )
  })
  
  #---------------------------------------
  portfolio_compare <- eventReactive(input$do_regression_portfolio,{
   
    tableau <-(portfolio_hedging())
    tableau <- as.data.frame(cbind(rownames(tableau), tableau))


    tableau2<-levels(tableau[,1])
    tableau2 <- tableau2[-1]
 
    volume1 <- as.data.frame(as.numeric(levels(tableau[,3])[as.numeric(tableau[,3])]))
    volume1 <- as.data.frame(volume1[-length(volume1[,1]),1])
    tableau3<- as.data.frame(portfolioPrices_regression_portfolio())[,tableau2]

    tableau4 <- loadData()
    tableau5<-tableau4[,1]

    volume2 <- as.data.frame(tableau4[,3])
    tableau6<- as.data.frame(portfolioPrices_regression_portfolio())[, tableau5]
    
    tableau3 <- as.data.frame(cbind(tableau3, tableau6))
    
    

    names(volume1)[1] <- "volume"
    names(volume2)[1] <- "volume"
    volume_tot <- as.data.frame(rbind(volume1, volume2))

    
    #Recuperer prix au lieu de rendement 
    
    actif_portfolio<-NULL
    PNL=0
    total=0
    i=1
    j=1
    
    while(i <= length(tableau3[,1])){
      total=0
      j=1
      
      while(j<=length(tableau3)){
        total=tableau3[i,j]*volume_tot[j,]+total
        j=j+1
      }
      PNL=total
      actif_portfolio<-rbind(actif_portfolio, PNL)
      i=i+1
    }
    
    tableau3 <- cbind(tableau3,actif_portfolio)
    
    tableau3<-as.data.frame(tableau3[1:(length(tableau3[,1])),length(tableau3)])
    
    j = 2 #lignes
    while (j <= length(tableau3[,1])){
      rendement =  log(tableau3[j,1:length( tableau3)])- log(tableau3[(j-1),1:length( tableau3)])
      
      tableau3[(j-1),length(tableau3)]=rendement
      j = j + 1 
    }
    
    
    rendement_portfolio_hedge <- as.data.frame(tableau3[-length(tableau3[,1]),1])
    base100_portfolio_hedge <- as.data.frame(100)
    
    h=2
    while (h<=length(rendement_portfolio_hedge[,1])){
      dayli100=rendement_portfolio_hedge[(h-1),1]
      if(!is.na(dayli100)){
        calcul_base_100_t=base100_portfolio_hedge[(h-1),1]*(1+dayli100)
      }else{
        calcul_base_100_t=base100_portfolio_hedge[(h-1),1]
      }
      base100_portfolio_hedge <- as.data.frame(rbind(base100_portfolio_hedge, calcul_base_100_t))
      h=h+1
    }

    datee <- rownames(tableau6)[-1]

    
    tablee <- as.data.frame(cbind(datee,base100_portfolio_hedge, representative_compare(portfolioPrices_regression_portfolio())[,2]))
    
    names(tablee)[1]="Date"
    names(tablee)[2]="Portfolio hedge"
    names(tablee)[3]="Portfolio not hedge"
    tablee <- pivot_longer(data = tablee, cols = -Date, names_to = "Tickers", values_to = "Prices")

    tablee
    filter(Tickers =="Portfolio hedge" ||Tickers =="Portfolio not hedge", .data =  tablee)
    
  })
 
  portfolio_hedging <- reactive({
    table <- coef_regression(modele_regression_portfolio(),1, input$action_ref_portfolio, portfolioPrices_regression())
    })
  
  
  #---- Fais afficher les actions selectionner dans le tab Graphique ----
  output$compare_hedging_portfolio <- renderPlotly({
    plot <- plot_ly(
      portfolio_compare(), 
      x = ~Date, 
      y = ~Prices, 
      name = ~Tickers,
      type="scatter", 
      mode="lines"
    )
  })
  
                                                                   #-----------------------------#
#------------------------------------------------------------------EVOLUTION DES COURS DE BOURSE-------------------------------------------------------------------------------------  
                                                                  #-----------------------------#
  
  #---- Permet de recuperer touts les Close_Prices des actions du CAC40 pour l'onglet ----
  portfolioPricesss<-reactive({
    ticker <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
                "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
                "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
                "Hermes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
                "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
                "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
                "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
                "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")
    
    
    portfolioPricesss <- NULL
    
    i = 1
    for (symbol in ticker){
      portfolioPricesss <- cbind(portfolioPricesss, getSymbols(symbol, from = input$dates[1], to = input$dates[2], src = "yahoo", periodicity = "daily", auto.assign=FALSE)[,4])
      names(portfolioPricesss)[i] <- c(symbol)
      i = i + 1
    }
    
    portfolioPricesss = as.data.frame(portfolioPricesss)
    portfolioPricesss = cbind(as.Date(rownames(portfolioPricesss)), portfolioPricesss)
    names(portfolioPricesss)[1] = 'Date'

    portfolioPricesss <- pivot_longer(data = portfolioPricesss, cols = -Date, names_to = "Tickers", values_to = "Prices")
    portfolioPricesss
  })
  
  #---- Permet de choisir seulement les actions dans le tableau de tous les Prices du CAC40 que l'on veut ----
  mydataplot <- reactive({
    portfolioPricesss()
    filter(Tickers %in% input$global_indices_input, .data =  portfolioPricesss())
  })
   
  #---- Permet de faire afficher les actions dans le graphique ----
  output$plot <- renderPlotly({
    plot <- plot_ly(
      mydataplot(), 
      x = ~Date, 
      y = ~Prices, 
      name = ~Tickers,
      type="scatter", 
      mode="lines"
    )
  })
  
  #---- Permet de recuperer touts les Close_Prices des actions du CAC40 (sans la date) ----
  get_symbol_rendement1 <- reactive({
    ticker <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
                "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
                "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
                "Hermes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
                "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
                "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
                "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
                "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")
    portfolioRegression <- NULL
    i = 1
    data = ""
    for (data in ticker){
      portfolioRegression <- cbind(portfolioRegression, getSymbols(data, from = input$dates[1], to = input$dates[2], src = "yahoo", periodicity = "daily", auto.assign=FALSE)[,4])
      names(portfolioRegression)[i] <- c(data)
      
      i = i + 1
    }
    portfolioRegression
  })
  
  #---- Permet de recuperer les rendements journaliers ----
  tauxdecroissancej <- reactive({
    table1 <- get_symbol_rendement1()
    ticker1 <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
                 "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
                 "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
                 "Hermes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
                 "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
                 "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
                 "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
                 "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")
    i =1 
    data = ""
    Daily_returns <- NULL
    for (data in ticker1){
      Daily_returns <- cbind(Daily_returns,periodReturn(table1[,data],period='daily',subset=NULL,type='log',leading=TRUE))
      names(Daily_returns)[i] <- c(data)
      i = i + 1
    }
    Daily_returns <- as.data.frame(round(Daily_returns*100,2))
    Daily_returns <- cbind(as.Date(rownames(Daily_returns)),Daily_returns)
    names(Daily_returns)[1] <- 'Date'
    Daily_returns <- as.data.frame(Daily_returns[,c("Date",input$global_indices_input)])
    names(Daily_returns)[1]<- c("Date")
    
    if (length(Daily_returns) != 1){
      Daily_returns<-Daily_returns[-1,]
    }
    Daily_returns
  })
  
  #---- Fais afficher les rendements journaliers ----
  output$tauxdecroissancej <- DT::renderDataTable({
    a = tauxdecroissancej()
    h = ncol(a)
    b = NULL
    i = 2
    while (i<= length(a)){
      if (i==2){
        b <- as.data.frame(cbind(b,paste(round(a[1:length(a[,1]),1],2))))
        names(b)[1] <-names(a)[1]
      }
      b <- as.data.frame(cbind(b,paste(round(a[1:length(a[,1]),i],2),"%")))
      names(b)[i] <-names(a)[i]
      i=i+1
    }
    datatable(a, options=list("searching"=FALSE, "paging"=FALSE))
    b
  },rownames=FALSE)
  
  #---- Permet de recuperer les rendements mensuels ----
  tauxdecroissancem <- reactive({
    table2 <- get_symbol_rendement1()
    ticker2 <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
                 "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
                 "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
                 "Hermes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
                 "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
                 "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
                 "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
                 "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")
    i =1 
    data = ""
    Monthly_returns <- NULL
    for (data in ticker2){
      Monthly_returns <- cbind(Monthly_returns,periodReturn(table2[,data],period='monthly',subset=NULL,type='log',leading=TRUE))
      names(Monthly_returns)[i] <- c(data)
      i = i + 1
    }
    Monthly_returns <- as.data.frame(round(Monthly_returns*100,2))
    Monthly_returns <- cbind(as.Date(rownames(Monthly_returns)), Monthly_returns)
    names(Monthly_returns)[1] <- 'Date'
    Monthly_returns <- as.data.frame(Monthly_returns[,c("Date",input$global_indices_input)])
    names(Monthly_returns)[1]<- c("Date")
    Monthly_returns
  })
  
  #---- Fais afficher les rendements mensuels ----
  output$tauxdecroissancem <- DT::renderDataTable({
    a = tauxdecroissancem()
    h = ncol(a)
    b = NULL
    i = 2
    while (i<= length(a)){
      if (i==2){
        b <- as.data.frame(cbind(b,paste(round(a[1:length(a[,1]),1],2))))
        names(b)[1] <-names(a)[1]
      }
      b <- as.data.frame(cbind(b,paste(round(a[1:length(a[,1]),i],2),"%")))
      names(b)[i] <-names(a)[i]
      i=i+1
    }
    datatable(a, options=list("searching"=FALSE, "paging"=FALSE))
    b
  },rownames=FALSE)
  
  #---- Permet de recuperer les rendements annuels ----
  tauxdecroissancea<-reactive({
    table3 <- get_symbol_rendement1()
    ticker3 <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
                 "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
                 "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
                 "Hermes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
                 "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
                 "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
                 "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
                 "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")
    i =1 
    data = ""
    Yearly_returns <- NULL
    for (data in ticker3){
      Yearly_returns <- cbind(Yearly_returns,periodReturn(table3[,data],period='yearly',subset=NULL,type='log',leading=TRUE))
      names(Yearly_returns)[i] <- c(data)
      i = i + 1
    }
    Yearly_returns <- as.data.frame(round(Yearly_returns*100,2))
    Yearly_returns <- cbind(as.Date(rownames(Yearly_returns)), Yearly_returns)
    names(Yearly_returns)[1] <- 'Date'
    Yearly_returns <- as.data.frame(Yearly_returns[,c("Date",input$global_indices_input)])
    names(Yearly_returns)[1]<- c("Date")
    Yearly_returns
  })
  
  #---- Fais afficher les rendements annuels ----
  output$tauxdecroissancea <- DT::renderDataTable({
    a = tauxdecroissancea()
    h = ncol(a)
    b = NULL
    i = 2
    while (i<= length(a)){
      if (i==2){
        b <- as.data.frame(cbind(b,paste(round(a[1:length(a[,1]),1],2))))
        names(b)[1] <-names(a)[1]
      }
      b <- as.data.frame(cbind(b,paste(round(a[1:length(a[,1]),i],2),"%")))
      names(b)[i] <-names(a)[i]
      i=i+1
    }
    datatable(a, options=list("searching"=FALSE, "paging"=FALSE))
    b
  },rownames=FALSE)
  
  #---- Fais afficher le dernier rendement journalier ----
  output$infoj <- DT::renderDataTable({
    table1 <- tauxdecroissancej()
    datej <- as.data.frame(table1[length(table1[,1]),1])
    table1 <- table1[,-1]
    table1 <- tail(table1, 1)
    table1 <- as.data.frame(t(table1))
    names(table1)[1] <- as.character(datej[1,1])
    table1
    h=length(table1)
    for(i in 1:length(table1)){
      table1 <- as.data.frame(cbind(table1,paste(table1[1:length(table1[,1]),i],"%")))
    }
    table1 <- subset(table1, select=-c(1:h))
    names(table1)[1] <- as.character(datej[1,1])
    datatable(table1, options=list("searching"=FALSE, "paging"=FALSE))
  },rownames=FALSE)
  
  #---- Fais afficher le dernier rendement mensuel ---- 
  output$infom <- DT::renderDataTable({
    table1 <- tauxdecroissancem()
    datem <- as.data.frame(table1[length(table1[,1]),1])
    table1 <- table1[,-1]
    table1 <- tail(table1, 1)
    table1 <- as.data.frame(t(table1))
    datem<- as.character(datem[1,1])
    tab<- strsplit(datem,'-')
    
    #names(table1)[1] <- list(tab[[1]][1:2])
    datej <- paste(tab[[1]][1],"-",tab[[1]][2])
    names(table1)[1] <- datej
    table1
    h=length(table1)
    for(i in 1:length(table1)){
      table1 <- as.data.frame(cbind(table1,paste(table1[1:length(table1[,1]),i],"%")))
    }
    table1 <- subset(table1, select=-c(1:h))
    names(table1)[1] <- datej
    datatable(table1, options=list("searching"=FALSE, "paging"=FALSE))
  },rownames=FALSE)
  
  #---- Fais afficher le dernier rendement annuel ----
  output$infoa <- DT::renderDataTable({
    table1 <- tauxdecroissancea()
    datea <- as.data.frame(table1[length(table1[,1]),1])
    table1 <- table1[,-1]
    table1 <- tail(table1, 1)
    table1 <- as.data.frame(t(table1))
    datea<- as.character(datea[1,1])
    tab<- strsplit(datea,'-')
    names(table1)[1] <-tab[[1]][1]
    table1
    h=length(table1)
    for(i in 1:length(table1)){
      table1 <- as.data.frame(cbind(table1,paste(table1[1:length(table1[,1]),i],"%")))
    }
    table1 <- subset(table1, select=-c(1:h))
    names(table1)[1] <-tab[[1]][1]
    datatable(table1, options=list("searching"=FALSE, "paging"=FALSE))
  },rownames=FALSE)
  
                                                                     #------------------------#
#---------------------------------------------------------------------SIMULATION DE COUVERTURE--------------------------------------------------------------------------------------- 
                                                                     #------------------------#
  
  #---- Permet de recuperer touts les Close_Prices des actions du CAC40 ----
  portfolioPrices_regression<-reactive({
    ticker <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
                "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
                "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
                "Hermes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
                "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
                "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
                "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
                "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")

    
      portfolioPricesss <- NULL
      
      i = 1
      for (symbol in ticker){
        portfolioPricesss <- cbind(portfolioPricesss, getSymbols(symbol, from = input$datess[1], to = input$datess[2], src = "yahoo", periodicity = "daily", auto.assign=FALSE)[,4])
        names(portfolioPricesss)[i] <- c(symbol)
        i = i + 1
      }
      
      portfolioPricesss = as.data.frame(portfolioPricesss)
      portfolioPricesss = cbind(as.Date(rownames(portfolioPricesss)), portfolioPricesss)
      names(portfolioPricesss)[1] = 'Date'
      
      portfolioPricesss
    })
  
  #---- Permet de recuperer touts les Close_Prices des actions du CAC40 (3 colonnes: Date/Ticker/Price) ----
  portfolioPrices_regression_2<-reactive({
    ticker <- c("Accor SA" = "AC.PA", "Air Liquide SA" = "AI.PA", "Airbus SE" = "AIR.PA", "Arcelor Mittal SA" = "MT.AS", "Atos SE" = "ATO.PA",
                "AXA SA" = "CS.PA", "BNP Paribas SA" = "BNP.PA", "Bouygues SA" = "EN.PA", "Capgemini SE" = "CAP.PA", "Carrefour SA" = "CA.PA",
                "Credit Agricole SA" = "ACA.PA", "Danone SA" = "BN.PA", "Dassault Systemes SA" = "DSY.PA", "ENGIE SA" = "ENGI.PA", "EssilorLuxottica SA" = "EL.PA",
                "Hermes " = "RMS.PA", "Kering SA" = "KER.PA", "Legrand SA" = "LR.PA", "LVMH SE" = "MC.PA", "Michelin SA" = "ML.PA", "Orange SA" = "ORA.PA",
                "L'Oreal SA" = "OR.PA", "Pernod Ricard SA" = "RI.PA", "Peugeot SA" = "UG.PA", "Publicis Groupe SA" = "PUB.PA", "Renault" = "RNO.PA",
                "Safran" = "SAF.PA", "Saint-Gobain SA" = "SGO.PA", "Sanofi" = "SAN.PA", "Schneider Electric SE" = "SU.PA", "Societe Generale SA" = "GLE.PA",
                "Sodexo SA" = "SW.PA", "StMicroElectronics" = "STM.PA", "Thales SA" = "HO.PA", "TOTAL SA" = "FP.PA", "Unibail Rodamco Wes" = "URW.AS",
                "Veolia Environnement SA" = "VIE.PA", "Vinci SA" = "DG.PA", "Vivendi SA" = "VIV.PA", "Worldline SA" = "WLN.PA")
    

    portfolioPricesss <- NULL
    
    i = 1
    for (symbol in ticker){
      portfolioPricesss <- cbind(portfolioPricesss, getSymbols(symbol, from = input$datess[1], to = input$datess[2], src = "yahoo", periodicity = "daily", auto.assign=FALSE)[,4])
      names(portfolioPricesss)[i] <- c(symbol)
      i = i + 1
    }
    
    portfolioPricesss = as.data.frame(portfolioPricesss)
    portfolioPricesss = cbind(as.Date(rownames(portfolioPricesss)), portfolioPricesss)
    names(portfolioPricesss)[1] = 'Date'
    
    #Permet de passer en 3 colonnes
    portfolioPricesss <- pivot_longer(data = portfolioPricesss, cols = -Date, names_to = "Tickers", values_to = "Prices") 
    
    portfolioPricesss
    
  })
  
  #---- Lorsque le bouton "Calcul" est lance, appel de la fonction get_symbol_regression ----
  mydataregression <- eventReactive(input$do_regression, {
    get_symbol_regression(input$action_ref, input$datess[1], input$datess[2])
  })
  
  #---- Calcul les modeles ----
  modele_regression <- reactive({
    n <- length(ticker)
    mod_regression <- regsubsets(data = mydataregression(), Var_exp ~ ., nvmax = n, force.in=NULL, force.out=NULL, nbest = 1, intercept = FALSE)
  })
  
  
  #---- Fais afficher le meilleur R2 ----
  output$information_R2 <- renderInfoBox({ 
    res.sum <- summary(modele_regression())
    Adj.R2 = which.max(res.sum$adjr2)
    
    infoBox("Max R2 Adjusted", round(res.sum$adjr2[Adj.R2],5), color = "blue")
  })
  
  #---- Fais afficher le nombre d'action composant le meilleur modele ----
  output$information_modele <- renderInfoBox({ 
    res.sum <- summary(modele_regression())
    Adj.R2 = which.max(res.sum$adjr2)
    
    infoBox("Number of shares in the modele", paste(Adj.R2,"shares"), color = "blue")
  })
  
  #---- ais afficher le tableau recapitulatif avec les coefficients ----
  output$modele_optimal <- DT::renderDataTable({
    table <- coef_regression(modele_regression(),input$actions, input$action_ref, portfolioPrices_regression())
    datatable(table)%>%formatStyle("Share",backgroundColor=styleEqual("Sell", "#99e699"))%>%formatStyle("Share",backgroundColor=styleEqual("Buy", "#ff8080" ))%>% formatStyle(0, cursor = 'pointer')
  })
  
  #---- Fais afficher les actions selectionner dans le tab Graphique ----
  output$plot_regression <- renderPlotly({
    plot <- plot_ly(
      mydataplot_regression(), 
      x = ~Date, 
      y = ~Prices, 
      name = ~Tickers,
      type="scatter", 
      mode="lines"
    )
  })
  
  plot_regression_1 <- reactive({
    models <- regsubsets(data = mydataregression(), Var_exp ~ ., nvmax = 39, force.in=NULL, force.out=NULL, nbest = 1, intercept = FALSE)
    plot(models, scale = "adjr2", ylim = 0.99999)
  })
  
  #---- Fais afficher le graphique des meilleurs combinaisons ----
  output$modele_plot <- renderPlot({
    #plot_regression(mydataregression())
    plot_regression_1()
  })
  
  #---- Lorsque le bouton "Afficher les graphiques" est appuye, ca charge le tab Graphique ----
  observeEvent(input$do_plot,{
    updateTabsetPanel(session, "tabs", "Graphic")
  })
  
  
  #---- Recupere le tableau des actions selectionner dans le tab Graphique ----
  mydataplot_regression <- reactive({
    table <- portfolio_hedging_actif()
    portfolioPrices_regression_2()
    filter(Tickers %in% rownames(table[input$modele_optimal_rows_selected,0]), .data =  portfolioPrices_regression_2())
  })
  
  #---------------------------------------
  portfolio_compare_actif <- eventReactive(input$do_regression,{
    
   
      tableau <-(portfolio_hedging_actif())
      tableau <- as.data.frame(cbind(rownames(tableau), tableau))
      
      
      tableau2<-levels(tableau[,1])
      tableau2 <- tableau2[-1]
  
      volume1 <- as.data.frame(as.numeric(levels(tableau[,3])[as.numeric(tableau[,3])]))
      volume1 <- as.data.frame(volume1[-length(volume1[,1]),1])
      tableau3<- as.data.frame(portfolioPrices_regression())[,tableau2]
      
  
      tableau5<-input$action_ref
      volume2 <- input$actions
      tableau6<- as.data.frame(portfolioPrices_regression())[, tableau5]
      tableau8 <-  tableau3
      
      
      tableau3 <- as.data.frame(cbind(tableau3, tableau6))
      tableau7 <- as.data.frame(cbind(tableau3, tableau6))
      
      names(volume1)[1] <- "volume"
      names(volume2)[1] <- "volume"
      volume_tot <- as.data.frame(rbind(volume1, volume2))
  
      
      #Recuperer prix au lieu de rendement 
      
      actif_portfolio<-NULL
      PNL=0
      total=0
      i=1
      j=1
      
      while(i <= length(tableau3[,1])){
        total=0
        j=1
        
        while(j<=length(tableau3)){
          total=tableau3[i,j]*as.numeric(volume_tot[j,])+total
          j=j+1
        }
        PNL=total
        actif_portfolio<-rbind(actif_portfolio, PNL)
        i=i+1
      }
      
      tableau3 <- cbind(tableau3,actif_portfolio)
      
      tableau3<-as.data.frame(tableau3[1:(length(tableau3[,1])),length(tableau3)])
      
      j = 2 #lignes
      while (j <= length(tableau3[,1])){
        rendement =  log(tableau3[j,1:length( tableau3)])- log(tableau3[(j-1),1:length( tableau3)])
        
        tableau3[(j-1),length(tableau3)]=rendement
        j = j + 1 
      }
      tableau3 <- as.data.frame(tableau3[-length(tableau3[,1]),1])
      
      base100 <- as.data.frame(100)
      
      
      h=2
      
      while (h<=length(tableau3[,1])){
        dayli100=tableau3[(h-1),1]

        if(!is.na(dayli100)){
          calcul_base_100=base100[(h-1),1]*(1+dayli100)
        }else{
          calcul_base_100=base100[(h-1),1]
        }
        
        base100 <- as.data.frame(rbind(base100, calcul_base_100))
        h=h+1
      }
      
      datee <- rownames(tableau8)[-1]

      #-----
      actif_portfolio<-NULL
      PNL=0
      total=0
      i=1
      j=1
      
  
        total=0
        
  
      while(j<=length(tableau7[,1])){
          total=tableau7[j,length(tableau7)]*as.numeric(volume2)
          j=j+1
          PNL=total
          actif_portfolio<-rbind(actif_portfolio, PNL)
      }
  
        tableau7 <- cbind(tableau7,actif_portfolio)
        tableau7<-as.data.frame(tableau7[1:(length(tableau7[,1])),length(tableau7)])
      
      
      j = 2 #lignes
      while (j <= length(tableau7[,1])){
        rendement =  log(tableau7[j,length( tableau7)])- log(tableau7[(j-1),length( tableau7)])
        
        tableau7[(j-1),length(tableau7)]=rendement
        j = j + 1 
      }
      tableau7 <- as.data.frame(tableau7[-length(tableau7[,1]),])
  
      base1001 <- as.data.frame(100)
      names(base1001)[1] <- "calcul_base_100"
      
      h=2
      
      while (h<=length(tableau7[,1])){
        dayli100=tableau7[(h-1),1]
        if(!is.na(dayli100)){
          calcul_base_1001=as.data.frame(base1001[(h-1),1]*(1+dayli100))
          names(calcul_base_1001)[1] <- "calcul_base_100"
        }else{
          calcul_base_1001=as.data.frame(base1001[(h-1),1])
          names(calcul_base_1001)[1] <- "calcul_base_100"
        }
        
        base1001 <- as.data.frame(rbind(base1001, calcul_base_1001))
        h=h+1
      }
  
      tablee <- as.data.frame(cbind(datee,base100,base1001))
      
      names(tablee)[1]="Date"
      names(tablee)[2]="Portfolio hedge"
      names(tablee)[3]="Portfolio not hedge"
      tablee <- pivot_longer(data = tablee, cols = -Date, names_to = "Tickers", values_to = "Prices")
      aaaaa <- FALSE
      tablee
      filter(Tickers =="Portfolio hedge" ||Tickers =="Portfolio not hedge", .data =  tablee)
      
    
  })
  
  portfolio_hedging_actif <- reactive({
    table <- coef_regression(modele_regression(),input$actions, input$action_ref, portfolioPrices_regression())
  })
  
  #---- Fais afficher les actions selectionner dans le tab Graphique ----
  output$compare_hedging <- renderPlotly({
    plot <- plot_ly(
      portfolio_compare_actif(), 
      x = ~Date, 
      y = ~Prices, 
      name = ~Tickers,
      type="scatter", 
      mode="lines"
    )
  })


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------FIN DU PROGRAMME -------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------SOMMAIRE (environ)------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------I) PRE-FONCTION-------------------------50/180-----------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------II) PARTIE GRAPHIQUE---------------------180/560----------------------------------------------------------------------
#------------------------------------------------------------------a) PRESENTATION-----------------------210/245---------------------------------------------------------------------
#------------------------------------------------------------------b) MON PORTEFEUILLE-------------------245/280---------------------------------------------------------------------
#------------------------------------------------------------------c) COUVERTURE DE MON PORTEFEUILLE-----280/3600--------------------------------------------------------------------
#------------------------------------------------------------------d) EVOLUTION DU COURS DE BOURSE-------360/470---------------------------------------------------------------------
#------------------------------------------------------------------e) SIMULATION DE COUVERTURE-----------470/550---------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------III) PARTIE SERVEUR-----------------------560/1240---------------------------------------------------------------------
#------------------------------------------------------------------a) AUTRE------------------------------565/585---------------------------------------------------------------------
#------------------------------------------------------------------b) MON PORTEFEUILLE-------------------585/720---------------------------------------------------------------------
#------------------------------------------------------------------c) COUVERTURE DE MON PORTEFEUILLE-----720/890---------------------------------------------------------------------
#------------------------------------------------------------------d) EVOLUTION DU COURS DE BOURSE-------890/1105--------------------------------------------------------------------
#------------------------------------------------------------------e) SIMULATION DE COUVERTURE-----------1105/1240-------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  
}

shinyApp(ui, server)