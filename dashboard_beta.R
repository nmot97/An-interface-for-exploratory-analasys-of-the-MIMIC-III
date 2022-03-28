## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(eeptools)

ADMISSIONS <- read.csv("~/GitHub/MIMIC-III/ADMISSIONS.csv")
ICUSTAYS <- read.csv("~/GitHub/MIMIC-III/ICUSTAYS.csv")
PATIENTS <- read.csv("~/GitHub/MIMIC-III/PATIENTS.csv")

x <- ADMISSIONS$HADM_ID
n_adm <- n_distinct(x)

#n_hospitalizacoes <- ADMISSIONS$SUBJECT_ID

y <- ADMISSIONS$SUBJECT_ID
n_distdoentes <- n_distinct(y) # PODEMOS OBTER TAMBEM CONTANDO ROWS OF PATIENTS

o <- ICUSTAYS$HADM_ID
n_icudist <- n_distinct(o)

#GENDER
males <- sum(PATIENTS$GENDER == 'M')
females<- sum(PATIENTS$GENDER == 'F')

percMales <- males/length(PATIENTS$GENDER)

#numero de mortes 
n_deaths <- sum(PATIENTS$DOD != "")

PATIENTS$DOB <- gsub(PATIENTS$DOB,pattern=" 00:00:00",replacement="",fixed=T)
PATIENTS$DOD <- gsub(PATIENTS$DOD,pattern=" 00:00:00",replacement="",fixed=T)
PATIENTS$DOD_HOSP <- gsub(PATIENTS$DOD_HOSP,pattern=" 00:00:00",replacement="",fixed=T)
PATIENTS$DOD_SSN <- gsub(PATIENTS$DOD_SSN,pattern=" 00:00:00",replacement="",fixed=T)



header <- dashboardHeader(title="MIMIC-III"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "home", icon = icon("home")),
    menuItem("Pesquisa", icon = icon("search"), tabName = "search",
             badgeLabel = "beta", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h4("Welcome to the home page"),
            box(
              title = "Basic MIMIC-III statistics between 2001-2012", width = 8, solidHeader = TRUE,
              #colocar dia e mes
              #cat("OLA"),
              HTML('<b> Number of distinct ICU stays:</b>' ),print(n_icudist), HTML('</br>'),
              
            
              #HTML('<b> Number of hospital admissions:</b>'), print(n_hospitalizacoes), HTML( '</br>'),
              HTML('<b> Number of distinct patients:</b>' ), print(n_distdoentes), HTML( '</br>'),
             # HTML('<b> Gender, Male %:</b>'), print(percMales), HTML('</br>'),
              HTML('<b> Average age, years:</b> 65.8 <br>'),
              HTML('<b> ICU length of stay, average days:</b> 2.1 <br>'),
              HTML('<b> Hospital length of stay, average days:</b> 6.9 <br>'),
              HTML('<b> ICU Mortality, %:</b> 8.5 <br>'),
              HTML('<b> Hospital mortality,% :</b> 11.5 <br>')
            ),
            
          
            

    ),
    
    
    
    tabItem(tabName = "search",
            h5("Here you can make different kind of searches")
    )
  )
  
  
  
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) { }

shinyApp(ui, server)

