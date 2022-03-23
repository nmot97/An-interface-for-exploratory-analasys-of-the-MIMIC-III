## app.R ##
library(shiny)
library(shinydashboard)

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
              
              HTML('<b> Number of distinct ICU stays:</b> 53 423 <br>'),
              HTML('<b> Number of hospital admissions:</b> 49 785 <br>'),
              HTML('<b> Number of distinct patients:</b> 38 597 <br>'),
              HTML('<b> Gender, Male %:</b> 55.9% <br>'),
              HTML('<b> Average age, years:</b> 65.8'),
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

