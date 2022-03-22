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
            h5("Welcome to the home page"),
            box(
              title = "Basic MIMIC-III statistics", width = 15, solidHeader = TRUE,
              
              HTML('Hello <')
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
