## app.R ##

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(eeptools)

ADMISSIONS <- read.csv("~/GitHub/MIMIC-III/ADMISSIONS.csv")
ICUSTAYS <- read.csv("~/GitHub/MIMIC-III/ICUSTAYS.csv")
PATIENTS <- read.csv("~/GitHub/MIMIC-III/PATIENTS.csv")
dfmerge <- read.csv("~/GitHub/MIMIC-III/dfmerge.csv")


n_adm <- n_distinct(ADMISSIONS$SUBJECT_ID)

n_hospitalizacoes <- length(ADMISSIONS$SUBJECT_ID)


n_distdoentes <- length(PATIENTS$SUBJECT_ID) # PODEMOS OBTER TAMBEM CONTANDO ROWS OF PATIENTS


n_icudist <- n_distinct(ICUSTAYS$HADM_ID)

#GENDER
males <- sum(PATIENTS$GENDER == 'M')
females<- sum(PATIENTS$GENDER == 'F')

percMales <- males/length(PATIENTS$GENDER)
percMales <- round( percMales, 3)

#numero de mortes 
n_deaths <- sum(PATIENTS$DOD != "")

PATIENTS$DOB <- gsub(PATIENTS$DOB,pattern=" 00:00:00",replacement="",fixed=T)
PATIENTS$DOD <- gsub(PATIENTS$DOD,pattern=" 00:00:00",replacement="",fixed=T)
PATIENTS$DOD_HOSP <- gsub(PATIENTS$DOD_HOSP,pattern=" 00:00:00",replacement="",fixed=T)
PATIENTS$DOD_SSN <- gsub(PATIENTS$DOD_SSN,pattern=" 00:00:00",replacement="",fixed=T)


#ADMISSIONS <- ADMISSIONS %>% distinct(SUBJECT_ID, .keep_all = TRUE) ## remover rows com duplicados baseado no id
#df = merge(x=ADMISSIONS_unique,y=PATIENTS,by="SUBJECT_ID")
#View(df)

#df <- df %>% distinct(SUBJECT_ID, .keep_all =  TRUE)
#df$age <- age_calc(df$DOB, df$ADMITTIME, units = "years",precise = FALSE)

#gender <- data.frame(unclass(table(dfmerge$GENDER)))
#n_males <-gender[2,] / sum(gender) 


dfmerge$ADMITTIME <- as.Date(dfmerge$ADMITTIME)
dfmerge$DISCHTIME <- as.Date(dfmerge$DISCHTIME)
#dfmerge$len_stay <- age_calc(dfmerge$ADMITTIME, dfmerge$DISCHTIME, units = "days",precise = FALSE)
#dfmerge$len_stay <- gsub(dfmerge$len_stay,pattern=" days",replacement="",fixed=T)
#dfmerge$len_stay <- as.numeric(dfmerge$len_stay)

#mortalitysum(!is.na(dfmerge$DOD))

tempage <- filter(dfmerge, age <300 | age> 0 )
average_age <- mean(tempage$age)

dfmerge$GENDER <- as.factor(dfmerge$GENDER)
dfmerge$DIAGNOSIS <- as.factor(dfmerge$DIAGNOSIS)


header <- dashboardHeader(title="MIMIC-III"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "home", icon = icon("home")),
    menuItem("Pesquisa", icon = icon("search"), tabName = "search",
             badgeLabel = "beta", badgeColor = "green"),
    menuItem("Patients", tabName = "patients", icon = icon("hospital-user"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h4("Welcome to the home page"),
            fluidRow(
              box(
                title = "Basic MIMIC-III statistics between 2001-2012", width = 8, solidHeader = TRUE,
                #colocar dia e mes
                #cat("OLA"),
                HTML('<b> Number of distinct ICU stays:</b>' ),print(n_icudist), HTML('</br>'),
                
                
                HTML('<b> Number of hospital admissions:</b>'), print(n_hospitalizacoes), HTML( '</br>'),
                HTML('<b> Number of distinct patients:</b>' ), print(n_distdoentes), HTML( '</br>'),
                HTML('<b> Gender, Male %:</b>'), print(percMales), HTML('</br>'),
                HTML('<b> Average age, years:</b> '),print( round(average_age,3) ), HTML('</br>'),
                HTML('<b> ICU length of stay, average days:</b>'), print( round(mean(ICUSTAYS$LOS, na.rm =  TRUE),3) ), HTML('</br>'),
                #HTML('<b> Hospital length of stay, average days:</b> 6.9 <br>'),
                #HTML('<b> ICU Mortality, %:</b> 8.5 <br>'),
                HTML('<b> Hospital mortality,% :</b>'), print(round(n_deaths/46520,3)), HTML('</br>'),
              ),
              
              box(
                plotlyOutput("los_graph")
              ),
              
              box(
                plotlyOutput("age_graph")
              )
            
            )
            
            
            
            
    ),
    
    tabItem(tabName = "patients",
            h4("Search for especific patient"),
            
            # fluidRow(
            #   box(selectInput("v_ae", label = "Gender" , choices = unique(dfmerge$GENDER), width = 12)
            #  ),
            #   
            #   #print( length (res$SUBJECT_ID)),
            #   box(sliderInput("v_select", "Select age",
            #                   min = 0, max = 89, value = 20)
            #   ),
            #   verbatimTextOutput("stats")
            # )
            sidebarLayout(
              
            
              sidebarPanel(
                
               
                selectInput(inputId = "in_gender",
                            label = "Choose a gender:",
                            choices = unique(dfmerge$GENDER)),
                
                
              #   sliderInput("in_age", "Select age", min = 10, max = 89, value = 20)
              # ),
              # 
              ),
              mainPanel(
                
                
                verbatimTextOutput("summary")
                
                
                # tableOutput("view")
                
                ),
                
              
      
            
            ), 
            
    ), #fimpatients
    
    
    
    tabItem(tabName = "search",
            h5("Here you can make different kind of searches"),
            sidebarLayout(
              sidebarPanel (
                selectInput( "inState", "Select a field to create histogram", choices = names(dfmerge) )
              ),
              
              
              mainPanel(
                plotOutput("grafico")
              )
            )
            
    ) #fim search
  ) #fimtab items
  
  
  
)

ui <- dashboardPage(header, sidebar, body)


server <- (function(input, output) {
  
  output$grafico <- renderPlot({
    if( is.factor(dfmerge[,input$inState]) || is.character(dfmerge[,input$inState])) {
      
      barplot(summary(as.factor(dfmerge[,input$inState])),col = "#75AADB")
    }
    else
      
      hist((dfmerge[,input$inState]),col = "#75AADB")
  })
  
  
  
  
  # output$age_graph <- renderPlot({
  #   barplot(table(dfmerge$age),main = "Age frequency", 
  #           xlab = "Frequency", 
  #           ylab = "Age")
  # })
  
  # output$los_graph <- renderPlot({
  #   hist(ICUSTAYS$LOS,breaks= "FD",xlim = c(0,50),main = "Length of Stay frequency", 
  #           xlab = "Frequency", 
  #           ylab = "LOS")
  # })
  
  
  
  output$los_graph <- renderPlotly({
    plot_ly(
      data = ICUSTAYS,
      x = ~LOS,
      type = "histogram"
    ) %>%
      layout(title= "Histogram of Length of Stay", 
             xaxis= list(title = "Days" )
      )
    
    
  })
  
  output$age_graph <- renderPlotly({
    plot_ly(
      data = filter(dfmerge, age <300 & age > 0),
      x = ~age,
      type="histogram"
    ) %>%
      layout(title= "Histogram of age", 
             xaxis= list(title = "Years" )
        
      )
  })
  
  # datasetInput <- reactive({
  #   switch(input$dfmerge,
  #          "age" = age,
  #          "GENDER" = GENDER)
  # })
  
  dfInput <- reactive({
    dfmerge <- filter(dfmerge, GENDER == "input$in_gender")
  })
  
  output$summary <- renderPrint({
    df1 <- dfInput()
    summary(df1)
  })
  
  # output$summary <- renderPrint({
  #   dfmerge <- datasetInput()
  #   summary(dfmerge)
  # })
  
  # output$view <- renderTable({
  #   head(datasetInput(), n = input$in_age)
  # })
  # 
  
  
  
  
  #updateSelectizeInput(session, 'foo', choices = (dfmerge$GENDER), server = TRUE, label = NULL)
  
  # genderx <- reactive({
  #   
  #   
  #  # res <- dfmerge %>% filter( GENDER == input$v_select)
  #   res <- dfmerge %>% filter ( age == input$age_select)
  #   output$graficoage <- renderPlot({
  #     hist(res$age)
  #   })
  #   
  # })
  
  # output$select_example <- renderPlot({
  #   
  #   hist(dfmerge$age[input$v_select])
  # 
  # })
  
  # output$stats <- renderPrint({
  #   count(dfmerge$age(input$v_select) )
  # })

  
  
  
  
  
})

shinyApp(ui, server)

