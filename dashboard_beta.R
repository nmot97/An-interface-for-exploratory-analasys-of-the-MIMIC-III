## app.R ##

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(eeptools)
library(tibble)
library(DT)

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
n_deaths <- sum( is.na(PATIENTS$DOD))

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
dfmerge$DEATHTIME <- as.Date(dfmerge$DEATHTIME)
dfmerge$DEATHTIME <- as.Date(dfmerge$DEATHTIME)
dfmerge$ADMISSION_TYPE <- as.factor(dfmerge$ADMISSION_TYPE)
dfmerge$ADMISSION_LOCATION <- as.factor(dfmerge$ADMISSION_LOCATION)
dfmerge$INSURANCE <- as.factor(dfmerge$INSURANCE)
dfmerge$RELIGION <- as.factor(dfmerge$RELIGION)
dfmerge$MARITAL_STATUS <- as.factor(dfmerge$MARITAL_STATUS)
dfmerge$ETHNICITY <- as.factor(dfmerge$ETHNICITY)
dfmerge$EDREGTIME <- as.Date(dfmerge$EDREGTIME)
dfmerge$EDOUTTIME <- as.Date(dfmerge$EDOUTTIME)
dfmerge$DIAGNOSIS <- as.factor(dfmerge$DIAGNOSIS)
dfmerge$DOB <- as.Date(dfmerge$DOB)
dfmerge$DOD <- as.Date(dfmerge$DOD)
dfmerge$DOD_HOSP <- as.Date(dfmerge$DOD_HOSP)
dfmerge$DOD_SSN <- as.Date(dfmerge$DOD_SSN)
dfmerge$LANGUAGE <- as.factor(dfmerge$LANGUAGE)


ADMISSIONS$DIAGNOSIS <- as.factor(ADMISSIONS$DIAGNOSIS)

admission_temp <- data.frame(unclass(summary(as.factor(ADMISSIONS$ADMISSION_TYPE))), check.names = FALSE, stringsAsFactors = TRUE)
colnames(admission_temp) <- c("Admission type frequency")
admission_loc <- data.frame(unclass(summary(as.factor(ADMISSIONS$ADMISSION_LOCATION))), check.names = FALSE, stringsAsFactors = TRUE)
colnames(admission_loc) <- c("Admission location frequency")
discharge_loc <- data.frame(unclass(summary(as.factor(ADMISSIONS$DISCHARGE_LOCATION))), check.names = FALSE, stringsAsFactors = TRUE)
colnames(discharge_loc) <- c("Discharge location frequency")
insurance_temp <- data.frame(unclass(summary(as.factor(ADMISSIONS$INSURANCE))), check.names = FALSE, stringsAsFactors = TRUE)
colnames(insurance_temp) <- c("Insurance frequency")
diagnosis_temp <- as.data.frame(unclass(summary(as.factor(ADMISSIONS$DIAGNOSIS))), check.names = FALSE, stringsAsFactors = TRUE)
diagnosis_temp <- tibble::rownames_to_column(diagnosis_temp, "Diagnose")
colnames(diagnosis_temp) <- c("Diagnose","D")

# data <- data.frame(ADMISSIONS$DIAGNOSIS)
# data$ADMISSIONS.DIAGNOSIS <- factor(data$ADMISSIONS.DIAGNOSIS, levels = unique(data$ADMISSIONS.DIAGNOSIS)[order(data$Count, decreasing = TRUE)])



dfmerge$X <- NULL
dfmerge$ROW_ID.x <- NULL
dfmerge$...1 <- NULL
dfmerge$ROW_ID.y <- NULL


xpto <- unique(as.character(dfmerge$GENDER))
xpto <- c("ALL",xpto)

xpto2 <- unique(as.character(dfmerge$ETHNICITY))
xpto2 <- c("ALL",xpto2)

xpto3 <- unique( as.character(dfmerge$RELIGION))
xpto3 <- c("ALL", xpto3)






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
    menuItem("Patients", tabName = "patients", icon = icon("hospital-user")),
    menuItem("Admissions", tabName = "admissions", icon = icon("book-medical")),
    menuItem("Diagnoses", tabName = "diagnoses", icon = icon("stethoscope"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h4("Welcome to the home page"),
            fluidRow(
              box(
                title = "Basic MIMIC-III statistics between 2001-2012", width = 7, solidHeader = TRUE,
                status = "warning",
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
                
                status = "primary",
                plotlyOutput("los_graph")
              ),
              
              box(
               
                status = "primary",
                plotlyOutput("age_graph")
              )
              
            )
            
            
            
            
    ),
    
    tabItem(tabName = "patients",
            h4("Filter to find general info about patients"),
            
            
            sidebarLayout(
              
              
              sidebarPanel(
                
                
                selectInput(inputId = "in_gender",
                            label = "Choose a gender:",
                            choices = xpto),
                
                selectInput(inputId = "in_ethnicity",
                            label = "Choose an ethnicity:",
                            choices = xpto2 ),
                
                selectInput(inputId = "in_religion",
                            label = "Choose a religion:",
                            choices = xpto3),
                sliderInput( inputId = "in_age",
                             label = "Age", min = -1, max = 300,
                             value = c(-1,-1)
                ),
                print("Select -1 to not select age"),
                
                actionButton('select', 'Select'),
                width = 3
                
              ),
              mainPanel(
                
                
                verbatimTextOutput("summary"),
                width = 9
                
                
                
                
              ),
              
              
              
              
            ), 
            
    ), #fimpatients
    tabItem(tabName = "admissions",
            h3("Details of every patient admission on the hospital"),
            br(),
            h4("There's a total of 58976 admissions registered on the database. Each one has a diferent ID , called HADM_ID. "),
            br(),
            fluidRow(
              box(
                width = 10,status = "primary",
                tabsetPanel(
                  id = 'dataset',
                  tabPanel("Admission type", DT::dataTableOutput("mytable1"), plotlyOutput("grafico1")),
                  tabPanel("Admission location", DT::dataTableOutput("mytable2"), plotlyOutput("grafico2")),
                  tabPanel("Dischard location", DT::dataTableOutput("mytable3"),  plotlyOutput("grafico3")),
                  tabPanel("Insurance", DT::dataTableOutput("mytable4"),  plotlyOutput("grafico4")),
                  tabPanel("Diagnosis", DT::dataTableOutput("mytable5"),  plotlyOutput("grafico5")),
                ),
              )
              
            )
            
          
            
            
    ), #fim admissions
    
    tabItem(tabName = "diagnoses",
            h5("ICD-9 Codes"),
            fluidRow(
              box(
                selectInput( "code9", "Select the ICD9 code", choices = c(
                  "INFECTIOUS AND PARASITIC DISEASES (001-139)",
                  "NEOPLASMS (140-239)",
                  "ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES, AND IMMUNITY DISORDERS (240-279)",
                  "DISEASES OF THE BLOOD AND BLOOD-FORMING ORGANS (280-289)",
                  "MENTAL DISORDERS (290-319)",
                  "DISEASES OF THE NERVOUS SYSTEM AND SENSE ORGANS (320-389)",
                  "DISEASES OF THE CIRCULATORY SYSTEM (390-459)",
                  "DISEASES OF THE RESPIRATORY SYSTEM (460-519)",
                  "DISEASES OF THE DIGESTIVE SYSTEM (520-579)",
                  "DISEASES OF THE GENITOURINARY SYSTEM (580-629)",
                  "COMPLICATIONS OF PREGNANCY, CHILDBIRTH, AND THE PUERPERIUM (630-679)",
                  "DISEASES OF THE SKIN AND SUBCUTANEOUS TISSUE (680-709)",
                  "DISEASES OF THE MUSCULOSKELETAL SYSTEM AND CONNECTIVE TISSUE (710-739)",
                  "CONGENITAL ANOMALIES (740-759)",
                  "CERTAIN CONDITIONS ORIGINATING IN THE PERINATAL PERIOD (760-779)",
                  "SYMPTOMS, SIGNS, AND ILL-DEFINED CONDITIONS (780-799)",
                  "INJURY AND POISONING (800-999)",
                  "SUPPLEMENTARY CLASSIFICATION OF FACTORS INFLUENCING HEALTH STATUS AND CONTACT WITH HEALTH SERVICES (V01-V89)",
                  "SUPPLEMENTARY CLASSIFICATION OF EXTERNAL CAUSES OF INJURY AND POISONING (E800-E999)"
                  
                  
                  ) 
                )
              )
            )
    ),
            
    
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
  
  output$diseases <- renderDataTable(
    admission_temp
   
  )
  
  
  dfmerge2 <- subset(dfmerge, select = -c(HOSPITAL_EXPIRE_FLAG,HAS_CHARTEVENTS_DATA, EXPIRE_FLAG))
  
  filtered_gender <- reactive({
    if(input$in_gender == "ALL"){
      dfmerge2
    } else {
      dfmerge2 %>% 
        filter(GENDER == input$in_gender)
    }
  })
  
  filtered_ethnicity <- reactive({
    if(input$in_ethnicity == "ALL"){
      filtered_gender()
    } else {
      filtered_gender() %>% 
        filter(ETHNICITY == input$in_ethnicity)
    }
  })
  
  filtered_religion <- reactive({
    if(input$in_religion == "ALL"){
      filtered_ethnicity()
    } else {
      filtered_ethnicity() %>% 
        filter(RELIGION == input$in_religion)
    }
  })
  
  filtered_age <- reactive({
    if(input$in_age == "-1" ){
      filtered_religion()
    }
    else{
      filtered_religion() %>% 
        filter( age >= input$in_age[1] & age <= input$in_age[2])
    }
  })
  
  
  
  fully_filtered <- eventReactive(input$select, {
    filtered_age()
  })
  
  
  
  
  output$summary <- renderPrint({
    
    summary(fully_filtered())
  })
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(admission_temp)
  })
  
  output$grafico1 <- renderPlotly({
    plot_ly(
      data = ADMISSIONS,
      x = ~ADMISSION_TYPE,
      type="histogram"
    ) %>%
      layout(title= "Admission Type " ,
             xaxis= list(title = "Type" )
             
      )
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(admission_loc)
  })
  
  output$grafico2 <- renderPlotly({
    plot_ly(
      data = ADMISSIONS,
      x = ~ADMISSION_LOCATION,
      type="histogram"
    ) %>%
      layout(title= "Admission Location " ,
             xaxis= list(title = "Location" )
             
      )
  })
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(discharge_loc)
  })
  
  output$grafico3 <- renderPlotly({
    plot_ly(
      data = ADMISSIONS,
      x = ~DISCHARGE_LOCATION,
      type="histogram"
    ) %>%
      layout(title= "Discharge Location " ,
             xaxis= list(title = "Location" )
             
      )
  })
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(insurance_temp)
  })
  
  output$grafico4 <- renderPlotly({
    plot_ly(
      data = ADMISSIONS,
      x = ~INSURANCE,
      type="histogram"
    ) %>%
      layout(title= "Insurance " ,
             xaxis= list(title = "Insurance" ,
                         categoryorder = "total descending")
             
      )
  })
  
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(diagnosis_temp)
  })
  
  output$grafico5 <- renderPlotly({
    plot_ly(
      data = diagnosis_temp,
      x = ~D,
      type="bar"
    ) %>%
      layout(title= "Diagnosis " ,
             xaxis= list(title = "Diagnosis" )
             
      )
  })
  
  
})

shinyApp(ui, server)

