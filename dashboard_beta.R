## app.R ##

library(readr)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(eeptools)
library(tibble)
library(DT)
library(ggplot2)

  # WINDOWS
setwd("~/GitHub/MIMIC-III")
ADMISSIONS <- read.csv("~/GitHub/MIMIC-III/ADMISSIONS.csv")
ICUSTAYS <- read.csv("~/GitHub/MIMIC-III/ICUSTAYS.csv")
PATIENTS <- read.csv("~/GitHub/MIMIC-III/PATIENTS.csv")
dfmerge <- read.csv("~/GitHub/MIMIC-III/dfmerge.csv")
DIAGNOSES_ICD <- read.csv("~/GitHub/MIMIC-III/DIAGNOSES_ICD.csv")

#LINUX

# setwd("~/Documents/Github/MIMIC-III")
# ADMISSIONS <- read_csv("~/Documents/Github/MIMIC-III/ADMISSIONS.csv")
# ICUSTAYS <- read_csv("~/Documents/Github/MIMIC-III/ICUSTAYS.csv")
# PATIENTS <- read_csv("~/Documents/Github/MIMIC-III/PATIENTS.csv")
# dfmerge <- read_csv("~/Documents/Github/MIMIC-III/dfmerge.csv")
# DIAGNOSES_ICD <- read_csv("~/Documents/Github/MIMIC-III/DIAGNOSES_ICD.csv")

colnames(dfmerge)[colnames(dfmerge) == 'SUBJECT_ID.x'] <- 'SUBJECT_ID'
dfmerge <- dfmerge[-c(29)]

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


#### DIAGNOSES ####
v <- DIAGNOSES_ICD  %>% filter(str_detect(ICD9_CODE, "^V"))
e <- DIAGNOSES_ICD  %>% filter(str_detect(ICD9_CODE, "^E"))
na <- sum(is.na(DIAGNOSES_ICD$ICD9_CODE))
infections139 <-filter(DIAGNOSES_ICD, ICD9_CODE <= 1398)
neoplasms239 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 1400 , ICD9_CODE <= 2399)
endocrine279 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 2400 , ICD9_CODE <= 2799)
blood289 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 2800 , ICD9_CODE <= 2899)
mental319 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 2900 , ICD9_CODE <= 319)
nervous389 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 3200 , ICD9_CODE <= 3899)
circulatory459 <-filter(DIAGNOSES_ICD, ICD9_CODE >= 3900 , ICD9_CODE <= 4599)
respiratory519 <-filter(DIAGNOSES_ICD, ICD9_CODE >= 460 , ICD9_CODE <= 5199)
digestive579 <-filter(DIAGNOSES_ICD, ICD9_CODE >= 5200 , ICD9_CODE <= 5799)
genitourinary629 <-filter(DIAGNOSES_ICD, ICD9_CODE >= 5800 , ICD9_CODE <= 6299)
pregnancy679 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 6300 , ICD9_CODE <= 67914)
skin709 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 6800 , ICD9_CODE <= 7099)
muscle739 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 7100 , ICD9_CODE <= 7399)
congenital759 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 7400 , ICD9_CODE <= 7599)
perinatal779 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 7600 , ICD9_CODE <= 7799)
symptoms799 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 7800 , ICD9_CODE <= 7999)
injury999 <- filter(DIAGNOSES_ICD, ICD9_CODE >= 8000 , ICD9_CODE <= 9999)

ICD9CODE <- c("0-139", "140-239", "240-279", "280-289", "290-319", "320-389",
          "390-459", "460-519", "520-579","580-629","630-679","680-709","710-739",
          "740-759","760-779", "780-799", "800-999","V01-V091","E000-E999", "NA"
          )



Frequency <- c(nrow(infections139), nrow(neoplasms239), nrow(endocrine279), nrow(blood289), nrow(mental319),
        nrow(nervous389), nrow(circulatory459), nrow(respiratory519), nrow(digestive579), nrow(genitourinary629),
        nrow(pregnancy679), nrow(skin709), nrow(muscle739), nrow(congenital759), nrow(perinatal779), nrow(symptoms799),
        nrow(injury999), nrow(v), nrow(e), sum(is.na(DIAGNOSES_ICD$ICD9_CODE))
        )
a <- c("0-139", "140-239", "240-279", "280-289", "290-319", "320-389",
       "390-459", "460-519", "520-579","580-629","630-679","680-709","710-739",
       "740-759","760-779", "780-799", "800-999","V01-V091","E000-E999", "NA "
)

Frequency2 <- c(nrow(infections139_2), nrow(neoplasms239_2), nrow(endocrine279_2), nrow(blood289_2), nrow(mental319_2),
               nrow(nervous389_2), nrow(circulatory459_2), nrow(respiratory519_2), nrow(digestive579_2), nrow(genitourinary629_2),
               nrow(pregnancy679_2), nrow(skin709_2), nrow(muscle739_2), nrow(congenital759_2), nrow(perinatal779_2), nrow(symptoms799_2),
               nrow(injury999_2), nrow(v_2), nrow(e_2), sum(is.na(firstseq_num$ICD9_CODE))
        )



diagnosesPlot <- data.frame(ICD9CODE, Frequency)
diagnosesPlot2 <- data.frame(a, Frequency2)


# print (df)

dfmerge2 <- merge(dfmerge,DIAGNOSES_ICD,by="SUBJECT_ID")
dfmerge2 <- dfmerge2[ -c(2,4,5,6,11,15,16,18,19,21,22,23,24,28,29,30) ]
colnames(dfmerge2)[colnames(dfmerge2) == 'HADM_ID.x'] <- 'HADM_ID'





temp1 <-filter(dfmerge2, ICD9_CODE <= 1398)
temp2 <- filter(dfmerge2, ICD9_CODE >= 1400 , ICD9_CODE <= 2399)
temp3 <- filter(dfmerge2, ICD9_CODE >= 2400 , ICD9_CODE <= 2799)
temp4 <- filter(dfmerge2, ICD9_CODE >= 2800 , ICD9_CODE <= 2899)
temp5 <- filter(dfmerge2, ICD9_CODE >= 2900 , ICD9_CODE <= 319)
temp6 <- filter(dfmerge2, ICD9_CODE >= 3200 , ICD9_CODE <= 3899)
temp7 <-filter(dfmerge2, ICD9_CODE >= 3900 , ICD9_CODE <= 4599)
temp8 <-filter(dfmerge2, ICD9_CODE >= 460 , ICD9_CODE <= 5199)
temp9 <-filter(dfmerge2, ICD9_CODE >= 5200 , ICD9_CODE <= 5799)
temp10 <-filter(dfmerge2, ICD9_CODE >= 5800 , ICD9_CODE <= 6299)
temp11 <- filter(dfmerge2, ICD9_CODE >= 6300 , ICD9_CODE <= 67914)
temp12 <- filter(dfmerge2, ICD9_CODE >= 6800 , ICD9_CODE <= 7099)
temp13 <- filter(dfmerge2, ICD9_CODE >= 7100 , ICD9_CODE <= 7399)
temp14 <- filter(dfmerge2, ICD9_CODE >= 7400 , ICD9_CODE <= 7599)
temp15 <- filter(dfmerge2, ICD9_CODE >= 7600 , ICD9_CODE <= 7799)
temp16 <- filter(dfmerge2, ICD9_CODE >= 7800 , ICD9_CODE <= 7999)
temp17 <- filter(dfmerge2, ICD9_CODE >= 8000 , ICD9_CODE <= 9999)
temp18<- DIAGNOSES_ICD  %>% filter(str_detect(ICD9_CODE, "^V"))
temp19 <- DIAGNOSES_ICD  %>% filter(str_detect(ICD9_CODE, "^E"))

temp18 <- merge(temp18,ICUSTAYS, by = "ROW_ID", all.x  =TRUE) #LEFT OUTER JOIN
temp19 <- merge(temp19,ICUSTAYS, by = "ROW_ID", all.x  =TRUE) #LEFT OUTER JOIN

firstseq_num <- filter(DIAGNOSES_ICD, SEQ_NUM == "1")

infections139_2 <-filter(firstseq_num, ICD9_CODE <= 1398)
neoplasms239_2 <- filter(firstseq_num, ICD9_CODE >= 1400 , ICD9_CODE <= 2399)
endocrine279_2 <- filter(firstseq_num, ICD9_CODE >= 2400 , ICD9_CODE <= 2799)
blood289_2 <- filter(firstseq_num, ICD9_CODE >= 2800 , ICD9_CODE <= 2899)
mental319_2 <- filter(firstseq_num, ICD9_CODE >= 2900 , ICD9_CODE <= 319)
nervous389_2 <- filter(firstseq_num, ICD9_CODE >= 3200 , ICD9_CODE <= 3899)
circulatory459_2 <-filter(firstseq_num, ICD9_CODE >= 3900 , ICD9_CODE <= 4599)
respiratory519_2 <-filter(firstseq_num, ICD9_CODE >= 460 , ICD9_CODE <= 5199)
digestive579_2 <-filter(firstseq_num, ICD9_CODE >= 5200 , ICD9_CODE <= 5799)
genitourinary629_2 <-filter(firstseq_num, ICD9_CODE >= 5800 , ICD9_CODE <= 6299)
pregnancy679_2 <- filter(firstseq_num, ICD9_CODE >= 6300 , ICD9_CODE <= 67914)
skin709_2 <- filter(firstseq_num, ICD9_CODE >= 6800 , ICD9_CODE <= 7099)
muscle739_2 <- filter(firstseq_num, ICD9_CODE >= 7100 , ICD9_CODE <= 7399)
congenital759_2 <- filter(firstseq_num, ICD9_CODE >= 7400 , ICD9_CODE <= 7599)
perinatal779_2 <- filter(firstseq_num, ICD9_CODE >= 7600 , ICD9_CODE <= 7799)
symptoms799_2 <- filter(firstseq_num, ICD9_CODE >= 7800 , ICD9_CODE <= 7999)
injury999_2 <- filter(firstseq_num, ICD9_CODE >= 8000 , ICD9_CODE <= 9999)
v_2 <- firstseq_num  %>% filter(str_detect(ICD9_CODE, "^V"))
e_2 <- firstseq_num %>% filter(str_detect(ICD9_CODE, "^E"))






header <- dashboardHeader(title="MIMIC-III"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "home", icon = icon("home")),
    menuItem("Pesquisa", icon = icon("search"), tabName = "search",
             badgeLabel = "beta", badgeColor = "green"),
    menuItem("Patients", tabName = "patientss", icon = icon("hospital-user"),
             startExpanded = FALSE,
             menuSubItem(" General Patient Search",
                         tabName = "patients"),
             menuSubItem("Especific Patient Search",
                         tabName = "patients2")
             
    ),
    
    menuItem("Admissions", tabName = "admissions", icon = icon("book-medical")),
    
    menuItem("Diagnoses", tabName = "diagnoses", icon = icon("stethoscope"),
             startExpanded = FALSE,
             menuSubItem("General info",
                         tabName = "diagnoses1"),
             menuSubItem("Especific ICD9 Code",
                         tabName = "diagnoses2"),
             menuSubItem("First Diagnoses",
                         tabName = "diagnoses3")
             )
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
    
    tabItem(tabName = "patients2",
            h4("Filter to find general info about patients"),
            sidebarLayout(
              
              
              sidebarPanel(
                
                
                selectInput( "code93", "Select the ICD9 code", choices = c(
                  "-",
                  "INFECTIOUS AND PARASITIC DISEASES (001-139)" ="parasit",
                  "NEOPLASMS (140-239)" = "neoplasm",
                  "ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES, AND IMMUNITY DISORDERS (240-279)" = "endocrine",
                  "DISEASES OF THE BLOOD AND BLOOD-FORMING ORGANS (280-289)" = "blood",
                  "MENTAL DISORDERS (290-319)" = "mental",
                  "DISEASES OF THE NERVOUS SYSTEM AND SENSE ORGANS (320-389)" = "nervous",
                  "DISEASES OF THE CIRCULATORY SYSTEM (390-459)" = "circulatory",
                  "DISEASES OF THE RESPIRATORY SYSTEM (460-519)" = "respiratory",
                  "DISEASES OF THE DIGESTIVE SYSTEM (520-579)" = "digestive",
                  "DISEASES OF THE GENITOURINARY SYSTEM (580-629)" = "genitourinary",
                  "COMPLICATIONS OF PREGNANCY, CHILDBIRTH, AND THE PUERPERIUM (630-679)" = "pregnancy",
                  "DISEASES OF THE SKIN AND SUBCUTANEOUS TISSUE (680-709)" = "skin",
                  "DISEASES OF THE MUSCULOSKELETAL SYSTEM AND CONNECTIVE TISSUE (710-739)" ="muscle",
                  "CONGENITAL ANOMALIES (740-759)" = "anomalies",
                  "CERTAIN CONDITIONS ORIGINATING IN THE PERINATAL PERIOD (760-779)" = "perinatal",
                  "SYMPTOMS, SIGNS, AND ILL-DEFINED CONDITIONS (780-799)" ="signs",
                  "INJURY AND POISONING (800-999)" ="poison",
                  "SUPPLEMENTARY CLASSIFICATION OF FACTORS INFLUENCING HEALTH STATUS AND CONTACT WITH HEALTH SERVICES (V01-V89)" ="v1",
                  "SUPPLEMENTARY CLASSIFICATION OF EXTERNAL CAUSES OF INJURY AND POISONING (E800-E999)" = "v2"
                ) 
                ),
                
                width = 3
                
              ),
              mainPanel(
                
                
                DT::dataTableOutput("doentes"),
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
    
    tabItem(tabName = "diagnoses1",
            h3("General view of the ICD-9 Codes"),
            fluidRow(
              column(  width = 12, 
                box(
                  plotlyOutput("graficoICDS"),
                  # height = 10,
                ),
                box(
                  plotlyOutput("boxplotcompare"),
                  # height = 10,
                ),
              ),
              
              box(
                h3("Select ICD9 code to see a summary:"),
                width = 3,
                selectInput( "code92", "Select the ICD9 code", choices = c(
                  "-",
                  "INFECTIOUS AND PARASITIC DISEASES (001-139)" ="parasit",
                  "NEOPLASMS (140-239)" = "neoplasm",
                  "ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES, AND IMMUNITY DISORDERS (240-279)" = "endocrine",
                  "DISEASES OF THE BLOOD AND BLOOD-FORMING ORGANS (280-289)" = "blood",
                  "MENTAL DISORDERS (290-319)" = "mental",
                  "DISEASES OF THE NERVOUS SYSTEM AND SENSE ORGANS (320-389)" = "nervous",
                  "DISEASES OF THE CIRCULATORY SYSTEM (390-459)" = "circulatory",
                  "DISEASES OF THE RESPIRATORY SYSTEM (460-519)" = "respiratory",
                  "DISEASES OF THE DIGESTIVE SYSTEM (520-579)" = "digestive",
                  "DISEASES OF THE GENITOURINARY SYSTEM (580-629)" = "genitourinary",
                  "COMPLICATIONS OF PREGNANCY, CHILDBIRTH, AND THE PUERPERIUM (630-679)" = "pregnancy",
                  "DISEASES OF THE SKIN AND SUBCUTANEOUS TISSUE (680-709)" = "skin",
                  "DISEASES OF THE MUSCULOSKELETAL SYSTEM AND CONNECTIVE TISSUE (710-739)" ="muscle",
                  "CONGENITAL ANOMALIES (740-759)" = "anomalies",
                  "CERTAIN CONDITIONS ORIGINATING IN THE PERINATAL PERIOD (760-779)" = "perinatal",
                  "SYMPTOMS, SIGNS, AND ILL-DEFINED CONDITIONS (780-799)" ="signs",
                  "INJURY AND POISONING (800-999)" ="poison",
                  "SUPPLEMENTARY CLASSIFICATION OF FACTORS INFLUENCING HEALTH STATUS AND CONTACT WITH HEALTH SERVICES (V01-V89)" ="v1",
                  "SUPPLEMENTARY CLASSIFICATION OF EXTERNAL CAUSES OF INJURY AND POISONING (E800-E999)" = "v2"

                  
                ) 
                )
              ),
              
              box(
                verbatimTextOutput("summary2"), width = 8,
              ),
              
              
            )
    ),
    
    #***************************
    tabItem(tabName = "diagnoses2",
            h5("ICD-9 Codes -Diseases and Parasitic Diseases"),
            fluidRow(
              box(
                width = 3,
                selectInput( "code9", "Select the ICD9 code", choices = c(
                  "INFECTIOUS AND PARASITIC DISEASES (001-139)" ="parasit",
                  "NEOPLASMS (140-239)" = "neoplasm",
                  "ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES, AND IMMUNITY DISORDERS (240-279)" = "endocrine",
                  "DISEASES OF THE BLOOD AND BLOOD-FORMING ORGANS (280-289)" = "blood",
                  "MENTAL DISORDERS (290-319)" = "mental",
                  "DISEASES OF THE NERVOUS SYSTEM AND SENSE ORGANS (320-389)" = "nervous",
                  "DISEASES OF THE CIRCULATORY SYSTEM (390-459)" = "circulatory",
                  "DISEASES OF THE RESPIRATORY SYSTEM (460-519)" = "respiratory",
                  "DISEASES OF THE DIGESTIVE SYSTEM (520-579)" = "digestive",
                  "DISEASES OF THE GENITOURINARY SYSTEM (580-629)" = "genitourinary",
                  "COMPLICATIONS OF PREGNANCY, CHILDBIRTH, AND THE PUERPERIUM (630-679)" = "pregnancy",
                  "DISEASES OF THE SKIN AND SUBCUTANEOUS TISSUE (680-709)" = "skin",
                  "DISEASES OF THE MUSCULOSKELETAL SYSTEM AND CONNECTIVE TISSUE (710-739)" ="muscle", 
                  "CONGENITAL ANOMALIES (740-759)" = "anomalies",
                  "CERTAIN CONDITIONS ORIGINATING IN THE PERINATAL PERIOD (760-779)" = "perinatal",
                  "SYMPTOMS, SIGNS, AND ILL-DEFINED CONDITIONS (780-799)" ="signs",
                  "INJURY AND POISONING (800-999)" ="poison",
                  "SUPPLEMENTARY CLASSIFICATION OF FACTORS INFLUENCING HEALTH STATUS AND CONTACT WITH HEALTH SERVICES (V01-V89)" ="v1",
                  "SUPPLEMENTARY CLASSIFICATION OF EXTERNAL CAUSES OF INJURY AND POISONING (E800-E999)" = "v2"
                
                  
                ) 
                )
              ), #fim box
              
              
              box(
                width = 11,
                plotlyOutput("icddiagnoses")
              ),
              
              box(
                width = 11,
                
                plotlyOutput("boxplot")
              )
              
            )
    ),
    
    tabItem(tabName = "diagnoses3",
            h3("First Diagnoses  "),
            fluidRow(
                 box(
                      plotlyOutput("graficoICDS2"),
                      
                  ),
                 
                 box(
                   h3("Description of the ICD-9 codes:"),
                   br(),
                   h4("
                   
                      Infectious and parasitic diseases (001-139) 
                      Neoplasms (140-239) <br>
                      Endocrine, nutritional and metabolic diseases, and immunity disorders (240-279)<br>
                      Diseases of the blood and blood-forming organs (280-289)<br>
                      Mental disorders (290-319)<br>
                      Diseases of the nervous system and sense organs (320-389) <br>
                      Diseases of the circulatory system (390-459)<br>
                      Diseases of the respiratory system (460-519)<br>
                      Diseases of the digestive system (520-579)<br>
                      Diseases of the genitourinary system (580-629)<br>
                      Complications of pregnancy, childbirth, and the puerperium (630-679)<br>
                      Diseases of the skin and subcutaneous tissue (680-709)
                      Diseases of the musculoskeletal system and connective tissue (710-739)
                      Congenital anomalies (740-759)
                      Certain conditions originating in the perinatal period (760-779)
                      Symptoms, signs, and ill-defined conditions (780-799)
                      Injury and poisoning (800-999)
                      Supplementary classification of factors influencing health status and contact with health services (v01-v89)
                      Supplementary classification of external causes of injury and poisoning (e800-e999)
                      
                   
                      
                      
                      ")
                   
                 ),
                  
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

# ********************************************* SERVER *****************************************************
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
  output$graficoICDS <-  renderPlotly({
    plot_ly(
      data = diagnosesPlot,
      x = ~ICD9CODE,
      y = ~Frequency2,
      type = "bar",
      text = ~Frequency,
      textposition = "auto",
      hoverinfo = "text",
      hovertext = paste("IC9-Code:", diagnosesPlot$ICD9CODE)
    ) %>%
      layout(title= "Frequency of each ICD9 code"
             
      )
    
    
  })
  
  output$boxplot <- renderPlotly({
    
    
    if (input$code9 == "parasit"){
      
      plot_ly(
        data = temp1,
        y = ~LOS,
        type = "box",
        name = "001-139"
      ) %>%
        layout(title= "Length of stay"
               
        )
      
    } 
    else if (input$code9 == "neoplasm") {
      plot_ly(
        data = temp2,
        y = ~LOS,
        type = "box",
        name = "140-239"
      ) %>%
        layout(title= "Length of stay"
               
        )
      
    }
    else if (input$code9 == "endocrine") {
      plot_ly(
        data = temp3,
        y = ~LOS,
        type = "box",
        name = "240-279"
      ) %>%
        layout(title= "Length of stay"
               
        )
      
    }
    
    else if ( input$code9 == "blood" )
      
      plot_ly(
        data = temp4,
        y = ~LOS,
        type = "box",
        name = "280-289"
      ) %>%
      layout(title= "Length of stay"
             
      )
    
    
    
    else if (input$code9 == "mental") {
      plot_ly(
        data = temp5,
        y = ~LOS,
        type = "box",
        name = "290-319"
      ) %>%
        layout(title= "Length of stay"
               
        )
      
    }
    
    else if (input$code9 == "nervous") {
      plot_ly(
        data = temp6,
        y = ~LOS,
        type = "box",
        name = "320-389"
      ) %>%
        layout(title= "Length of stay"
               
        )
      
    }
    
    else if (input$code9 == "circulatory") {
      plot_ly(
        data = temp7,
        y = ~LOS,
        type = "box",
        name = "390-459"
      ) %>%
        layout(title= "Length of stay"
               
        )
      
    }
    else if (input$code9 == "respiratory") {
      plot_ly(
        data = temp8,
        y = ~LOS,
        type = "box",
        name = "460-519"
      ) %>%
        layout(title= "Length of stay"
               
        )
    }
    
    else if (input$code9 == "digestive") {
      plot_ly(
        data = temp9,
        y = ~LOS,
        type = "box",
        name = "520-579"
      ) %>%
        layout(title= "Length of stay"
               
        )
    }
    
    else if (input$code9 == "genitourinary") {
      plot_ly(
        data = temp10,
        y = ~LOS,
        type = "box",
        name = "580-629"
      ) %>%
        layout(title= "Length of stay"
               
        )
    }
    
    else if (input$code9 == "pregnancy") {
      plot_ly(
        data = temp11,
        y = ~LOS,
        type = "box",
        name = "630-679"
      ) %>%
        layout(title= "Length of stay"
               
        )
      
    }
    
    else if (input$code9 == "skin") {
      plot_ly(
        data = temp12,
        y = ~LOS,
        type = "box",
        name = "680-709"
      ) %>%
        layout(title= "Length of stay"
               
        )
      
    }
    
    else if (input$code9 == "muscle") {
      plot_ly(
        data = temp13,
        y = ~LOS,
        type = "box",
        name = "710-739"
      ) %>%
        layout(title= "Length of stay"
               
        )
    }
    
    else if (input$code9 == "anomalies") {
      plot_ly(
        data = temp14,
        y = ~LOS,
        type = "box",
        name = "740-759"
      ) %>%
        layout(title= "Length of stay"
               
        )
    }
    
    else if (input$code9 == "perinatal") {
      plot_ly(
        data = temp15,
        y = ~LOS,
        type = "box",
        name = "760-779"
      ) %>%
        layout(title= "Length of stay"
               
        )
    }
    
    else if (input$code9 == "signs") {
      plot_ly(
        data = temp16,
        y = ~LOS,
        type = "box",
        name = "780-799"
      ) %>%
        layout(title= "Length of stay"
               
        )
    }
    
    else if (input$code9 == "poison") {
      plot_ly(
        data = temp17,
        y = ~LOS,
        type = "box",
        name = "800-999"
      ) %>%
        layout(title= "Length of stay"
               
        )
      
    }
    
    
    else if (input$code9 == "v1") {
      plot_ly(
        data = temp18,
        y = ~LOS,
        type = "box",
        name = "V01-V89"
      ) %>%
        layout(title= "Length of stay"
               
        )
    }
    
    else if (input$code9 == "v2") {
      plot_ly(
        data = temp19,
        y = ~LOS,
        type = "box",
        name = "E800-E999"
      ) %>%
        layout(title= "Length of stay"
               
        )
    }
    
    
    
    
  })
  
  output$icddiagnoses <- renderPlotly({
    
    if(input$code9 == "blood"){
      counts <- as.data.frame(table(blood289$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
              )
    } 
    else if (input$code9 == "neoplasm") {
      counts <- as.data.frame(table(neoplasms239$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )

    }
    else if (input$code9 == "parasit") {
      counts <- as.data.frame(table(infections139$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    else if (input$code9 == "endocrine") {
      counts <- as.data.frame(table(endocrine279$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    
    else if (input$code9 == "mental") {
      counts <- as.data.frame(table(mental319$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "nervous") {
      counts <- as.data.frame(table(nervous389$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "circulatory") {
      counts <- as.data.frame(table(circulatory459$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    else if (input$code9 == "respiratory") {
      counts <- as.data.frame(table(respiratory519$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "digestive") {
      counts <- as.data.frame(table(digestive579$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "genitourinary") {
      counts <- as.data.frame(table(genitourinary629$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "pregnancy") {
      counts <- as.data.frame(table(pregnancy679$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "skin") {
      counts <- as.data.frame(table(skin709$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "muscle") {
      counts <- as.data.frame(table(muscle739$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "anomalies") {
      counts <- as.data.frame(table(congenital759$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "perinatal") {
      counts <- as.data.frame(table(perinatal779$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "signs") {
      counts <- as.data.frame(table(symptoms799$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "poison") {
      counts <- as.data.frame(table(injury999$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    
    else if (input$code9 == "v1") {
      counts <- as.data.frame(table(v$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "ICD Code" ,
                           categoryorder = "total descending")
        )
      
    }
    
    else if (input$code9 == "v2") {
      counts <- as.data.frame(table(e$ICD9_CODE))
      
      plot_ly(
        data = counts,
        x = ~Var1,
        y=~Freq,
        type = "bar"
        
      ) %>%
        layout(title= "Frequency of each ICD9 ",
               xaxis= list(title = "Range of ICD9 Codes" ,
                           categoryorder = "total descending")
        )
      
    }
    
   })
 
  output$summary2 <- renderPrint({
    
    if (input$code92 == "parasit"){
    
        summary(temp1)
  
    } 
    else if (input$code92 == "neoplasm") {
      summary(temp2)
      
    }
    else if (input$code92 == "endocrine") {
      summary(temp3)
      
    }
    
    else if ( input$code92 == "blood" )
      
      summary(temp4)
   
   
    
    else if (input$code92 == "mental") {
      summary(temp5)
      
    }
    
    else if (input$code92 == "nervous") {
      summary(temp6)
      
    }
    
    else if (input$code92 == "circulatory") {
      summary(temp7)
      
    }
    else if (input$code92 == "respiratory") {
      summary(temp8)
    }
    
    else if (input$code92 == "digestive") {
      summary(temp9)
    }
    
    else if (input$code92 == "genitourinary") {
      summary(temp10)
    }
    
    else if (input$code92 == "pregnancy") {
      summary(temp11)
      
    }
    
    else if (input$code92 == "skin") {
      summary(temp12)
      
    }
    
    else if (input$code92 == "muscle") {
      summary(temp13)
    }
    
    else if (input$code92== "anomalies") {
      summary(temp14)
    }
    
    else if (input$code92 == "perinatal") {
      summary(temp15)
    }
    
    else if (input$code92 == "signs") {
      summary(temp16)
    }
    
    else if (input$code92 == "poison") {
      summary(temp17)
      
    }
    
    
    else if (input$code92 == "v1") {
      summary(temp18)
    }
    
    else if (input$code92 == "v2") {
      summary(temp19)
    }
    
  })
  

  output$doentes <- DT::renderDataTable({
    
    if ( input$code93 == "parasit")
      DT::datatable(subset(infections139, select = -c(ROW_ID) ) )
    
    else if (input$code93 == "neoplasm") {
      DT::datatable(subset(neoplasms239, select = -c(ROW_ID) ))
      
    }
    else if (input$code93 == "endocrine") {
      DT::datatable(subset(endocrine279, select = -c(ROW_ID) ))
      
    }
    
    else if ( input$code93 == "blood" )
      
      DT::datatable(subset(blood289, select = -c(ROW_ID) ))
    
    
    
    else if (input$code93 == "mental") {
      DT::datatable(subset(mental319, select = -c(ROW_ID) ))
      
    }
    
    else if (input$code93 == "nervous") {
      DT::datatable(subset(nervous389, select = -c(ROW_ID) ))
      
    }
    
    else if (input$code93 == "circulatory") {
      DT::datatable(subset(circulatory459, select = -c(ROW_ID) ))
      
    }
    else if (input$code93 == "respiratory") {
      DT::datatable(subset(respiratory519, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "digestive") {
      DT::datatable(subset(digestive579, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "genitourinary") {
      DT::datatable(subset(genitourinary629, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "pregnancy") {
      DT::datatable(subset(pregnancy679, select = -c(ROW_ID) ))
      
    }
    
    else if (input$code93 == "skin") {
      DT::datatable(subset(skin709, select = -c(ROW_ID) ))
      
    }
    
    else if (input$code93 == "muscle") {
      DT::datatable(subset(muscle739, select = -c(ROW_ID) ))
    }
    
    else if (input$code93== "anomalies") {
      DT::datatable(subset(congenital759, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "perinatal") {
      DT::datatable(subset(perinatal779, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "signs") {
      DT::datatable(subset(symptoms799, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "poison") {
      DT::datatable(subset(injury999, select = -c(ROW_ID) ))
      
    }
    
    
    else if (input$code93 == "v1") {
      DT::datatable(subset(v, select = -c(ROW_ID) ))
    }
    
    else if (input$code93 == "v2") {
      DT::datatable(subset(e, select = -c(ROW_ID) ))
    }
    
    
    })
  
  output$boxplotcompare <- renderPlotly({
    fig <- plot_ly(
      data = temp1,
      y = ~LOS,
      type = "box",
      name= "001-139"
      
      )
    fig <- fig %>% add_trace(
      data = temp2,
      y = ~LOS,
      type = "box",
      name="140-239"
      
    )
    
    
    fig <- fig %>% add_trace(
      data = temp3,
      y = ~LOS,
      type = "box",
      name="240-279"
      
    )
    fig <- fig %>% add_trace(
      data = temp4,
      y = ~LOS,
      type = "box",
      name="280-289"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp5,
      y = ~LOS,
      type = "box",
      name="290-319"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp6,
      y = ~LOS,
      type = "box",
      name="320-389"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp7,
      y = ~LOS,
      type = "box",
      name="390-459"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp8,
      y = ~LOS,
      type = "box",
      name="460-519"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp9,
      y = ~LOS,
      type = "box",
      name="520-579"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp10,
      y = ~LOS,
      type = "box",
      name="580-629"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp11,
      y = ~LOS,
      type = "box",
      name="630-679"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp12,
      y = ~LOS,
      type = "box",
      name="680-709"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp13,
      y = ~LOS,
      type = "box",
      name="710-739"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp14,
      y = ~LOS,
      type = "box",
      name="740-759"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp15,
      y = ~LOS,
      type = "box",
      name="760-779"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp16,
      y = ~LOS,
      type = "box",
      name="780-799"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp17,
      y = ~LOS,
      type = "box",
      name="800-999"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp18,
      y = ~LOS,
      type = "box",
      name="V01-V89"
      
    )
    
    fig <- fig %>% add_trace(
      data = temp19,
      y = ~LOS,
      type = "box",
      name="E800-E999"
      
    )
   
  })
  
  output$graficoICDS2 <-  renderPlotly({
    plot_ly(
      data = diagnosesPlot2,
      x = ~ICD9CODE,
      y = ~Frequency2,
      type = "bar",
      text = ~Frequency2,
      textposition = "auto",
      hoverinfo = "text",
      hovertext = paste("IC9-Code:", diagnosesPlot2$a)
    ) %>%
      layout(title= "Frequency of each ICD9 code"
             
      )
    
    
  })
  

  })

shinyApp(ui, server)

#TODO 04/7/2022

#ORDENAR POR SEQ NUMB 
#CRIAR SEARCH BY PATIENT ID
#STATS DE DOEN?AS COM NUMERO 1 / HISTOGRAMA COM CORES DONE
#DESCRICAO DOEN?A long title
#boxplot com len stay -> especific tab + grafico com varios para comparar