# Chosse your own project: COVID patient cause of death analysis
# Edx HarvadX: Data Sience Capstone
# Iván de Luna
# July 2021

###############################################################################
## Load required packages
###############################################################################

# Load libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(xfun)) install.packages("xfun", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", dependencies = c("Depends", "Suggests"), repos = "http://cran.us.r-project.org")

# Load Libraries
library(tidyverse)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(caret)
library(matrixStats)

# Import Mexico Covid-19 database from the Dirección General de Epidemiología
# which changes everyday but they keep historical datasets.
# https://www.gob.mx/salud/documentos/datos-abiertos-152127

# The file is in zip format
temp <- tempfile()
download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/historicos/2021/07/datos_abiertos_covid19_10.07.2021.zip", temp)
data <- read.csv(unzip(temp, "210710COVID19MEXICO.csv"))
unlink(temp)

# Additionally, there is a data dictionary which contains the information
# regarding every variable in the dataset (diccionario_datos_covid19) and that contains
# a file for the description (201128 Descriptores_.xlsx) and a catalog (201128 Catalogos.xlsx)
# for some of the variables

###############################################################################
# Exploratory Analysis
###############################################################################

# We can take a look at the database, which contains 7,732,694 records with 40 variables of people
# that have been registered for COVID19 issues, but not necessarily are COVID19 positive.
glimpse(data)

# There are some variables (FECHA_ACTUALIZACION, FECHA_INGRESO, FECHA_SINTOMSA, FECHA_DEF) which we will parse into a POSIXt format
# but first for a more convenient workflow, we lowercase the variable names.
names(data) <- tolower(names(data))

# Data update date
data$fecha_actualizacion <- as_datetime(data$fecha_actualizacion)
# Hospital register date
data$fecha_ingreso <- as_datetime(data$fecha_ingreso)
# Initial symptons date
data$fecha_sintomas <- as_datetime(data$fecha_sintomas)
# Defunction date
data$fecha_def <- as_datetime(data$fecha_def)

# For the actual objective of this analysis, we do not need all the variables that are contained
# in the dataset, so we will keep the ones that will be useful in the case of death - conditions analysis
# which are:

# id_registro - Unique identifier
# sector - the type of medical unit the patient was registered
# sexo - gender of the patient
# tipo_paciente - classifies if the patient stayed at the hospital or leaved
# fecha_ingreso - date when the patient arrived to the medical unit
# fecha_sintomas - date when the patient considers the symptoms started
# fecha_def - date when the patient died
# intubado - the patient was assigned a ventilator
# neumonia - Neumony diagnose
# edad - patient age
# embarazo - Pregnancy condition
# diabetes - Diabetes condition
# epoc - EPOC condition
# asma -  Asthma condition
# inmusupr - Immunosuppresion condition
# hipertension - Hypertension condition
# otra_com - Other conditions
# cardiovascular - Cardiovascular condition
# obesidad - Obesity condition
# renal_cronica - Chronic renal condition
# tabaquismo - Smoker condition
# clasificacion_final - Final results for COVID19 test

vars <- c("id_registro",
          "sector",
          "sexo",
          "tipo_paciente",
          "fecha_ingreso",
          "fecha_sintomas",
          "fecha_def",
          "intubado",
          "neumonia",
          "edad",
          "embarazo",
          "diabetes",
          "epoc",
          "asma",
          "inmusupr",
          "hipertension",
          "otra_com",
          "cardiovascular",
          "obesidad",
          "renal_cronica",
          "tabaquismo",
          "clasificacion_final")

data1 <- data %>% filter(entidad_res == 5)
covid <- subset(data1, select = vars)

rm(data)

# We will need to subset the data because of the number of records, so we can
# focus on a particular state or city


# Now we take a look at the remaining variables
glimpse(covid)

# We only have to classes, integer and datetime, as most of the variables regarding
# a health condition are defined in the data catalog as having five options:
# 1 - yes
# 2 - no
# 97 - does not apply
# 98 - is ignored or not enough information
# 99 - not specified

conditions <- c("intubado",
                "neumonia",
                #"embarazo",
                "diabetes",
                "epoc",
                "asma",
                "inmusupr",
                "hipertension",
                "otra_com",
                "cardiovascular",
                "obesidad",
                "renal_cronica",
                "tabaquismo")

# For this analysis we need to drop the 97, 98 and 99 values from the conditions:
covid <- covid %>% filter(intubado %in% c(1,2))
covid <- covid %>% filter(neumonia %in% c(1,2))
#covid <- covid %>% filter(embarazo %in% c(1,2))
covid <- covid %>% filter(diabetes %in% c(1,2))
covid <- covid %>% filter(epoc %in% c(1,2))
covid <- covid %>% filter(asma %in% c(1,2))
covid <- covid %>% filter(inmusupr %in% c(1,2))
covid <- covid %>% filter(hipertension %in% c(1,2))
covid <- covid %>% filter(otra_com %in% c(1,2))
covid <- covid %>% filter(cardiovascular %in% c(1,2))
covid <- covid %>% filter(obesidad %in% c(1,2))
covid <- covid %>% filter(renal_cronica %in% c(1,2))
covid <- covid %>% filter(tabaquismo %in% c(1,2))

#covid[conditions] <- lapply(covid[conditions], factor)

#sapply(covid, class)

# For the gender case, the possible values are:
# 1 - Man
# 2 - Woman
# 3 - Not specified

covid$sexo <- as.factor(covid$sexo)

# and in the sector variable we have this possibilities:
# 1 - Red Cross
# 2 - DIF (Local social assistance program)
# 3 - State medical service
# 4 - National Social Security Health Service (NHS)
# 5 - NHS in the Bienestar program
# 6 - State Worker Social Security Service
# 7 - Municipal hospital
# 8 - PEMEX (State Oil Company)
# 9 - Private
# 10 - Army Health Service
# 11 - Navy Health Service
# 12 - Health Secretariat
# 13 - University Hospitals
# 99 - Not specified

# Given that most of them are of public character, we can reclassified them into
# two options:
# 1 - Public Health Service
# 2 - Private Health Service
# Given that only the value of 9 referse to Private health service, we do the following:

# First we drop the Not specified values
covid <- covid %>% filter(sector != 99)

# Then we redefine everything that is not private as 1 and private as 2
covid$sector[!(covid$sector)==9] <- 1
covid$sector[covid$sector==9] <- 2
# Finally we convert it to factors
covid$sector <- as.factor(covid$sector)

# In the tipo_paciente variable, they classify the patient as:
# 1 - Ambulatory (Did not stayed at the medical unit)
# 2 - Hospitalized
# 99 - Not specified
# Here as the sector variable, we need to know if it is a meaningfull condition so
# we drop the Not specified value and convert everything into factors.
# But we see that there are not 99 values
unique(covid$tipo_paciente)
# so we convert into factors
covid$tipo_paciente <- as.factor(covid$tipo_paciente)

# In the original database there are some variables that are related to the COVID-19
# test, but the variable clasificacion_final compiles this results into seven possible values:
# 1 - COVID-19 positive confirmed by being in contact with another COVID-19 positive person
# 2 - COVID-19 positive confirmed only if the patient died and was determined post-mortem
# 3 - COVID-19 positive given laboratory results
# 4 - COVID-19 negative result given association with positive person
# 5 - COVID-19 can not be determined
# 6 - COVID-19 awaiting for lab test
# 7 - COVID-19 negative result

# Which we can reclassify as:
# 1 - COVID-19 Positive for values 1,2,3
# 2 - COVID-19 Negative for values 4,5,7
# 3 - Not specified for values 6

covid$clasificacion_final[covid$clasificacion_final == 1] <- 1
covid$clasificacion_final[covid$clasificacion_final == 2] <- 1
covid$clasificacion_final[covid$clasificacion_final == 3] <- 1
covid$clasificacion_final[covid$clasificacion_final == 4] <- 2
covid$clasificacion_final[covid$clasificacion_final == 5] <- 2
covid$clasificacion_final[covid$clasificacion_final == 7] <- 2
covid$clasificacion_final[covid$clasificacion_final == 6] <- 3

# We check that the values have been reclassified
unique(covid$clasificacion_final)

# and we convert to factor
covid$clasificacion_final <- as.factor(covid$clasificacion_final)

# We need to define a variable which can classify the patient as
# having died or not, this can be determinet with the fecha_def variable
# as they register the defunction date. If the patient has not died, they assign
# the value of 9999-99-99.
# So we follow the same scheme as for the other variables and assing:
# 1 - Survived
# 2 - Died

covid <- covid %>% mutate(status = if_else(fecha_def == "9999-99-99", 1, 2))
unique(covid$status)

###############################################################################
#
## Data Analysis
#
###############################################################################

# First we want to know how many cases we have given their actual COVID-19 results

# by sex
covid %>% select(sexo, clasificacion_final) %>% 
  group_by(sexo, clasificacion_final) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = as.factor(clasificacion_final), y = n, fill = as.factor(sexo))) +
  geom_bar(stat = "identity") +
  labs(title = "COVID-19 Cases in México by test result as of july 10th, 2021",
       x = "Test results",
       y = "Count") +
  scale_fill_discrete(name = "Gender",
                      labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Positive", "Negative", "Not specified"))

# by age
covid %>% select(clasificacion_final, edad) %>% 
  group_by(edad, clasificacion_final) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = edad, y = n, fill = as.factor(clasificacion_final))) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "COVID-19 Cases by age and test result",
       x = "Age",
       y = "Count") +
  scale_fill_discrete(name = "Test Result",labels = c("Positive", "Negative", "Not specified"))


# by date
covid %>% select(clasificacion_final, fecha_sintomas) %>% 
  group_by(fecha_sintomas, clasificacion_final) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = fecha_sintomas, y = n, color = as.factor(clasificacion_final))) +
  geom_line() +
  labs(title = "COVID-19 test results by date of initial symptons",
       x = "Date",
       y = "Count") +
  scale_color_discrete(name = "Test Result",labels = c("Positive", "Negative", "Not specified"))

# Now we diferentiate by outcome
covid %>% select(clasificacion_final, status) %>% 
  group_by(clasificacion_final, status) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = as.factor(clasificacion_final), y = n, fill = as.factor(status))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Positive", "Negative", "Not specified")) +
  scale_fill_discrete(name = "Patient status", labels = c("Live", "Deceased"))
  

###############################################################################
### Data Modeling
covid1 <- covid

###############################################################################


# Remove id_column
covid1 <- covid1[,-1]

covid1 <- covid %>% select(-c("fecha_ingreso", "fecha_sintomas", "fecha_def"))

covid1$sexo <- as.numeric(covid1$sexo)

covid1 <- covid1 %>% select(-c("clasificacion_final"))
covid1 <- covid1 %>% select(-c("embarazo"))

# Convert status to factor
covid1$status <- as.numeric(covid1$status)
# Generate features matrix and outcome list
covids <- list(x = as.matrix(covid1[,c("sexo","edad", conditions)]),
               y = covid1$status)


# Create train/test dataset

#Split dataset
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(covids$y, times = 1, p = 0.2, list = FALSE)
train <- list(x = covids$x[-test_index,], y = covids$y[-test_index])
test <- list(x = covids$x[test_index,], y = covids$y[test_index])

# Centre data around zero
train_c <- sweep(train$x, 2, colMeans(train$x))
train_s <- sweep(train_c, 2, colSds(train$x), FUN  = "/")

train_dist <- dist(train_s)
image(as.matrix(train_dist))

# distance between all samples
dist_samples <- round(mean(as.matrix(train_dist)), 2)

# distance between live patients
dist_live <- round(mean(as.matrix(train_dist)[train$y==1]),2)
# distante between dead patients
dist_live <- round(mean(as.matrix(train_dist)[train$y==2]),2)

h <- hclust(dist(t(train_s)))

dend <- as.dendogram(h)

plot(h)

# Logistic regression
fit_glm <- glm(y~x_1 + x_2, data = train, family="binomial")
p_hat_logistic <- predict(fit_glm, test)
y_hat_llogistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))