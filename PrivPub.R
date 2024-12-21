# Load necessary libraries
library(haven)
library(dplyr)
library(labelled)
library(broom)

# Set global variables
# Define the global directory
CHEMIN <- "C:/Users/ruby0/Box/Personal/Year2024-25/Fall_Sep-Mar/Monday_2nd period(10.40-12.20)_Topics in Economics(Data and Econometric Methods II)/Lectures/2024.12.23_IVpaper/PrivPub/113904-V1/Archive"
# Set the working directory to the global directory
setwd(CHEMIN)
# Verify the current working directory
getwd()

CHEMIND <- "C:/Users/ruby0/Box/Personal/Year2024-25/Fall_Sep-Mar/Monday_2nd period(10.40-12.20)_Topics in Economics(Data and Econometric Methods II)/Lectures/2024.12.23_IVpaper/PrivPub/Rcode/Result"
mkdir(CHEMIND, recursive = TRUE)
# Verify the directory creation
list.dirs(path = "C:/Users/ruby0/Box/Personal/Year2024-25/Fall_Sep-Mar/Monday_2nd period(10.40-12.20)_Topics in Economics(Data and Econometric Methods II)/Lectures/2024.12.23_IVpaper/PrivPub/", full.names = TRUE)

# Load data
data <- read_dta(paste0(CHEMIND, "/dataPrivatePublic.dta"))

# X variables definition and label
data <- data %>%
  mutate(
    IdF = as.numeric(nregion == "116"),
    North = as.numeric(nregion == "311"),
    EconLayoff = as.numeric(motins == "1"),
    PersLayoff = as.numeric(motins == "2"),
    EndCDD = as.numeric(motins == "4"),
    EndInterim = as.numeric(motins == "5"),
    Otherend = 1 - EconLayoff - PersLayoff - EndCDD - EndInterim,
    exper0 = as.numeric(exper == "00"),
    exper1_5 = as.numeric(exper %in% c("01", "02", "03", "04", "05")),
    experM5 = 1 - exper0 - exper1_5,
    rsqstat2 = as.numeric(rsqstat == "RS2"),
    rsqstat3 = as.numeric(rsqstat == "RS3"),
    Orsqstat = 1 - rsqstat2 - rsqstat3,
    tempcomp = as.numeric(temps == "1"),
    Otemp = as.numeric(temps != "1"),
    dezus = as.numeric(zus == "ZU"),
    salaireA = as.numeric(salaire == "A"),
    salaireB = as.numeric(salaire == "B"),
    salaireC = as.numeric(salaire == "C"),
    salaireD = as.numeric(salaire == "D"),
    salaireE = as.numeric(salaire == "E"),
    salaireG = as.numeric(salaire %in% c("G", "")),
    ce1 = as.numeric(cemploi == "CE1"),
    ce2 = as.numeric(cemploi == "CE2"),
    cemiss = as.numeric(cemploi == ""),
    primo = as.numeric(ndem == 1),
    Cadre = as.numeric(CS == 3),
    Techn = as.numeric(CS == 4),
    EmployQ = as.numeric(CS == 51),
    EmployNQ = as.numeric(CS == 56),
    OuvrQ = as.numeric(CS == 61),
    OuvrNQ = as.numeric(CS %in% c(66, 99)),
    African = as.numeric(nation >= "31" & nation <= "49"),
    EasternEurope = as.numeric(nation >= "90" & nation <= "98" | nation %in% c("24", "25", "27")),
    SouthEuropTurkey = as.numeric(nation %in% c("02", "03", "14", "19", "21", "22", "24", "26", "27")),
    nochild = as.numeric(nenf == 0),
    onechild = as.numeric(nenf == 1),
    twoormorechild = as.numeric(nenf > 1),
    woman = as.numeric(sexe == "2"),
    agegr26 = as.numeric(age < 26),
    French = etranger,
    Otherregion = 1 - IdF - North,
    Othernation = 1 - French - African
  )

# Add labels
var_label(data$IdF) <- "Paris_region"
var_label(data$North) <- "North"
var_label(data$EconLayoff) <- "Economic_Layoff"
var_label(data$PersLayoff) <- "Personnal_Layoff"
var_label(data$EndCDD) <- "End_of_Fixed_Term_Contract"
var_label(data$EndInterim) <- "End_of_Temporary_Work"
var_label(data$Otherend) <- "Other_reasons_of_unemployment"
var_label(data$exper0) <- "No_exp_in_the_job"
var_label(data$exper1_5) <- "1_to_5_years_of_exp_in_the_job"
var_label(data$experM5) <- "More_5_years_of_exp_in_the_job"
var_label(data$rsqstat2) <- "Statistical_risk_level_2"
var_label(data$rsqstat3) <- "Statistical_risk_level_3"
var_label(data$Orsqstat) <- "Other_Statistical_risk"
var_label(data$tempcomp) <- "Search_for_a_full_time_position"
var_label(data$Otemp) <- "Do_not_search_for_a_full_time_position"
var_label(data$dezus) <- "Sensitive_suburban_area"
var_label(data$salaireA) <- "Wage_target_1200-1349_euros"
var_label(data$salaireB) <- "Wage_target_1350-1549_euros"
var_label(data$salaireC) <- "Wage_target_1550-1799_euros"
var_label(data$salaireD) <- "Wage_target_1800-2200_euros"
var_label(data$salaireE) <- "Wage_target_2200_euros"
var_label(data$salaireG) <- "No_Wage_target"
var_label(data$ce1) <- "Employment_component_level_1"
var_label(data$ce2) <- "Employment_component_level_2"
var_label(data$cemiss) <- "Employment_component_missing"
var_label(data$primo) <- "First_unemployment_spell"
var_label(data$Cadre) <- "Manager"
var_label(data$Techn) <- "Technician"
var_label(data$EmployQ) <- "Skilled_clerical_worker"
var_label(data$EmployNQ) <- "Unskilled_clerical_worker"
var_label(data$OuvrQ) <- "Skilled_blue_colar"
var_label(data$OuvrNQ) <- "Unskilled_blue_colar"
var_label(data$African) <- "African"
var_label(data$EasternEurope) <- "Eastern_Europe"
var_label(data$SouthEuropTurkey) <- "South_Europe_and_Turkey"
var_label(data$nochild) <- "No_child"
var_label(data$onechild) <- "One_child"
var_label(data$twoormorechild) <- "More_than_one_child"
var_label(data$woman) <- "Woman"
var_label(data$agegr26) <- "age_below_26"
var_label(data$French) <- "French"
var_label(data$Otherregion) <- "Other_regions"
var_label(data$Othernation) <- "Other_Nationality"

# Definition of variables corresponding to Region of the various operator type
data <- data %>%
  mutate(
    TypeOPP = case_when(
      lot %in% c("6", "10", "14", "15", "16", "17") ~ "Counseling",
      lot %in% c("12", "13", "19", "24", "25") ~ "Interim",
      lot %in% c("7", "18", "22", "23") ~ "Insertion",
      TRUE ~ ""
    ),
    conseil = as.numeric(TypeOPP == "Counseling"),
    interim = as.numeric(TypeOPP == "Interim"),
    insertion = as.numeric(TypeOPP == "Insertion"),
    Econseil = 0,
    Einterim = 0,
    Einsertion = 0
  )

# Further data manipulation and calculations would follow similar patterns

# Example of regression and summarizing results
model <- lm(EMPLOI_6MOIS ~ . - 1, data = data, weights = POIDS_PZ_6MOIS, subset = SAMPLE_CVEOPP == 1 & CLA == 1)

# Summarize and output regression results
summary(model)

# Save results to a file (example for one model)
write.csv(tidy(model), file = paste0(CHEMIN, "/tableA1.csv"))

# The rest of the code would follow a similar pattern of translating Stata commands to R equivalents