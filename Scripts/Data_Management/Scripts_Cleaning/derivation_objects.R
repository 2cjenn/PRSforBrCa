# Jennifer Collister
# 22/09/20

library(glue)
library(lubridate)
library(readxl)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}
# Check the 
showcase_censoring <- yaml.load_file(file.path(dirname(config$data$database), "censoring.yml"))
cancercol_icd9 <- showcase_censoring$Cancer$icd9_col
cancercol_icd10 <- showcase_censoring$Cancer$icd10_col

# Source the function definitions
source(file.path(config$scripts$cleaning, "basic_functions.R"), local = TRUE)


# Formatting of existing UKB variables

ID <- function() {
  list(
    name = "ID",
    source = "ID",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "ID",
    description = "The unique participant identifier"
  )
}

BaC_Sex <- function() {
  list(
    name = "BaC_Sex",
    source = "BaC_Sex.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "Gender",
    description = "Participant's self-reported gender"
  )
}

Rec_DateAssess <- function() {
  list(
    name = "Rec_DateAssess",
    source = c("Rec_DateAssess.0.0"),
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = "Date of baseline assessment",
    description = "Date of baseline assessment"
  )
}

Eth_Ethnicity <- function() {
  list(
    name = "Eth_Ethnicity",
    source = "Eth_Ethnicity.0.0",
    mapper = FN_factor(
      levelorder = c(
        "White",
        "British",
        "Irish",
        "Any other white background",
        "Mixed",
        "White and Black Caribbean",
        "White and Black African",
        "White and Asian",
        "Any other mixed background",
        "Asian or Asian British",
        "Indian",
        "Pakistani",
        "Bangladeshi",
        "Any other Asian background",
        "Black or Black British",
        "Caribbean",
        "African",
        "Any other Black background",
        "Chinese",
        "Other ethnic group",
        "Do not know",
        "Prefer not to answer"
      )
    ),
    post_exclusion = FALSE,
    display_name = "Ethnic group",
    description = "The participant's self-reported ethnicity (raw UKB categories)"
  )
}

BaC_RsnLostFU <- function() {
  list(
    name = "BaC_RsnLostFU.0.0",
    source = "BaC_RsnLostFU.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "Reason lost to follow-up",
    description = "The reported reason for loss to follow-up"
  )
}

BaC_Age <- function(){
  list(
    name = "BaC_Age",
    source = "BaC_Age.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Age at assessment",
    description = "Age when attended assessment centre"
  )
}

BaC_AgeRecruit <- function(){
  list(
    name = "BaC_AgeRecruit",
    source = "BaC_AgeRecruit.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Age at baseline",
    description = "Age at baseline, supplied by UKB and truncated to nearest year"
  )
}

TEU_BaC_YoB <- function() {
  list(
    name = "TEU_BaC_YoB",
    source = c("BaC_BirthYear.0.0"),
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Year of birth",
    description = "Year of birth"
  )
}

TEU_BaC_DateOfBirth <- function() {
  list(
    name = "TEU_BaC_DateOfBirth",
    source = c("BaC_BirthMonth.0.0", "BaC_BirthYear.0.0"),
    mapper = FN_MYtoDate(
      day = 15,
      monthField = "BaC_BirthMonth.0.0",
      yearField = "BaC_BirthYear.0.0"
    ),
    post_exclusion = FALSE,
    display_name = "Date of Birth",
    description = "The participant's approximate date of birth, derived from self-reported month and year with date estimated as 15th"
  )
}

TEU_BaC_AgeAtRec <- function() {
  list(
    name = "TEU_BaC_AgeAtRec",
    source = c("TEU_BaC_DateOfBirth", "Rec_DateAssess"),
    mapper = function(data) {
      as.numeric(round(difftime(data[["Rec_DateAssess"]], data[["TEU_BaC_DateOfBirth"]], unit ="days") / 365.25,
                       digits = 2))
    },
    post_exclusion = FALSE,
    display_name = "Age at recruitment, years",
    description = "The participant's approximate age at recruitment, derived from date of assessment centre visit and self-reported month and year of birth (date of birth estimated as 15th of the month)"
  )
}


BSM_BMI<-function(){
  list(
    name = 'BSM_BMI',
    source = 'BSM_BMI.0.0',
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = 'BMI',
    description = 'Body mass index (BMI) Kg/m2'
  )
}

TEU_BSM_BMIcat <- function() {
  list(
    name = "TEU_BSM_BMIcat",
    source = "BSM_BMI.0.0",
    mapper = function(x) {
      y <-
        as.character(cut(x, breaks = c(0, 18.5, 25, 30, 200), right = FALSE))
      y[is.na(y)] <- "Unknown"
      y <-
        factor(
          y,
          levels = c("[18.5,25)", "[0,18.5)", "[25,30)", "[30,200)", "Unknown"),
          labels = c("Normal", "Underweight", "Overweight", "Obese", "Unknown")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "BMI",
    description = "BMI below 18.5 was considered “underweight”, between 18.5 and 25 was “normal”, between 25 and 30 was “overweight” and above 30 was “obese”"
  )
}

TEU_Rec_AssessCentre <- function() {
  list(
    name = "TEU_Rec_AssessCentre",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding10_AssessmentCentre.csv"))
      # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=10
      y <- map[["meaning"]][match(x, map$Code)]
    },
    post_exclusion = FALSE,
    display_name = "AssessCentre",
    description = "Which assessment centre did the participant attend"
  )
}

TEU_Rec_Country <- function() {
  list(
    name = "TEU_Rec_Country",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c(
          10003,
          11001,
          11002,
          11006,
          11007,
          11008,
          11009,
          11010,
          11011,
          11012,
          11013,
          11014,
          11016,
          11017,
          11018,
          11020,
          11021,
          11024,
          11025,
          11026,
          11027,
          11028
        ) ~ "England",
        x %in% c(11004, 11005) ~ "Scotland",
        x %in% c(11003, 11022, 11023) ~ "Wales",
        TRUE ~ x
      )
      if (!all(y %in% c("England", "Scotland", "Wales"))) {
        warning(paste0("Unrecognised centre code: ", y[!y %in% c("England", "Scotland", "Wales")]))
      }
      y <- factor(y, levels=c("England", "Scotland", "Wales"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "UK country of residence",
    description = "Which country does the participant live in"
  )
}

TEU_BaC_AgeCat <- function() {
  list(
    name = "TEU_BaC_AgeCat",
    source = "TEU_BaC_AgeAtRec",
    mapper = FN_buckets(
      breaks = c(40, 50, 60, 70),
      labels = c("40-49", "50-59", "60-69"),
      right = FALSE
    ),
    post_exclusion = TRUE,
    display_name = "Age group, years",
    description = "Categorised age in years"
  )
}

TEU_HMH_Meds_HRT <- function() {
  list(
    name = "TEU_HMH_Meds_HRT",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
    ),
    mapper = FN_HMHmeds_type(medtype = "Hormone replacement therapy", string = "HRT"),
    post_exclusion = FALSE,
    display_name = "Self-reported HRT (TQ)",
    description = "Participant self-reported taking HRT medication in the touchscreen questionnaire"
  )
}

TEU_HMH_Meds_pill <- function() {
  list(
    name = "TEU_HMH_Meds_pill",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
    ),
    mapper = FN_HMHmeds_type(medtype = "Oral contraceptive pill or minipill", string = "Oral contraceptives"),
    post_exclusion = FALSE,
    display_name = "Self-reported oral contraceptives (TQ)",
    description = "Participant self-reported taking the contraceptive pill in the touchscreen questionnaire"
  )
}

TEU_ethnicgrp <- function() {
  list(
    name = "TEU_ethnicgrp",
    source = "Eth_Ethnicity.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        x %in% c(
          "Mixed",
          "White and Black Caribbean",
          "White and Black African",
          "White and Asian",
          "Any other mixed background"
        ) ~ "Mixed",
        x %in% c("Indian", "Pakistani", "Bangladeshi") ~ "S. Asian",
        x %in% c(
          "Black or Black British",
          "Caribbean",
          "African",
          "Any other Black background"
        ) ~ "Black",
        x %in% c(
          "Other ethnic group",
          "Asian or Asian British",
          "Any other Asian background",
          "Chinese"
        ) ~ "Other",
        x %in% c("Do not know", "Prefer not to answer") ~ "Unanswered",
        is.na(x) ~ "Unanswered",
        TRUE ~ "Error"
      )
      y <-
        factor(
          y,
          ordered = FALSE,
          levels = c("White", "Black", "S. Asian", "Mixed",
                     "Other", "Unanswered")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Ethnic group",
    description = "The participant's self-reported ethnicity, condensed into categories.\n'White', 'British', 'Irish' and 'Any other white background' were coded as 'White'.\n'Indian', 'Pakinstani' and 'Bangladeshi' were coded as 'S. Asian'.\n'Black or Black British', 'Carribean', 'African' and 'Any other Black background' were coded as 'Black'.\n'Mixed', 'White and Black Caribbean', 'White and Black African', 'White and Asian' and 'Any other mixed background' were coded as 'Mixed'.\n'Other ethnic group', 'Asian or Asian British', 'Any other Asian background' and 'Chinese' were coded as 'Other'"
  )
}

TEU_FaH_BrCa <- function() {
  list(
    name = "TEU_FaH_BrCa",
    source = c(
      paste0("FaH_FatherIll.0.", c(0:9)),
      paste0("FaH_MotherIll.0.", c(0:10)),
      paste0("FaH_SibIll.0.", c(0:11))
    ),
    mapper = FN_FamHist(
      conditions = c("Breast cancer"),
      label = "Breast cancer"
    ),
    post_exclusion = FALSE,
    display_name = "Family history of breast cancer",
    description = "First-degree history of breast cancer, derived by combining reported medical history of father, mother and siblings (adopted relatives were not included)"
  )
}

VeI_PregnantNow <- function() {
  list(
    name = "VeI_PregnantNow", 
    source = c("VeI_PregnantNow.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Pregnant",
    description = "Was the participant pregnant at baseline"
  )
}

Height <- function(){
  list(
    name = "Height",
    source = c("BSM_HeightStand.0.0"),
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Standing height, cm",
    description = "Standing height"
  )
}

Weight <- function(){
  list(
    name = "Weight",
    source = c("BSM_Weight.0.0"),
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Weight, kg",
    description = "Weight"
  )
}


GeP_PC <- function(pc=1) {
  list(
    name = paste0("GeP_PC_", pc), 
    source = glue("GeP_PC.0.{pc}"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = glue("Principal component {pc}"),
    description = glue("Genetic principal component {pc}, from Bycroft")
  )
}

GeP_Array <- function() {
  list(
    name = "GeP_Array", 
    source = "GeP_Batch.0.0", 
    mapper = function(x){
      coding <- read.csv_kdrive(file.path(config$cleaning$coding, "coding22000_flat_GenotypingArray.csv"))
      y <- factor(coding$L0[match(x, coding$Code)], levels=c("Axiom", "BiLEVE"))
    },
    post_exclusion = FALSE,
    display_name = "Genotype array",
    description = "Genotype array - UK BiLEVE or Biobank Axiom Array"
  )
}

GeP_ethnic <- function() {
  list(
    name = "GeP_ethnic", 
    source = "GeP_ethnic.0.0", 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Genotype ethnic grouping",
    description = "Genotype ethnic grouping"
  )
}


GeP_UsedInPCA <- function() {
  list(
    name = "GeP_UsedInPCA", 
    source = c("GeP_UsedInPCA.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Outliers <- function() {
  list(
    name = "GeP_Outliers", 
    source = c("GeP_Outliers.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Sex <- function() {
  list(
    name = "GeP_Sex", 
    source = c("GeP_Sex.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

# XL add: cancer except skin cancer (exclusion criteria)
TEU_VeI_cancer<- function(){
  list(
    name = 'TEU_VeI_cancer',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_CancerCode.0.", c(0:5)),
               paste0("VeI_CancerYear.0.",c(0:5))),
    mapper =function(data){
      
      cancer_mapper=read.csv_kdrive(file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))
      cancer_codes=cancer_mapper[-which(cancer_mapper$L0%in% c('skin cancer')),]$Code
      
      y<- FN_VI_filtercodes(dx_codes = cancer_codes,
                            colname = "VeI_Cancer",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))(data)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Type of cancer (except skin cancer)',
    description = 'Category of cancer (except skin cancer) reported in verbal interview at baseline'
  )
}

# Admin censoring date
#https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates#:~:text=Censoring%20dates,that%20provider%20is%20mostly%20complete.
# England 31/03/2017, Scotland 31/10/2016, Wales 29/02/2016
Admin_CensorDate<-function(record_level=TRUE){
  list(
    name = 'Admin_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        HES <- read_yaml(file.path(config$data$portal$HES, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
        
        deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
        
        for(name in names(HES)){
          HES[[name]] <- min(HES[[name]], deaths[[name]])
        }
      }
      else {
        HES <- read_yaml(file.path(dirname(config$data$database), "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
      }
      
      y <- dplyr::case_when(
        x=='England' ~ HES$England,
        x=='Scotland' ~ HES$Scotland,
        x=='Wales' ~ HES$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date from UKB',
    description = 'Censoring date according to origin of hospital data'
  )
}


# Lost to follow-up
BaC_LostFUDate<-function(){
  list(
    name = 'BaC_LostFUDate',
    source = 'BaC_DateLostFU.0.0',
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = 'Date lost to follow-up',
    description = 'Date lost to follow-up'
  )
}

TEU_BreastCancer <- function() {
  code_list <- list(ICD10="C50", ICD9="174")
  type <- "prevalent"
  
  list(
    list(
      name = 'VI_BrCa',
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_CancerCode.0.", c(0:5)),
                 paste0("VeI_CancerYear.0.",c(0:5))),
      mapper =function(data){
        
        cancer_mapper=read.csv_kdrive(file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))
        cancer_codes=cancer_mapper[cancer_mapper$meaning == "breast cancer",]$Code
        
        y<- FN_VI_filtercodes(dx_codes = cancer_codes,
                              colname = "VeI_Cancer",
                              instance = 0,
                              return_label = "dx",
                              mapper = file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))(data)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "VI breast cancer",
      description = "Breast cancer recorded in verbal interview data using code 1002"
    ),
    list(
      name = "BrCaAge_Any",
      source = c("ID",
                 paste0("CaR_DiagAge.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagDate.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, cancercol_icd9, by=1), ".0")),
      mapper = function(data) {
        y <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                       code_list=code_list, type="all", keepcol="DiagAge")(data)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Age at first BrCa dx",
      description = "Age at first breast cancer diagnosis, from cancer registries"
    ),
    list(
      name = "BrCaDate_Any",
      source = c("ID",  
                 "TEU_HES_BrCa_incdate", "TEU_Dth_BrCa_primary",
                 paste0("CaR_DiagDate.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, cancercol_icd9, by=1), ".0")),
      mapper = function(data){
        CaR <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=code_list, type="all", keepcol="DiagDate")(data)
        y <- coalesce(coalesce(CaR, data$TEU_Dth_BrCa_primary), data$TEU_HES_BrCa_incdate)
      },
      post_exclusion = FALSE,
      display_name = "BrCa dx date",
      description = "Date of first breast cancer diagnosis, from cancer registries"
    ),
    list(
      name = "BrCaDx_Prevalent",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, cancercol_icd9, by=1), ".0")),
      mapper = function(data) {
        codingICD10 <- read.csv(file.path(config$cleaning$coding, "coding19_flat_Icd10.csv"))
        ICD10 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                           code_list=code_list, type="prevalent", keepcol="DiagICD10")(data)
        meaningICD10 <- codingICD10$meaning[match(ICD10, codingICD10$Code)]
        
        codingICD9 <- read.csv(file.path(config$cleaning$coding, "coding87_flat_Icd9.csv"))
        ICD9 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                          code_list=code_list, type="prevalent", keepcol="DiagICD9")(data)
        meaningICD9 <- codingICD9$meaning[match(ICD9, codingICD9$Code)]
        
        y <- coalesce(meaningICD10, meaningICD9)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Prevalent BrCa dx",
      description = "Type of first breast cancer diagnosis before baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaDate_Incident",
      source = c("ID", "Rec_DateAssess", 
                 "TEU_HES_BrCa_incdate", "TEU_Dth_BrCa_primary",
                 paste0("CaR_DiagDate.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, cancercol_icd9, by=1), ".0")),
      mapper = function(data){
        CaR <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=code_list, type="incident", keepcol="DiagDate")(data)
        y <- coalesce(coalesce(CaR, data$TEU_HES_BrCa_incdate), data$TEU_Dth_BrCa_primary)
      },
      post_exclusion = FALSE,
      display_name = "Incident BrCa dx date",
      description = "Date of first breast cancer diagnosis after baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaInSitu_Prevalent",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, cancercol_icd10, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, cancercol_icd9, by=1), ".0")),
      mapper = FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=list(ICD10="D05", ICD9="2330"), type="prevalent", keepcol="DiagDate"),
      post_exclusion = FALSE,
      display_name = "Prevalent breast carcinoma in situ dx date",
      description = "Date of first breast carcinoma in situ diagnosis before baseline assessment, from cancer registries"
    )
  )
}

# Other cause dth date
TEU_Dth_NotBrCa_dthdate <-function(record_level=TRUE, ICD10_codes, exclude=FALSE){
  list(
    name = 'TEU_Dth_NotBrCa_dthdate',
    source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
    mapper = function(data){
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', 
                            record_level=record_level, exclude=exclude)(data)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Non BrCa death date',
    description = 'Death date caused by non BrCa from Death Registry data'
  )
}

# Other cause dth code
TEU_Dth_NotBrCa_dthcause <-function(record_level=TRUE, ICD10_codes, exclude=FALSE){
  list(
    name = 'TEU_Dth_NotBrCa_dthcause',
    source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
    mapper = function(data){
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_code', 
                            record_level=record_level, exclude=exclude)(data)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Non BrCa cause of death',
    description = 'Death caused by non BrCa from Death Registry data'
  )
}

TEU_Dth_BrCa_primary <- function(record_level=TRUE, ICD10_codes, exclude=FALSE){
  list(
    name='TEU_Dth_BrCa_primary',
    source=if(record_level){
      c("ID", "Admin_CaR_CensorDate")
    } else {
      c("ID", "Admin_CaR_CensorDate", paste0("Dth_ICD10Underlying.", seq(0, 1, by=1), ".0"))
    },
    mapper=function(data){
      ID <- data[["ID"]]
      brcadeathdates <- FN_Dth_filtercodes(ICD10_codes = ICD10_codes, return_label = 'dth_date', 
                                           record_level=record_level, exclude=exclude)(ID)

      data <- cbind(data, brcadeathdates)
      data$brcadeath <- data$brcadeathdates
      data$brcadeath[!is.na(data$brcadeathdates) &
                       data$brcadeathdates <= data$Admin_CaR_CensorDate] <- NA
      
      y <- data[["brcadeath"]]
      return(y)
    },
    post_exclusion=FALSE,
    display_name='Date of breast cancer in Death Registry',
    description='Date of breast cancer identified from Death Registry (ICD-9, ICD-10) data after baseline'
  )
}

# BrCa censoring date (Based on Death date by non Breast Cancer + Admin censoring date + lost to follow-up + Incident Mastectomy)
BrCa_censordate<-function(){
  list(
    name = 'BrCa_censordate',
    source = c('TEU_Dth_NotBrCa_dthdate','Admin_CensorDate_Cancer','BaC_LostFUDate','TEU_HES_Mast_incdate'),
    mapper = function(data){
      y<-pmin(data$TEU_Dth_NotBrCa_dthdate,data$Admin_CensorDate_Cancer,data$BaC_LostFUDate,data$TEU_HES_Mast_incdate,
              na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'BrCa censoring date',
    description = 'Censoring date for BrCa outcome'
  )
}

# BrCa event status (0=censored 1=BrCa event)
TEU_BrCa_status<-function(){
  list(
    name = 'TEU_BrCa_status',
    source = c('BrCa_censordate','BrCaDate_Incident'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$BrCa_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(BrCaDate_Incident) & BrCaDate_Incident<=BrCa_censordate ~ 1,
          is.na(BrCaDate_Incident) |(!is.na(BrCaDate_Incident)&BrCaDate_Incident>BrCa_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'BrCa event status',
    description = 'Event status of BrCa (0=censored, 1=BrCa event)'
    
  )
}

# BrCa follow-up time
TEU_BrCa_time<-function(){
  list(
    name = 'TEU_BrCa_time',
    source = c('TEU_BrCa_status','BrCa_censordate','BrCaDate_Incident','Rec_DateAssess'),
    mapper = function(data){
      
      data=data %>%
        mutate(
          time=case_when(
            TEU_BrCa_status==0 ~ as.numeric(difftime(BrCa_censordate, Rec_DateAssess, unit='days'))/365.25,
            TEU_BrCa_status==1 ~ as.numeric(difftime(BrCaDate_Incident, Rec_DateAssess, unit='days'))/365.25
          )
        ) %>%
        mutate(
          time=ifelse(time < 0, 0, time)
        )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'BrCa follow up time',
    description = 'If event status=0, this fields returns time difference in days between censoring date and baseline date.
    If event status=1, this fields returns time to BrCa event.'
  )
}

# BrCa follow-up age
TEU_BrCa_age<-function(){
  list(
    name = 'TEU_BrCa_age',
    source = c('TEU_BaC_AgeAtRec','TEU_BrCa_time'),
    mapper = function(data){
      data$TEU_BaC_AgeAtRec+data$TEU_BrCa_time
    },
    post_exclusion = FALSE,
    display_name = 'BrCa follow up age',
    description = 'If event status=0, this fields returns age in years between censoring date and baseline date.
    If event status=1, this fields returns age at BrCa event.'
  )
}

# 1. HES source 
TEU_HES_BrCa_inc<-function(record_level=TRUE){
  list(
    name='TEU_HES_BrCa_inc',
    source=if(record_level){
      c("ID","Rec_DateAssess", "Admin_CaR_CensorDate")
    } else {
      c("ID", "Rec_DateAssess", "Admin_CaR_CensorDate", 
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
        #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
      )
    },
    mapper=function(data){
      # Only interested in HES dx after cancer registry censoring
      data <- data %>% mutate(Rec_DateAssess = Admin_CaR_CensorDate)
      y <- FN_HES_First(
        ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210707.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup',
        record_level = record_level)(data)
      return(y)
    },
    post_exclusion=FALSE,
    display_name='Breast cancer in HES',
    description='Breast cancer status identified from HES (ICD-9, ICD-10) data after baseline'
  )
}

TEU_HES_BrCa_incdate<-function(record_level=TRUE){
  list(
    name='TEU_HES_BrCa_incdate',
    source=if(record_level){
      c("ID","Rec_DateAssess", "Admin_CaR_CensorDate")
    } else {
      c("ID", "Rec_DateAssess", "Admin_CaR_CensorDate",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
        #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
      )
    },
    mapper=function(data){
      # Only interested in HES dx after cancer registry censoring
      data <- data %>% mutate(Rec_DateAssess = Admin_CaR_CensorDate)
      y <- FN_HES_First(
        ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210707.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup_date',
        record_level = record_level)(data)
      return(y)
    },
    post_exclusion=FALSE,
    display_name='Date of breast cancer in HES',
    description='Date of breast cancer identified from HES (ICD-9, ICD-10) data after baseline'
  )
}


# Mastectomy

TEU_Mastectomy<-function(record_level=TRUE){
  list(
    list(
      name='TEU_HES_Mast_prev',
      source=c("ID","Rec_DateAssess"),
      mapper=FN_HES_First(
        OPCS3_xlsx = file.path(config$cleaning$mapping,'HES_OPCS3_Mapping_20230524.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'baseline',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data prior to or at baseline',
      description='Mastectomy status identified from HES (OPCS-3 or -4) data prior to or at baseline'
    ),
    list(
      name='TEU_HES_Mast_inc',
      source=c("ID","Rec_DateAssess"),
      mapper=FN_HES_First(
        OPCS3_xlsx = file.path(config$cleaning$mapping,'HES_OPCS3_Mapping_20230524.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data after baseline',
      description='Mastectomy status identified from HES (OPCS-3 or -4) data after baseline'
    ),
    list(
      name='TEU_HES_Mast_incdate',
      source=c("ID","Rec_DateAssess"),
      mapper=FN_HES_First(
        OPCS3_xlsx = file.path(config$cleaning$mapping,'HES_OPCS3_Mapping_20230524.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup_date',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy date from HES data after baseline',
      description='Mastectomy date from HES (OPCS-3 or -4) data after baseline'
    )
  )
}

# Admin censoring date (hospital inpatient)
#https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates#:~:text=Censoring%20dates,that%20provider%20is%20mostly%20complete.
# England 31/03/2017, Scotland 31/10/2016, Wales 29/02/2016
Admin_HES_CensorDate<-function(record_level=TRUE){
  list(
    name = 'Admin_HES_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        HES <- read_yaml(file.path(config$data$portal$HES, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
        # deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml"))
        # datelist <- lapply(x, FUN = function(z) {min(FN_toDate(HES[[z]]), FN_toDate(deaths[[z]]))})
        # y <- do.call(c, datelist)
      }
      else {
        HES <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$HES %>%
          lapply(., FUN=FN_toDate)
      } 
      y <- dplyr::case_when(
        x=='England' ~ HES$England,
        x=='Scotland' ~ HES$Scotland,
        x=='Wales' ~ HES$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date from UKB HES',
    description = 'Censoring date according to origin of hospital data'
  )
}

# Admin censoring date (Cancer outcome)
# https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates
# Note there is no record-level option for cancer registry data
Admin_CaR_CensorDate<-function(){
  list(
    name = 'Admin_CaR_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      
      CaR <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$Cancer %>%
        lapply(., FUN=FN_toDate)
      
      y <- dplyr::case_when(
        x=='England' ~ CaR$England,
        x=='Scotland' ~ CaR$Scotland,
        x=='Wales' ~ CaR$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date for cancer registries data',
    description = 'Administrative censoring date for cancer registries data by country, using assessment centre as a proxy for country'
  )
}

# Admin censoring date (Death registry)
# https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates

Admin_Dth_CensorDate<-function(record_level=TRUE){
  list(
    name = 'Admin_Dth_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
      } else {
        deaths <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$Death %>%
          lapply(., FUN=FN_toDate)
      }
      
      y <- dplyr::case_when(
        x=='England' ~ deaths$England,
        x=='Scotland' ~ deaths$Scotland,
        x=='Wales' ~ deaths$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date for death registry data',
    description = 'Administrative censoring date for death registries data by country, using assessment centre as a proxy for country'
  )
}


# XL: Added Admin_HES_CensorDate at the back because we are using operation data from HES.
Admin_CensorDate_Cancer <- function(){
  list(
    name = 'Admin_CensorDate_Cancer',
    source = c("Admin_HES_CensorDate", "Admin_Dth_CensorDate", "Admin_CaR_CensorDate"),
    mapper = function(data){
      y <- pmin(data$Admin_Dth_CensorDate, pmax(data$Admin_CaR_CensorDate, data$Admin_HES_CensorDate),data$Admin_HES_CensorDate)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date',
    description = 'Administrative censoring date, taken as the minimum of (death censoring and the maximum of (cancer registry and HES censoring))'
  )
}

FemaleSpecificFactors <- function(){
  list(
    list(
      name = "FSF_BreastScreeningEver",
      source = c("FSF_BCSEver.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Breast cancer screening",
      description = "Ever had breast cancer screening/mammogram"
    ),
    list(
      name = "FSF_YearsSinceScreen",
      source = c("FSF_BCSYears.0.0"),
      mapper = FN_timesince(),
      post_exclusion = FALSE,
      display_name = "Years since breast cancer screening",
      description = "Years since last breast cancer screening/mammogram"
    ),
    list(
      name = "FSF_AgeMenarche",
      source = c("FSF_PeriodAge.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Age at menarche, years",
      description = "Age when periods started (menarche)"
    ),
    list(
      name = "FSF_MenopauseStatus",
      source = c("FSF_Menopause.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Menopause status",
      description = "Menopause status"
    ),
    list(
      name = "FSF_AgeMenopause",
      source = c("FSF_MenopauseAge.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Age at menopause",
      description = "Age at menopause"
    ),
    list(
      name = "FSF_TimeSincePeriod",
      source = c("FSF_PeriodTime.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Time since last period",
      description = "Time since last period"
    ),
    list(
      name = "FSF_MenstrualCycle",
      source = c("FSF_CycleLength.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Length of menstrual cycle",
      description = "Length of menstrual cycle"
    ),
    list(
      name = "FSF_MenstrualToday",
      source = c("FSF_PeriodToday.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Menstruating today",
      description = "Menstruating today"
    ),
    list(
      name = "FSF_NLiveBirths",
      source = c("FSF_NLiveBirths.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Number of live births",
      description = "Number of live births"
    ),
    list(
      name = "FSF_NStillBirths",
      source = c("FSF_NStillB.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Number of still births",
      description = "Number of still births"
    ),
    list(
      name = "FSF_FirstBirthWeight",
      source = c("FSF_FirstBirthWt.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Birth weight of first child",
      description = "Birth weight of first child"
    ),
    list(
      name = "FSF_PrimipBirthAge",
      source = c("FSF_FirstBirthAge.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Age at first birth",
      description = "Age at first birth, for primiparous women"
    ),
    list(
      name = "FSF_AgeFirstBirth",
      source = c("FSF_FirstLiveBirthAge.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Age at first live birth, years",
      description = "Age at first birth, for multiparous women"
    ),
    list(
      name = "FSF_AgeLastBirth",
      source = c("FSF_LastLiveBirthAge.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Age at last live birth",
      description = "Age at last birth"
    ),
    list(
      name = "FSF_PregLoss",
      source = c("FSF_StillBMiscCTerm.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Ever had pregnancy loss",
      description = "Ever had a stillbirth, spontaneous miscarriage or termination"
    ),
    list(
      name = "FSF_NMiscarriage",
      source = c("FSF_NMisc.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Number of miscarriages",
      description = "Number of spontaneous miscarriages"
    ),
    list(
      name = "FSF_NTermination",
      source = c("FSF_NTerm.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Number of terminations",
      description = "Number of pregnancy terminations"
    ),
    list(
      name = "AgeFirstBirth",
      source = c("FSF_FirstLiveBirthAge.0.0", "FSF_FirstBirthAge.0.0"),
      mapper = function(data){
        y <- dplyr::coalesce(data[["FSF_FirstLiveBirthAge.0.0"]], data[["FSF_FirstBirthAge.0.0"]])
        # Values -3 and -4 mean Prefer not to answer and Do not remember, respectively
        y[y<0 & !is.na(y)] <- NA
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Age at first birth",
      description = "Age at first birth, combined from age at birth for primiparous women and age at first birth for multiparous women"
    ),
    list(
      name = "FSF_ContraceptivePill",
      source = c("FSF_OCPillEver.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Oral contraceptives",
      description = "Ever taken oral contraceptive pill"
    ),
    list(
      name = "FSF_ContraceptiveAgeStart",
      source = c("FSF_OCPillAgeStart.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Age started oral contraceptive pill",
      description = "Age started oral contraceptive pill"
    ),
    list(
      name = "FSF_ContraceptiveAgeStop",
      source = c("FSF_OCPillAgeLast.0.0"),
      mapper = FN_toNA(), # Note -11 means currently taking, not currently accounted for
      post_exclusion = FALSE,
      display_name = "Age when last used oral contraceptive pill",
      description = "Age when last used oral contraceptive pill"
    ),
    list(
      name = "FSF_HRT",
      source = c("FSF_HRTEver.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Ever used HRT",
      description = "Ever used hormone replacement therapy (HRT)"
    ),
    list(
      name = "FSF_HRTAgeStart",
      source = c("FSF_HRTAgeStart.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Age started HRT",
      description = "Age started hormone replacement therapy (HRT)"
    ),
    list(
      name = "FSF_HRTAgeStop",
      source = c("FSF_HRTAgeLast.0.0", "TEU_BaC_AgeAtRec"),
      mapper = function(data){
        y <- data[["FSF_HRTAgeLast.0.0"]] 
        y[y %in% c(-1, -3)] <- NA
        # y[y == -11 & !is.na(y)] <- data[["TEU_BaC_AgeAtRec"]][y == -11 & !is.na(y)]
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Age when last used HRT",
      description = "Age when last used hormone replacement therapy (HRT)"
    ),
    list(
      name = "FSF_HRTDuration",
      source = c("FSF_HRTAgeStart", "FSF_HRTAgeStop", "TEU_BaC_AgeAtRec"),
      mapper = function(data){
        start <- data[["FSF_HRTAgeStart"]]
        start[is.na(start) | start < 0] <- NA
        stop <- ifelse(!is.na(data[["FSF_HRTAgeStop"]]) & data[["FSF_HRTAgeStop"]] == -11,
                       data[["TEU_BaC_AgeAtRec"]], data[["FSF_HRTAgeStop"]])
        stop[is.na(stop) | stop < 0] <- NA
        
        y <- ifelse(is.na(start) | is.na(stop), NA, stop - start)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "HRT duration, years",
      description = "Duration of hormone replacement therapy (HRT) use, calculated from age of starting HRT and age of stopping, or current age if still taking it"
    ),
    list(
      name = "FSF_HRT_timesince",
      source = c("FSF_HRT", "FSF_HRTAgeStop", "TEU_BaC_AgeAtRec"),
      mapper = function(data){
        y <- ifelse(!is.na(data[["FSF_HRTAgeStop"]]) & data[["FSF_HRTAgeStop"]] > 0,
                    data[["TEU_BaC_AgeAtRec"]] - data[["FSF_HRTAgeStop"]], NA)
        
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Time since last HRT use, years",
      description = "Time since last HRT use, in years."
    ),
    list(
      name = "FSF_Hysterectomy",
      source = c("FSF_hysterect.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Ever had hysterectomy",
      description = "Ever had hysterectomy (womb removed)"
    ),
    list(
      name = "FSF_HysterectomyAge",
      source = c("FSF_hysterectAge.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Age at hysterectomy",
      description = "Age at hysterectomy"
    ),
    list(
      name = "FSF_BilOophorectomy",
      source = c("FSF_BilOophorect.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Had bilateral oophorectomy",
      description = "Had bilateral oophorectomy (both ovaries removed)"
    ),
    list(
      name = "FSF_BilOophorectomyAge",
      source = c("FSF_BilOophorectAge.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Age at bilateral oophorectomy",
      description = "Age at bilateral oophorectomy (both ovaries removed)"
    )
  )
}

EarlyLifeFactors <- function(){
  list(
    list(
      name = "ELF_BodySize10",
      source = c("ELF_BodySize10.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Comparative body size at age 10",
      description = "Comparative body size at age 10"
    ),
    list(
      name = "ELF_Height10",
      source = c("ELF_Height10.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Comparative height at age 10",
      description = "Comparative height at age 10"
    )
  )
}

HRT_current <- function(){
  list(
    name = "HRT_current",
    source = c("FSF_HRTAgeLast.0.0"),
    mapper = function(x) {
      y <- (x == -11 & !is.na(x))
    },
    post_exclusion = FALSE,
    display_name = "Currently taking HRT",
    description = "Currently taking hormone replacement therapy (HRT)"
  )
}


MenarcheAge_Cat <- function(){
  list(
    name = "MenarcheAge_Cat",
    source = c("FSF_PeriodAge.0.0"),
    mapper = FN_buckets(breaks=c(0, 6, 11, 13, 100), 
                        labels=c("6 or younger", "7 to 11", "12 to 13", "14 or older")),
    post_exclusion = FALSE,
    display_name = "Age at menarche, categorical",
    description = "Age at menarche, categorised for the Gail model"
  )
}


AgeFirstBirth_Cat <- function(){
  list(
    name = "AgeFirstBirth_Cat",
    source = c("AgeFirstBirth", "FSF_NLiveBirths.0.0"),
    mapper = function(data){
      y <- cut(data[["AgeFirstBirth"]], breaks=c(0, 19, 24, 29, 100), labels=c("<20", "20-24", "25-29", "30 or older"), right=TRUE)
      levels(y) <- c(levels(y), "No births", "Unknown")
      y[data[["FSF_NLiveBirths.0.0"]] == 0 & !is.na(data[["FSF_NLiveBirths.0.0"]])] <- "No births"
      y[is.na(y)] <- "Unknown"
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Age at first birth, categorical",
    description = "Age at first birth, categorised for the Gail model"
  )
}

FamilyBrCa_HowMany <- function(){
  list(
    name = "FamilyBrCa_HowMany",
    source = c("ID",
      paste0("FaH_FatherIll.0.", c(0:9)),
      paste0("FaH_MotherIll.0.", c(0:10)),
      paste0("FaH_SibIll.0.", c(0:11))
    ),
    mapper = function(data){
      long_data <- tidyr::pivot_longer(data, cols=starts_with("FaH"), 
                                       names_to=c("Relative", "Array"), 
                                       names_pattern="FaH_(.*)Ill.0.(.*)",
                                       values_drop_na=TRUE) 
      
      zero <- long_data %>% filter(value == "None of the above (group 2)") %>% count(ID) %>% filter(n == 3)
      brca <- long_data %>% filter(value == "Breast cancer") %>% count(ID)

      y <- brca[["n"]][match(data[["ID"]], brca$ID)]
      y[data[["ID"]] %in% zero$ID] <- 0
      y[is.na(y)] <- 0 # To mimic Spaeth
      
      # y <- factor(y, levels = c(0, 1, 2, -1), labels=c("None", "One", "More than one", "Unknown"))
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Number of first-degree relatives with breast cancer",
    description = "Number of first-degree relatives with breast cancer, derived from reported health conditions of parents and siblings"
  )
}

Gail_ethnicity <- function(){
  list(
    name = "Gail_ethnicity",
    source = c("Eth_Ethnicity.0.0"),
    mapper = function(x){
      y <- dplyr::case_when(
        x %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        x %in% c("Black or Black British", "African", "Caribbean", "Any other Black background") ~ "African American",
        x == "Chinese" ~ "Chinese",
        x %in% c("Asian or Asian British", "Indian", "Pakistani", "Bangladeshi", "Any other Asian background") ~ "Other Asian",
        x %in% c("Mixed",
                 "White and Black Caribbean",
                 "White and Black African",
                 "White and Asian",
                 "Any other mixed background",
                 "Other ethnic group",
                 "Do not know",
                 "Prefer not to answer") ~ "Unknown",
        TRUE ~ "Unknown"
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Ethnic group",
    description = "Ethnic group, mapped to the categorisations used in the Gail risk prediction model for breast cancer"
  )
}

Gail_ethniccat <- function(){
  list(
    name = "Gail_ethniccat",
    source = c("Gail_ethnicity"),
    mapper = function(x){
      y <- dplyr::case_when(
        x == "White" ~ "White",
        x == "African American" ~ "African American",
        x == "Chinese" ~ "Chinese",
        x == "Other Asian" ~ "Other Asian",
        TRUE ~ "Native American and other unknown race" # Native American and other unknown race
      )
      return(y)
    },
    post_exclusion= FALSE,
    display_name = "Ethnic category",
    description = "Ethnic group, mapped to numeric categories used in Gail risk prediction model"
  )
}

TEU_BrCa_313_PRS <- function() {
  list(
    name = "TEU_BrCa_313_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/brca313-mavaddat2018/outputs/prs_20230209.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "BrCa 313 PRS",
    description = "Breast cancer polygenic risk score, 313 SNPs from Mavaddat 2018 paper"
  )
}

TEU_BrCa_313_PRS_center <- function() {
  list(
    name = "TEU_BrCa_313_PRS_center", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/brca313-mavaddat2018/outputs/prs_20230329_center.sscore",
                        colname="SCORE1_SUM"),
    post_exclusion = FALSE,
    display_name = "BrCa 313 PRS",
    description = "Breast cancer polygenic risk score, 313 SNPs from Mavaddat 2018 paper"
  )
}

TEU_BrCa_313_PRS_quintiles <- function() {
  list(
    name = "TEU_BrCa_313_PRS_quintiles", 
    source = c("TEU_BrCa_313_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "BrCa 313 PRS quintiles",
    description = "Quintiles of the 313 SNP breast cancer PRS score"
  )
}

TEU_BrCa_100k_PRS <- function() {
  list(
    name = "TEU_BrCa_100k_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/brca100k-fritsche2020/outputs/prs_20230209.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "BrCa 100k PRS",
    description = "Breast cancer polygenic risk score, 100k SNPs from Fritsche 2020 paper"
  )
}

TEU_BrCa_100k_PRS_center <- function() {
  list(
    name = "TEU_BrCa_100k_PRS_center", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/brca100k-fritsche2020/outputs/prs_20230403_center.sscore",
                        colname="SCORE1_SUM"),
    post_exclusion = FALSE,
    display_name = "BrCa 100k PRS",
    description = "Breast cancer polygenic risk score, 100k SNPs from Fritsche 2020 paper"
  )
}

TEU_BrCa_100k_PRS_quintiles <- function() {
  list(
    name = "TEU_BrCa_100k_PRS_quintiles", 
    source = c("TEU_BrCa_100k_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "BrCa 100k PRS quintiles",
    description = "Quintiles of the 100k SNP breast cancer PRS score"
  )
}


genomics_prs <- function(){
  list(
    list(
      name = "PRS_menopause",
      source = c("GPLC_stdPRS_AAM.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Genomics plc standard PRS for age at menopause",
      description = "Genomics plc standard PRS for age at menopause"
    ),
    list(
      name = "PRS_bc_std",
      source = c("GPLC_stdPRS_BC.0.0"),
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Genomics plc standard PRS for breast cancer",
      description = "Genomics plc standard PRS for breast cancer"
    ),
    list(
      name = "PRS_bc_std_q",
      source = c("PRS_bc_std"),
      mapper = FN_quantiles(
        quant = 5,
        labels = c("Q1: lowest", "Q2", "Q3", "Q4", "Q5: highest")
      ),
      post_exclusion = TRUE,
      display_name = "Breast cancer PRS quintiles",
      description = "Quintiles of Genomics plc standard PRS for breast cancer"
    ),
    list(
      name = paste0("GPLC_PC_1"), 
      source = glue("GPLC_principalcomponents.0.0"), 
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Principal component 1",
      description = "Genetic principal component 1, from Genomics plc"
    ),
    list(
      name = paste0("GPLC_PC_2"), 
      source = glue("GPLC_principalcomponents.0.1"), 
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Principal component 2",
      description = "Genetic principal component 2, from Genomics plc"
    ),
    list(
      name = paste0("GPLC_PC_3"), 
      source = glue("GPLC_principalcomponents.0.2"), 
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Principal component 3",
      description = "Genetic principal component 3, from Genomics plc"
    ),
    list(
      name = paste0("GPLC_PC_4"), 
      source = glue("GPLC_principalcomponents.0.3"), 
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = "Principal component 4",
      description = "Genetic principal component 4, from Genomics plc"
    )
  )
}


breast_biopsies <- function(){
  list(
    list(
      name = "VI_BreastBiopsy_Ever",
      source = c("ID", "Rec_DateAssess", 
                 paste0("VeI_OperationCode.0.", c(0:31)),
                 paste0("VeI_OperationYear.0.", c(0:31))),
      mapper = function(data){
        
        vi_mapper=read.csv(file.path(config$cleaning$coding,"coding5_flat_Operation.csv"))
        biopsy_codes=vi_mapper[vi_mapper$meaning == "breast biopsy",]$Code
        
        y <- FN_VI_filtercodes(dx_codes = biopsy_codes,
                              colname = "VeI_Operation",
                              instance = 0,
                              return_label = "dx",
                              mapper = file.path(config$cleaning$coding,"coding5_flat_Operation.csv"))(data)
        y <- factor(ifelse(is.na(y), "No", "Yes"), levels=c("Yes", "No"))
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Ever had a breast biopsy",
      description = "Breast biopsy recorded in verbal interview data using code 1544"
    ),
    list(
      name='OPCS4_BreastBiopsy_Prevalent',
      source=c("ID","Rec_DateAssess"),
      mapper=FN_OPCS_count(
        opcs=4,
        condition='Biopsy', 
        HES_xlsx=file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        HESPath=config$data$portal$HES),
      post_exclusion=FALSE,
      display_name='Prevalent breast biopsy in OPCS',
      description='Breast biopsy identified from HES (OPCS-4) data using code B32 prior to or at baseline'
    ),
    list(
      name='OPCS3_BreastBiopsy_Prevalent',
      source=c("ID","Rec_DateAssess"),
      mapper=FN_OPCS_count(
        opcs=3,
        condition='Biopsy', 
        HES_xlsx=file.path(config$cleaning$mapping,'HES_OPCS3_Mapping_20230524.xlsx'),
        HESPath=config$data$portal$HES),
      post_exclusion=FALSE,
      display_name='Prevalent breast biopsy in OPCS',
      description='Breast biopsy identified from HES (OPCS-3) data using code 387 prior to or at baseline'
    ),
    list(
      name="Biopsy_Prevalent",
      source=c("AtypicalHyperplasia", "VI_BreastBiopsy_Ever", 
               "OPCS3_BreastBiopsy_Prevalent", "OPCS4_BreastBiopsy_Prevalent"),
      mapper=function(data){
        Atyp <- ifelse(data[["AtypicalHyperplasia"]]=="Yes", 1, 0)
        VI <- ifelse(data[["VI_BreastBiopsy_Ever"]]=="Yes", 1, 0)
        OPCS3 <- ifelse(is.na(data[["OPCS3_BreastBiopsy_Prevalent"]]), 0, data[["OPCS3_BreastBiopsy_Prevalent"]])
        OPCS4 <- ifelse(is.na(data[["OPCS4_BreastBiopsy_Prevalent"]]), 0, data[["OPCS4_BreastBiopsy_Prevalent"]])
        y <- ifelse(Atyp + VI + OPCS3 + OPCS4 > 0, 1, 0)
      },
      post_exclusion=FALSE,
      display_name="Previous breast biopsy status",
      description="Whether the participant has prevalent breast biopsies, using OPCS3 code 387, OPCS4 code B32 and Verbal Interview code 1544"
    )
  )
}

AtypicalHyperplasia <- function(){
  list(
    name = "AtypicalHyperplasia",
    source = c("ID", "Rec_DateAssess"),
    mapper = function(data){
      y <- FN_HES_First(
        ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210707.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        condition = 'Hyperplasia',
        return_label = 'baseline',
        record_level = TRUE)(data)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Atypical hyperplasia",
    description = "Prevalent diagnosis of atypical hyperplasia, using ICD9: 610.8 and ICD10: N60.8"
  )
}

Gail_AtypicalHyperplasia <- function(){
  list(
    name = "Gail_AtypicalHyperplasia",
    source = c("Biopsy_Prevalent", "AtypicalHyperplasia"),
    mapper = function(data){
      data[["GailAtypicalHyperplasia"]] <- ifelse(data[["AtypicalHyperplasia"]]=="Yes", 1, 0)
      data[["GailAtypicalHyperplasia"]][data[["Biopsy_Prevalent"]]==0] <- NA
      
      return(data[["GailAtypicalHyperplasia"]])
    },
    post_exclusion = FALSE,
    display_name = "Atypical hyperplasia",
    description = "Prevalent diagnosis of atypical hyperplasia, using ICD9: 610.8 and ICD10: N60.8"
  )
}

BOAD_Alc_Status <- function() {
  list(
    name = "BOAD_Alc_Status",
    source = c("Alc_Status.0.0"),
    mapper = function(x) {
      y <- dplyr::case_when(
        x == "Current" ~ "Yes",
        x %in% c("Previous", "Never") ~ "No",
        TRUE ~ "Unknown"
      )
      y <- factor(y)
    },
    post_exclusion = FALSE,
    display_name = "Alcohol status",
    description = "Self-reported alcohol drinking status"
  )
}

TC_Parity <- function(){
  list(
    name = "TC_Parity", 
    source = c("FSF_NLiveBirths.0.0"),
    mapper = function(x){
      y <- dplyr::case_when(
        is.na(x) ~ "Missing",
        x == -3 ~ "Missing",
        x == 0 ~ "Nulliparous",
        x >= 1 ~ "Parous",
        TRUE ~ "Missing"
      )
    },
    post_exclusion = FALSE,
    display_name = "Parity",
    description = "Number of livebirths; 0 = Nulliparous, 1 = Parous, if unknown set to 2 and age at first birth to 0"
  )
}

TC_MenoStatus <- function(){
  list(
    name = "TC_MenoStatus",
    source = c("FSF_Menopause.0.0"),
    mapper = function(x){
      y <- dplyr::case_when(
        is.na(x) ~ 3,
        x == "No" ~ 0,
        x == "Yes" ~ 2,
        TRUE ~ 3
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Menopause status",
    description = "Menopause status. All women who were not post-menopausal were categorised as pre-menopausal, we do not have any way to determine peri-menopausal status"
  )
}



TC_LCIS <- function(){
  list(
    name = "TC_LCIS",
    source = c("BrCaLobInSitu_Prevalent"),
    mapper = function(x){
      y <- ifelse(is.na(x), 0, 1)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "History of lobular carcinoma in situ",
    description = "History of lobular carcinoma in situ, set to 0 as missing or unknown for all participants"
  )
}

TC_OvarianCaDx <- function(){
  list(
    name = "TC_OvarianCaDx",
    source = c("TC_OvarianCaAge"),
    mapper = function(x){
      y <- ifelse(is.na(x), "No", "Yes")
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Prevalent Ovarian Cancer",
    description = "Ovarian cancer diagnosis before baseline assessment, from cancer registries"
  )
}

TC_OvarianCaAge <- function(){
  list(
    name = "TC_OvarianCaAge",
    source = c("ID", "Rec_DateAssess",
               paste0("CaR_DiagAge.", seq(0, cancercol_icd10, by=1), ".0"),
               paste0("CaR_DiagDate.", seq(0, cancercol_icd10, by=1), ".0"),
               paste0("CaR_DiagICD10.", seq(0, cancercol_icd10, by=1), ".0"),
               paste0("CaR_DiagICD9.", seq(0, cancercol_icd9, by=1), ".0")),
    mapper = FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                       code_list=list(ICD10="C56", ICD9="1830"), 
                       type="prevalent", keepcol="DiagAge"),
    post_exclusion = FALSE,
    display_name = "Age at diagnosis of prevalent Ovarian Cancer, years",
    description = "Age at first ovarian cancer diagnosis before baseline assessment, from cancer registries"
  )
}

TC_HRT_raw <- function(){
  list(
    name = "TC_HRT_raw",
    source = c("FSF_HRT", "FSF_HRTAgeStop", "FSF_HRT_timesince"),
    mapper = function(data){
      
      HRT <- dplyr::case_when(
        data[["FSF_HRT"]] == "No" ~ "Never",
        data[["FSF_HRT"]] %in% c("Do not know", "Prefer not to answer") ~ "Unanswered",
        data[["FSF_HRT"]] == "Yes" & is.na(data[["FSF_HRTAgeStop"]]) ~ "Unknown",
        data[["FSF_HRT"]] == "Yes" & data[["FSF_HRTAgeStop"]] == -11 ~ "Current",
        data[["FSF_HRT"]] == "Yes" & data[["FSF_HRT_timesince"]] < 5 ~ "Previous user (less than 5 years ago)",
        data[["FSF_HRT"]] == "Yes" & data[["FSF_HRT_timesince"]] >= 5 ~ "Previous user (more than 5 years ago)",
        TRUE ~ "Case not accounted for"
      )
      
      return(HRT)
    },
    post_exclusion = FALSE,
    display_name = "HRT use",
    description = "HRT use, 0 is never, 1 is previous (more than 5 years ago), 2 is previous (less than 5 years ago), 3 is current"
  )
}

TC_HRT_imputed <- function(){
  list(
    name = "TC_HRT_imputed",
    source = c("ID", "TC_HRT_raw", "TEU_BaC_AgeAtRec"),
    mapper = function(data){
      
      set.seed(12345)

      data <- data %>% 
        mutate(age = floor(TEU_BaC_AgeAtRec),
               random = runif(n(), 0, 1)
        ) %>%
        group_by(age) %>%
        mutate(prop = sum(TC_HRT_raw == "Previous user (less than 5 years ago)") / 
                 sum(TC_HRT_raw %in% c("Previous user (less than 5 years ago)", 
                                       "Previous user (more than 5 years ago)")),
               TC_HRT_imputed = dplyr::case_when(
                 TC_HRT_raw != "Unknown" ~ TC_HRT_raw,
                 random <= prop ~ "Previous user (less than 5 years ago)",
                 random > prop ~ "Previous user (more than 5 years ago)",
                 TRUE ~ "Other"
               )) %>%
        ungroup()
      
      return(data[["TC_HRT_imputed"]])
    },
    post_exclusion = TRUE,
    display_name = "HRT use",
    description = "HRT use, 0 is never, 1 is previous (more than 5 years ago), 2 is previous (less than 5 years ago), 3 is current. Women with unknown duration since stopping have had 'Previous' category imputed according to proportion of women with known status by age"
  )
}

TC_FaH <- function(){
  list(
    list(
      name = "TC_MotherBrCa",
      source = c("ID", paste0("FaH_MotherIll.0.", c(0:10))),
      mapper = function(data){
        brca <- data %>% pivot_longer(cols=starts_with("FaH_MotherIll.0.")) %>%
          filter(value =="Breast cancer")
        y <- ifelse(data[["ID"]] %in% brca$ID, "Yes", "No")
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Maternal breast cancer status",
      description = "Has the mother had breast cancer, set to 0 if not relevant, missing or unknown"
    ),
    list(
      name = "TC_MotherAge",
      source = c("FaH_MotherAge.0.0", "FaH_MotherDeathAge.0.0"),
      mapper = function(data){
        alive_age <- FN_toNA()(data[["FaH_MotherAge.0.0"]])
        dead_age <- FN_toNA()(data[["FaH_MotherDeathAge.0.0"]])
        y <- dplyr::coalesce(alive_age, dead_age)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Mother's age, at baseline (if alive) or at death, years",
      description = "Mother's age, at baseline (if alive) or at death."
    ),
    list(
      name = "TC_NSisters",
      source = c("FaH_NSisters.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Number of full sisters",
      description = "Number of full sisters (questions asked about full or adopted, not half)"
    ),
    list(
      name = "TC_NBrothers",
      source = c("FaH_NBrothers.0.0"),
      mapper = FN_toNA(),
      post_exclusion = FALSE,
      display_name = "Number of full brothers",
      description = "Number of full brothers (questions asked about full or adopted, not half)"
    ),
    list(
      name = "TC_SiblingBrCa",
      source = c("ID", paste0("FaH_SibIll.0.", c(0:11))),
      mapper = function(data){
        brca <- data %>% pivot_longer(cols=starts_with("FaH_SibIll.0.")) %>%
          filter(value =="Breast cancer")
        y <- ifelse(data[["ID"]] %in% brca$ID, "Yes", "No")
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Sibling(s) breast cancer status",
      description = "Has a sibling had breast cancer, set to 0 if not relevant, missing or unknown"
    )
  )
  
}

TC_risk <- function(dir=config$data$derived, years=10, prs=NULL){
  list(
    name = ifelse(!is.null(prs), glue("TC_{years}risk_{prs}"), glue("TC_{years}risk")),
    source = c("ID"),
    mapper = function(x){
      riskyears <- ifelse(years == "l", "lifetime", glue("{years}yr"))
      
      if(is.null(prs)) {
        dir <- file.path(dir, "noPRS", riskyears)
      } else {
        dir <- file.path(dir, "withPRS", prs, riskyears)
      }
      
      filenames <- list.files(dir, full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
      
      allrisks <- lapply(filenames, function(x){
        read.table(x, header=FALSE, sep="\t",
                   col.names=c("ID", "nyear", "risk", "pop_risk", "brca1", "brca2"))
      })
      
      risks <- dplyr::bind_rows(allrisks)
      
      y <- risks$risk[match(x, risks$ID)]
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = glue("Tyrer-Cuzick {ifelse(years == 'l', 'lifetime', glue('{years}yr'))} risk {if (!is.null(prs)) {paste0(' with PRS from ', prs)} else {' without PRS'}}"),
    description = glue("{ifelse(years == 'l', 'Lifetime', glue('{years}yr'))} risk calculated by the Tyrer-Cuzick model {if (!is.null(prs)) {paste0(' with PRS from ', prs)} else {' without PRS'}}")
  )
}

TC_risk_cal <- function(years=10, prs=NULL) {
  list(
    name = ifelse(!is.null(prs), glue("TC_{years}risk_{prs}_cal"), glue("TC_{years}risk_cal")),
    source = c(ifelse(!is.null(prs), glue("TC_{years}risk_{prs}"), glue("TC_{years}risk")), 
               "TEU_BrCa_time", "TEU_BrCa_status"),
    mapper = function(data){
      risk_col <- ifelse(!is.null(prs), glue("TC_{years}risk_{prs}"), glue("TC_{years}risk"))
      
      # Calibrate on 10 years of follow-up only
      data <- data[!is.na(risk_col),] %>% 
        mutate(
          TEU_BrCa_status = ifelse(TEU_BrCa_time < 10, TEU_BrCa_status, 0),
          TEU_BrCa_time = ifelse(TEU_BrCa_time < 10, TEU_BrCa_time, 10)
          )
      
      data$original <- data[[risk_col]]
      
      deciles <- quantile(data$original, probs = seq(0, 1, 0.1))
      
      data$risk_dec <- cut(data$original, breaks=deciles, 
                              right=TRUE, include.lowest=TRUE) 
      
      # Mean predicted risk per decile
      predicted <- data %>%
        group_by(risk_dec) %>% 
        summarise(predicted = mean(original))
      
      # Fit survival curves from Kaplan-Meier and extract observed risks by predicted risk decile
      fit <- survfit(Surv(TEU_BrCa_time, TEU_BrCa_status) ~ risk_dec, data=data)
      fit_time <- summary(fit, times = years)
      
      observed <- data.frame(risk_dec = fit_time$strata,
                             observed = 1 - fit_time$surv) %>%
        mutate(risk_dec = as.factor(str_remove(risk_dec, "risk_dec=")))
      
      # Join predicted and observed
      calibration <- predicted %>% inner_join(observed, by="risk_dec")
      
      cal_model <- lm(observed ~ predicted, data=calibration)
      cal_slope <- cal_model$coefficients[2]
      cal_intercept <- cal_model$coefficients[1]
      
      y <- cal_intercept + (cal_slope * data[[risk_col]])
      
      return(y)
    },
    post_exclusion = TRUE,
    display_name = glue("Calibrated Tyrer-Cuzick {ifelse(years == 'l', 'lifetime', glue('{years}yr'))} risk {if (!is.null(prs)) {paste0(' with PRS from ', prs)} else {'without PRS'}}"),
    description = glue("{ifelse(years == 'l', 'Lifetime', glue('{years}yr'))} risk calculated by the Tyrer-Cuzick model {if (!is.null(prs)) {paste0(' with PRS from ', prs)} else {'without PRS'}} and calibrated for the UK Biobank population")
  )
}

ukbtools_related <- function(){
  list(
    name = "ukbtools_related",
    source = c("ID"),
    mapper = function(x){
      source("Scripts/Stats_Analysis/ukbtools_relatedness.R")
      
      # Relatedness
      rel <- fread(config$data$relatedness)
      ukb_with_data <- as.character(x)
      
      remove <- ukb_gen_samples_to_remove(data=rel, ukb_with_data=ukb_with_data)
      
      y <- remove[match(x, remove)]
      return(y)
    },
    post_exclusion = TRUE,
    display_name = "Related individuals",
    description = "Maximal subset of related individuals identified by KING relatedness coefficient, prioritising retaining individuals with available phenotypic information"
  )
}

Gail_risk <- function(years=10) {
  list(
    name = glue("gail_{years}risk"),
    source = c("ID", "BaC_Age", "Biopsy_Prevalent", "Gail_AtypicalHyperplasia", "FSF_AgeMenarche",
               "AgeFirstBirth", "FamilyBrCa_HowMany", "Gail_ethniccat"),
    mapper = function(data){
      gail <- data %>% transmute(
        ID = ID,
        T1 = BaC_Age, # Initial age
        T2 = BaC_Age + years, # BrCa projection age
        N_Biop = Biopsy_Prevalent, # Number of breast biopsies
        HypPlas = ifelse(is.na(Gail_AtypicalHyperplasia), 99, Gail_AtypicalHyperplasia), # Dx of hyperplasia
        AgeMen = ifelse(is.na(FSF_AgeMenarche), 99, FSF_AgeMenarche), # Age at menarche
        Age1st = ifelse(is.na(AgeFirstBirth), 99, AgeFirstBirth), # Age at first birth
        N_Rels = ifelse(is.na(FamilyBrCa_HowMany), 99, FamilyBrCa_HowMany), # Number of first-degree relatives with breast cancer
        Race = dplyr::case_when(
          Gail_ethniccat == "White" ~ 1,
          Gail_ethniccat == "African American" ~ 2,
          Gail_ethniccat == "Chinese" ~ 6,
          Gail_ethniccat == "Other Asian" ~ 11,
          TRUE ~ 4 # Native American and other unknown race
        ) # Ethnicity category
      )
      
      y <- absolute.risk(gail)/100 # Put it on % scale
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = glue("Gail {years}yr risk"),
    description = glue("{years}yr risk calculated by the Gail model")
  )
}
 

train_set <- function(proportion_train=0.8){
  list(
    name = "train_set",
    source = "ID",
    mapper = function(x){
      set.seed(2305)
      y <- ifelse(runif(x, min=0, max=1)<=0.8, TRUE, FALSE)
      return(y)
    },
    post_exclusion = TRUE,
    display_name = "In training set",
    description = "Indicates whether this participants has been randomly allocated to the training set, if not then the test set."
  )
}

Gail_risk_cal <- function(years=10) {
  list(
    name = glue("gail_{years}risk_cal"),
    source = c(glue("gail_{years}risk"), "TEU_BrCa_time", "TEU_BrCa_status"),
    mapper = function(data){
      risk_col <- glue("gail_{years}risk")
      
      # Calibrate on 10 years of follow-up only
      data <- data[!is.na(risk_col),] %>% 
        mutate(
          TEU_BrCa_status = ifelse(TEU_BrCa_time < 10, TEU_BrCa_status, 0),
          TEU_BrCa_time = ifelse(TEU_BrCa_time < 10, TEU_BrCa_time, 10)
        )
      
      data$original <- data[[risk_col]]

      deciles <- quantile(data$original, probs = seq(0, 1, 0.1))

      data$risk_dec <- cut(data$original, breaks=deciles,
                              right=TRUE, include.lowest=TRUE)

      # Mean predicted risk per decile
      predicted <- data %>%
        group_by(risk_dec) %>%
        summarise(predicted = mean(original))

      # Fit survival curves from Kaplan-Meier and extract observed risks by predicted risk decile
      fit <- survfit(Surv(TEU_BrCa_time, TEU_BrCa_status) ~ risk_dec, data=data)
      fit_time <- summary(fit, times = years)

      observed <- data.frame(risk_dec = fit_time$strata,
                             observed = 1 - fit_time$surv) %>%
        mutate(risk_dec = as.factor(str_remove(risk_dec, "risk_dec=")))

      # Join predicted and observed
      calibration <- predicted %>% inner_join(observed, by="risk_dec")

      cal_model <- lm(observed ~ predicted, data=calibration)
      cal_slope <- cal_model$coefficients[2]
      cal_intercept <- cal_model$coefficients[1]

      y <- cal_intercept + (cal_slope * data[[risk_col]])

      return(y)
    },
    post_exclusion = TRUE,
    display_name = glue("Calibrated Gail {ifelse(years == 'l', 'lifetime', glue('{years}yr'))} risk"),
    description = glue("{ifelse(years == 'l', 'Lifetime', glue('{years}yr'))} risk calculated by the Gail model and calibrated for the UK Biobank population")
  )
}
