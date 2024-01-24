# Jennifer Collister
# 30/09/20

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}


specs <- function() {
  
  # If you're here to write a new spec, you can run this source line interactively
  # to load all the variable derivation objects into your working environment
  # so you get autocomplete when typing them!
  source(file.path(config$scripts$cleaning, "derivation_objects.R"),
         local = if (sys.nframe() == 0L) {
           FALSE
           } else {
             TEUmaps <- new.env()
             }
         )
  if (exists("TEUmaps")) {
    attach(TEUmaps)
    on.exit(detach(TEUmaps))
  }
  
  # Dataset specifications
  
  TEUvars_common <- list(
    ID,
    BaC_Sex,
    TEU_BaC_DateOfBirth,
    TEU_BaC_YoB,
    Rec_DateAssess,
    BaC_Age,
    BaC_AgeRecruit,
    TEU_BaC_AgeAtRec,
    TEU_BaC_AgeCat,
    TEU_ethnicgrp,
    TEU_Rec_AssessCentre,
    TEU_Rec_Country,
    BSM_BMI,
    TEU_BSM_BMIcat
  )
  
  UKB_genetic <- list(
    ID,
    GeP_UsedInPCA, # Identifies participants which met UKB QC for inclusion in PCA
    GeP_Outliers, # Identifies participants who are outliers for missingness and heterozygosity
    GeP_ethnic, # Identifies participants with genetic White British ancestry
    GeP_Array,
    GeP_PC(pc=1),
    GeP_PC(pc=2),
    GeP_PC(pc=3),
    GeP_PC(pc=4),
    GeP_PC(pc=5),
    GeP_PC(pc=6),
    GeP_PC(pc=7),
    GeP_PC(pc=8),
    GeP_PC(pc=9),
    GeP_PC(pc=10), # Genetic Principal Components of ancestry
    GeP_Sex, # Used to check for sex discordance
    BaC_Sex # Used to check for sex discordance
  )

  
  breast_cancer <- c(
    TEU_BreastCancer(),
    TEU_Mastectomy(record_level=TRUE),
    list(
      VeI_PregnantNow,
      TEU_Dth_NotBrCa_dthdate(record_level=TRUE, ICD10_codes=paste0("C50", seq(1, 9, by=1)), exclude=TRUE),
      TEU_Dth_BrCa_primary(record_level=TRUE, ICD10_codes=paste0("C50", seq(1, 9, by=1)), exclude=FALSE),
      TEU_HES_BrCa_inc(record_level=TRUE),
      TEU_HES_BrCa_incdate(record_level=TRUE),
      Admin_CaR_CensorDate,
      Admin_HES_CensorDate,
      Admin_Dth_CensorDate,
      Admin_CensorDate_Cancer,
      BaC_LostFUDate,
      BrCa_censordate,
      TEU_BrCa_status,
      TEU_BrCa_time,
      TEU_BrCa_age,
      TEU_BrCa_313_PRS,
      TEU_BrCa_313_PRS_quintiles,
      TEU_BrCa_100k_PRS,
      TEU_BrCa_100k_PRS_quintiles
      )
    )
  
  GailModel <- c(
    TEUvars_common,
    FemaleSpecificFactors(),
    UKB_genetic,
    genomics_prs(),
    breast_cancer,
    breast_biopsies(),
    list(
      AtypicalHyperplasia,
      Gail_AtypicalHyperplasia,
      TEU_BaC_AgeAtRec,
      Gail_ethnicity,
      Gail_ethniccat,
      MenarcheAge_Cat,
      AgeFirstBirth_Cat,
      FamilyBrCa_HowMany,
      Gail_risk(years=10),
      Gail_risk_cal(years=10)
    )
  )
  
  TyrerCuzick <- c(
    TEUvars_common,
    FemaleSpecificFactors(),
    TC_FaH(),
    UKB_genetic,
    genomics_prs(),
    breast_cancer,
    list(
      TC_Parity,
      TC_MenoStatus,
      Height,
      Weight,
      AtypicalHyperplasia,
      TC_OvarianCaDx,
      TC_OvarianCaAge,
      TC_HRT_raw,
      TC_HRT_imputed,
      TEU_BrCa_313_PRS,
      TEU_BrCa_313_PRS_center,
      TEU_BrCa_313_PRS_quintiles,
      TEU_BrCa_100k_PRS_center,
      TEU_BrCa_100k_PRS,
      TEU_BrCa_100k_PRS_quintiles
    )
  )
  
  PRSforBrCa <- c(
    TyrerCuzick,
    GailModel,
    list(
      train_set,
      TC_risk(dir=file.path(config$data$derived), years=10, prs=NULL),
      TC_risk(dir=file.path(config$data$derived), years=10, prs="GenomicsPLC"),
      TC_risk(dir=file.path(config$data$derived), years=10, prs="Mavaddat2019"),
      TC_risk_cal(years=10, prs=NULL),
      TC_risk_cal(years=10, prs="GenomicsPLC"),
      TC_risk_cal(years=10, prs="Mavaddat2019")
    )
  )
 
  return(environment())
}

TEU_SPECS <- specs()
