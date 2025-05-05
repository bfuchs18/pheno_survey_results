
#### import ####
file = "data/responses_2025-05-05.csv"
survey <- read.csv(file, na.strings = "")
survey_colnames <- read.csv("data/survey_colnames.csv")
print(paste("using data from", file))

# set col names
names(survey) <- survey_colnames$name

# remove non-pheno users 
survey <- survey[survey$pheno_check == "Yes",]

#### clean up ####

##### combining sets free-text #####

survey$combined_sfari_pheno_text <-
  ifelse(
    survey$combined_sfari_pheno_text %in% c(
      "SSC and SPARK",
      "SPARK and SSC to get a mix of the various phenotypic data"
    ),
    "SPARK and SSC",
    ifelse(
      survey$combined_sfari_pheno_text %in% c(
        "Maximal cross-collaboration! ",
        "not sure",
        "no specific plans -- just seems likely to be relevant in future",
        "Ideally, all datasets could be easily and reliably combined.",
        "N/A"
      ),
      NA,
      survey$combined_sfari_pheno_text
    )
  )


##### levels #####

preference_levels <- c("1", "2", "3", "4", "5")
survey$coding_boolean <- factor(survey$coding_boolean, levels = preference_levels)
survey$coding_nominal <- factor(survey$coding_nominal, levels = preference_levels)
survey$coding_ordinal <- factor(survey$coding_ordinal, levels = preference_levels)
survey$coding_dk_pna <- factor(survey$coding_dk_pna, levels = preference_levels)
survey$coding_skipped <- factor(survey$coding_skipped, levels = preference_levels)

# set factor levels for processing proficiency
prof_levels <- c("1", "2", "3", "4", "5")
survey$processing_proficiency <- factor(survey$processing_proficiency, levels = prof_levels)

# set factor levels for n pheno datasets
survey$n_pheno_datasets <- ifelse(grepl("SFARI phenotypic data was my first", survey$n_pheno_datasets), "0", survey$n_pheno_datasets)
ndataset_levels <- c("0", "1-2", "3-5", "6-10", "10+")
survey$n_pheno_datasets <- factor(survey$n_pheno_datasets, levels = ndataset_levels)

## pheno roles checklist
survey$pheno_roles_pi <- as.factor(ifelse(grepl("Principal Investigator", survey$pheno_roles), "yes", ifelse(is.na(survey$pheno_roles), NA, "no")))
survey$pheno_roles_coi <- as.factor(ifelse(grepl("Co-investigator", survey$pheno_roles), "yes", ifelse(is.na(survey$pheno_roles), NA, "no")))
survey$pheno_roles_determine_data <- as.factor(ifelse(grepl("Determined which datasets", survey$pheno_roles), "yes", ifelse(is.na(survey$pheno_roles), NA, "no")))
survey$pheno_roles_process <- as.factor(ifelse(grepl("Primary person who managed or processed raw data", survey$pheno_roles), "yes", ifelse(is.na(survey$pheno_roles), NA, "no")))
survey$pheno_roles_analyses <- as.factor(ifelse(grepl("conducted analyses", survey$pheno_roles), "yes", ifelse(is.na(survey$pheno_roles), NA, "no")))
survey$pheno_roles_ra <- as.factor(ifelse(grepl("Research Assistant", survey$pheno_roles), "yes", ifelse(is.na(survey$pheno_roles), NA, "no")))

# create compiled var
survey$compiled_pheno_role_hands_on <- ifelse(
  grepl("Determined which datasets|Primary person who managed or processed raw data|conducted analyses|Research Assistant", survey$pheno_roles), "yes", "no")


## pheno roles checklist
survey$proc_tool_excel_basic <- as.factor(ifelse(grepl("manual editing", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no")))
survey$proc_tool_excel_automate <- as.factor(ifelse(grepl("advanced automation", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no")))
survey$proc_tool_r <- as.factor(ifelse(grepl("R", survey$processing_tools, fixed = TRUE), "yes", ifelse(is.na(survey$processing_tools), NA, "no")))
survey$proc_tool_python <- as.factor(ifelse(grepl("Python", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no")))
survey$proc_tool_spss <- as.factor(ifelse(grepl("SPSS", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no")))
survey$proc_tool_jupyter <- as.factor(ifelse(grepl("Jupyter Notebook", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no")))
survey$proc_tool_stata <- as.factor(ifelse(grepl("STATA", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no")))
survey$proc_tool_sas <- as.factor(ifelse(grepl("SAS", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no")))
survey$proc_tool_matlab <- as.factor(ifelse(grepl("Matlab|matlab", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no"))) # added as "other" by respondent

# TO DO: make other option

#survey$proc_tool_apl <- as.factor(ifelse(grepl("APL", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no"))) # added as "other" by respondent
#survey$proc_tool_jmp <- as.factor(ifelse(grepl("JMP", survey$processing_tools), "yes", ifelse(is.na(survey$processing_tools), NA, "no"))) # added as "other" by respondent



# degree_field 
survey$degree_field <- tolower(survey$degree_field)

# education 
### change values / levels
### reorder

# re-order levels
#education_levels <- c("Bachelor's degree", "Masters's degree", "Doctoral degree (e.g., PhD)", "Professional degree (e.g., MD, JD, DDS)")
#n_datasets_levels <- c("0 (SFARI phenotypic data was my first)", "1-2", "3-5", "10+")

#survey$education <- factor(survey$education, levels=education_levels)
