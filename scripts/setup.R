
#### import ####
file = "data/responses_2025-05-11.csv"
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

survey <- survey %>%
  mutate(combined_sfari_pheno_text =
           case_when(combined_sfari_pheno_text == "SSC and SPARK"  ~ "SPARK and SSC",
                     str_detect(combined_sfari_pheno_text, 'SPARK and SSC to get a mix')  ~ "SPARK and SSC",
                     str_detect(combined_sfari_pheno_text, 'Maximal cross-collaboration|not sure|no specific plans| Ideally, all datasets|N/A')  ~ NA,
                     str_detect(combined_sfari_pheno_text, 'Across Research Match datasets')  ~ "SPARK and SSC; across RM",
                     str_detect(combined_sfari_pheno_text, 'Simons Searchlight, SSC, AIC')  ~ "Searchlight, SSC, AIC",
                     str_detect(combined_sfari_pheno_text, 'SPARK, searchlight')  ~ "SPARK and Searchlight",
                     str_detect(combined_sfari_pheno_text, 'would like to look into')  ~ "SPARK and SSC; would like to SPARK and Searchlight",
                     T ~ combined_sfari_pheno_text))

survey <- survey %>%
  mutate(combined_sfari_nonsfari_text =
           case_when(str_detect(combined_sfari_nonsfari_text, 'other research studies at Marcus') ~ "SPARK and Marcus/Emory",
                     str_detect(combined_sfari_nonsfari_text, 'so gnarly') ~ "SSC, NDAR, ASC",
                     str_detect(combined_sfari_nonsfari_text, 'facilitated several investigators') ~ "Searchlight and data from investigators and consortium groups (e.g., COMBINEDBrain)",
                     str_detect(combined_sfari_nonsfari_text, 'SPARK with my NET webcam database')  ~ "SPARK and investigaor webcam database",
                     str_detect(combined_sfari_nonsfari_text, 'my own data based on interviews with SPARK participants')  ~ "SPARK and investigator interview data",
                     str_detect(combined_sfari_nonsfari_text, 'everything I can')  ~ "everything I can",
                     str_detect(combined_sfari_nonsfari_text, 'Simons Searchlight and Citizen Health')  ~ "Searchlight and Citizen Health",
                     str_detect(combined_sfari_nonsfari_text, 'cohorts at Cardiff University, UCLA and Emory')  ~ "SFARI and cohorts at Cardiff University, UCLA and Emory",
                     str_detect(combined_sfari_nonsfari_text, 'Simons Searchlight, SSC, AIC & NDAR')  ~ "Searchlight, SSC, AIC & NDAR",
                     combined_sfari_nonsfari_text == "no"  ~ NA,
                     str_detect(combined_sfari_nonsfari_text, 'not yet')  ~ NA,
                     T ~ combined_sfari_nonsfari_text))


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
  grepl("Primary person who managed or processed raw data|conducted analyses|Research Assistant", survey$pheno_roles), "yes", "no")

survey$compiled_pheno_role_primary_hands_on <- ifelse(
  grepl("Primary person who managed or processed raw data|conducted analyses", survey$pheno_roles), "yes", "no")

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
survey$edu_clean <- ifelse(grepl("MD, MPH", survey$education), "Professional degree (e.g., MD, JD, DDS)", survey$education)
edu_clean_levels <- c("Bachelor's degree", "Masters's degree", "Doctoral degree (e.g., PhD)", "Professional degree (e.g., MD, JD, DDS)")
survey$edu_clean <- factor(survey$edu_clean, levels=edu_clean_levels)


