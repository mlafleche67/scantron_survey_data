# load packages

library(readxl)
library(tidyverse)
library(tidyr)
library(janitor)
library(dplyr)
library(fs)

# import, clean and merge files----------------------------------------------------------

import_all_data <- function(directory_name) {
  all_files <- dir_ls(path = str_glue("data-raw/{directory_name}"))
  
  import_single_file <- function(file) {
    read_excel(file,
               na = c("*", "**"),
               col_types = "text") %>%
      clean_names() %>%
      {
        if ("agency_code" %in% names(.))
          rename(., agency_id = agency_code)
        else
          .
      } %>%
      mutate(file = str_remove(file, "data-raw/")) %>%
      separate(file,
               sep = "/",
               into = c("survey_name", "file_name")) %>%
      mutate(timing = ifelse(grepl("pre", survey_name),"pre", "post")) %>%
      separate(id,
               sep = "-",
               into = c("intervention_code","setting_code","supplementals","group")) %>%
      left_join(intervention_codes) %>%
      left_join(setting_codes) %>%
      separate(setting,
               sep = ":",
               into = c("domain_2", "setting")) %>%
      select(-domain_2) %>%
      left_join(agency_codes)
  }
  map_df(all_files, import_single_file) %>%
    type_convert()
}


# bring in agency code list-------------------------------------------------------

agency_codes <-
  read_excel("//fileserver01/Share/MNN Data and Evaluation/FY20/AgencyProgramList.xlsx") %>%
  clean_names() %>%
  rename(agency_id = agency_code) %>%
  rename(agency = agency_name) %>%
  select(agency_id, agency)


# bring in intervention code list-------------------------------------------------

intervention_codes <-
  read_excel("//fileserver01/Share/MNN Data and Evaluation/FY21/Barcode files/BarcodeGenerator.xlsm", sheet="Interventions") %>%
  clean_names() %>%
  rename("intervention"= 1) %>%
  rename("intervention_code" = id) 

intervention_codes$intervention_code <- as.character(intervention_codes$intervention_code)

intervention_codes = intervention_codes[-1,]


# bring in setting code list------------------------------------------------------

setting_codes <-
  read_excel(
    "//fileserver01/Share/MNN Data and Evaluation/FY21/Barcode files/BarcodeGenerator.xlsm",
    sheet = "EARS settings"
  ) %>%
  clean_names() %>%
  rename("setting_code" = 2)

setting_codes = setting_codes[-1, ]


# run import_all_data function over each directory--------------------------------

afq_ie_post <- import_all_data("Adult Food Questionnaire-ie-post")
afq_de_post <- import_all_data("Adult Food Questionnaire-de-post")
afq_de_pre <- import_all_data("Adult Food Questionnaire-de-pre")
fv_screener_adult_ie_post <- import_all_data("FV screener-adult-ie-post")
fv_screener_youth_post <- import_all_data("Fv screener-youth-post")
fv_screener_youth_pre <- import_all_data("Fv screener-youth-pre")
pa_screener_adult_post <- import_all_data("PA screener-adult-post")
pa_screener_adult_pre <- import_all_data("PA screener-adult-pre")
pa_screener_youth_pre <- import_all_data("PA screener-youth-pre")
pa_screener_youth_post <- import_all_data("PA screener-youth-post")
parent_survey_comm <- import_all_data("Parent Survey-comm")
parent_survey_school <- import_all_data("Parent Survey-school")
process_eval_adult <- import_all_data("Process eval-adults")
process_eval_youth <- import_all_data("Process eval-youth")
thats_me <- import_all_data("Thats Me")










