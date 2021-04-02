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
  read_excel("//fileserver01/Share/MNN Data and Evaluation/FY21/fy21_Agency-Program_code list.xlsx") %>%
  clean_names() %>%
  rename(agency_id = agency_code) %>%
  rename(agency = agency_name) %>%
  select(agency_id, agency) %>%
  write_rds("output/agency_codes.rds")


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


# run import_all_data function over each directory; save as rds file--------------------------------


afq_ie_post <- import_all_data("Adult Food Questionnaire-post-ie") %>%
  write_rds("output/afq_ie_post.rds")
afq_de_post <- import_all_data("Adult Food Questionnaire-post-de") %>%
  write_rds("output/afq_de_post.rds")
afq_de_pre <- import_all_data("Adult Food Questionnaire-pre-de") %>%
  write_rds("output/afq_de_pre.rds")
# fv_screener_adult_ie_post <- import_all_data("FV screener-adult-post-ie")
fv_screener_youth_post <- import_all_data("Fv screener-youth-post") %>%
  write_rds("output/fv_screener_youth_post.rds")
fv_screener_youth_pre <- import_all_data("Fv screener-youth-pre") %>%
  write_rds("output/fv_screener_youth_pre.rds")
pa_screener_adult_post <- import_all_data("PA screener-adult-post") %>%
  write_rds("output/pa_screener_adult_post.rds")
pa_screener_adult_pre <- import_all_data("PA screener-adult-pre") %>%
  write_rds("output/pa_screener_adult_pre.rds")
pa_screener_youth_pre <- import_all_data("PA screener-youth-pre") %>%
  write_rds("output/pa_screener_youth_pre.rds")
pa_screener_youth_post <- import_all_data("PA screener-youth-post") %>%
  write_rds("output/pa_screener_youth_post.rds")
parent_survey_comm <- import_all_data("Parent Survey-comm") %>%
  write_rds("output/parent_survey_comm.rds")
parent_survey_school <- import_all_data("Parent Survey-school") %>%
  write_rds("output/parent_survey_school.rds")
process_eval_adult <- import_all_data("Process eval-adults") %>%
  write_rds("output/process_eval_adult.rds")
process_eval_youth <- import_all_data("Process eval-youth") %>%
  write_rds("output/process_eval_youth.rds")
thats_me <- import_all_data("That's Me") %>%
  write_rds("output/thats_me.rds")










