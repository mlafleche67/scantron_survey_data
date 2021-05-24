# load packages

library(readxl)
library(tidyverse)
library(tidyr)
library(janitor)
library(dplyr)
library(fs)


# copy data ----------------------------------------------------------------------------

copy_data <- function(data_directory) {    # copies raw survey data from share drive to project
  
  dir_to_copy <- str_glue("//fileserver01/Share/MNN Data and Evaluation/{data_directory}")
  
  dir_copy(path = dir_to_copy,
           new_path = "data-raw/scantron-data/survey-folders",
           overwrite = TRUE)
}

copy_data(data_directory = str_glue("FY21/Statewide eval/Data/"))



# import, clean and merge files----------------------------------------------------------

import_all_data <- function(directory_name) {
  all_files <- dir_ls(path = str_glue("data-raw/scantron-data/survey-folders/{directory_name}"))
  
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
      mutate(file = str_remove(file, "data-raw/scantron-data/survey-folders/")) %>%
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
  write_rds("code-lists/agency_codes.rds")


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


# run import_all_data function over each directory; save as rds file

# adult food and health questionnaire----------------------------------------------

afq_ie_post <- import_all_data("AFQ-IE (Post)") %>%
  write_rds("data-raw/scantron-data/afhq-data-post-ie-s.rds")

afq_de_post <- import_all_data("AFQ-DE (Post)") %>%
  write_rds("data-raw/scantron-data/afhq-data-post-de-s.rds")

afq_de_pre <- import_all_data("AFQ-DE (Pre)") %>%
  write_rds("data-raw/scantron-data/afhq-data-pre-de-s.rds")

# combining adult food health questionnaire pre and post DE data
afq_de <- rbind(afq_de_post, afq_de_pre) %>%
  write_rds("data-raw/scantron-data/afhq-data-de-s.rds")

# youth fruit and vegetable screener-----------------------------------------------

fv_screener_youth_post <- import_all_data("Youth FV (Post)") %>%
  write_rds("data-raw/scantron-data/yfv-data-post-s.rds")

fv_screener_youth_pre <- import_all_data("Youth FV (Pre)") %>%
  write_rds("data-raw/scantron-data/yfv-data-pre-s.rds")

fv_screener_youth <- rbind(fv_screener_youth_post, fv_screener_youth_pre) %>%
  write_rds("data-raw/scantron-data/yfv-data-s.rds")

# adult physical activity screener-------------------------------------------------

pa_screener_adult_post <- import_all_data("Adult PA (Post)") %>%
  write_rds("data-raw/scantron-data/apa-data-post-s.rds")

pa_screener_adult_pre <- import_all_data("Adult PA (Pre)") %>%
  write_rds("data-raw/scantron-data/apa-data-pre-s.rds")

# combining adult physical activity pre and post data
pa_screener_adult <- rbind(pa_screener_adult_pre, pa_screener_adult_post) %>%
  write_rds("data-raw/scantron-data/apa-data-s.rds")

# youth physical activity screener-------------------------------------------------

pa_screener_youth_pre <- import_all_data("Youth PA (Pre)") %>%
  write_rds("data-raw/scantron-data/ypa-data-pre-s.rds")

pa_screener_youth_post <- import_all_data("Youth PA (Post)") %>%
  write_rds("data-raw/scantron-data/ypa-data-post-s.rds")

# combining youth physical activity screener pre and post data
pa_screener_youth <- rbind(pa_screener_youth_pre, pa_screener_youth_post) %>%
  write_rds("data-raw/scantron-data/ypa-data-s.rds")

# parent survey-------------------------------------------------------------------

# community version
parent_survey_comm <- import_all_data("Parent Survey (Comm)") %>%
  write_rds("data-raw/scantron-data/ps-c-s.rds")
# school version
parent_survey_school <- import_all_data("Parent Survey (School)") %>%
  write_rds("data-raw/scantron-data/ps-s-s.rds")

# program evaluation survey-------------------------------------------------------

process_eval_adult <- import_all_data("Program Eval (Adults)") %>%
  write_rds("data-raw/scantron-data/pea-data-s.rds")
process_eval_youth <- import_all_data("Program Eval (Youth)") %>%
  write_rds("data-raw/scantron-data/pey-data-s.rds")

# that's me survey----------------------------------------------------------------

thats_me <- import_all_data("That's Me") %>%
  write_rds("data-raw/scantron-data/tm-data-s.rds")














