
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(fs)


# Adult Food Questionnaire ------------------------------------------------


# Parent Survey School ----------------------------------------------------

all_files <- dir_ls(path = "data-raw/Parent Survey-school")

import_parent_survey_data <- function(file_name) {
  
  read_excel(file_name,
             na = "*",
             guess_max = 1000) %>% 
    clean_names() %>% 
    mutate(grade = as.character(grade)) %>% 
    mutate(birth_month = as.character(birth_month)) %>% 
    mutate(birth_day = as.character(birth_day)) 
  
}


parent_surveys <- map_df(all_files, import_parent_survey_data)


# That's Me ---------------------------------------------------------------

thats_me_files <- dir_ls(path = "data-raw/Thats Me")

import_all_data <- function(file_name) {
  
  read_excel(file_name,
             na = c("*", "**"),
             col_types = "text") %>% 
    clean_names() 
  
}


thats_me_data <- map_df(thats_me_files, import_all_data)


# General Function --------------------------------------------------------

import_all_data <- function(directory_name) {
  
  all_files <- dir_ls(path = str_glue("data-raw/{directory_name}"))
  
  import_single_file <- function(file_name) {
    
    read_excel(file_name,
               na = c("*", "**"),
               col_types = "text") %>% 
      clean_names() 
    
  }
  
  map_df(all_files, import_single_file) %>% 
    type_convert()
  
}


process_eval_youth <- import_all_data("Process eval-youth") 

fv_screener_adult_ie_post <- import_all_data("FV screener-adult-ie-post")

