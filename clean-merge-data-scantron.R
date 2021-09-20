# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(flextable)
library(officer)
library(dplyr)


# count surveys function

count_surveys <- function(df) {
  if (nrow(df) > 0) {
    merged_file <- df %>%
      count(agency_id) %>%
      #rename(total = n) %>%
      drop_na()
  } else {
    df %>%
      add_column(agency_id = "", n = 0)
  }
}

# function to select specific columns from each survey rds file

select_columns <- function(df) {
  if ("agency_id" %in% colnames(df)) {
    survey_intervention <- df %>%
      select(agency_id, survey_name, intervention)
  } else {
    df %>%
      add_column(agency_id = "", survey_name = "", intervention = "") 
  }
}

# read in agency codes

agency_codes <- read_rds("code-lists/agency_codes.rds")  

# count surveys and rename total column--------------------------------------------
# afq_ie_post_count <-
#   count_surveys(read_rds("data-raw/scantron-data/afhq-data-post-ie-s.rds")) %>%
#   rename("AFQ-IE (Post)" = n)

afq_de_post_count <-
  count_surveys(read_rds("data-raw/scantron-data/afhq-data-de-s.rds")) %>%
  rename("AFQ-DE (Post)" = n)

afq_de_pre_count <-
  count_surveys(read_rds("data-raw/scantron-data/afhq-data-pre-de-s.rds")) %>%
  rename("AFQ-DE (Pre)" = n)

fvyouth_pre_count <-
  count_surveys(read_rds("data-raw/scantron-data/yfv-data-pre-s.rds")) %>%
  rename("Youth FV (Pre)" = n)

fvyouth_post_count <-
  count_surveys(read_rds("data-raw/scantron-data/yfv-data-post-s.rds")) %>%
  rename("Youth FV (Post)" = n)

adultpa_pre_count <-
  count_surveys(read_rds("data-raw/scantron-data/apa-data-pre-s.rds")) %>%
  rename("Adult PA (Pre)" = n)

adultpa_post_count <-
  count_surveys(read_rds("data-raw/scantron-data/apa-data-post-s.rds")) %>%
  rename("Adult PA (Post)" = n)

youthpa_pre_count <-
  count_surveys(read_rds("data-raw/scantron-data/ypa-data-pre-s.rds")) %>%
  rename("Youth PA (Pre)" = n)

youthpa_post_count <-
  count_surveys(read_rds("data-raw/scantron-data/ypa-data-post-s.rds")) %>%
  rename("Youth PA (Post)" = n)

parent_comm_count <-
  count_surveys(read_rds("data-raw/scantron-data/ps-c-s.rds")) %>%
  rename("Parent Survey (Comm)" = n)

parent_school_count <-
  count_surveys(read_rds("data-raw/scantron-data/ps-s-s.rds")) %>%
  rename("Parent Survey (School)" = n)

thats_me_count <-
  count_surveys(read_rds("data-raw/scantron-data/tm-data-s.rds")) %>%
  rename("That's Me" = n)

process_youth_count <-
  count_surveys(read_rds("data-raw/scantron-data/pey-data-s.rds")) %>%
  rename("Program Eval (Youth)" = n)

process_adult_count <-
  count_surveys(read_rds("data-raw/scantron-data/pea-data-s.rds")) %>%
  rename("Program Eval (Adults)" = n)

# running select columns function over each rds file------------------------------------

# afq_ie_post_intervention_count <-
#   select_columns(read_rds("data-raw/scantron-data/afhq-data-post-ie-s.rds")) 


afq_de_post_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/afhq-data-de-s.rds")) 


afq_de_pre_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/afhq-data-pre-de-s.rds")) 


fvyouth_pre_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/yfv-data-pre-s.rds")) 


fvyouth_post_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/yfv-data-post-s.rds")) 


adultpa_pre_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/apa-data-pre-s.rds")) 


adultpa_post_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/apa-data-post-s.rds")) 


youthpa_pre_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/ypa-data-pre-s.rds")) 


youthpa_post_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/ypa-data-post-s.rds")) 


parent_comm_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/ps-c-s.rds")) 


parent_school_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/ps-s-s.rds")) 


thats_me_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/tm-data-s.rds")) 


process_youth_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/pey-data-s.rds")) 


process_adult_intervention_count <-
  select_columns(read_rds("data-raw/scantron-data/pea-data-s.rds")) 

# binding intervention count data frames into one--------------------------------------

all_survey_interventions <- 
  rbind(#afq_ie_post_intervention_count,
        afq_de_pre_intervention_count,
        afq_de_post_intervention_count,
        fvyouth_pre_intervention_count,
        fvyouth_post_intervention_count,
        adultpa_pre_intervention_count,
        adultpa_post_intervention_count,
        youthpa_pre_intervention_count,
        youthpa_post_intervention_count,
        parent_comm_intervention_count,
        parent_school_intervention_count,
        thats_me_intervention_count,
        process_youth_intervention_count,
        process_adult_intervention_count) %>%
  count(agency_id, survey_name, intervention) %>%
  left_join(agency_codes) %>%
  mutate_at("intervention", ~replace(., is.na(.), "no intervention identified")) %>%
  select(agency_id, agency, everything())


# join agency code list with all survey count data frames and add total column---------

all_survey_count <-
  left_join(agency_codes, afq_de_pre_count, by = "agency_id") %>%
  left_join(afq_de_post_count) %>%
  #left_join(afq_ie_post_count) %>%
  left_join(fvyouth_pre_count) %>%
  left_join(fvyouth_post_count) %>%
  left_join(adultpa_pre_count) %>%
  left_join(adultpa_post_count) %>%
  left_join(youthpa_pre_count) %>%
  left_join(youthpa_post_count) %>%
  left_join(parent_comm_count) %>%
  left_join(parent_school_count) %>%
  left_join(process_youth_count) %>%
  left_join(process_adult_count) %>%
  left_join(thats_me_count) %>%
  adorn_totals(where = c("row", "col"), fill = "Total") %>%
mutate_at(c(3:15), ~replace(., is.na(.), "")) %>%
  write_rds("output/all_survey_count.rds")


all_survey_count_long <- all_survey_count %>%
  pivot_longer(cols = -c(agency_id, agency, Total),
               names_to = "survey_name") 


# join all_survey_count and all_survey_interventions data frames into one-------------

joined_survey_intervention <- all_survey_count_long %>%
  left_join(all_survey_interventions, by = c("agency_id", "agency", "survey_name")) %>%
  rename("survey_total" = value) %>%
  rename("intervention_total" = n) %>%
  subset(agency != "Total") %>%
  pivot_wider(id_cols = c("agency_id", "agency", "intervention", "intervention_total", "Total"), names_from = "survey_name", values_from = "intervention_total") %>%
  rename("all_surveys_total" = "Total") %>%
  relocate("intervention", .after="That's Me") %>%
  select(-agency_id) %>%
  mutate_at(c(2:15), as.numeric) %>%
  clean_names

# remove duplicate total lines and NAs, rearrange and rename columns 

joined_survey_intervention_final <- joined_survey_intervention %>%
  filter(all_surveys_total >=0 & (!is.na(afq_de_pre) |
                                    !is.na(afq_de_post) |
                                    #!is.na(afq_ie_post) |
                                    !is.na(youth_fv_pre) | 
                                    !is.na(youth_fv_post) | 
                                    !is.na(adult_pa_pre) | 
                                    !is.na(adult_pa_post) |
                                    !is.na(youth_pa_pre) | 
                                    !is.na(youth_pa_post) | 
                                    !is.na(parent_survey_comm) | 
                                    !is.na(parent_survey_school) | 
                                    !is.na(program_eval_youth) | 
                                    !is.na(program_eval_adults) | 
                                    !is.na(thats_me))) %>%
  right_join(agency_codes) %>%
  mutate_at("all_surveys_total", ~replace(., is.na(.), 0)) %>%
  arrange(., agency) %>%
  relocate("intervention", .after = "all_surveys_total") %>%
  rename("Organization" = agency, 
         "AFHQ-DE (Pre)" = afq_de_pre, 
         "AFHQ-DE (Post)" = afq_de_post,
         #"AFHQ-IE (Post)" = afq_ie_post,
         "Youth FV (Pre)" = youth_fv_pre,
         "Youth FV (Post" = youth_fv_post,
         "Adult PA (Pre)" = adult_pa_pre,
         "Adult PA (Post)" = adult_pa_post,
         "Youth PA (Pre)" = youth_pa_pre,
         "Youth PA (Post)" = youth_pa_post,
         "Parent Survey (Comm)" = parent_survey_comm,
         "Parent Survey (School)" = parent_survey_school,
         "Program Eval (Youth)" = program_eval_youth,
         "Program Eval (Adults)" = program_eval_adults,
         "That's Me" = thats_me,
         #"Total Surveys" = all_surveys_total,
         "Intervention" = intervention) %>%
  select(-all_surveys_total) %>%
  mutate_at(c(3:15), as.numeric) %>%
  adorn_totals(where = "col", fill = "Total") %>%
  write_rds("output/joined_survey_intervention_final.rds")
  