
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rmarkdown)
library(multireport)
library(fs)
library(filesstrings)

# multireport -------------------------------------------------------------

# remotes::install_github("dgkeyes/multireport")

parameters <- read_rds("output/all_survey_count.rds") %>%
 distinct(agency_id) # %>%
# rename(program = program_id)


multireport(
  rmarkdown_file = "survey_report.Rmd",
  params_data_frame = parameters,
  report_title_param = "agency_id",
  report_format = "html_document",
  report_output_directory = "scantron_reports"
) 


report_list = list(list.files(path="scantron_reports"))


move_report <- function(report) {
  folder_name = before_last_dot(report)
  file.move(str_glue("scantron_reports/{report}"),
            dir_create(str_glue("scantron_reports/{folder_name}/")),
            overwrite = TRUE)
  file.rename(str_glue("scantron_reports/{folder_name}/{report}"),str_glue("scantron_reports/{folder_name}/default.html"))
}
  
pwalk(report_list, move_report) 

  
  
  

