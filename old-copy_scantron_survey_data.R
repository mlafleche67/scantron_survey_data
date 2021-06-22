# load packages

library(tidyverse)
library(fs)

# copy data function----------------------------------------------------------------

# copies raw survey data from share drive to project

copy_data <- function(data_directory) {
  
  dir_to_copy <- str_glue("//fileserver01/Share/MNN Data and Evaluation/{data_directory}")
  
  dir_copy(path = dir_to_copy,
           new_path = "data-raw",
           overwrite = TRUE)
}

copy_data(data_directory = str_glue("FY21/Statewide eval/Data/"))

