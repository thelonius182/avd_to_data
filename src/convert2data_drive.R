library(tidyverse)
library(magrittr)
library(readr)
library(fs)

rlprg_files <- dir_ls(path = "/home/lon/R_projects/avd_to_data/resources/AVD2DATA_files/Programs/",
                      type = "file",
                      regexp = "[.]rlprg") %>% as_tibble()

for (a_file in rlprg_files$value) {
  
  oe_raw <- read_file(file = a_file) %>% as_tibble()
  
  oe_data <- oe_raw %>% str_replace_all(pattern = "/Volumes/Avonden/", replacement = "/Volumes/Data/")
  
  oe_data_file_name <- str_replace(a_file, pattern = "/AVD2DATA_files/Programs/", replacement = "/AVD2DATA_conv_files/")
  
  write_file(oe_data, file = oe_data_file_name)
}
