rm(list = ls())
source("/Users/JasonWang/Documents/LibR/fun_generic.R")
source("/Users/JasonWang/Documents/LibR/modules_shiny.R")



################################ BEGIN: Import contol file ##########################
### import control file
file_control <- "1-CONTROL-APP.xlsx"
tab_control <- readxl::excel_sheets(paste0("DATA/", file_control))
lst_control <- purrr::map(tab_control, function(x) readxl::read_excel(paste0("DATA/", file_control), sheet=x))
names(lst_control) <- tab_control
## checks data assumptions
Check.ExistVarInDF(lst_control, c("dir_data", "dir_model"))
Check.ExistVarInDF(lst_control$dir_data, c("nam_data", "dir_data"))
Check.Unique(lst_control$dir_data$nam_data)
stopifnot(all("data_raw" %in% lst_control$dir_data$nam_data))
###########################################################END: Import contol file ##


################################ BEGIN: LOAD data & model ##########################
# load raw data
env_data_RAW <- new.env()
lst_control$dir_data %>% 
  filter(nam_data == "data_raw") %>% 
  select(dir_data) %>% 
  (function(x) x[[1]]) %>% 
  load(envir = env_data_RAW)
###########################################################END: LOAD data & model##