### import data
rm(list = ls())
source("/Users/JasonWang/Documents/LibR/fun_generic.R")

### PRA's data assumption:
lst_data_assum <- lst(nam_data_pra = paste0("DATA/variablepaths.xlsx"),
                      nam_scenario = c("hist", "base", "stress"),
                      sheet_scenario = c("Macroeconomic variables (b) ", "Macroeconomic variables (b) ", "Macroeconomic variables (s)"),
                      varNameRow = rep(2, 3), 
                      dataRowIndex = lst(hist = 4:67, base = 69:88, stress = 69:88),
                      year = lst(hist = 2000:2015, base = 2016:2020, stress = 2016:2020))
## we assume a Quarterly data
lst_data_assum$date <- map(names(lst_data_assum$year), function(x) {
  paste0(kronecker(lst_data_assum$year[[x]], rep(1, 4)), 
         rep(c("-03-31", "-06-30", "-09-30", "-12-31"), length(lst_data_assum$year[[x]]))) %>% 
    as.Date()
}) 
names(lst_data_assum$date) <- names(lst_data_assum$year)

################################## BEGIN B: import data from PRA's spreadsheet ############################
### import economic data from Moody's scenario
## B.1. import data dictionary
dict_eco <- readxl::read_excel("DATA/data_dict.xlsx", sheet = "ECO_PRA_STRESS_2016")
Check.Unique(dict_eco$varR)
## B.2. import data
lst_eco <- map(1:length(lst_data_assum$nam_scenario), function(x) {
  tmp_lst <- Import_Xls_2_Df(lst_data_assum$nam_data_pra, dict_eco,
                             namSheet = lst_data_assum$sheet_scenario[x], 
                             varNameRow = lst_data_assum$varNameRow[x],
                             dataRowIndex = lst_data_assum$dataRowIndex[[x]])
  tmp_lst$data$date <- lst_data_assum$date[[x]]
  tmp_lst$data
})
names(lst_eco) <- lst_data_assum$nam_scenario
lst_eco$data <- lst_eco$hist
## prepare data list
lst_eco$meta <- dict_eco %>% 
  inner_join(Get_Info_Of_Df_Num(lst_eco$data), "varR")
setdiff(dict_eco$varR, names(lst_eco$data))
############################################################ END B: import data from Moody's spreadsheet ##




################################## BEGIN B: fetch data from BoE database ############################  
### B. import business driver data from BoE
## import data dictionary
tmp_df <- readxl::read_excel("DATA/data_dict.xlsx", sheet = "businessDriver")
dict_boe_fech <- filter(tmp_df, (! is.na(varId )))
dict_busDr <- filter(tmp_df, (!(grepl("TMP_", nameR))))
## fectch data from BoE
tmp <- pdfetch::pdfetch_BOE(dict_boe_fech$varId, from = "1990-01-01")
Check.StopIf(!identical(setdiff(names(tmp), dict_boe_fech$varId), character(0)), "Some of varId cannot be found in the BoE database!")
## prepare fectched data to tibble class
df_busVar <- 1:dim(tmp)[2] %>% 
  purrr::map(function(x) as.numeric(tmp[, x])) %>% 
  (function(x) {
    names(x) <- dict_boe_fech$nameR[match(names(tmp), dict_boe_fech$varId)]
    x
  }) %>% 
  as_tibble() %>% 
  mutate(date = zoo::index(tmp))
## prepare L&A vol and NIM
# get L&A volumn
vec_vol <- apply(select(df_busVar, starts_with("TMP_IND_LoanVOL")), 1, sum)
vec_nim_100bps <- (df_busVar$BUS_IND_NII / vec_vol) * 100
stopifnot(all(vec_nim_100bps[!is.na(vec_nim_100bps)] < 5))
if (any(vec_nim_100bps[!is.na(vec_nim_100bps)] > 2) ){
  warning("Check NIM data quality, we have NIM observations greater than 2%!")
} 
plot(df_busVar$date, vec_nim_100bps, main = "Implied NIM over time (unit 100 bps)")
df_busVar<- df_busVar %>% 
  bind_cols(data_frame(BUS_IND_NII_VOL = vec_vol, BUS_IND_NII_NIM = vec_nim_100bps)) %>% 
  select(-starts_with("TMP_IND_LoanVOL"))
lst_busVar <- lst(data=df_busVar)
## prepare meta data
lst_busVar$meta <- dict_busDr %>% 
  inner_join(Get_Info_Of_Df_Num(lst_busVar$data), by = c("nameR" = "varR"))
############################################################ END B: fetch data from BoE database ##  

############ SAVE DATA
save(lst_busVar, lst_eco, file="DATA/DataRaw_PRA_eco.Rdata")
######################