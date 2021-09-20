## Validation Script
rm(list= ls())

## Libraries
library(tidyverse)
library(openxlsx)
library(cluster)
library(cleaninginspectoR)

## Sources
source("./R/check_log.R")
source("./R/data_falsification.R")

## Load Inputs
### Cholera
#cholera_hh <- read.xlsx("./input/WASH_WANTS Cholera_cleandata_2021-06-16_hh.xlsx")
#cholera_ki <- read.xlsx("./input/WASH_WANTS Cholera_cleandata_2021-06-17_ki.xlsx")

### Common
common_hh <- read.xlsx("./input/WASH_WANTS Common_HH_cleandata_2021-08-22.xlsx")
common_ki <- read.xlsx("./input/WASH_WANTS Common_KI_cleandata_2021-08-22.xlsx")

## Check issues
### Cholera
hh.i <- inspect_all(cholera_hh) %>% filter(!is.na(index))
write.xlsx(hh.i, paste0("./output/cholera hh cleaning issues_",Sys.Date(),".xlsx"))

ki.i <- inspect_all(cholera_ki) %>% filter(!is.na(index))
write.xlsx(ki.i, paste0("./output/cholera ki cleaning issues_",Sys.Date(),".xlsx"))

### Common
comm.hh.i <- inspect_all(common_hh) %>% filter(!is.na(index))
write.xlsx(comm.hh.i, paste0("./output/common hh cleaning issues_",Sys.Date(),".xlsx"))

comm.ki.i <- inspect_all(common_ki) %>% filter(!is.na(index))
write.xlsx(comm.ki.i, paste0("./output/common ki cleaning issues_",Sys.Date(),".xlsx"))


## Falsification
### Cholera
cholera.ki.tool <- read.xlsx("./input/tools/Cholera_KI_tool.xlsx")
ki.f <- calculateDifferences(cholera_ki, cholera.ki.tool) %>% filter(number.different.columns < 10)
write.xlsx(ki.f, paste0("./output/cholera ki falsification_",Sys.Date(),".xlsx"))

cholera.hh.tool <- read.xlsx("./input/tools/Cholera_HH_tool.xlsx")
hh.f <- calculateDifferences(cholera_hh, cholera.hh.tool) %>% filter(number.different.columns < 10)
write.xlsx(hh.f, paste0("./output/cholera hh falsification_",Sys.Date(),".xlsx"))


### Common
common.ki.tool <- read.xlsx("./input/tools/Common_KI_tool.xlsx")
comm.ki.f <- calculateDifferences(common_ki, common.ki.tool) %>% filter(number.different.columns < 10)
write.xlsx(comm.ki.f, paste0("./output/common ki falsification_",Sys.Date(),".xlsx"))

common.hh.tool <- read.xlsx("./input/tools/Common_HH_tool.xlsx")
comm.hh.f <- calculateDifferences(common_hh, common.hh.tool) %>% filter(number.different.columns < 10)
write.xlsx(comm.hh.f, paste0("./output/common hh falsification_",Sys.Date(),".xlsx"))


## Other checks
### Cholera
hh.size <- cholera_hh %>% select("uuid", "Minfant", "Finfant", "Mchild",
                                 "Fchild", "Madult", "Fadult", "Melderly", "Felderly", "hh_number") %>%
                          rowwise() %>%
                          mutate(hh_size = sum(c_across(Minfant:Felderly), na.rm = TRUE),
                                 check = ifelse(hh_size == hh_number, "ok", "error")) %>%
                         filter(check == "error")

write.xlsx(hh.size , paste0("./output/cholera hh size issue_",Sys.Date(),".xlsx"))

p.check <- cholera_hh %>% select("uuid", "Finfant", "Fchild", "Fadult", "Felderly", "pregnant_hh_member") %>%
                          filter(is.na(Fadult) & !is.na(pregnant_hh_member))

write.xlsx(p.check , paste0("./output/cholera pregnant check_",Sys.Date(),".xlsx"))


#### Common
comm.hh.size <- common_hh %>% select("uuid", "Minfant", "Finfant", "Mchild",
                                 "Fchild", "Madult", "Fadult", "Melderly", "Felderly", "hh_number") %>%
                              rowwise() %>%
                              mutate(hh_size = sum(c_across(Minfant:Felderly), na.rm = TRUE),
                              check = ifelse(hh_size == hh_number, "ok", "error")) %>%
                              filter(check == "error")

write.xlsx(comm.hh.size , paste0("./output/common hh size issue_",Sys.Date(),".xlsx"))

common.p.check <- common_hh %>% select("uuid", "Finfant", "Fchild", "Fadult", "Felderly", "pregnant_hh_member") %>%
                         filter(is.na(Fadult) & !is.na(pregnant_hh_member))

write.xlsx(common.p.check , paste0("./output/common pregnant check_",Sys.Date(),".xlsx"))
