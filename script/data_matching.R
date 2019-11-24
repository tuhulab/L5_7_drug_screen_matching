#install packages
#install.packages("tidyverse")
#install.packages("readr")
#load package
library(tidyverse)
library(readr)
library(readxl)

#example outlier
#outlier <- c("d10","e10")

outlier <- c("d10")
plate <- "10" 
section <- "A"

#read raw data
raw_data <- read_xlsx(file.path("data",paste0("plate",plate,section,".xlsx")),
                      sheet = 2)
data_raw <- raw_data[1:10,1:11]
threshold_up <- data_raw[2,(which(data_raw[1,] == "N"))] %>%
  as.numeric() %>% mean()
sd <- data_raw[2,(which(data_raw[1,] == "N"))] %>% 
        as.numeric() %>% sd()
threshold_down <- data_raw[2,(which(data_raw[1,] == "R"))] %>% 
  as.numeric() %>% 
  mean()

e <- expand.grid(1:10,letters[1:8])
location <- paste0(e[,2],e[,1])

plate_data <- data.frame(location,
                         value=data_raw[3:10,2:11] %>% 
                         as.matrix() %>% t() %>%
                         as.numeric() %>% 
                         as.matrix(ncol=1)) %>% filter(location!=outlier) %>% 
                         filter(is.na(value)==FALSE)

up <- plate_data$location[which(plate_data$value > threshold_up + sd)] %>% as.character()
down <- plate_data$location[which(plate_data$value < threshold_down)] %>% as.character()


drug_info <- read_xlsx("data/drug_info.xlsx",sheet = 2)
drug_location_l <- drug_info$`Plate Location` %>% str_extract_all("[:alpha:]")
drug_location_d <- drug_info$`Plate Location` %>% str_extract_all("\\d")

t <- 1:length(drug_location_d)

drug_info_new <- drug_info %>% mutate(`Plate Location`=drug_location_new <- sapply(t,
drug_location_retrive <- function(t=...){
if(length(drug_location_d[[t]])==1){
  raw_location <- drug_location_d[[t]] %>% as.numeric()
  new_location <- paste0(drug_location_l[[t]],raw_location -1)
  return(new_location)
} else {
  raw_location <- paste0(drug_location_d[[t]][1],
         drug_location_d[[t]][2]) %>% 
    as.numeric()
  new_location <- paste0(drug_location_l[[t]],raw_location -1)
  return(new_location)
}}),
`Rack Number`=drug_info$`Rack Number`%>% str_extract("-\\d{1,2}") %>% str_remove("-")
) %>% filter(`Rack Number`==plate)

up_drug <- drug_info_new %>% filter(`Plate Location` %in% up) %>% mutate(UP_DOWN="UP") %>% select(UP_DOWN,`Rack Number`,
                                                                         `Plate Location`,
                                                                         `Product Name`,
                                                                         `Catalog Number`,
                                                                         Target,
                                                                         Pathway,
                                                                         Information,
                                                                         BARCODE) 
down_drug <- drug_info_new %>% filter(`Plate Location` %in% down) %>% mutate(UP_DOWN="DOWN")%>% select(UP_DOWN, `Rack Number`,
                                                                             `Plate Location`,
                                                                             `Product Name`,
                                                                             `Catalog Number`,
                                                                             Target,
                                                                             Pathway,
                                                                             Information,
                                                                             BARCODE)
name_up_drug <- paste0(plate,section,"up.csv")
name_down_drug <- paste0(plate,section,"down.csv")
write_csv(up_drug,path = paste0("drug_screened/",name_up_drug))
write_csv(down_drug,path = paste0("drug_screened/",name_down_drug))

