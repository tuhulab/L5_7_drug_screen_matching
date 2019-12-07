library(tidyverse)
library(readr)
library(readxl)
library(webchem)

drug_info <- read_xlsx("data/drug_info.xlsx",sheet = 2)
# KEGG <- cts_convert(query = drug_info$`CAS Number`[3],
#                     from = "CAS", 
#                     to="KEGG")
# cts_convert(query = drug_info$`CAS Number`[1],from = "CAS", to="KEGG")

t <- 1:1953
#query_result <- sapply(t,function(t){
drug_name <- drug_info$`Product Name`[t]
query_result <- 
  keggFind("drug",drug_name %>% 
                           str_remove(pattern = "\\(.{1,50}\\)") %>% 
                           str_replace("HCl|2HCl", "hydrochloride"))  %>% 
                           names() %>% str_remove("dr:") %>% 
                           keggGet()
query_result[[1]]$TARGET


# })

###
drug_info$`Product Name`[t] %>% str_remove(pattern = "\\(.{1,50}") %>% str_replace("HCl", "hydrochloride")
