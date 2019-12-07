library(tidyverse)
library(readr)
library(readxl)
library(KEGGREST)


drug_info <- read_xlsx("data/drug_info.xlsx",sheet = 2) %>% 
                select(M.w.,Formula,`Catalog Number`,`CAS Number`,SMILES)

candidate <- read_xlsx("drug_screened/down drugs.xlsx") %>% left_join(drug_info)

kegg <- cts_convert(query = candidate$`CAS Number`,from = "CAS", to="KEGG")

t <- 1:length(kegg)
kegg_combine <- sapply(t, function(t){
  paste(kegg[[t]],collapse = "_")
  })  %>% str_remove_all("C\\d{5}_") %>% str_remove_all("C\\d{5}")

candidate_kegg <- candidate %>% mutate(kegg_code = kegg_combine)
## hand curate
kegg_combine_demo <- kegg_combine %>% str_remove("_D\\d{5}") %>% unique
kegg_combine_demo[kegg_combine_demo != "NA" & kegg_combine_demo != ""]


kegg_get_demo <-  keggGet(kegg_combine_demo[kegg_combine_demo != "NA" & kegg_combine_demo != ""])

###
drug_info$`Product Name`[t] %>% str_remove(pattern = "\\(.{1,50}") %>% str_replace("HCl", "hydrochloride")
