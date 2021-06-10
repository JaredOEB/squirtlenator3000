library(RCurl)
library(dplyr)
library(xlsx)
library(googlesheets4)


#PBDB Materials 
#-------------------------------------------------------------------------------
#URL for PBDB
data_url <- "https://paleobiodb.org/data1.2/occs/list.csv?max_ma=541&min_ma
=416&show=genus,classext,paleoloc,stratext,lith,env,timebins"

#Load from Internet
cos <- getURL(data_url)
cos <- read.csv(textConnection(cos), header=T)

#Alter
cos <- cos %>%
  select(accepted_name, accepted_rank, genus, family, order, class, phylum, 
         formation)

#Fill Empty
cos[(cos == "")] <- NA

Fezouata <- cos %>%
  filter(formation == "Upper Fezouata" | formation == "Lower Fezouata" | 
           formation == "Fezouata" | formation == "Fezouata Shale") %>%
  filter(accepted_rank == "species" | accepted_rank == "genus" | accepted_rank == "subgenus" | accepted_rank == "subspecies")

names_fez <- Fezouata %>%
  select(-formation, -genus, -accepted_rank) %>%
  distinct() %>%
  mutate(Taxon = accepted_name, Phylum = phylum, Class = class, Order = order, 
         Family = family, References = NA, "Images (Y/N)" = NA) %>%
  select(-accepted_name, -phylum, -class, -order, -family)


#Karma Compilation
#-------------------------------------------------------------------------------
#Karma Data
karma_url <- "https://docs.google.com/spreadsheets/d/1E9HW-xrevOkuT77Nc
H2M2bKDXmSXeR11bUFPkeCbwrA/edit?usp=sharing"

karma_df <- read_sheet(karma_url)


#Combining of Datasets (PBDB + Karma)
#-------------------------------------------------------------------------------
final <- rbind(karma_df, names_fez)

final <- final %>%
  distinct(Taxon, .keep_all =  TRUE)

write.xlsx(final, file = "PBDB_Karma.xlsx", sheetName = "Both",
           col.names = TRUE, row.names = FALSE, append = FALSE)

