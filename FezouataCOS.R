library(ggplot2)
library(dplyr)
library(janitor)
library(data.table)
library(divDyn)
library(xlsx)
library(zoo)
library(vegan)
library(tidyr)
library(geosphere)
library(dlookr)
library(OneR)
library(colorRamps)
library(cowplot)
library(ggpol)
library(remotes)
remotes::install_github("willgearty/deeptime")
library(deeptime)
library(RCurl)
library(reshape2)

# Load Data, Rename, and Select Columns
#-------------------------------------------------------------------------------

#URL
data_url <- "https://paleobiodb.org/data1.2/occs/list.csv?max_ma=541&min_ma=416&show=genus,classext,paleoloc,stratext,lith,env,timebins"

#Load from Internet
cos <- getURL(data_url)
cos <- read.csv(textConnection(cos), header=T)

#Alter
cos <- cos %>%
  select(accepted_name, accepted_rank, genus, family, order, class, phylum, 
         min_ma, max_ma, formation, lithology1, environment, paleolat, paleolng) %>%
  mutate("Max" = round(max_ma/0.5)*0.5, "Min" = round(min_ma/0.5)*0.5) %>%
  rename(lithology = lithology1, plat = paleolat, plon = paleolng) %>%
  mutate(Range = abs(Max - Min)) %>%
  select(-min_ma, -max_ma)

#Fill Empty
cos[(cos == "")] <- NA

#Export
write_xlsx(cos,"cos_PBDB.xlsx")

# Filter for Hard, Biomineralized, and Recalcitrant Taxa
#-------------------------------------------------------------------------------

#Remove Rows with no accepted rank

cos <- cos %>% filter(!is.na(accepted_name))

#Remove Plants, Protists, Soft Phyla, Chordates

cos <- cos %>% filter(!phylum %in% c("Bryophyta","Chlorophyta","Cycadophyta",
                                     "Foraminifera","Lycopodophyta","Phaeophyta","Prasinophyta",
                                     "Psilophytophyta","Pteridophyta","Radiolaria","Rhizopodea","Rhodophyta",
                                     "Sarcodina","Spermatophyta","Sphenophyta","Thallophyta","Tracheophyta",
                                     "Zosterophyllophyta","Ctenophora","Cyanobacteria","Entoprocta",
                                     "Hemichordata","Onychophora","Petalonamae", "Priapulida","Sipuncula"))

#Phyla
phyla <- c("Agmata", "Brachiopoda", "Calcispongea", "Hyolitha")

#Classes
classes <- c("Trilobita","Xiphosura","Ostracoda","Hexanauplia","Merostomoidea",
             "Stenolaemata","Actinopteri","Chondrichthyes","Conodonta",
             "Galeaspida","Osteichthyes","Osteostraci","Placodermi","Conulata",
             "Camptostromoidea","Cincta","Coronoidea","Crinoidea","Echinoidea",
             "Tentaculita","Bivalvia","Helcionelloida","Monoplacophora",
             "Paragastropoda","Polyplacophora","Rostroconchia","Scaphopoda",
             "Stenothecoida","Tergomya","Siphonoconcha","Archaeocyatha",
             "Stromatoporoidea", "Calcarea","Demospongea")


#Orders
orders <- c("Aglaspidida","Eurypterida","Cheilostomata","Auloporida","Cystiphyllida","
            Favositida","Halysitida","Heliolitida","Lichenariida","Sarcinulida","
            Stauriida","Tetradiida","Calyptoptomatida","Camerothecida","Fissiculata","
            Hyolithida","Mitrosagophora","Orthothecida","Phlebolepidiformes","
            Protremata","Rhombifera","Spiraculata","Tryblidioidea","
            Actinocerida","Archaeogastropoda","Ascocerida","Barrandeocerida","
            Belemnitida","Bellerophontida","Cyrtoneritimorpha","Discosorida","
            Dissidocerida","Ellesmerocerida","Endocerida","Euomphalina","Intejocerida","
            Murchisoniina","Nautilida","Oncocerida","Orthocerida","Pseudorthocerida","
            Sorbeoconcha","Tarphycerida")

#Families
families <- c("Anthinocrinidae","Callyspongiidae","Pycnoidocyathidae")

#Select Biomineralized, Hard, Shelly taxa
cos_hard <- cos %>% filter(phylum %in% phyla | class %in% classes | order %in% orders | family %in% families)

#Tools for Manipulation and Exploration
unique(cos$phylum)

tabyl(cos %>% filter(phylum == "NO_PHYLUM_SPECIFIED" & order == "NO_ORDER_SPECIFIED" & class == "NO_CLASS_SPECIFIED"), family)
tabyl(cos %>% filter(is.na(class)), order)

tabyl(cos, phylum)
tabyl(cos %>% filter(phylum == "Chordata"), class)

cos %>% filter(is.na(class))

#Create Data Frame with Number of Observations by Time
#-------------------------------------------------------------------------------

#Time Binning Function
time_wrangler <- function(raw_data) {
  maximum <- max(raw_data$Max)
  minimum <- min(raw_data$Min)
  raw_data <- raw_data %>% mutate(time = NA)
  dummy <- raw_data[0,]
  
  for (i in seq(minimum,maximum, 1)) {
    temp <- raw_data %>% filter(Min <= i & Max >= i)
    temp <- temp %>% mutate(time = i)
    dummy <- rbind(dummy,temp)
  }
  return(dummy)
}

#Time Binning Function Call
cos_hard_time <- time_wrangler(cos_hard)

#Data Frame with Number of Fossils by Time
cos_hard_time_occurence <- cos_hard_time %>% 
  group_by(time) %>% 
  count() %>% 
  ungroup()%>% 
  mutate(fossils = n) %>%
  mutate(roll = rollmean(n,5, na.pad = TRUE)) %>%
  select(time, fossils, roll)

#Time Series with Number of Fossils
ggplot(data = cos_hard_time_occurence) +
  geom_line(aes(x = time, y=fossils), size = 2) +
  geom_line(aes(x = time, y = roll), color = "red", linetype = "dashed", size = 1.5) + 
  scale_x_reverse() +
  ylab("Number of Fossils") +
  xlab("Time (Ma)") + 
  theme_bw() +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  coord_geo(dat = list("stages","periods"), xlim = c(541, 419), ylim = c(0, 45000),
            pos = list("b", "b"), abbrv = list(TRUE, FALSE)) +
  theme_classic()

#Number of Genera by Time
#-------------------------------------------------------------------------------

#Data Frame with Number of Genera by Time
cos_hard_time_genera <- cos_hard_time %>%
  filter(genus != "NO_GENUS") %>% 
  filter(genus != "NO_GENUS_SPECIFIED") %>%
  group_by(time) %>%
  summarise(count = n_distinct(genus)) %>% 
  mutate(roll = rollmean(count,5, na.pad = TRUE)) %>%
  mutate(genera = count) %>%
  select(-count)

##Time Series with Number of Genera
ggplot(data = cos_hard_time_genera) +
  geom_line(aes(x = time, y=genera), size = 2) +
  geom_line(aes(x = time, y = roll), color = "red", linetype = "dashed", size = 1.5) + 
  scale_x_reverse() +
  ylab("Number of Shelly and Biomineralized Genera") +
  xlab("Time (Ma)") + 
  theme_bw() +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  coord_geo(dat = list("stages","periods"), xlim = c(541, 419), ylim = c(0, 2100),
            pos = list("b", "b"), abbrv = list(TRUE, FALSE)) +
  theme_classic()


#Ordovician Geographic Distance and Community Dissimilarity
#-------------------------------------------------------------------------------

#Create Dataframe of Localities
cos_hard_localities <- cos_hard %>%
  filter(genus != "")  %>%
  filter(genus != "NO_GENUS_SPECIFIED") %>%
  filter(genus != "NO_GENUS") %>%
  filter(genus != "NA") %>%
  group_by(formation, plon, plat) %>%
  count() %>%
  filter(n >= 10) %>%
  ungroup() %>%
  mutate(locality = paste(formation, plat, plon, sep = '_'))

o_hard_localities <- cos_hard %>%
  filter(genus != "")  %>%
  filter(genus != "NO_GENUS_SPECIFIED") %>%
  filter(genus != "NO_GENUS") %>%
  filter(genus != "NA") %>%
  filter(Max <= 485.5 & Min >= 443) %>%
  group_by(formation, plon, plat, Min, Max) %>%
  count() %>%
  filter(n >= 10) %>%
  ungroup() %>%
  mutate(locality = paste(formation, plat, plon, sep = '_'))

cos_hard_localities_specimens <- cos_hard %>% 
  mutate(locality = paste(formation, plat, plon, sep = '_')) %>%
  filter(locality %in% cos_hard_localities$locality)

#Select Ordovician Localities
o_hard_localities_specimens <- cos_hard_localities_specimens %>% filter(Max <= 485.5 & Min >= 443)

#Count Genera by Locality
o_group_genus <- o_hard_localities_specimens %>% 
  group_by(locality, genus) %>%
  count(genus) %>%
  ungroup(locality, genus)

#Community Creation and Naming
o_communities <- pivot_wider(o_group_genus, names_from = genus, values_from = n)
o_communities[is.na(o_communities)] = 0
names <- o_communities[,1]
names$num <- 1:nrow(o_communities)

#Pairwise Comparisons between Localities
o_comm_comp <- as.matrix(vegdist(o_communities[2:ncol(o_communities)], method = "horn"))
o_comm_comp <- melt(o_comm_comp)[melt(upper.tri(o_comm_comp))$value,]
names(o_comm_comp) <- c("c1", "c2", "eco_distance")
o_comm_comp <- as.data.frame(o_comm_comp)

#Pairwise Comparison Table Merging
o_comm_comp <- merge(o_comm_comp, names,by.x = "c1", by.y = "num")
o_comm_comp <- merge(o_comm_comp, names,by.x = "c2", by.y = "num")
o_comm_comp <- o_comm_comp %>%
  select(-c1,-c2)

#Coordinates
o_hard_cord <- o_hard_localities %>%
  select(-formation)

#Pairwise Comparison Table Merging with Coordinates
o_comm_comp <- merge(o_comm_comp, o_hard_cord, by.x = "locality.x", by.y = "locality")
o_comm_comp <- merge(o_comm_comp, o_hard_cord,by.x = "locality.y", by.y = "locality")
o_comm_comp$dist_km <- NA

#Geographic Distances

#Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

lonA <- o_comm_comp$plon.x
latA <- o_comm_comp$plat.x
lonB <- o_comm_comp$plon.y
latB <- o_comm_comp$plat.y
distance <- NA

for (i in 1:nrow(o_comm_comp)) {
  distance[i] = gcd.hf(lonA[i],latA[i], lonB[i], latB[i])
}

o_comm_comp$dist_km <- distance

#Remove Geographic Distanceless Points
o_comm_comp <- o_comm_comp %>%
  filter(!is.na(dist_km))

#Create Logarithmic Geographic Distance Metrics
o_comm_comp <- o_comm_comp %>%
  mutate(log_dist_km = log10(dist_km + 1))

#Calculate New Distance Metrics
bin_dist_km_holder <- bin(o_comm_comp$dist_km, nbins = 10, method = "length", na.omit = TRUE)
bin_log_dist_km_holder <- bin(o_comm_comp$log_dist_km, nbins = 10, method = "length", na.omit = TRUE)

#Add New Distance Metrics to Table, Rename Dissimilarity, Alter Negative Distances
o_comm_comp$bin_dist_km <- bin_dist_km_holder
o_comm_comp$bin_log_dist_km <- bin_log_dist_km_holder
o_comm_comp <- o_comm_comp %>%
  rename(horn_diss = eco_distance) %>%
  mutate(horn_sim = 1 - horn_diss)
o_comm_comp$bin_dist_km <- as.character(o_comm_comp$bin_dist_km)
o_comm_comp[o_comm_comp == "(-20,2e+03]"] <- "(0,2e+03]"
o_comm_comp$bin_log_dist_km <- as.character(o_comm_comp$bin_log_dist_km)
o_comm_comp[o_comm_comp == "(-0.0043,0.43]"] <- "(0,0.43]"

#Number of Specimens (Sum and Average)
o_comm_comp <- o_comm_comp %>%
  mutate(nSum = n.x + n.y, nAvg = ((n.x + n.y)/2))

#Determine which are Upper Fezouata sites
u_fez_coord <- o_hard_localities %>%
  filter(formation == "Upper Fezouata" | formation == "Lower Fezouata") %>%
  select(plon,plat)

o_comm_comp <- o_comm_comp %>%
  mutate(FEZ = if_else(plon.x %in% u_fez_coord$plon & plon.y %in% u_fez_coord$plon & plat.x %in% u_fez_coord$plat & plat.y %in% u_fez_coord$plat, TRUE, FALSE),
         fez_bin_dist_km = if_else(FEZ, "Fezouata",bin_dist_km),
         fez_bin_log_dist_km = if_else(FEZ, "Fezouata", bin_log_dist_km)) %>%
  select(-FEZ)

#Determine the average distance between Fezouata sites
log_dist_fezouata <- o_comm_comp %>% 
  filter(fez_bin_log_dist_km == "Fezouata")

fez_log_dist_avg <- round(mean(log_dist_fezouata$log_dist_km), digits = 2)
fez_log_dist_med <- median(log_dist_fezouata$log_dist_km)

o_comm_comp[o_comm_comp == "Fezouata"] <- paste("Fezouata ", "AVG LOG DIST: ", fez_log_dist_avg, sep = "")

#Choose Color Ramp with six colors
clrs <- colorRampPalette(c("white", "blue"))
cls <- clrs(10)

#Graph
ggplot() + 
  geom_violin(data = o_comm_comp, aes(x = fez_bin_log_dist_km, y = horn_diss, fill = fez_bin_log_dist_km), width = 3) + 
  #geom_boxjitter(data = o_comm_comp, aes(x = fez_bin_log_dist_km, y = (horn_sim), fill = fez_bin_log_dist_km), width=0.2, outlier.shape = NA) + 
  xlab("Log of Paleogeographic Distance (km)") +
  ylab("Morista Horn Similarity") +
  ggtitle("Ordovician Paleocommunity Similarity and Paleogeographic Distance") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  guides(fill=guide_legend(title="Pariwise Comparison Group")) + 
  scale_fill_manual(values = c(cls, "#CC5500"))

#Test Graph
ggplot() + 
  geom_point(data = o_comm_comp, aes(x = dist_km, y = (horn_sim)/nSum, color = fez_bin_log_dist_km)) + 
  xlab("Log of Paleogeographic Distance (km)") +
  ylab("Morista Horn Similarity") +
  ggtitle("Ordovician Paleocommunity Similarity and Paleogeographic Distance") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  guides(color=guide_legend(title="Pariwise Comparison Group")) + 
  scale_color_manual(values = c(cls, "#CC5500"))

summary(lm( (horn_sim/nSum) ~ dist_km, o_comm_comp))

#Additional Columns for Early Ordovician and Polar localities
o_comm_comp <- o_comm_comp %>% 
  mutate(Polar = ifelse(abs(plat.x) >= 66 & abs(plat.y) >= 66, TRUE, FALSE)) %>%
  mutate(EarlyO = ifelse((Max.x <= 485.5 & Min.x >= 470 & Max.y <= 485.5 & Min.y >= 470),TRUE,FALSE))
#Polar Ordovician Geographic Distance and Community Dissimilarity
#-------------------------------------------------------------------------------

#Choose Color Ramp with six colors
clrs <- colorRampPalette(c("white", "blue"))
cls <- clrs(9)

#Graph
ggplot() + 
  #geom_violin(data = o_comm_comp, aes(x = fez_bin_log_dist_km, y = horn_diss, fill = fez_bin_log_dist_km), width = 3) + 
  geom_boxjitter(data = o_comm_comp %>% filter(Polar), aes(x = fez_bin_log_dist_km, y = (horn_sim), fill = fez_bin_log_dist_km), width=0.2, outlier.shape = NA) + 
  xlab("Log of Paleogeographic Distance (km)") +
  ylab("Morista Horn Similarity") +
  ggtitle("Polar Ordovician Paleocommunity Similarity and Paleogeographic Distance") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  guides(fill=guide_legend(title="Pariwise Comparison Group")) + 
  scale_fill_manual(values = c(cls, "#CC5500"))


#Early Ordovician Geographic Distance and Community Dissimilarity
#-------------------------------------------------------------------------------

#Choose Color Ramp with six colors
clrs <- colorRampPalette(c("white", "blue"))
cls <- clrs(10)

#Graph
ggplot() + 
  #geom_violin(data = o_comm_comp, aes(x = fez_bin_log_dist_km, y = horn_diss, fill = fez_bin_log_dist_km), width = 3) + 
  geom_boxjitter(data = o_comm_comp %>% filter(EarlyO), aes(x = fez_bin_log_dist_km, y = (horn_sim), fill = fez_bin_log_dist_km), width=0.2, outlier.shape = NA) + 
  xlab("Log of Paleogeographic Distance (km)") +
  ylab("Morista Horn Similarity") +
  ggtitle("Early Ordovician Paleocommunity Similarity and Paleogeographic Distance") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  guides(fill=guide_legend(title="Pariwise Comparison Group")) + 
  scale_fill_manual(values = c(cls, "#CC5500"))


#Early Polar Ordovician Geographic Distance and Community Dissimilarity
#-------------------------------------------------------------------------------

#Choose Color Ramp with six colors
clrs <- colorRampPalette(c("white", "blue"))
cls <- clrs(9)

#Graph
ggplot() + 
  #geom_violin(data = o_comm_comp, aes(x = fez_bin_log_dist_km, y = horn_diss, fill = fez_bin_log_dist_km), width = 3) + 
  geom_boxjitter(data = o_comm_comp %>% filter(EarlyO & Polar), aes(x = fez_bin_log_dist_km, y = (horn_sim), fill = fez_bin_log_dist_km), width=0.2, outlier.shape = NA) + 
  xlab("Log of Paleogeographic Distance (km)") +
  ylab("Morista Horn Similarity") +
  ggtitle("Early Polar Ordovician Paleocommunity Similarity and Paleogeographic Distance") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  guides(fill=guide_legend(title="Pariwise Comparison Group")) + 
  scale_fill_manual(values = c(cls, "#CC5500"))


#Cambrian Geographic Distance and Community Dissimilarity
#-------------------------------------------------------------------------------

#Create Dataframe of Localities
cos_hard_localities <- cos_hard %>%
  filter(genus != "")  %>%
  filter(genus != "NO_GENUS_SPECIFIED") %>%
  filter(genus != "NO_GENUS") %>%
  filter(genus != "NA") %>%
  group_by(formation, plon, plat) %>%
  count() %>%
  filter(n >= 15) %>%
  ungroup() %>%
  mutate(locality = paste(formation, plat, plon, sep = '_'))

c_hard_localities <- cos_hard %>%
  filter(genus != "")  %>%
  filter(genus != "NO_GENUS_SPECIFIED") %>%
  filter(genus != "NO_GENUS") %>%
  filter(genus != "NA") %>%
  filter(Max <= 541 & Min >= 485.5) %>%
  group_by(formation, plon, plat, Max, Min) %>%
  count() %>%
  filter(n >= 15) %>%
  ungroup() %>%
  mutate(locality = paste(formation, plat, plon, sep = '_'))

c_hard_localities_specimens <- cos_hard %>% 
  mutate(locality = paste(formation, plat, plon, sep = '_')) %>%
  filter(locality %in% c_hard_localities$locality)

#Select Cambrian Localities
c_hard_localities_specimens <- c_hard_localities_specimens %>% filter(Max <= 541 & Min >= 485.5)

#Count Genera by Locality
c_group_genus <- c_hard_localities_specimens %>% 
  group_by(locality, genus) %>%
  count(genus) %>%
  ungroup(locality, genus)

#Community Creation and Naming
c_communities <- pivot_wider(c_group_genus, names_from = genus, values_from = n)
c_communities[is.na(c_communities)] = 0
names <- c_communities[,1]
names$num <- 1:nrow(c_communities)

#Pairwise Comparisons between Localities
c_comm_comp <- as.matrix(vegdist(c_communities[2:ncol(c_communities)], method = "horn"))
c_comm_comp <- melt(c_comm_comp)[melt(upper.tri(c_comm_comp))$value,]
names(c_comm_comp) <- c("c1", "c2", "distance")
c_comm_comp <- as.data.frame(c_comm_comp)

#Pairwise Comparison Table Merging
c_comm_comp <- merge(c_comm_comp, names,by.x = "c1", by.y = "num")
c_comm_comp <- merge(c_comm_comp, names,by.x = "c2", by.y = "num")
c_comm_comp <- c_comm_comp %>%
  select(-c1,-c2)

#Coordinates
c_hard_cord <- c_hard_localities %>%
  select(-formation)

#Pairwise Comparison Table Merging with Coordinates
c_comm_comp <- merge(c_comm_comp, c_hard_cord, by.x = "locality.x", by.y = "locality")
c_comm_comp <- merge(c_comm_comp, c_hard_cord,by.x = "locality.y", by.y = "locality")
c_comm_comp$dist_km <- NA

#Table with Geographic Distances
c_comm_comp <- c_comm_comp %>% rowwise() %>%
  mutate(dist_km = distm(c(plon.x,plat.x),c(plon.y,plat.y), fun = distHaversine)/1000)

#Remove Geographic Distanceless Points
c_comm_comp <- c_comm_comp %>%
  filter(!is.na(dist_km))

#Create Logarithmic Geographic Distance Metrics
c_comm_comp <- c_comm_comp %>%
  mutate(log_dist_km = log10(dist_km + 1))

#Calculate New Distance Metrics
bin_dist_km_holder <- bin(c_comm_comp$dist_km, nbins = 8, method = "length", na.omit = TRUE)
bin_log_dist_km_holder <- bin(c_comm_comp$log_dist_km, nbins = 8, method = "length", na.omit = TRUE)

#Add New Distance Metrics to Table, Rename Dissimilarity, Alter Negative Distances
c_comm_comp$bin_dist_km <- bin_dist_km_holder$data
c_comm_comp$bin_log_dist_km <- bin_log_dist_km_holder$data
c_comm_comp <- c_comm_comp %>%
  rename(horn_diss = distance) 
c_comm_comp$bin_dist_km <- as.character(c_comm_comp$bin_dist_km)
c_comm_comp[c_comm_comp == "(-20,2.5e+03]"] <- "(0,2.5e+03]"
c_comm_comp$bin_log_dist_km <- as.character(c_comm_comp$bin_log_dist_km)
c_comm_comp[c_comm_comp == "(-0.0043,0.538]"] <- "(0,0.538]"

#Determine which are Chengjiang sites
chengjiang_coord <- c_hard_localities %>%
  filter(formation == "Qiongzhusi" | formation == "Yu'anshan" | formation == "Yuanshan") %>%
  select(plon,plat)

#Determine which are Burgess Shales
burgess_coord <- c_hard_localities %>%
  filter(formation == "Burgess Shale") %>%
  mutate(locality_b = paste(formation, plat, plon, sep = '_')) %>%
  select(locality_b)

c_comm_comp <- c_comm_comp %>%
  mutate(CHE = ifelse(plon.x %in% chengjiang_coord$plon & plon.y %in% chengjiang_coord$plon & plat.x %in% chengjiang_coord$plat & plat.y %in% chengjiang_coord$plat, TRUE, FALSE),
         BRG = ifelse(locality.x %in% burgess_coord$locality_b & locality.y %in% burgess_coord$locality_b , TRUE, FALSE),
         c_bin_dist_km = ifelse(CHE, "Chengjiang", ifelse(BRG, "Burgess", bin_dist_km)),
         c_bin_log_dist_km = ifelse(CHE, "Chengjiang", ifelse(BRG, "Burgess", bin_log_dist_km))) %>%
  select(-CHE,-BRG)

#Determine the average distance between Chengjiang sites
log_dist_CHE <- c_comm_comp %>% 
  filter(c_bin_log_dist_km == "Chengjiang")

CHE_log_dist_avg <- round(mean(log_dist_CHE$log_dist_km), digits = 2)
CHE_log_dist_med <- median(log_dist_CHE$log_dist_km)

c_comm_comp[c_comm_comp == "Chengjiang"] <- paste("Cheng ", "AVGLOGDIST: ", CHE_log_dist_avg, sep = "")

#Determine the average distance between Burgess sites
log_dist_BUR <- c_comm_comp %>% 
  filter(c_bin_log_dist_km == "Burgess")

BUR_log_dist_avg <- round(mean(log_dist_BUR$log_dist_km), digits = 2)
BUR_log_dist_med <- median(log_dist_BUR$log_dist_km)

c_comm_comp[c_comm_comp == "Burgess"] <- paste("Burg ", "AVGLOGDIST: ", BUR_log_dist_avg, sep = "")

#Choose Color Ramp with six colors
clrs <- colorRampPalette(c("white", "#7FA056"))
cls <- clrs(8)

#Graph
ggplot() + 
  #geom_violin(data = c_comm_comp, aes(x = c_bin_log_dist_km, y = horn_diss, fill = c_bin_log_dist_km), width = 3) + 
  geom_boxjitter(data = c_comm_comp, aes(x = c_bin_log_dist_km, y = (horn_diss), fill = c_bin_log_dist_km), width=0.2, outlier.shape = NA) + 
  xlab("Log of Paleogeographic Distance (km)") +
  ylab("Morista Horn Dissimilarity") +
  ggtitle("Cambrian Paleocommunity Dissimilarity and Paleogeographic Distance") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  guides(fill=guide_legend(title="Pariwise Comparison Group")) + 
  scale_fill_manual(values = c(cls, "Gray", "Purple"))

#Test Graph
ggplot() + 
  geom_point(data = c_comm_comp, aes(x = dist_km, y = horn_diss, color = c_bin_log_dist_km)) + 
  xlab("Log of Paleogeographic Distance (km)") +
  ylab("Morista Horn Dissimilarity") +
  ggtitle("Cambrian Paleocommunity Dissimilarity and Paleogeographic Distance") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  guides(color=guide_legend(title="Pariwise Comparison Group")) + 
  scale_color_manual(values = c(cls, "Gray", "Purple"))

#Additional Columns for Temporal and Latitudes localities
c_comm_comp <- c_comm_comp %>% 
  mutate(SubTropical_Tropical = ifelse(abs(plat.x) <= 35 & abs(plat.y) <= 35, TRUE, FALSE)) %>%
  mutate(MidC = ifelse((Max.x <= 521 & Min.x >= 500.5 & Max.y <= 521 & Min.y >= 500.5),TRUE,FALSE))
