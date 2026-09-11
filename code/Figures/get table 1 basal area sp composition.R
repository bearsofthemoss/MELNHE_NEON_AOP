
## Generate Table 1 sp composition info based in tree inventory

#  Read in Tree DBH information ####


rten <- read.csv(here::here("data_folder","MELNHE_TreeDiameters_GreaterThan10cm_2008-2023.csv"))
names(rten)
tn <- rten[ , c("stand","Plot","Treatment","Subplot","Species","CurrentTag","DBH2019")]

tn$DBH2019 <- as.numeric(tn$DBH2019)
tn$Age[tn$stand=="C1"]<-"~30 years old"
tn$Age[tn$stand=="C2"]<-"~30 years old"
tn$Age[tn$stand=="C3"]<-"~30 years old"
tn$Age[tn$stand=="C4"]<-"~60 years old"
tn$Age[tn$stand=="C5"]<-"~60 years old"
tn$Age[tn$stand=="C6"]<-"~60 years old" 
tn$Age[tn$stand=="C7"]<-"~100 years old"
tn$Age[tn$stand=="C8"]<-"~100 years old"
tn$Age[tn$stand=="C9"]<-"~100 years old"

tn <- tn[!is.na(tn$Age),]

tn <- tn[tn$Treatment!="Ca",]

tn$BA <- (tn$DBH2019/2)^2 * 3.14159 / 10000





head(tn)

tn$staplo <- paste(tn$stand, tn$Plot)

b <- tn %>%
  group_by(stand, Species) %>%
  summarise(sp_BA = sum(BA, na.rm = TRUE), .groups = "drop") %>%
  group_by(stand) %>%
  mutate(pct = sp_BA / sum(sp_BA)) %>%
  filter(pct > 0.01) %>%
  arrange(stand, desc(pct)) %>%
  select(stand, Species, pct)

b[b$stand=="C5",]
