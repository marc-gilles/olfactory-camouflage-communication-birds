# Script used to describe the review process and to obtain details about the studies and species included in the review and analysis

# Study: Olfactory camouflage and communication in birds

# Authors: Leanne A. Grieves, Marc Gilles, Innes C. Cuthill, Tamás Székely, Elizabeth A. MacDougall-Shackleton, Barbara A. Caspers


#### Data ####

studies <- read.csv("inclusion_studies_review.csv") # detail of inclusion of studies in the review
species <- read.csv("inclusion_species_analysis_1row1studywithinspecies.csv") # detail of inclusion of species in the analyses (one row = one species of one study)
species.unique <- read.csv("inclusion_species_analysis_1row1species.csv") # detail of the results for each species (one row = one species) (only the most relevant study was kept for each species) (see details in the legend of inclusion_species_analysis_1row1species.csv) )
data_sex <- read.csv("data_sex.csv") # data on sex differences
data_season <- read.csv("data_season.csv")


#### Check the consistency of the different data sets ####

# Sex analysis

# data used in analysis
data_sex.studies <- unique(data_sex$Study)
data_sex.studies
length(data_sex.studies)

# data summarizing data used in sex analysis (loaded in df "species")
species.sex.analysis <- species[species$incl.sex.analysis==1,] 
descr_data_sex.studies <- unique(species.sex.analysis$study)
descr_data_sex.studies
length(descr_data_sex.studies)
# 39 unique studies in dataset describing the inclusion of species/studies

# difference between the two datasets
setdiff(data_sex.studies,descr_data_sex.studies)
# data sets describing the studies/species and data sets used for the analysis are consistent

# Season analysis

# data used in analysis
data_season <-data_season[!is.na(data_season$sex),] # remove rows where sex is undefined (NA) as this information is needed for the analysis
data_season.studies <- unique(data_season$study)
data_season.studies
length(data_season.studies)

# data summarizing data used in season anaylsis (loaded in df "species")
species.season.analysis <- species[species$incl.season.analysis==1,] 
descr_data_season.studies <- unique(species.season.analysis$study)
descr_data_season.studies
length(descr_data_season.studies)
# 25 unique studies in dataset describing the inclusion of species/studies

# difference between the two datasets
setdiff(data_season.studies,descr_data_season.studies)
# data sets describing the studies/species and data sets used for the analysis are consistent


# ---- LITERATURE REVIEW ---

# Studies assessed for eligibility
nrow(studies)

# Studies reviewed included/excluded
table(studies$incl)
studies.incl <- studies[studies$incl==1,] # data with included studies only
nrow(studies.incl) # 55 studies included in review

# Reasons for exclusions
studies.excl <- studies[studies$incl==0,] 
table(studies.excl$reason.exclusion.review)

# Studies reviewed found on Web of Science or other source
table(studies$wos.search)
studies.wos <- studies[studies$wos.search==1,] # data with studies obtained from Web of Science only
studies.othersource <-  studies[studies$wos.search==0,] # data with studies obtained from other sources only

# Within studies included, how many were obtained from search WoS
table(studies.incl$wos.search)

# Within studies that were added from other source, how many were included/excluded
table(studies.othersource$incl)


# ---- STUDIES INCLUDED IN REVIEW ----

# Number of studies included in the review
nrow(studies.incl)

# Number of studies where sex differences were studied
table(studies.incl$sex.studied)

# Number of studies where seasonal differences were studied
table(studies.incl$season.studied)

# Number of studies where both sex and seasonal differences were studied
sexandseason.studied <- studies.incl[which(studies.incl$sex.studied==1 & studies.incl$season.studied==1),]
nrow(sexandseason.studied)

# Bird origin
table(studies.incl$bird.origin)


# ---- SPECIES INCLUDED IN REVIEW ----

# Number of species included in the review in total
nrow(species.unique)
length(unique(species$species))

# Number of species included in the review where sex is studied
table(species.unique$sex.studied)

# Number of species included in the review where season is studied
table(species.unique$season.studied)

# Proportion of species with sex diff
table(species.unique$sex.studied)
table(species.unique$sex.diff)
28/59 # 47%

# Proportion of species with season diff
table(species.unique$season.studied)
table(species.unique$season.diff)
57/60 # 95%


# ---- STUDIES INCLUDED IN REVIEW THAT STUDIED SEX DIFFERENCES ----

# Number of studies included that studied sex differences
studies.incl.sex <- studies.incl[studies.incl$sex.studied==1,] 
nrow(studies.incl.sex)

# Period / Time of year
table(studies.incl.sex$period)

# Captivity and photoperiod
table(studies.incl.sex$bird.origin,studies.incl.sex$photoperiod)


# ---- STUDIES INCLUDED IN REVIEW THAT STUDIED SEASONAL DIFFERENCES ----

# Number of studies included that studied sex differences
studies.incl.season <- studies.incl[studies.incl$season.studied==1,] 
nrow((studies.incl.season))

# Period / Time of year
table(studies.incl.season$period)

# Season test 
table(studies.incl.season$season.test)

# Photoperiod
table(studies.incl.season$photoperiod)

# Captivity and photoperiod
table(studies.incl.season$bird.origin)
table(studies.incl.season$bird.origin,studies.incl.season$photoperiod)
  

# ---- STUDIES ON CAPTIVE BIRDS ----

# Photoperiod
studies.incl.captive <- studies.incl[!studies.incl$bird.origin=="W",]
table(studies.incl.captive$photoperiod)


# ---- DATA FOR SEX ANALYSIS ----

# Number of occurences
nrow(data_sex)

# Number of studies
length(unique(data_sex$Study))

# Number of species
length(unique(data_sex$Species))


# ---- DATA FOR SEASON ANALYSIS ----

# Number of occurences
nrow(data_season)

# Number of studies
length(unique(data_season$study))

# Number of species
length(unique(data_season$species))


# ---- How many studies and species in total in analyses? ----

species_total <- species[which(species$incl.sex.analysis == 1 | species$incl.season.analysis == 1),] # species that were used either for the sex analysis or the season analysis

# Number of studies in the analyses in total
length(unique(species_total$study))

# Number of species in the analyses in total
length(unique(species_total$species))


# ---- SPECIES STUDIED ---

# How many species have been studied with respect to sex?
table(species.unique$sex.studied)

# How many species have been studied with respect to season?
table(species.unique$season.studied)

# How many species were studied with respect to sex OR season?
length(which(species.unique$sex.studied==1 | species.unique$season.studied))
nrow(species.unique)


# ---- PRISMA (Figure S1) ----

# Additional records identified from other sources
table(studies$wos.search)

# Full text articles assessed for eligibility
nrow(studies)

# Full text articles excluded
table(studies$incl)
table(studies$reason.exclusion.review)

# Studies included in review
table(studies$incl)

# Studies inlcuded in sex analysis
species.sex.analysis <- species[species$incl.sex.analysis==1,] 
length(unique(species.sex.analysis$study))

# Studies included in season analysis
species.season.analysis <- species[species$incl.season.analysis==1,] 
length(unique(species.season.analysis$study))


# ---- STUDIES PER ORDER (Figure 2) ----

# For each order, how many species were studied with respect to sex?
species.unique.sex <- species.unique[species.unique$sex.studied==1,]
table(species.unique.sex$order)

# For each order, how many species were studied with respect to season?
species.unique.season <- species.unique[species.unique$season.studied==1,]
table(species.unique.season$order)

# For each order, how many species were studied (with respect to sex or season)?
species.unique.sexorseason <- species.unique[species.unique$season.studied==1|species.unique$sex.studied==1,]
table(species.unique.sexorseason$order)