#------------------------------------------------------------------------------
#PURPOSE: This script cleans the data for the 2018 Guam and CNMI small boat 
  #fisheries cost-earnings survey.
#AUTHOR: Crystal Dombrow
#DATE: February 2024
#------------------------------------------------------------------------------

#------------------
#SET UP WORKSPACE
#------------------

#Load libraries
library(tidyverse)


#Import data set. Originally, data set was an .xlsx file; author saved the .xlsx 
  #as a .csv, & added manual data cleaning for individual observations. Steps are 
  #documented in GuamCNMI_DataCleaningDocumentation.docx in the "Data" folder for 
  #this R program.
guam.cnmi.data.raw <- 
  read_csv("Data/Marianas2018_SurveyData_checkedskippattern.csv")


#--------------------
#CLEAN DATA
#--------------------
#Each object name describes the work being done on it


#Remove excess NA rows.
remove.na.rows <- guam.cnmi.data.raw %>%
  rowwise() %>%
  filter(!all(is.na(across(everything())))) %>%
  ungroup()


#Remove survey responses with no boat fishing trips.
remove.nonboat.responses <- remove.na.rows %>%
  filter(Survey != "1120") %>%
  filter(Survey != "1186") %>% 
  filter(Survey != "1214") %>%
  filter(Survey != "3027")


#Remove survey responses with charter fishing as primary motivation.
remove.charter.vessels <- remove.nonboat.responses %>%
  filter(Survey != "3031") %>%
  filter(Survey != "3043")


#Remove partial response.
remove.partial.response <- remove.charter.vessels %>%
  filter(Survey != "1031") #skipped Q12, Q14-16, trip costs, Q49, Q53-54, Q59-60


#Recode -1 and -2 to NA.
recode.na <- remove.partial.response %>%
  mutate(across(everything(), ~if_else(.x == "-1" | .x == "-2", NA, .x)))

#Replace character strings to make single-species open-ended responses uniform.
#NOTE: There is a specific order of these mutate functions to ensure species
#are properly renamed.
recode.species <- recode.na %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "All bottom fish|bottom|Bottom|Bottom Fish|bottom fish|Bottom Fish - Asst|other bottom fish|Some Bottom fish|some bottomfish",
                replacement = "Bottomfish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "All deep bottom fish|Deep bottom species|Other deep Bottomfish|Other deep bottom|All deep Bottomfish|Deep Bottomfish species",
                replacement = "Deep bottomfish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "All pelagics yellowfin|Yellowfin Tuna|YF|YFT", 
                replacement = "Yellowfin tuna", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "All Reef Fish|hipovores|omnivores|reef fish|some reef fish|hipovores|Some reef fish", 
                replacement = "Reef fish", x = .))) %>% 
  mutate(across(Q56A:Q57C, ~gsub(pattern = "atulai|Atuli|mackerel|Bigeye Scad|scad", 
                replacement = "Atulai", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Baracuda|barracuda|barricuda|Barcuda|Barricuda", 
                replacement = "Barracuda", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Blue fin|Bluefin Trevally", 
                replacement = "Bluefin trevally", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Blue Marlin", 
                replacement = "Blue marlin", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "bonita|Bonita|Bonita tuna|Bonito|Katsu Tuna|katsuo|Katsuo|skipjack tuna", 
                replacement = "Skipjack tuna", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "caranx spp.|Scad|Tarakitu|Tarikito|travalley|trevally", 
                replacement = "Trevally", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "crustaceans", 
                replacement = "Crustaceans", x = .))) %>% 
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Dodu", 
                replacement = "Doddo", x = .))) %>% #Sergeant damselfishes, Mia/Eric to confirm
  mutate(across(Q56A:Q57C, ~gsub( pattern = "Emperors|lililok|lililook|long nose emperor|mafute|Mafute|mafuti|Mafuti",
                replacement = "Emperor fish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Etelis",
                replacement = "Ehu", x = .))) %>% #deepwater ruby snapper/squirrelfish
  mutate(across(Q56A:Q57C, ~gsub(pattern = "gadao|grouper|groupers|Groupers", 
                replacement = "Grouper", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Gendai", 
                replacement = "Gindai", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "guili|guilli|kyphosus sp.|rudderfish|Rudderfish", 
                replacement = "Guili", x = .))) %>% #rudderfish
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Gupao|Hiyok|Hiyuk", 
                replacement = "Surgeonfish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Haggon", 
                replacement = "Stout emperor", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Hargun", 
                replacement = "Orangespine unicornfish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Kale Kale|KaleKale|Kali kali|Kalikali|Karikari|Yellow tail", 
                replacement = "Kali kali", x = .))) %>% #Japanese rubyfish, deep bf
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Kichu|Kochu",
                replacement = "Convict tang", x = .))) %>% #Manini, reef fish
  mutate(across(Q56A:Q57C, ~gsub(pattern = "lobster", 
                replacement = "Lobster", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "mahi mahi|Mahi mahi|mahimahi|Mahimahi fish|Maho",
                replacement = "Mahimahi", x = .))) %>% 
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Mamulan|gta", 
                replacement = "Ulua", x = .))) %>% #Giant trevally
  mutate(across(Q56A:Q57C, ~gsub(pattern = "marlin|Marlin overflow", 
                replacement = "Marlin", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Milk Fish", 
                replacement = "Milkfish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "onaga", 
                replacement = "Onaga", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Pakapaka", 
                replacement = "Opakapaka", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "parrot fish|Parrot fish|Parrot Fish|parrotfish|Lugua", 
                replacement = "Parrotfish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "pelagic|Pealgic|some pelagic", 
                replacement = "Pelagic", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Pomfrett|wonder woman|Wonder Woman|Wonder woman|Monchong", 
                replacement = "Pomfret", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "rabbit fish|rabbitfish|siganus spp", 
                replacement = "Rabbitfish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Rainbow Runner", 
                replacement = "Rainbow runner", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "sas|Sasu", 
                replacement = "Taape", x = .))) %>% #Blue-lined snapper
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Silvermouth", 
                replacement = "Lehi", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Snappers", 
                replacement = "Snapper", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Some tuna|tuna", 
                replacement = "Tuna", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Dogtooth", 
                replacement = "Dogtooth tuna", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Tagafi|Red Snapper", 
                replacement = "Red snapper", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "tataga|Tataga|unicornfish", 
                replacement = "Unicornfish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "blue spine unicorn fish", 
                replacement = "Bluespine unicornfish", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "wahoo", 
                replacement = "Wahoo", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "Wahoo or mahi mahi depend on the season|Wahoo or Mahimahi depend on the season|wahoo/mahi", 
              replacement = "Wahoo/Mahimahi", x = .))) %>%
  mutate(across(Q56A:Q57C, ~gsub(pattern = "yellow tail", 
                replacement = "Yellowtail kalekale", x = .)))


#Create species groups from species names.
create.species.groups <- recode.species %>%
  mutate(Q56A.spp = Q56A,
         Q56B.spp = Q56B,
         Q56C.spp = Q56C,
         Q57A.spp = Q57A,
         Q57B.spp = Q57B,
         Q57C.spp = Q57C,) %>%
  mutate(across(Q56A.spp:Q57C.spp, ~gsub(pattern = "Barracuda|Blue Marlin|Bluefin Trevally|Dogtooth tuna|Jacks|Mahimahi|Marlin|Milkfish|Pelagic|Pomfret|Rainbow runner|Shark|Skipjack Tuna|Trevally|Tuna|Ulua|Wahoo|Wahoo/Mahimahi|Yellowfin Tuna", 
                replacement = "Pelagics", x = .))) %>%
  mutate(across(Q56A.spp:Q57C.spp, ~gsub(pattern = "Deep bottomfish|Ehu|Emperor fish|Gindai|Grouper|Kali kali|Lehi|Onaga|Opakapaka|Red snapper|Snapper|Stout emperor|Taape|Uku|Yellow Emperor|Yellowtail kalekale", 
                replacement = "Bottomfish", x = .))) %>% 
  mutate(across(Q56A.spp:Q57C.spp, ~gsub(pattern = "Atulai|Reef fish|Bluespine unicornfish|Convict tang|Doddo|Guili|Orangespine Unicornfish|Parrotfish|Rabbitfish|Surgeonfish|Triggerfish|Unicornfish|Wrasse|Yellow Tang", 
                replacement = "Nearshore", x = .))) %>% 
  mutate(across(Q56A.spp:Q57C.spp, ~gsub(pattern = "Lobster", 
                replacement = "Crustaceans", x = .)))


#Create full sample columns for calculations
create.full.sample.column <- create.species.groups %>%
         #Excluding duplicate of vessel info for data summaries and distributions
  mutate(Full.sample = ifelse(Survey == "1190B", NA, "full sample"),
         #Including duplicate of vessel info for vessel characteristics questions
         Full.sample.vessel.char = "full sample for vessel char.") %>%
  mutate_at(vars(Q17A:Q17G, Q18A:Q18C, Q19A:Q19G, Q20A:Q20B, Q21A:Q21C, Q22),
            list(~ifelse(sell.fish == 0, NA, .))) #Recoding those who did not sell fish to NA, has to be here in this script


#Create new columns that will be recoded later with midpoint values, to maintain
 #both codebook values and midpoint values for analyses.
create.midpoints.variables <- create.full.sample.column %>%
  mutate(Q1B.mid = Q1B, Q2A.mid = Q2A, Q2B.mid = Q2B, Q2C.mid = Q2C, Q2D.mid = Q2D,
         Q2E.mid = Q2E, Q2F.mid = Q2F, Q2G.mid = Q2G, Q3B.mid = Q3B, Q4A.mid = Q4A,
         Q4B.mid = Q4B, Q4C.mid = Q4C, Q4D.mid = Q4D, Q4E.mid = Q4E, Q4F.mid = Q4F,
         Q4G.mid = Q4G, Q4H.mid = Q4H, Q4I.mid = Q4I, Q5A.mid = Q5A, Q5B.mid = Q5B,
         Q8A.mid = Q8A, Q8B.mid = Q8B, Q8C.mid = Q8C, Q9.mid = Q9, Q13A.mid = Q13A,
         Q13B.mid = Q13B, Q15.mid = Q15, Q16A.mid = Q16A, Q16B.mid = Q16B,
         Q16C.mid = Q16C, Q16D.mid = Q16D, Q16E.mid = Q16E, Q16F.mid = Q16F,
         Q16G.mid = Q16G, Q17A.mid = Q17A, Q17B.mid = Q17B, Q17C.mid = Q17C,
         Q17D.mid = Q17D, Q17E.mid = Q17E, Q17F.mid = Q17F, Q17G.mid = Q17G,
         Q20A.mid = Q20A, Q21A.mid = Q21A, Q21B.mid = Q21B, Q21C.mid = Q21C,
         Q22.mid = Q22, Q28.mid = Q28, Q43.mid = Q43, Q53.mid = Q53)


#Recode codebook ranges to midpoint values & recode several other variables.
recode.variables <- create.midpoints.variables %>%
  mutate_at(vars(Q2A.mid, Q2B.mid, Q2C.mid, Q2D.mid, Q2E.mid, Q2F.mid, Q2G.mid,
                 Q4A.mid, Q4B.mid, Q4C.mid, Q4D.mid, Q4E.mid, Q4F.mid, Q4G.mid,
                 Q4H.mid, Q4I.mid, Q5A.mid, Q5B.mid, Q9.mid, Q13A.mid, Q13B.mid,
                 Q15.mid, Q16A.mid, Q16B.mid, Q16C.mid, Q16D.mid, Q16E.mid,
                 Q16F.mid, Q16G.mid, Q17A.mid, Q17B.mid, Q17C.mid, Q17D.mid,
                 Q17E.mid, Q17F.mid, Q17G.mid, Q21A.mid, Q21B.mid, Q21C.mid,
                 Q22.mid, Q28.mid), 
            list(~recode(., `1` = 0, 
                         `2` = 5,
                         `3` = 24.5,
                         `4` = 49.5,
                         `5` = 74.5,
                         `6` = 95))) %>%
  mutate_at(vars(Q1B.mid, Q3B.mid), 
            list(~recode(., `1` = 6, 
                         `2` = 18, 
                         `3` = 37,
                         `4` = 74.5,
                         `5` = 150,
                         `6` = 200))) %>% 
  mutate_at(vars(Q7A, Q10A:Q10H, Q10J, Q24A, Q24C, Q47:Q47H, Q47G, Q54A:Q54C),
            list(~recode(.,`0` = 2, #Recoding to fix issues with using "0" in calculations
                         `1` = 1))) %>%
  mutate(Q8A = ifelse(!is.na(Q8A.1) & Q8A.1 > 500, 6, Q8A)) %>% #adding 500+ lbs category for distribution tables
  mutate(Q8B = ifelse(!is.na(Q8B.1) & Q8B.1 > 500, 6, Q8B)) %>%
  mutate(Q8C = ifelse(!is.na(Q8C.1) & Q8C.1 > 500, 6, Q8C)) %>%
  mutate_at(vars(Q8A.mid, Q8B.mid, Q8C.mid),
            list(~recode(., `1` = 0,
                         `2` = 25.5,
                         `3` = 75.5,
                         `4` = 175.5,
                         `5` = 375.5))) %>%
  mutate_at(vars(Q18A:Q18C), #For those who sold fish, removing the 0's
            list( ~recode(., `0` = 2,
                          `1` = 1, 
                          `2` = 3))) %>% 
  mutate_at(vars(Q20A.mid),
            list(~recode(., `1` = 50.5,
                         `2` = 300.5,
                         `3` = 750.5,
                         `4` = 3000.5,
                         `5` = 7500.5,
                         `6` = 10000))) %>% 
  mutate(Q34A = recode(Q34A, `8` = 12)) %>% #So Q34A and Q37A have the same gear codebook values
  mutate_at(vars(Q43.mid),
            list(~recode(., `1` = 24,
                         `2` = 29.5,
                         `3` = 39.5,
                         `4` = 49.5,
                         `5` = 59.5,
                         `6` = 65))) %>%
  mutate(Q48A = recode(Q48A, `0` = 1, #Recoding to fix issues with using "0" in calculations
                       `1` = 2,
                       `2` = 3,
                       `3` = 4,
                       `4` = 5)) %>%
  mutate_at(vars(Q53.mid),
            list(~recode(., `1` = 2499, #Categories don't match with 2020 Census data, so leaving categories as is
                         `2` = 3749.5,
                         `3` = 7499.5,
                         `4` = 12499.5,
                         `5` = 19999.5,
                         `6` = 27499.5,
                         `7` = 34999.5,
                         `8` = 44999.5,
                         `9` = 62499.5,
                         `10` = 87499.5,
                         `11` = 100000))) 


#Create columns for if.else variables, and clean variables with unneeded 0's
create.ifelse.variables <- recode.variables %>%
  mutate(Q1A = ifelse(Q1A == 0, NA, Q1A), #Remove 0's from analysis
         Q2A.yesno = ifelse(Q2A > 1, 1, 2), #1 = yes, 2 = no
         Q2B.yesno = ifelse(Q2B > 1, 1, 2),
         Q2C.yesno = ifelse(Q2C > 1, 1, 2),
         Q2D.yesno = ifelse(Q2D > 1, 1, 2),
         Q2E.yesno = ifelse(Q2E > 1, 1, 2),
         Q2F.yesno = ifelse(Q2F > 1, 1, 2),
         Q2G.yesno = ifelse(Q2G > 1, 1, 2),
         Q3A = ifelse(Q3A == 0, NA, Q3A),
         Q1.mid.ifelse = ifelse(!is.na(Q1A), Q1A, Q1B.mid), 
         Q3.mid.ifelse = ifelse(!is.na(Q3A), Q3A, Q3B.mid), 
         Q7.combined = ifelse(Q7A == 1, Q7A, Q7B), #Total number of harbors/ramps used
         Q8A.mid.ifelse = ifelse(!is.na(Q8A.1), Q8A.1, Q8A.mid), #Analysis: Leave in 0's for catch volume (Q8)
         Q8B.mid.ifelse = ifelse(!is.na(Q8B.1), Q8B.1, Q8B.mid),
         Q8C.mid.ifelse = ifelse(!is.na(Q8C.1), Q8C.1, Q8C.mid),
         Q11A.primarymotiv = ifelse(Q11A == 1 & Q11I == 0, 1, 0), #1 = primary motivation, 0 = n/a, secondary, or tertiary
         Q11B.primarymotiv = ifelse(Q11B == 1 & Q11I == 0, 1, 0),
         Q11C.primarymotiv = ifelse(Q11C == 1 & Q11I == 0, 1, 0),
         Q11D.primarymotiv = ifelse(Q11D == 1 & Q11I == 0, 1, 0),
         Q11E.primarymotiv = ifelse(Q11E == 1 & Q11I == 0, 1, 0),
         Q11F.primarymotiv = ifelse(Q11F == 1 & Q11I == 0, 1, 0),
         Q11G.primarymotiv = ifelse(Q11G == 1 & Q11I == 0, 1, 0),
         vendor = ifelse(Q12A > 1, "vendor", "independent fisher"), #Defaults to independent fisher for NA's
         Q17A.yesno = ifelse(Q17A > 1, 1, 2), #1 = yes, 2 = no
         Q17B.yesno = ifelse(Q17B > 1, 1, 2),
         Q17C.yesno = ifelse(Q17C > 1, 1, 2),
         Q17D.yesno = ifelse(Q17D > 1, 1, 2),
         Q17E.yesno = ifelse(Q17E > 1, 1, 2),
         Q17F.yesno = ifelse(Q17F > 1, 1, 2),
         Q17G.yesno = ifelse(Q17G > 1, 1, 2),
         Q17A.dummy = ifelse(Q17A > 1, 1, 0), #1 = yes, 0 = no
         Q17B.dummy = ifelse(Q17B > 1, 1, 0),
         Q17C.dummy = ifelse(Q17C > 1, 1, 0),
         Q17D.dummy = ifelse(Q17D > 1, 1, 0),
         Q17E.dummy = ifelse(Q17E > 1, 1, 0),
         Q17F.dummy = ifelse(Q17F > 1, 1, 0),
         Q17G.dummy = ifelse(Q17G > 1, 1, 0),
         Q20.mid.ifelse = ifelse(!is.na(Q20B), Q20B, Q20A.mid),
         Q23.pelagics.yesno = ifelse(Q8A > 1 & Q23 > 1, 1, 2), #1 = yes, 2 = no
         Q23.bottomfish.yesno = ifelse(Q8B > 1 & Q23 > 1, 1, 2),
         Q23.reef.yesno = ifelse(Q8C > 1 & Q23 > 1, 1, 2),
         Q25.boatowner = ifelse(Q24A == 1, Q25, NA), #filter for boat owners
         Q26.boatowner = ifelse(Q24A == 1, Q26, NA),
         Q27.boatowner = ifelse(Q24A == 1, Q27, NA),
         Q28.boatowner = ifelse(Q24A == 1, Q28, NA),
         Q29.boatowner = ifelse(Q24A == 1, Q29, NA),
         Q30.boatowner = ifelse(Q24A == 1, Q30, NA),
         Q31.boatowner = ifelse(Q24A == 1, Q31, NA),
         Q33.boatowner = ifelse(Q24A == 1, Q33, NA),
         Q51 = ifelse(Q51 == 0, NA, Q51)) #Removes 0's from analysis


Q17.dummy.columns <- c("Q17A.dummy", "Q17B.dummy", "Q17C.dummy", "Q17D.dummy", 
                       "Q17E.dummy", "Q17F.dummy", "Q17G.dummy")


#Second batch of variable recoding, needs to be executed after previous recodes
recode.more.variables <- create.ifelse.variables %>%
  mutate(vendor = ifelse(is.na(vendor), "independent fisher", vendor),
         Q11A.yesno = ifelse(Q11A > 0, 1, 2), #1 = yes, 2 = no
         Q11B.yesno = ifelse(Q11B > 0, 1, 2),
         Q11C.yesno = ifelse(Q11C > 0, 1, 2),
         Q11D.yesno = ifelse(Q11D > 0, 1, 2),
         Q11E.yesno = ifelse(Q11E > 0, 1, 2),
         Q11F.yesno = ifelse(Q11F > 0, 1, 2),
         Q11G.yesno = ifelse(Q11G > 0, 1, 2),
         Q17.dummy.total = rowSums(select(., all_of(Q17.dummy.columns))),
         Q54A.caught = ifelse(Q8A.mid.ifelse > 0, Q54A, NA), #To limit question to those who caught the species group
         Q54B.caught = ifelse(Q8B.mid.ifelse > 0, Q54B, NA),
         Q54C.caught = ifelse(Q8C.mid.ifelse > 0, Q54C, NA)) %>%
  mutate(Q28.mid.boatowner = recode(Q28.boatowner, `1` = 0, 
                                    `2` = 5,
                                    `3` = 24.5,
                                    `4` = 49.5,
                                    `5` = 74.5,
                                    `6` = 95)) %>%
  mutate(Q34A.combined = recode(Q34A, `1` = "pelagic", #Group by species for trip costs section, same groups as Michel's work with this data
                                `2` = "bottomfish",
                                `3` = "bottomfish",
                                `4` = "nearshore",
                                `5` = "nearshore",
                                `6` = "nearshore",
                                `7` = "nearshore",
                                `12` = "mixed")) %>%
  mutate(Q37A.combined = recode(Q37A, `1` = "pelagic", #Group by species for trip costs section, same groups as Michel's work with this data
                                `2` = "bottomfish",
                                `3` = "bottomfish",
                                `4` = "nearshore",
                                `5` = "nearshore",
                                `6` = "nearshore",
                                `7` = "nearshore",
                                `8` = "shore",
                                `9` = "shore",
                                `10` = "shore",
                                `11` = "shore",
                                `12` = "mixed")) %>%
  mutate(Q30.nozeros = ifelse(Q30.boatowner == 0, NA, Q30.boatowner),
         Q31.nozeros = ifelse(Q31.boatowner == 0, NA, Q31.boatowner),
         Q32.nozeros = ifelse(Q32 == 0, NA, Q32),
         Q33.nozeros = ifelse(Q33.boatowner == 0, NA, Q33.boatowner))


#Create new variables needed for data disaggregations in the analysis. 
  #Final groups are character vectors so tables are easiest to read.
create.data.groups <- recode.more.variables %>%
         #Boat owner, 1190B is the same respondent as 1190A but with 2 boats
  mutate(boat.owner = ifelse(Q24A == 1 & Survey != "1190B", "boat owner", "not boat owner"),
         #Sell fish
         sell.fish.chr = ifelse(sell.fish == 1, "sold fish", "did not sell fish"),
         #GFCA member
         GFCA.member = ifelse(Q47A == 1, "gfca", "non-gfca"), 
         #Highliner. Calculates highliners for each species group, then combines them
         highliner.pelagic = ifelse(sell.fish == 1 & Q8A.mid.ifelse >= 2000, 1, 0),
         highliner.bf = ifelse(sell.fish == 1 & Q8B.mid.ifelse >= 500, 1, 0),
         highliner.reef = ifelse(sell.fish == 1 & Q8C.mid.ifelse >= 375.5, 1, 0),
         highliner = ifelse(highliner.pelagic == 1 | highliner.bf == 1 | 
                            highliner.reef == 1, "highliner", NA), 
         #Primary target
         primary.target = ifelse(Q2A.mid > Q2B.mid + Q2C.mid + 
                                 Q2E.mid + Q2F.mid + Q2G.mid, "pelagic",
                                 ifelse(Q2B.mid + Q2C.mid > Q2A.mid + 
                                        Q2D.mid + Q2E.mid + Q2F.mid +
                                        Q2G.mid, "bottomfish", 
                                        ifelse(Q2D.mid + Q2E.mid + Q2F.mid > 
                                               Q2A.mid + Q2B.mid + Q2C.mid +
                                               Q2G.mid, "reef", #"Reef" in R program = "nearshore" in report
                                               ifelse(Q2G.mid > Q2A.mid + 
                                                      Q2D.mid + Q2B.mid + 
                                                      Q2C.mid + Q2E.mid + 
                                                      Q2F.mid, "other", 
                                                      "no primary"))))) #Only 2 for "other" in Guam data, so suppressed the code for it
          #Island (CNMI report only)                                       

recode.highliner.nas <- create.data.groups %>%
  mutate(highliner = ifelse(sell.fish == 1 & is.na(highliner), "not highliner", 
                            highliner))


#Change object name to reflect that data cleaning is finished, for use in 
  #analyses scripts.
guam.cnmi.data.cleaned <- recode.highliner.nas


#Save as a CSV, so we have a comprehensive cleaned data set to share as needed.
write_csv(guam.cnmi.data.cleaned, "Data/GuamCNMI_SBF_20182019_DataCleaned.csv")


#Select Guam observations for the Guam report.
guam.data.cleaned <- guam.cnmi.data.cleaned %>%
  filter(Island == "Guam")


#Remove unneeded objects from global environment so it doesn't overcrowd the program.
rm(remove.na.rows, remove.nonboat.responses, #guam.cnmi.data.raw,
   remove.charter.vessels, remove.partial.response, recode.na, recode.species,
   create.data.groups, create.full.sample.column, create.ifelse.variables,
   create.midpoints.variables, create.species.groups, recode.variables,
   recode.highliner.nas, recode.more.variables, Q17.dummy.columns)


