#######################
# 2018 Guam small boat fishery cost-earnings survey
# Fishing Activity section
# Calculations for tables
#######################

#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Load libraries
library(tidyverse)
library(dplyr)
library(plotrix)


#Run functions used in the analyses below
source("Functions/DataSummariesFunction.R")
source("Functions/DistributionFunction.R")
source("Functions/DistributionFunction_NoTable.R")
source("Functions/DistributionFunction_MultipleVariables_GearTrips.R")
source("Functions/DistributionFunction_MultipleVariables_TripDays.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q1. Approximately how many boat fishing trips did you take over the past 12 months? 
#Excluded 0's, must be at least 1 trip

#DISTRIBUTION
q1.distr <- distribution.function(guam.data.cleaned, q.number = "Q1B", 
                                  categories = 1:6)
#Q1B includes Q1A responses recoded into Q1B categories

#DATA SUMMARIES
q1.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q1.mid.ifelse")


#---------------------------------------------------------------------------

# Q2. In the past 12 months, how many of your BOAT fishing trips were primarily:

#------------------------
# Percentage of fishers using gear types on a boat fishing trip in 2020. 
#------------------------

#TROLLING
q2a.yesno <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2A.yesno", categories = 1:2) 

#DEEP BOTTOMFISH
q2b.yesno <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2B.yesno", categories = 1:2)

#SHALLOW BOTTOMFISH
q2c.yesno <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2C.yesno", categories = 1:2)

#ATULAI
q2d.yesno <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2D.yesno", categories = 1:2)

#REEF FISHING (SPEAR)
q2e.yesno <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2E.yesno", categories = 1:2)

#REEF FISHING (NET)
q2f.yesno <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2F.yesno", categories = 1:2)

#OTHER
q2g.yesno <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2G.yesno", categories = 1:2)



#------------------------
#DISTRIBUTION TABLES
#------------------------

#TROLLING
q2a.distr <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2A", categories = 1:6)

#DEEP BOTTOMFISH
q2b.distr <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2B", categories = 1:6)

#SHALLOW BOTTOMFISH
q2c.distr <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2C", categories = 1:6)

#ATULAI
q2d.distr <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2D", categories = 1:6)

#REEF FISHING (SPEAR)
q2e.distr <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2E", categories = 1:6)

#REEF FISHING (NET)
q2f.distr <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2F", categories = 1:6)

#OTHER
q2g.distr <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q2G", categories = 1:6)


#-------------------------------
#NUMBER OF DIFFERENT GEARS USED
#-------------------------------
q2i.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q2I")


#-----------------------------
#DISTRIBUTION ACROSS VARIABLES
#-----------------------------
#FULL SAMPLE
q2.full.sample <- distribution.function.gear.trips(guam.data.cleaned, 
                                                   breakdown = "Full.sample")

#GFCA
q2.gfca <- distribution.function.gear.trips(guam.data.cleaned, 
                                            breakdown = "GFCA.member")

#SELL FISH
q2.sell.fish <- distribution.function.gear.trips(guam.data.cleaned, 
                                                 breakdown = "sell.fish.chr")

#HIGHLINER
q2.highliner <- distribution.function.gear.trips(guam.data.cleaned, 
                                                 breakdown = "highliner")

#PRIMARY TARGET
q2.primary.target <- distribution.function.gear.trips(guam.data.cleaned, 
                                                      breakdown = "primary.target")

#BOAT OWNER
q2.boat.owner <- distribution.function.gear.trips(guam.data.cleaned, 
                                                  breakdown = "boat.owner")



#Create output table, just need to get number of observations from above objects
q2.gear.trips.table <- rbind(q2.full.sample,
                            q2.gfca,
                            q2.sell.fish,
                            q2.highliner,
                            q2.primary.target,
                            q2.boat.owner)

row.order <- c("full sample", "gfca", "non-gfca", "sold fish", "highliner", 
               "not highliner", "did not sell fish", "pelagic", "bottomfish", 
               "reef", "no primary", "other", "boat owner", "not boat owner")

q2.gear.trips.table <- q2.gear.trips.table %>%
  slice(match(row.order, brk.down))


write.csv(q2.gear.trips.table, 
          paste("Tables/Q2_distribution_multiplevariables.csv"), 
          row.names = F)


#---------------------------------------------------------------------------

# Q3. Percentage of respondents reporting shore-based trips in 2017:

q3 <- guam.data.cleaned %>%
  select(Q3.mid.ifelse) %>%
  drop_na()

full.sample <- guam.data.cleaned %>%
  select(Full.sample) %>%
  drop_na()

q3.percent <- guam.data.cleaned %>%
  select(Q3.mid.ifelse, Full.sample) %>%
  mutate(n.q3 = nrow(q3),
         n.full.sample = nrow(full.sample)) %>%
  mutate(percent = round(n.q3 / n.full.sample * 100, 1))


q3.data.sum <- data.summaries.function(guam.data.cleaned, 
                                       q.number = "Q3.mid.ifelse")


#---------------------------------------------------------------------------

# Q5. In the past 12 months, how many of your boat fishing trips were:
# single day/multiday?

#DISTRIBUTION
q5a.distr <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q5A", categories = 1:6)

q5b.distr <- distribution.function(guam.data.cleaned, 
                                   q.number = "Q5B", categories = 1:6)


#DATA SUMMARIES
q5a.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q5A.mid")

q5b.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q5B.mid")


#-----------------------------
#DISTRIBUTION ACROSS VARIABLES
#-----------------------------
#FULL SAMPLE
q5.full.sample <- distribution.function.trip.days(guam.data.cleaned, 
                                                  breakdown = "Full.sample")

#GFCA
q5.gfca <- distribution.function.trip.days(guam.data.cleaned, 
                                           breakdown = "GFCA.member")

#SELL FISH
q5.sell.fish <- distribution.function.trip.days(guam.data.cleaned, 
                                                breakdown = "sell.fish.chr")
#HIGHLINER
q5.highliner <- distribution.function.trip.days(guam.data.cleaned, 
                                                breakdown = "highliner")

#PRIMARY TARGET
q5.primary.target <- distribution.function.trip.days(guam.data.cleaned, 
                                                     breakdown = "primary.target")

#BOAT OWNER
q5.boat.owner <- distribution.function.trip.days(guam.data.cleaned, 
                                                 breakdown = "boat.owner")


#Create output table, just need to get number of observations from above objects
q5.trip.days.table <- rbind(q5.full.sample,
                              q5.gfca,
                              q5.sell.fish,
                              q5.highliner,
                              q5.primary.target,
                              q5.boat.owner)

row.order <- c("full sample", "gfca", "non-gfca", "sold fish", "highliner", 
               "not highliner", "did not sell fish", "pelagic", "bottomfish", 
               "reef", "no primary", "other", "boat owner", "not boat owner")

q5.trip.days.table <- q5.trip.days.table %>%
  slice(match(row.order, brk.down))


write.csv(q5.trip.days.table, 
          paste("Tables/Q5_distribution_multiplevariables.csv"), 
          row.names = F)


#---------------------------------------------------------------------------

# Q6. How many people in total, including yourself, are on board for an average 
# boat fishing trip? 

#DATA SUMMARIES
q6.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q6")


#---------------------------------------------------------------------------

# Q7. Do you always fish out of the same boat ramp or harbor?

#DISTRIBUTION
q7a.distr <- distribution.function(guam.data.cleaned, q.number = "Q7A", 
                                   categories = 1:2)


# If no, on average, how many different boat ramps or harbors do you use in a year?

#DATA SUMMARIES
q7b.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q7B")

q7combined.data.sum <- data.summaries.function(guam.data.cleaned, 
                                               q.number = "Q7.combined")


#---------------------------------------------------------------------------

# Q8A. In the past 12 months, approximately how many total pounds of pelagic 
# fish did you catch?

#DATA SUMMARIES
q8a.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q8A.mid.ifelse") 

#DISTRIBUTION
q8a.distr <- distribution.function(guam.data.cleaned, q.number = "Q8A", 
                                   categories = 1:6)


#---------------------------------------------------------------------------

# Q8B. In the past 12 months, approximately how many total pounds of bottomfish 
# did you catch?

#DATA SUMMARIES
q8b.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q8B.mid.ifelse")

#DISTRIBUTION
q8b.distr <- distribution.function(guam.data.cleaned, q.number = "Q8B", 
                                   categories = 1:6)


#---------------------------------------------------------------------------

# Q8C. In the past 12 months, approximately how many total pounds of reef 
# fish did you catch?

#DATA SUMMARIES
q8c.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q8C.mid.ifelse")

#DISTRIBUTION
q8c.distr <- distribution.function(guam.data.cleaned, q.number = "Q8C", 
                                   categories = 1:6)


#---------------------------------------------------------------------------

# Q9. In the past 12 months, how many of your fishing trips did you fish at Fish 
# Aggregating Devices (FADs)?

#DISTRIBUTION
q9.distr <- distribution.function(guam.data.cleaned, q.number = "Q9", 
                                  categories = 1:6)


#DATA SUMMARIES
q9.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q9.mid")


#---------------------------------------------------------------------------

# Q56. What are the top three species you target to sell?

  #right_join in the distribution function is breaking because of the character 
   #values, this seems to work fine.

q56a.spp.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q56A.spp")
q56b.spp.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q56B.spp")
q56c.spp.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q56C.spp")


q56a.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q56A")
q56b.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q56B")
q56c.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q56C")

q56a.freq.counts <- q56a.distr[["q.full.sample.dist"]] %>%
  count(q.num)
q56b.freq.counts <- q56b.distr[["q.full.sample.dist"]] %>%
  count(q.num)
q56c.freq.counts <- q56c.distr[["q.full.sample.dist"]] %>%
  count(q.num)


#---------------------------------------------------------------------------

# Q57. What are the top three species you target to give away?

q57a.spp.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q57A.spp")
q57b.spp.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q57B.spp")
q57c.spp.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q57C.spp")


q57a.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q57A")
q57b.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q57B")
q57c.distr <- distribution.no.table.function(guam.data.cleaned, 
                                             q.number = "Q57C")

q57a.freq.counts <- q57a.distr[["q.full.sample.dist"]] %>%
  count(q.num)
q57b.freq.counts <- q57b.distr[["q.full.sample.dist"]] %>%
  count(q.num)
q57c.freq.counts <- q57c.distr[["q.full.sample.dist"]] %>%
  count(q.num)


