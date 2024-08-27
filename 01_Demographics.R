#######################
# 2018 Guam small boat fishery cost-earnings survey
# Demographics section
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


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q43. What is your age?
    
#DATA SUMMARIES
q43.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q43.mid")


#DISTRIBUTION
q43.distr <- distribution.function(guam.data.cleaned, q.number = "Q43", 
                                   categories = 1:6)


#---------------------------------------------------------------------

# Q45. How long have you lived in the Marianas?

#DATA SUMMARIES
q45.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q45")


#---------------------------------------------------------------------

# Q46. How long have you fished from a boat?

#DATA SUMMARIES
q45.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q46")


#---------------------------------------------------------------------

# Q44. What village do you live in?

#DISTRIBUTION
q44.distr <- distribution.no.table.function(guam.data.cleaned, q.number = "Q44")


#Create table; can't be produced through distribution tables function
q44.table <- q44.distr["q.full.sample.dist"] %>%
  as.data.frame() %>%
  unique() %>%
  arrange()

#Save table
write.csv(q44.table, "Tables/Q44_distribution.csv", row.names = F)


#---------------------------------------------------------------------

# Q49. What is your race? (check all that apply) 

#DISTRIBUTION
q49.distr <- distribution.function(guam.data.cleaned, q.number = "Q49A.1", 
                                   categories = 1:16)


#----------------------------------------------------------------------

# Q48. Are you Hispanic, Latino, or of Spanish Origin?

#DISTRIBUTION
q48.distr <- distribution.function(guam.data.cleaned, q.number = "Q48A", 
                                   categories = 1:5)


#----------------------------------------------------------------------

# Q50. Are you currently employed?

#DISTRIBUTION
q50.distr <- distribution.function(guam.data.cleaned, q.number = "Q50A", 
                                   categories = 1:7)


#----------------------------------------------------------------------

# Q52. What is the highest level of education you have completed?

#DISTRIBUTION
q52.distr <- distribution.function(guam.data.cleaned, q.number = "Q52", 
                                   categories = 1:7)


#---------------------------------------------------------------------

# Q53. What was your total household income, before taxes, in 2017, including 
# fishing income?

#DISTRIBUTION
q53.distr <- distribution.function(guam.data.cleaned, q.number = "Q53", 
                                   categories = 1:11)


#DATA SUMMARIES
q53.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q53.mid")


#---------------------------------------------------------------------
  
# Q42. What is your gender?
  
#DISTRIBUTION
q42.distr <- distribution.function(guam.data.cleaned, q.number = "Q42", 
                                   categories = 1:2)


#----------------------------------------------------------------------

# Q51. How many hours per week do you work for pay (non-fishing)?

#DATA SUMMARIES
q51.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q51")


