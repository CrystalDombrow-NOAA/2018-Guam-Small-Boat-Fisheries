#######################
# 2018 Guam small boat fishery cost-earnings survey
# Vessel Characteristics Section
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


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
  # (MOST QUESTIONS FOR BOAT OWNERS ONLY)
#-------------------------------------

# Q24A. Do you own the boat that you fish on?
q24a.distr <- distribution.function(guam.data.cleaned, q.number = "Q24A", 
                                    categories = 1:2)

#---------------
# Q24B. If yes, what percentage of your time do you fish on your own boat?

#DATA SUMMARIES
q24b.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q24B")


#---------------

# Q24C. If no, do you always fish on the same boat?

#DISTRIBUTION
q24c.distr <- distribution.function(guam.data.cleaned, q.number = "Q24C", 
                                    categories = 1:2)

#---------------------------------------------------------------------

# Q25. What is the length of your boat?

#DATA SUMMARIES
q25.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q25.boatowner")


#---------------------------------------------------------------------

# Q26. What is the total horsepower? 

#DATA SUMMARIES
q26.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q26.boatowner")


#---------------------------------------------------------------------

# Q27. In what year was the boat built?

#DATA SUMMARIES
q27.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q27.boatowner")


#---------------------------------------------------------------------

# Q28. In 2020, what percent of time did other people (other than family members) 
# use the boat without you? 

#DATA SUMMARIES
q28.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q28.mid.boatowner")

#DISTRIBUTION
q28.distr <- distribution.function(guam.data.cleaned, q.number = "Q28.boatowner", 
                                   categories = 1:6)


#---------------------------------------------------------------------

# Q29. In what year did you purchase the boat you fish on?

#DATA SUMMARIES
q29.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q29.boatowner")


