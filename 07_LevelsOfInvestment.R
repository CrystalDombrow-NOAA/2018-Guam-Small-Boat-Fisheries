#######################
# 2018 Guam small boat fishery cost-earnings survey
# Levels of Investment Section
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


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q30. How much did you pay to purchase the boat you fish on?
# (If homebuilt – how much did it cost to build it?) 

#DATA SUMMARIES
q30.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q30.boatowner")

q30.nozeros.data.sum <- data.summaries.function(guam.data.cleaned, 
                                                q.number = "Q30.nozeros")


#---------------------------------------------------------------------

# Q31. What is the current market value, in dollars, of the electronics you 
# currently use to fish?

#DATA SUMMARIES
q31.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q31.boatowner")

q31.nozeros.data.sum <- data.summaries.function(guam.data.cleaned, 
                                                q.number = "Q31.nozeros")


#---------------------------------------------------------------------

# Q32. What is the current market value, in dollars, of the gear you 
# currently use to fish (not including electronics)?

#DATA SUMMARIES
q32.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q32")

q32.nozeros.data.sum <- data.summaries.function(guam.data.cleaned, 
                                                q.number = "Q32.nozeros")


#---------------------------------------------------------------------

# Q33 What is the current market value, in dollars, of your boat (considering 
# age & current condition & including motor(s) & trailer, but not including 
# electronics or gear mentioned above)? 

#DATA SUMMARIES
q33.data.sum <- data.summaries.function(guam.data.cleaned, 
                                        q.number = "Q33.boatowner")

q33.nozeros.data.sum <- data.summaries.function(guam.data.cleaned, 
                                                q.number = "Q33.nozeros")


