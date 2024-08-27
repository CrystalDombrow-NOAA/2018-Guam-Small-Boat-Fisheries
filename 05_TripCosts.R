#######################
# 2018 Guam small boat fishery cost-earnings survey
# TRIP COSTS section
# Calculations for tables
#######################

#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Run trip costs function
source("Functions/DataSummariesFunction.R")
source("Functions/DistributionFunction.R")
source("Functions/TripCostsFunction.R")


#-------------------------------------
# CALCULATIONS
#-------------------------------------

# Q35. On average, how did you pay for your fishing trip costs for your most 
# common gear type?

q35a.distr <- distribution.function(guam.data.cleaned, q.number = "Q35A", 
                                    categories = 1:4)


q35b.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q35B")
q35c.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q35C")
q35d.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q35D")
  

#-----------------------------------------------------------------------------

# Q36. On average, what was the total trip costs for your most common 
  # (question 34) gear type trip? 

# Q39. On average, what was the total trip costs for your second most 
  # common (question 37) gear type trip? 


#------------------
#PELAGIC
#------------------

#Full sample
q36.39.pelagic.full <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "pelagic", 
                      breakdown = "Full.sample")

#Sell fish
q36.39.pelagic.sell.fish <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "pelagic", 
                      breakdown = "sell.fish.chr")

#Highliner
q36.39.pelagic.highliner <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "pelagic", 
                      breakdown = "highliner")

# #GFCA
# q36.39.pelagic.gfca <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "pelagic", breakdown = "GFCA.member")
# 
# #Primary target
# q36.39.pelagic.primary.target <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "pelagic", breakdown = "primary.target")
# 
# #Boat owner
# q36.39.pelagic.boat.owner <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "pelagic", breakdown = "boat.owner")


#------------------
#BOTTOMFISH
#------------------

#Full sample
q36.39.bottomfish.full <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "bottomfish", 
                      breakdown = "Full.sample")

#Sell fish
q36.39.bottomfish.sell.fish <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "bottomfish", 
                      breakdown = "sell.fish.chr")

#Highliner
q36.39.bottomfish.highliner <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "bottomfish", 
                      breakdown = "highliner")

# #GFCA
# q36.39.bottomfish.gfca <- 
#   trip.costs.functionguam.data.cleaned, (gear.type.trip = "bottomfish", breakdown = "GFCA.member")
# 
# #Primary target
# q36.39.bottomfish.primary.target <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "bottomfish", breakdown = "primary.target")
# 
# #Boat owner
# q36.39.bottomfish.boat.owner <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "bottomfish", breakdown = "boat.owner")


#------------------
#NEARSHORE
#------------------

#Full sample
q36.39.nearshore.full <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "nearshore", 
                      breakdown = "Full.sample")

#Sell fish
q36.39.nearshore.sell.fish <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "nearshore", 
                      breakdown = "sell.fish.chr")

#Highliner
q36.39.nearshore.highliner <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "nearshore", 
                      breakdown = "highliner")

#GFCA
# q36.39.nearshore.gfca <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "nearshore", breakdown = "GFCA.member")
# 
# #Primary target
# q36.39.nearshore.primary.target <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "nearshore", breakdown = "primary.target")
# 
# #Boat owner
# q36.39.nearshore.boat.owner <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "nearshore", breakdown = "boat.owner")


#------------------
#MIXED GEAR
#------------------

#Full sample
q36.39.mixed.full <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "mixed", 
                      breakdown = "Full.sample")

#Sell fish
q36.39.mixed.sell.fish <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "mixed", 
                      breakdown = "sell.fish.chr")

#Highliner
q36.39.mixed.highliner <- 
  trip.costs.function(guam.data.cleaned, gear.type.trip = "mixed", 
                      breakdown = "highliner")

# #GFCA
# q36.39.mixed.gfca <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "mixed", breakdown = "GFCA.member")
# 
# #Primary target
# q36.39.mixed.primary.target <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "mixed", breakdown = "primary.target")
# 
# #Boat owner
# q36.39.mixed.boat.owner <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "mixed", breakdown = "boat.owner")


#------------------
#SHORE -- *NOTE: no "shore" responses for Q34A, so can't use trip costs function.
#                 need to create separate function to analyze these data for Q37A.
#------------------

#Full sample
# q36.39.shore.full <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "shore", breakdown = "Full.sample")
# 
# #Sell fish
# q36.39.shore.sell.fish <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "shore", breakdown = "sell.fish.chr")
# 
# #Highliner
# q36.39.shore.highliner <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "shore", breakdown = "highliner")
# 
# #GFCA
# q36.39.shore.gfca <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "shore", breakdown = "GFCA.member")
# 
# #Primary target
# q36.39.shore.primary.target <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "shore", breakdown = "primary.target")
# 
# #Boat owner
# q36.39.shore.boat.owner <- 
#   trip.costs.function(guam.data.cleaned, gear.type.trip = "shore", breakdown = "boat.owner")



#-------------------------------
# GALLONS OF GAS/DIESEL
#-------------------------------
#Code to analyze if there are significant findings for gallons of gas/diesel
  #reported as part of trip costs. -- only 3 respondents use boat diesel, 1 of 
  # these also uses truck diesel.

q36.39.gas.diesel <- guam.data.cleaned %>% 
  select(Q36A:Q36D, Q39A:Q39D)

write.csv(q36.39.gas.diesel, "Tables/Q36.39_gasdiesel.csv", 
          row.names = F)


#-----------------------------------------------------------------------------

# Q38. On average, how did you pay for your fishing trip costs for your second 
# most common gear type?

q38a.distr <- distribution.function(guam.data.cleaned, q.number = "Q38A", 
                                    categories = 1:4)


q38b.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q38B")
q38c.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q38C")
q38d.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q38D")

