#----------------
#FUNCTION FOR TRIP COSTS CALCULATIONS
#2018 Guam small boat fishery cost-earnings survey
#----------------

#Load libraries
library(dplyr)
library(tidyverse)
library(plotrix)



#Write function
trip.costs.function <- function(guam.data.cleaned, gear.type.trip, breakdown){
  
  
  #Rename argument for group_by to work below
  guam.cleaned.trip.costs <- guam.data.cleaned %>% 
    rename(brkdwn = breakdown)

  #-------------------------------------
  #-------------------------------------
  #COMBINED CALCULATIONS
  
  #Set up dataframe for primary gear so can rbind later
  primary <- guam.cleaned.trip.costs %>%
    select(any_of(c("Q36A", "Q36C", "Q36E", "Q36F", "Q36G", "Q34A.combined", 
                    "Q36H", "Q36I", "brkdwn"))) %>%
    rename(gear = Q34A.combined,
           boat.fuel = Q36A,
           truck.fuel = Q36C,
           ice = Q36E,
           bait = Q36F,
           food.bev = Q36G,
           main.rep = Q36H,
           other = Q36I)
  
  
  #Set up dataframe for secondary gear so can rbind later
  secondary <- guam.cleaned.trip.costs %>%
    select(any_of(c("Q39A", "Q39C", "Q39E", "Q39F", "Q39G", "Q37A.combined", 
                    "Q39H", "Q39I", "brkdwn"))) %>%
    rename(gear = Q37A.combined,
           boat.fuel = Q39A,
           truck.fuel = Q39C,
           ice = Q39E,
           bait = Q39F,
           food.bev = Q39G,
           main.rep = Q39H,
           other = Q39I)
  
  
  #Combine both dataframes, but lose indication of primary and secondary
  primary.plus.secondary <- rbind(primary, secondary)
  
  
  #Combined calculations
  trip.costs.combined <- primary.plus.secondary %>%
    filter(gear == gear.type.trip) %>%
    drop_na() %>%
    group_by(brkdwn) %>%
           #BOAT FUEL
    mutate(avg.boat.fuel = round(mean(boat.fuel, na.rm = T), 2),
           boat.fuel.std.err = round(std.error(boat.fuel, na.rm = T), 1),
           med.boat.fuel = round(median(boat.fuel, na.rm = T), 2),
           #TRUCK FUEL
           avg.truck.fuel = round(mean(truck.fuel, na.rm = T), 2),
           truck.fuel.std.err = round(std.error(truck.fuel, na.rm = T), 1),
           med.truck.fuel = round(median(truck.fuel, na.rm = T), 2),
           #ICE
           avg.ice = round(mean(ice, na.rm = T), 2),
           ice.std.err = round(std.error(ice, na.rm = T), 1),
           med.ice = round(median(ice, na.rm = T), 2),
           #BAIT
           avg.bait = round(mean(bait, na.rm = T), 2),
           bait.std.err = round(std.error(bait, na.rm = T), 1),
           med.bait = round(median(bait, na.rm = T), 2),
           #FOOD & BEVERAGE
           avg.food.bev = round(mean(food.bev, na.rm = T), 2),
           food.bev.std.err = round(std.error(food.bev, na.rm = T), 1),
           med.food.bev = round(median(food.bev, na.rm = T), 2),
           #DAILY MAINTENANCE & REPAIR
           avg.main.rep = round(mean(main.rep, na.rm = T), 2),
           main.rep.std.err = round(std.error(main.rep, na.rm = T), 1),
           med.main.rep = round(median(main.rep, na.rm = T), 2),
           #OTHER
           avg.other = round(mean(other, na.rm = T), 2),
           other.std.err = round(std.error(other, na.rm = T), 1),
           med.other = round(median(other, na.rm = T), 2),
           #MINS & MAXS
           min.boat.fuel = round(min(boat.fuel, na.rm = T), 2),
           max.boat.fuel = round(max(boat.fuel, na.rm = T), 2),
           min.truck.fuel = round(min(truck.fuel, na.rm = T), 2),
           max.truck.fuel = round(max(truck.fuel, na.rm = T), 2),
           min.ice = round(min(ice, na.rm = T), 2),
           max.ice = round(max(ice, na.rm = T), 2),
           min.bait = round(min(bait, na.rm = T), 2),
           max.bait = round(max(bait, na.rm = T), 2),
           min.food.bev = round(min(food.bev, na.rm = T), 2),
           max.food.bev = round(max(food.bev, na.rm = T), 2),
           min.main.rep = round(min(main.rep, na.rm = T), 2),
           max.main.rep = round(max(main.rep, na.rm = T), 2),
           min.other = round(min(other, na.rm = T), 2),
           max.other = round(max(other, na.rm = T), 2)) %>%
    mutate(avg.trip = unique(avg.boat.fuel + avg.truck.fuel + avg.ice +
                             avg.food.bev + avg.bait + avg.main.rep + avg.other)) %>%
    mutate(std.err.trip = std.error(boat.fuel + truck.fuel + ice + bait + 
                                      food.bev + main.rep + other)) %>%
    mutate(med.trip = unique(med.boat.fuel + med.truck.fuel + med.ice +
                               med.food.bev + med.bait + med.main.rep + med.other)) %>% 
    mutate(min.trip = unique(min.boat.fuel + min.truck.fuel + min.ice +
                              min.food.bev + min.bait + min.main.rep + min.other)) %>%
    mutate(max.trip = unique(max.boat.fuel + max.truck.fuel + max.ice +
                              max.food.bev + max.bait + max.main.rep + max.other)) %>%
    mutate(bt.fuel.per = round(unique(avg.boat.fuel / avg.trip * 100), 1),
           truck.fuel.per = round(unique(avg.truck.fuel / avg.trip * 100), 1),
           ice.per = round(unique(avg.ice / avg.trip * 100), 1),
           bait.per = round(unique(avg.bait / avg.trip * 100), 1),
           food.bev.per = round(unique(avg.food.bev / avg.trip * 100), 1),
           main.rep.per = round(unique(avg.main.rep / avg.trip * 100), 1),
           other.per = round(unique(avg.other / avg.trip * 100), 1))
  
  # 
  # #----------------------------------------------
  # #PRIMARY CALCULATIONS
  # 
  # trip.costs.primary <- guam.cleaned.trip.costs %>%
  #   select(any_of(c("Q34A.combined", "Q36A", "Q36C", "Q36E", "Q36F", "Q36G", 
  #                   "Q36H", "Q36I", "brkdwn"))) %>%
  #   filter(Q34A.combined == gear.type.trip) %>%
  #   drop_na() %>%
  #   group_by(brkdwn) %>%
  #          #BOAT FUEL
  #   mutate(avg.boat.fuel = round(mean(Q36A, na.rm = T), 2),
  #          boat.fuel.std.err = round(std.error(Q36A, na.rm = T), 1),
  #          med.boat.fuel = round(median(Q36A, na.rm = T), 2),
  #          # min.boat.fuel = round(min(Q36A, na.rm = T), 2),
  #          # max.boat.fuel = round(max(Q36A, na.rm = T), 2),
  #          #TRUCK FUEL
  #          avg.truck.fuel = round(mean(Q36C, na.rm = T), 2),
  #          truck.fuel.std.err = round(std.error(Q36C, na.rm = T), 1),
  #          med.truck.fuel = round(median(Q36C, na.rm = T), 2),
  #          # min.truck.fuel = round(min(Q36C, na.rm = T), 2),
  #          # max.truck.fuel = round(max(Q36C, na.rm = T), 2),
  #          #ICE
  #          avg.ice = round(mean(Q36E, na.rm = T), 2),
  #          ice.std.err = round(std.error(Q36E, na.rm = T), 1),
  #          med.ice = round(median(Q36E, na.rm = T), 2),
  #          # min.ice = round(min(Q36E, na.rm = T), 2),
  #          # max.ice = round(max(Q36E, na.rm = T), 2),
  #          #BAIT
  #          avg.bait = round(mean(Q36F, na.rm = T), 2),
  #          bait.std.err = round(std.error(Q36F, na.rm = T), 1),
  #          med.bait = round(median(Q36F, na.rm = T), 2),
  #          # min.bait = round(min(Q36F, na.rm = T), 2),
  #          # max.bait = round(max(Q36F, na.rm = T), 2),
  #          #FOOD & BEVERAGE
  #          avg.food.bev = round(mean(Q36G, na.rm = T), 2),
  #          food.bev.std.err = round(std.error(Q36G, na.rm = T), 1),
  #          med.food.bev = round(median(Q36G, na.rm = T), 2), 
  #          # min.food.bev = round(min(Q36G, na.rm = T), 2),
  #          # max.food.bev = round(max(Q36G, na.rm = T), 2),
  #          #DAILY MAINTENANCE & REPAIR
  #          avg.main.rep = round(mean(Q36H, na.rm = T), 2),
  #          main.rep.std.err = round(std.error(Q36H, na.rm = T), 1),
  #          med.main.rep = round(median(Q36H, na.rm = T), 2),
  #          # min.main.rep = round(min(Q36H, na.rm = T), 2),
  #          # max.main.rep = round(max(Q36H, na.rm = T), 2),
  #          #OTHER
  #          avg.other = round(mean(Q36I, na.rm = T), 2),
  #          other.std.err = round(std.error(Q36I, na.rm = T), 1),
  #          med.other = round(median(Q36I, na.rm = T), 2)) %>%
  #          # min.other = round(min(Q36I, na.rm = T), 2),
  #          # max.other = round(max(Q36I, na.rm = T), 2)) %>% 
  #   mutate(avg.trip = unique(avg.boat.fuel + avg.truck.fuel + avg.ice +
  #                              avg.food.bev + avg.bait + avg.main.rep + avg.other)) %>%
  #   mutate(std.err.trip = std.error(Q36A + Q36C + Q36E + Q36F + Q36G + Q36H + Q36I)) %>%
  #   mutate(med.trip = unique(med.boat.fuel + med.truck.fuel + med.ice +
  #                              med.food.bev + med.bait + med.main.rep + med.other)) %>% 
  #   # mutate(min.trip = unique(min.boat.fuel + min.truck.fuel + min.ice +
  #   #                            min.food.bev + min.bait + min.main.rep + min.other)) %>%
  #   # mutate(max.trip = unique(max.boat.fuel + max.truck.fuel + max.ice +
  #   #                            max.food.bev + max.bait + max.main.rep + max.other)) %>%
  #   mutate(bt.fuel.per = round(unique(avg.boat.fuel / avg.trip * 100), 1),
  #          truck.fuel.per = round(unique(avg.truck.fuel / avg.trip * 100), 1),
  #          ice.per = round(unique(avg.ice / avg.trip * 100), 1),
  #          bait.per = round(unique(avg.bait / avg.trip * 100), 1),
  #          food.bev.per = round(unique(avg.food.bev / avg.trip * 100), 1),
  #          main.rep.per = round(unique(avg.main.rep / avg.trip * 100), 1),
  #          other.per = round(unique(avg.other / avg.trip * 100), 1))
  # 
  # 
  # 
  # #----------------------------------------------
  # #SECONDARY CALCULATIONS
  # 
  # trip.costs.secondary <- guam.cleaned.trip.costs %>%
  #   select(any_of(c("Q37A.combined", "Q39A", "Q39C", "Q39E", "Q39F", "Q39G", 
  #                   "Q39H", "Q39I", "brkdwn"))) %>%
  #   filter(Q37A.combined == gear.type.trip) %>%
  #   drop_na() %>%
  #   group_by(brkdwn) %>%
  #          #BOAT FUEL
  #   mutate(avg.boat.fuel = round(mean(Q39A, na.rm = T), 2),
  #          boat.fuel.std.err = round(std.error(Q39A, na.rm = T), 1),
  #          med.boat.fuel = round(median(Q39A, na.rm = T), 2),
  #          # min.boat.fuel = round(min(Q39A, na.rm = T), 2),
  #          # max.boat.fuel = round(max(Q39A, na.rm = T), 2),
  #          #TRUCK FUEL
  #          avg.truck.fuel = round(mean(Q39C, na.rm = T), 2),
  #          truck.fuel.std.err = round(std.error(Q39C, na.rm = T), 1),
  #          med.truck.fuel = round(median(Q39C, na.rm = T), 2),
  #          # min.truck.fuel = round(min(Q39C, na.rm = T), 2),
  #          # max.truck.fuel = round(max(Q39C, na.rm = T), 2),
  #          #ICE
  #          avg.ice = round(mean(Q39E, na.rm = T), 2),
  #          ice.std.err = round(std.error(Q39E, na.rm = T), 1),
  #          med.ice = round(median(Q39E, na.rm = T), 2),
  #          # min.ice = round(min(Q39E, na.rm = T), 2),
  #          # max.ice = round(max(Q39E, na.rm = T), 2),
  #          #BAIT
  #          avg.bait = round(mean(Q39F, na.rm = T), 2),
  #          bait.std.err = round(std.error(Q39F, na.rm = T), 1),
  #          med.bait = round(median(Q39F, na.rm = T), 2),
  #          # min.bait = round(min(Q39F, na.rm = T), 2),
  #          # max.bait = round(max(Q39F, na.rm = T), 2),
  #          #FOOD & BEVERAGE
  #          avg.food.bev = round(mean(Q39G, na.rm = T), 2),
  #          food.bev.std.err = round(std.error(Q39G, na.rm = T), 1),
  #          med.food.bev = round(median(Q39G, na.rm = T), 2), 
  #          # min.food.bev = round(min(Q39G, na.rm = T), 2),
  #          # max.food.bev = round(max(Q39G, na.rm = T), 2),
  #          #DAILY MAINTENANCE & REPAIR
  #          avg.main.rep = round(mean(Q39H, na.rm = T), 2),
  #          main.rep.std.err = round(std.error(Q39H, na.rm = T), 1),
  #          med.main.rep = round(median(Q39H, na.rm = T), 2),
  #          # min.main.rep = round(min(Q39H, na.rm = T), 2),
  #          # max.main.rep = round(max(Q39H, na.rm = T), 2),
  #          #OTHER
  #          avg.other = round(mean(Q39I, na.rm = T), 2),
  #          other.std.err = round(std.error(Q39I, na.rm = T), 1),
  #          med.other = round(median(Q39I, na.rm = T), 2)) %>%
  #          # min.other = round(min(Q39I, na.rm = T), 2),
  #          # max.other = round(max(Q39I, na.rm = T), 2)) %>% 
  #   mutate(avg.trip = unique(avg.boat.fuel + avg.truck.fuel + avg.ice +
  #                              avg.food.bev + avg.bait + avg.main.rep + avg.other)) %>%
  #   mutate(std.err.trip = std.error(Q39A + Q39C + Q39E + Q39F + Q39G + Q39H + Q39I)) %>%
  #   mutate(med.trip = unique(med.boat.fuel + med.truck.fuel + med.ice +
  #                              med.food.bev + med.bait + med.main.rep + med.other)) %>% 
  #   # mutate(min.trip = unique(min.boat.fuel + min.truck.fuel + min.ice +
  #   #                            min.food.bev + min.bait + min.main.rep + min.other)) %>%
  #   # mutate(max.trip = unique(max.boat.fuel + max.truck.fuel + max.ice +
  #   #                            max.food.bev + max.bait + max.main.rep + max.other)) %>%
  #   mutate(bt.fuel.per = round(unique(avg.boat.fuel / avg.trip * 100), 1),
  #          truck.fuel.per = round(unique(avg.truck.fuel / avg.trip * 100), 1),
  #          ice.per = round(unique(avg.ice / avg.trip * 100), 1),
  #          bait.per = round(unique(avg.bait / avg.trip * 100), 1),
  #          food.bev.per = round(unique(avg.food.bev / avg.trip * 100), 1),
  #          main.rep.per = round(unique(avg.main.rep / avg.trip * 100, 1)),
  #          other.per = round(unique(avg.other / avg.trip * 100, 1)))
  
  
  
  #---------------------------------------------
  #DISTRIBUTION OF PRIMARY/SECONDARY GEAR TYPE TRIPS
  
  # distribution.primary.secondary <- trip.costs.combined %>%
  #   mutate(per.primary = nrow(trip.costs.primary) / nrow(trip.costs.combined) * 100,
  #          per.secondary = nrow(trip.costs.secondary) / nrow(trip.costs.combined) * 100) %>%
  #   select(per.primary, per.secondary)
  
  
  
  #-------------------------------------
  #-------------------------------------
  #Clean up objects to later rbind into a table
  
  trip.costs.combined.table <- trip.costs.combined %>% 
    select(-gear, -boat.fuel, -truck.fuel, -ice, -bait, -food.bev, -main.rep, -other) %>%
    unique() %>%
    mutate(n = nrow(trip.costs.combined), .before = avg.boat.fuel) %>%
    mutate(trip.type = "combined", .before = n)
  
  # trip.costs.primary.table <- trip.costs.primary %>% 
  #   select(-Q34A.combined, -Q36A, -Q36C, -Q36E, -Q36F, -Q36G, -Q36H, -Q36I) %>%
  #   unique() %>%
  #   mutate(n = nrow(trip.costs.primary), .before = avg.boat.fuel) %>%
  #   mutate(trip.type = "primary", .before = n)
  # 
  # trip.costs.secondary.table <- trip.costs.secondary %>% 
  #   select(-Q37A.combined, -Q39A, -Q39C, -Q39E, -Q39F, -Q39G, -Q39H, -Q39I) %>%
  #   unique() %>%
  #   mutate(n = nrow(trip.costs.secondary), .before = avg.boat.fuel) %>%
  #   mutate(trip.type = "secondary", .before = n)
  
  
  #-------------------------------------
  #-------------------------------------
  #CREATE OUTPUT TABLES
  
  #Rbind together
  q.trip.costs.sum <- rbind(trip.costs.combined.table
                            #trip.costs.primary.table,
                            #trip.costs.secondary.table
                            ) %>%
    t()
  
  #Save final table 
  write.csv(q.trip.costs.sum, na = "0.0", paste("Tables/Q36.39_", 
                                                gear.type.trip,
                                                "_",
                                                breakdown, 
                                                sep = "", 
                                                "_tripcosts.csv"), 
            row.names = T)
  
    
  
  #-------------------------------------
  #-------------------------------------
  #Return final list of above objects
  output.list <- list("trip.costs.combined" = trip.costs.combined)
                      #"trip.costs.primary" = trip.costs.primary,
                      #"trip.costs.secondary" = trip.costs.secondary,
                      #"distribution.primary.secondary" = 
                        #distribution.primary.secondary)
  
  
  return(output.list)
  
}
  
  