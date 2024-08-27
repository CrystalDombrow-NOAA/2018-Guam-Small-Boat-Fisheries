#----------------
#FUNCTION FOR ANNUAL EXPENDITURES CALCULATIONS
#2018 Guam small boat fishery cost-earnings survey

#NOTE: Financial services are "0" for noncommercial fishers. For the table 
# "excluding zeros," need to manually comment out and delete financial services
# portions in order to get the correct total mean, standard error, and median.
#----------------

#Load libraries
library(dplyr)
library(tidyverse)
library(plotrix)


#Write function
annual.expenditures.function <- function(guam.data.cleaned, breakdown){
  
  
  #Rename argument for group_by to work below
  guam.annual.exp.cleaned <- guam.data.cleaned %>% 
    rename(brk.down = breakdown)
  
  #Set up dataframe for calculations below
  annual.expenditures.colnames <- guam.annual.exp.cleaned %>%
    select(any_of(c("Q40A", "Q40B", "Q40C", "Q40D", "Q40E", "Q40F", "Q40G",
                    "Q40H", "Q40I", "Q40J", "brk.down"))) %>% #Guam: All responses were $0 for "other"
    drop_na() %>%
    rename(boat.ins = Q40A,
           loan.pmt = Q40B,
           fin.services = Q40C, 
           mooring = Q40D,
           vessel.repair = Q40E,
           oil.lube = Q40F,
           gear = Q40G,
           electronics = Q40H,
           fees = Q40I,
           safety = Q40J)
  
  
  #--------------------
  #Calculate data summaries.
  
  #EXCLUDING ZEROS
  q40.excluding.zeros <- annual.expenditures.colnames %>%
    mutate(across(everything(), ~if_else(. == 0, NA, .))) %>%
    group_by(brk.down) %>%
    #BOAT INSURANCE
    mutate(mean.boat.ins = round(mean(boat.ins, na.rm = T), 2),
           std.e.boat.ins = round(std.error(boat.ins, na.rm = T), 1),
           med.boat.ins = round(median(boat.ins, na.rm = T), 2),
           # min.boat.ins = round(min(boat.ins, na.rm = T), 2), 
           # max.boat.ins = round(max(boat.ins, na.rm = T), 2),
           #LOAN PAYMENTS
           mean.loan.pmt = round(mean(loan.pmt, na.rm = T), 2),
           std.e.loan.pmt = round(std.error(loan.pmt, na.rm = T), 1),
           med.loan.pmt = round(median(loan.pmt, na.rm = T), 2),
           # min.loan.pmt = round(min(loan.pmt, na.rm = T), 2),
           # max.loan.pmt = round(max(loan.pmt, na.rm = T), 2),
           #FINANCIAL SERVICES (ACCOUNTING, TAXES)
           mean.fin.serv = round(mean(fin.services, na.rm = T), 2), 
           std.e.fin.serv = round(std.error(fin.services, na.rm = T), 1),
           med.fin.serv = round(median(fin.services, na.rm = T), 2),
           # min.fin.serv = round(min(fin.services, na.rm = T), 2),
           # max.fin.serv = round(max(fin.services, na.rm = T), 2),
           #MOORING FEES
           mean.mooring = round(mean(mooring, na.rm = T), 2), 
           std.e.mooring = round(std.error(mooring, na.rm = T), 1),
           med.mooring = round(median(mooring, na.rm = T), 2),
           # min.mooring = round(min(mooring, na.rm = T), 2),
           # max.mooring = round(max(mooring, na.rm = T), 2),
           #REPAIR, MAINTENANCES, AND IMPROVEMENTS FOR VESSEL, ENGINE, OR TRAILER
           mean.vessel.repair = round(mean(vessel.repair, na.rm = T), 2),  
           std.e.vessel.repair = round(std.error(vessel.repair, na.rm = T), 1),
           med.vessel.repair = round(median(vessel.repair, na.rm = T), 2),
           # min.vessel.repair = round(min(vessel.repair, na.rm = T), 2),
           # max.vessel.repair = round(max(vessel.repair, na.rm = T), 2),
           #OIL & LUBE
           mean.oil.lube = round(mean(oil.lube, na.rm = T), 2),
           std.e.oil.lube = round(std.error(oil.lube, na.rm = T), 1),
           med.oil.lube = round(median(oil.lube, na.rm = T), 2),
           # min.oil.lube = round(min(oil.lube, na.rm = T), 2), 
           # max.oil.lube = round(max(oil.lube, na.rm = T), 2),
           #GEAR (LINES, LURES, GAFFS, RODS, ELECTRIC/HYDRAULIC REELS, SPEARS, WETSUITS, COOLERS, ETC)
           mean.gear = round(mean(gear, na.rm = T), 2),
           std.e.gear = round(std.error(gear, na.rm = T), 1),
           med.gear = round(median(gear, na.rm = T), 2),
           # min.gear = round(min(gear, na.rm = T), 2),
           # max.gear = round(max(gear, na.rm = T), 2),
           #ELECTRONICS
           mean.electronics = round(mean(electronics, na.rm = T), 2),
           std.e.electronics = round(std.error(electronics, na.rm = T), 1),
           med.electronics = round(median(electronics, na.rm = T), 2),
           # min.electronics = round(min(electronics, na.rm = T), 2),
           # max.electronics = round(max(electronics, na.rm = T), 2),
           #FEES (REGISTRATION FOR TRUCK AND TRAILER, DRY DOCK FEES, FISHING CLUB DUES, COOP FEES, ETC)
           mean.fees = round(mean(fees, na.rm = T), 2),
           std.e.fees = round(std.error(fees, na.rm = T), 1),
           med.fees = round(median(fees, na.rm = T), 2),
           # min.fees = round(min(fees, na.rm = T), 2),
           # max.fees = round(max(fees, na.rm = T), 2),
           #SAFETY EQUIPMENT
           mean.safety = round(mean(safety, na.rm = T), 2),
           std.e.safety = round(std.error(safety, na.rm = T), 1),
           med.safety = round(median(safety, na.rm = T), 2)) %>%
    # min.safety = round(min(safety, na.rm = T), 2),
    # max.safety = round(max(safety, na.rm = T), 2)) %>%
    #TOTAL ANNUAL EXPENDITURES
    mutate(mean.tot = sum(unique(mean.boat.ins + mean.loan.pmt + mean.fin.serv + 
                                 mean.mooring + mean.vessel.repair + mean.oil.lube +
                                 mean.gear + mean.electronics + mean.fees +
                                 mean.safety), na.rm = T)) %>%
    mutate(std.e.tot = std.error(c(boat.ins, loan.pmt, fin.services, mooring,
                                   vessel.repair, oil.lube, gear, electronics,
                                   fees, safety), na.rm = T)) %>%
    mutate(med.tot = sum(unique(med.boat.ins + med.loan.pmt + med.fin.serv + 
                                med.mooring + med.vessel.repair + med.oil.lube +
                                med.gear + med.electronics + med.fees +
                                med.safety), na.rm = T)) 
  # mutate(min.tot = sum(unique(min.boat.ins + min.loan.pmt + min.fin.serv + 
  #                             min.mooring + min.vessel.repair + min.oil.lube +
  #                             min.gear + min.electronics + min.fees +
  #                             min.safety))) %>%
  # mutate(max.tot = sum(unique(max.boat.ins + max.loan.pmt + max.fin.serv + 
  #                             max.mooring + max.vessel.repair + max.oil.lube +
  #                             max.gear + max.electronics + max.fees +
  #                             max.safety)))
  
  
  
  #------------------
  #INCLUDING ZEROS
  
  q40.including.zeros <- annual.expenditures.colnames %>%
    group_by(brk.down) %>%
    #BOAT INSURANCE
    mutate(mean.boat.ins = round(mean(boat.ins, na.rm = T), 2),
           std.e.boat.ins = round(std.error(boat.ins, na.rm = T), 1),
           med.boat.ins = round(median(boat.ins, na.rm = T), 2),
           # min.boat.ins = round(min(boat.ins, na.rm = T), 2), 
           # max.boat.ins = round(max(boat.ins, na.rm = T), 2),
           #LOAN PAYMENTS
           mean.loan.pmt = round(mean(loan.pmt, na.rm = T), 2),
           std.e.loan.pmt = round(std.error(loan.pmt, na.rm = T), 1),
           med.loan.pmt = round(median(loan.pmt, na.rm = T), 2),
           # min.loan.pmt = round(min(loan.pmt, na.rm = T), 2),
           # max.loan.pmt = round(max(loan.pmt, na.rm = T), 2),
           #FINANCIAL SERVICES (ACCOUNTING, TAXES)
           mean.fin.serv = round(mean(fin.services, na.rm = T), 2), 
           std.e.fin.serv = round(std.error(fin.services, na.rm = T), 1),
           med.fin.serv = round(median(fin.services, na.rm = T), 2),
           # min.fin.serv = round(min(fin.services, na.rm = T), 2),
           # max.fin.serv = round(max(fin.services, na.rm = T), 2),
           #MOORING FEES
           mean.mooring = round(mean(mooring, na.rm = T), 2), 
           std.e.mooring = round(std.error(mooring, na.rm = T), 1),
           med.mooring = round(median(mooring, na.rm = T), 2),
           # min.mooring = round(min(mooring, na.rm = T), 2),
           # max.mooring = round(max(mooring, na.rm = T), 2),
           #REPAIR, MAINTENANCES, AND IMPROVEMENTS FOR VESSEL, ENGINE, OR TRAILER
           mean.vessel.repair = round(mean(vessel.repair, na.rm = T), 2),  
           std.e.vessel.repair = round(std.error(vessel.repair, na.rm = T), 1),
           med.vessel.repair = round(median(vessel.repair, na.rm = T), 2),
           # min.vessel.repair = round(min(vessel.repair, na.rm = T), 2),
           # max.vessel.repair = round(max(vessel.repair, na.rm = T), 2),
           #OIL & LUBE
           mean.oil.lube = round(mean(oil.lube, na.rm = T), 2),
           std.e.oil.lube = round(std.error(oil.lube, na.rm = T), 1),
           med.oil.lube = round(median(oil.lube, na.rm = T), 2),
           # min.oil.lube = round(min(oil.lube, na.rm = T), 2), 
           # max.oil.lube = round(max(oil.lube, na.rm = T), 2),
           #GEAR (LINES, LURES, GAFFS, RODS, ELECTRIC/HYDRAULIC REELS, SPEARS, WETSUITS, COOLERS, ETC)
           mean.gear = round(mean(gear, na.rm = T), 2),
           std.e.gear = round(std.error(gear, na.rm = T), 1),
           med.gear = round(median(gear, na.rm = T), 2),
           # min.gear = round(min(gear, na.rm = T), 2),
           # max.gear = round(max(gear, na.rm = T), 2),
           #ELECTRONICS
           mean.electronics = round(mean(electronics, na.rm = T), 2),
           std.e.electronics = round(std.error(electronics, na.rm = T), 1),
           med.electronics = round(median(electronics, na.rm = T), 2),
           # min.electronics = round(min(electronics, na.rm = T), 2),
           # max.electronics = round(max(electronics, na.rm = T), 2),
           #FEES (REGISTRATION FOR TRUCK AND TRAILER, DRY DOCK FEES, FISHING CLUB DUES, COOP FEES, ETC)
           mean.fees = round(mean(fees, na.rm = T), 2),
           std.e.fees = round(std.error(fees, na.rm = T), 1),
           med.fees = round(median(fees, na.rm = T), 2),
           # min.fees = round(min(fees, na.rm = T), 2),
           # max.fees = round(max(fees, na.rm = T), 2),
           #SAFETY EQUIPMENT
           mean.safety = round(mean(safety, na.rm = T), 2),
           std.e.safety = round(std.error(safety, na.rm = T), 1),
           med.safety = round(median(safety, na.rm = T), 2)) %>%
    # min.safety = round(min(safety, na.rm = T), 2),
    # max.safety = round(max(safety, na.rm = T), 2)) %>%
    #TOTAL ANNUAL EXPENDITURES
    mutate(mean.tot = sum(unique(mean.boat.ins + mean.loan.pmt + mean.fin.serv + 
                                 mean.mooring + mean.vessel.repair + mean.oil.lube +
                                 mean.gear + mean.electronics + mean.fees +
                                 mean.safety))) %>%
    mutate(std.e.tot = std.error(boat.ins + loan.pmt + fin.services + mooring +
                                 vessel.repair + oil.lube + gear + electronics +
                                 fees + safety)) %>%
    mutate(med.tot = sum(unique(med.boat.ins + med.loan.pmt + med.fin.serv + 
                                med.mooring + med.vessel.repair + med.oil.lube +
                                med.gear + med.electronics + med.fees +
                                med.safety)))
  # mutate(min.tot = sum(unique(min.boat.ins + min.loan.pmt + min.fin.serv + 
  #                             min.mooring + min.vessel.repair + min.oil.lube +
  #                             min.gear + min.electronics + min.fees +
  #                             min.safety))) %>%
  # mutate(max.tot = sum(unique(max.boat.ins + max.loan.pmt + max.fin.serv + 
  #                             max.mooring + max.vessel.repair + max.oil.lube +
  #                             max.gear + max.electronics + max.fees +
  #                             max.safety)))
  
  
  
  #-------------------- 
  #Clean up objects for tables
  q40.excluding.zeros <- q40.excluding.zeros %>% 
    select(-boat.ins, -loan.pmt, -fin.services, -mooring, -vessel.repair,
           -oil.lube, -gear, -electronics, -fees, -safety) %>%
    unique() %>%
    mutate(n = nrow(q40.excluding.zeros), .before = mean.boat.ins) %>%
    t()
  
  q40.including.zeros <- q40.including.zeros %>% 
    select(-boat.ins, -loan.pmt, -fin.services, -mooring, -vessel.repair,
           -oil.lube, -gear, -electronics, -fees, -safety) %>%
    unique() %>%
    mutate(n = nrow(q40.including.zeros), .before = mean.boat.ins) %>%
    t()
  
  
  #-------------------- 
  #Save final tables 
  write.csv(q40.excluding.zeros, na = "0.0", 
            paste("Tables/Q40_", breakdown, sep = "", 
                  "_annualexpenditures_excludingzeros.csv"), 
            row.names = T)
  
  
  write.csv(q40.including.zeros, na = "0.0", 
            paste("Tables/Q40_", breakdown, sep = "", 
                  "_annualexpenditures_includingzeros.csv"), 
            row.names = T)
  
  
  
  #--------------------
  #Create final list of above objects
  breakdown.output.list <- list("q40.excluding.zeros" = q40.excluding.zeros, 
                                "q40.including.zeros" = q40.including.zeros)
  
  
  #Return final list
  return(breakdown.output.list)
  
}

