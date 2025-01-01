
#------------------------------------------------------------------------------
#PURPOSE: This script runs misc. calculations with the 2018 Guam and CNMI 
#small boat fisheries cost-earnings survey. 
#AUTHOR: Crystal Dombrow
#DATE: February 2024

#INSTRUCTIONS: Run GuamCNMI_SBF_DataCleaning.R first.
#------------------------------------------------------------------------------


# For a question in the report
boat.owners.who.are.crew <- guam.cnmi.data.cleaned %>% 
  select(Survey, Q13B.mid, boat.owner, Q28.mid) %>% 
  filter(boat.owner == "boat owner", Q13B.mid > 0)



# For fisher observation data, for Adam 2/22/24
fisher.observations.data <- guam.data.cleaned %>% 
  filter(Survey %in% c(1114, 3004, "1190A")) %>%
  select(Survey, Q2A:Q2H, Q4A:Q4J, Q9, Q11A:Q11I, Q16A:Q16G, Q17A:Q17H, Q23, 
         Q42, Q43, Q46, Q47A:Q47G, Q48A:Q48B, Q49A.1:Q49A.4, Q49B:Q49D, 
         Q50A:Q50B, Q52, Q53) %>%
  write_csv("Data/FisherObservations_Guam2018.csv")



#CHARACTERISTICS ABOUT BOAT OWNERS -- if we need this later on
own.boat.characteristics <- as.sbs.data.cleaned %>%
  filter(Q22.chr == "yes") %>%
  select(Q22:Q30, Island, Q35.combo, Q14.combo, Q31C, Q32C, Q40, Q1.combo,
         Q38.combo, Q39)



#HIGHLINERS: Determine top 10-15% of catch by species group

#pelagics (n = 102), top 15%: 1,000 lbs (n = 20), top 10%: 2,000 lbs (n = 11)
pelagics.highliner <- guam.data.cleaned %>%
  select(Survey, Q8A.mid.ifelse) %>%
  slice_max(order_by = Q8A.mid.ifelse, prop = 0.09)

#bottomfish (n = 90), top 10% & 15%: 375.5 lbs (n = 17), top 9%: 500 lbs (n = 9)
bottomfish.highliner <- guam.data.cleaned %>%
  select(Survey, Q8B.mid.ifelse) %>%
  slice_max(order_by = Q8B.mid.ifelse, prop = 0.09)

#reef fish (n = 92), top 10% & 15%: 175.5 lbs (n = 25), top 9%: 375.5 lbs (n = 9)
reef.highliner <- guam.data.cleaned %>%
  select(Survey, Q8C.mid.ifelse) %>%
  slice_max(order_by = Q8C.mid.ifelse, prop = 0.09)

highliners.total <- guam.data.cleaned %>%
  select(Q8A.mid.ifelse:Q8C.mid.ifelse)



#-----------------------------------------------------------------------------
#Q40. Annual expenditures -- manual view into function objects

#Set up dataframe for calculations below
annual.expenditures.colnames <- guam.data.cleaned %>%
  select(Q40A:Q40J, sell.fish.chr) %>% #Guam: All responses were $0 for "other"
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
  group_by(sell.fish.chr) %>%
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
  mutate(std.e.tot = std.error(std.e.boat.ins + std.e.loan.pmt + 
                                 std.e.fin.serv + std.e.mooring + 
                                 std.e.vessel.repair + std.e.oil.lube +
                                 std.e.gear + std.e.electronics + std.e.fees +
                                 std.e.safety)) %>%
  mutate(med.tot = sum(unique(med.boat.ins + med.loan.pmt + med.fin.serv + 
                                med.mooring + med.vessel.repair + med.oil.lube +
                                med.gear + med.electronics + med.fees +
                                med.safety)))
# mutate(min.tot = sum(unique(min.boat.ins + min.loan.pmt + min.fin.serv + 
#                               min.mooring + min.vessel.repair + min.oil.lube +
#                               min.gear + min.electronics + min.fees +
#                               min.safety))) %>%
# mutate(max.tot = sum(unique(max.boat.ins + max.loan.pmt + max.fin.serv + 
#                               max.mooring + max.vessel.repair + max.oil.lube +
#                               max.gear + max.electronics + max.fees +
#                               max.safety)))



#------------------
#INCLUDING ZEROS

q40.including.zeros <- annual.expenditures.colnames %>%
  group_by(sell.fish.chr) %>%
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
  mutate(std.e.tot = std.error(std.e.boat.ins + std.e.loan.pmt + 
                                 std.e.fin.serv + std.e.mooring + 
                                 std.e.vessel.repair + std.e.oil.lube +
                                 std.e.gear + std.e.electronics + std.e.fees +
                                 std.e.safety)) %>%
  mutate(med.tot = sum(unique(med.boat.ins + med.loan.pmt + med.fin.serv + 
                                med.mooring + med.vessel.repair + med.oil.lube +
                                med.gear + med.electronics + med.fees +
                                med.safety)))
# mutate(min.tot = sum(unique(min.boat.ins + min.loan.pmt + min.fin.serv + 
#                               min.mooring + min.vessel.repair + min.oil.lube +
#                               min.gear + min.electronics + min.fees +
#                               min.safety))) %>%
# mutate(max.tot = sum(unique(max.boat.ins + max.loan.pmt + max.fin.serv + 
#                               max.mooring + max.vessel.repair + max.oil.lube +
#                               max.gear + max.electronics + max.fees +
#                               max.safety)))


#-----------------------------------------------------------
#Looking at Q8 to see if any NA's can be recoded to 0 (10/2/2024)

pelagic <- guam.data.cleaned %>% 
  select(Survey, Q8A.mid.ifelse:Q8C.mid.ifelse, Q2A.mid, Q18A, 
         Q21A.mid, Q56A.spp:Q57C.spp)

bottomfish <- guam.data.cleaned %>% 
  select(Survey, Q8A.mid.ifelse:Q8C.mid.ifelse, Q2B.mid:Q2C.mid, Q18B, 
         Q21B.mid, Q56A.spp:Q57C.spp)

nearshore <- guam.data.cleaned %>% 
  select(Survey, Q8C.mid.ifelse, Q2D.mid:Q2F.mid, Q18C, Q21C.mid, 
         Q56A.spp:Q57C.spp)


