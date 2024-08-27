#######################
# 2018 Guam small boat fishery cost-earnings survey
# ANNUAL FISHING EXPENDITURES section
# Calculations for tables
#######################

#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Source function
source("Functions/AnnualExpendituresFunction.R")
source("Functions/DataSummariesFunction.R")


#-------------------------------------
# CALCULATIONS BY QUESTION NUMBER
#-------------------------------------

# Q40. In an effort to better understand your economic contribution to the 
      # Mariana’s economy, we would like to ask about your fishing-related 
      # expenditures in 2017.  In the table below please indicate how much, 
      # if any, was spent on the following items during 2017.

#FULL SAMPLE
q40.full.sample <- annual.expenditures.function(guam.data.cleaned, 
                                                breakdown = "Full.sample")

#GFCA
 q40.gfca <- annual.expenditures.function(guam.data.cleaned, 
                                          breakdown = "GFCA.member")

#SELL FISH
q40.sell.fish <- annual.expenditures.function(guam.data.cleaned, 
                                              breakdown = "sell.fish.chr")

#HIGHLINER
q40.highliner <- annual.expenditures.function(guam.data.cleaned, 
                                              breakdown = "highliner")

#PRIMARY TARGET
q40.primary.target <- annual.expenditures.function(guam.data.cleaned, 
                                                   breakdown = "primary.target")

#BOAT OWNER
q40.boat.owner <- annual.expenditures.function(guam.data.cleaned, 
                                               breakdown = "boat.owner")


#--------------
#[n] for tables
#--------------
q40.n.fortables <- guam.data.cleaned %>%
  select(any_of(c("Q40A", "Q40B", "Q40C", "Q40D", "Q40E", "Q40F", "Q40G",
                  "Q40H", "Q40I", "Q40J", "Full.sample"))) %>% #or "highliner" or "sell.fish.chr"
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


#---------------------------------
#Percent of fleet with expenditure
#---------------------------------
q40.percent.fleet.expenditure <- q40.n.fortables %>%
  mutate_at(vars(everything()), ~round(sum(. > 0) / 110 * 100, 1)) %>%
  unique() %>%
  t()


#--------------------------------------------------------------------------

# Q41. Some fisher(men) purchase fishing gear, electronics, safety equipment or 
# other items off-island, online, or through a catalog. Approximately what 
# percentage of your expenditures were purchased off-island?

#DATA SUMMARIES
q41.data.sum <- data.summaries.function(guam.data.cleaned, q.number = "Q41")

