#----------------
# FUNCTION FOR DATA SUMMARY CALCULATIONS
# 2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------


#Load libraries
library(dplyr)
library(tidyverse)
library(plotrix)


#Write function
data.summaries.function <- function(guam.data.cleaned, q.number){
  
  #Rename argument for group_by to work below
  guam.cleaned.data.sum.fun <- guam.data.cleaned %>% 
    rename(q.num = q.number)
  
  
  #-------------------------------------
  #-------------------------------------
  #FULL SAMPLE
  
  #Calculate data summaries
  q.data.sum.full.sample <- guam.cleaned.data.sum.fun %>%
    select(any_of(c("q.num", "Full.sample"))) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Calculate number of observations for table
  q.full.sample.rows <- q.data.sum.full.sample %>%
    drop_na() %>%
    nrow()
  
  #Create data frame to later rbind into a table
  q.full.sample.sum <- q.data.sum.full.sample %>%
    mutate(n = q.full.sample.rows, .before = mean) %>%
    mutate(breakdown = Full.sample, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.full.sample.sum <- q.full.sample.sum[-c(1:2)] %>%
    unique()
  
  
  #-------------------- 
  #GFCA MEMBERSHIP
  
  #Calculate data summaries
  q.data.sum.gfca <- guam.cleaned.data.sum.fun %>%
    select(any_of(c("q.num", "GFCA.member"))) %>%
    drop_na() %>%
    group_by(GFCA.member) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Table
    #Set up rows for n
  q.gfca.rows <- q.data.sum.gfca %>%
    filter(GFCA.member == "gfca") %>%
    drop_na() %>%
    nrow() 
  
  q.non.gfca.rows <- q.data.sum.gfca %>%
    filter(GFCA.member == "non-gfca") %>%
    drop_na() %>%
    nrow()
  
    #Create data frame to later rbind into a table
  q.gfca.sum <- q.data.sum.gfca %>%
    mutate(n = (ifelse(GFCA.member == "gfca", q.gfca.rows, q.non.gfca.rows))) %>%
    mutate(breakdown = GFCA.member, .before = n)
  
    #Remove unneeded rows and select unique breakdown values
  q.gfca.sum <- q.gfca.sum [-c(1:2)] %>%
    unique()
  

  #-------------------- 
  #SELL FISH
  
  #Calculate data summaries
  q.data.sum.sell.fish <- guam.cleaned.data.sum.fun %>%
    select(any_of(c("q.num", "sell.fish.chr"))) %>%
    drop_na() %>%
    group_by(sell.fish.chr) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Table
   #Set up rows for n
  q.sell.fish.rows <- q.data.sum.sell.fish %>%
    filter(sell.fish.chr == "sold fish") %>%
    drop_na() %>%
    nrow() 
  
  q.did.not.sell.fish.rows <- q.data.sum.sell.fish %>%
    filter(sell.fish.chr == "did not sell fish") %>%
    drop_na() %>%
    nrow()
  
   #Create data frame to later rbind into a table
  q.sell.fish.sum <- q.data.sum.sell.fish %>%
    mutate(n = (ifelse(sell.fish.chr == "sold fish", q.sell.fish.rows, 
                       q.did.not.sell.fish.rows))) %>%
    mutate(breakdown = sell.fish.chr, .before = n)
  
   #Remove unneeded rows and select unique breakdown values
  q.sell.fish.sum <- q.sell.fish.sum [-c(1:2)] %>%
    unique()
  

  #-------------------- 
  #HIGHLINER
  
  #Calculate data summaries
  q.data.sum.highliner <- guam.cleaned.data.sum.fun %>%
    select(any_of(c("q.num", "highliner"))) %>%
    drop_na() %>%
    group_by(highliner) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Table
  #Set up rows for n
  q.highliner.rows <- q.data.sum.highliner %>%
    filter(highliner == "highliner") %>%
    drop_na() %>%
    nrow() 
  
  q.not.highliner.rows <- q.data.sum.highliner %>%
    filter(highliner == "not highliner") %>%
    drop_na() %>%
    nrow()
  
  #Create data frame to later rbind into a table
  q.highliner.sum <- q.data.sum.highliner %>%
    mutate(n = (ifelse(highliner == "highliner", q.highliner.rows, 
                       q.not.highliner.rows))) %>%
    mutate(breakdown = highliner, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.highliner.sum <- q.highliner.sum [-c(1:2)] %>%
    unique()
  
  
  #-------------------- 
  #PRIMARY TARGET, PART I
  
  #Calculate data summaries
  q.data.sum.pri.target <- guam.cleaned.data.sum.fun %>%
    select(any_of(c("q.num", "primary.target"))) %>%
    filter(primary.target == "pelagic" | primary.target == "bottomfish") %>%
    drop_na() %>%
    group_by(primary.target) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Table
  #Set up rows for n
  q.pelagic.rows <- q.data.sum.pri.target %>%
    filter(primary.target == "pelagic") %>%
    drop_na() %>%
    nrow() 
  
  q.bottomfish.rows <- q.data.sum.pri.target %>%
    filter(primary.target == "bottomfish") %>%
    drop_na() %>%
    nrow()
  
  
  #Create data frame to later rbind into a table
  q.pri.target.sum <- q.data.sum.pri.target %>%
    mutate(n = (ifelse(primary.target == "pelagic", q.pelagic.rows, 
                       q.bottomfish.rows))) %>%
    mutate(breakdown = primary.target, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.pri.target.sum <- q.pri.target.sum [-c(1:2)] %>%
    unique()
  
  
  
  #-------------------- 
  #PRIMARY TARGET, PART II, because rbind needed each code chunk to have max 2
    #variables.
  
  #Calculate data summaries
  q.data.sum.pri.target.2 <- guam.cleaned.data.sum.fun %>%
    select(any_of(c("q.num", "primary.target"))) %>%
    filter(primary.target == "reef" | primary.target == "no primary") %>%
    drop_na() %>%
    group_by(primary.target) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Table
  #Set up rows for n
  q.reef.rows <- q.data.sum.pri.target.2 %>%
    filter(primary.target == "reef") %>%
    drop_na() %>%
    nrow() 
  
  q.no.primary.rows <- q.data.sum.pri.target.2 %>%
    filter(primary.target == "no primary") %>%
    drop_na() %>%
    nrow()
  
  #Create data frame to later rbind into a table
  q.pri.target.sum.2 <- q.data.sum.pri.target.2 %>%
    mutate(n = ifelse(primary.target == "reef", q.reef.rows, 
                      q.no.primary.rows)) %>%
    mutate(breakdown = primary.target, .before = n)
  
  #Remove unneeded rows and select unique breakdown values
  q.pri.target.sum.2 <- q.pri.target.sum.2 [-c(1:2)] %>%
    unique()
  
  
  
  #-------------------- 
  #BOAT OWNERSHIP
  
  #Calculate data summaries
  q.data.sum.boat.owner <- guam.cleaned.data.sum.fun %>%
    select(any_of(c("q.num", "boat.owner"))) %>%
    drop_na() %>%
    group_by(boat.owner) %>%
    mutate(mean = round(mean(q.num, na.rm = T), 1),
           std.err = round(std.error(q.num, na.rm = T), 1),
           med = round(median(q.num, na.rm = T), 1),
           min = round(min(q.num, na.rm = T), 1),
           max = round(max(q.num, na.rm = T), 1))
  
  
  #Table
   #Set up rows for n
  q.boat.owner.rows <- q.data.sum.boat.owner %>%
    filter(boat.owner == "boat owner") %>%
    drop_na() %>%
    nrow() 
  
  q.not.boat.owner.rows <- q.data.sum.boat.owner %>%
    filter(boat.owner == "not boat owner") %>%
    drop_na() %>%
    nrow()
  
   #Create data frame to later rbind into a table
  q.boat.owner.sum <- q.data.sum.boat.owner %>%
    mutate(n = (ifelse(boat.owner == "boat owner", q.boat.owner.rows, 
                       q.not.boat.owner.rows))) %>%
    mutate(breakdown = boat.owner, .before = n)
  
   #Remove unneeded rows and select unique breakdown values
  q.boat.owner.sum <- q.boat.owner.sum [-c(1:2)] %>%
    unique()
  
  
  #-------------------------------------
  #-------------------------------------
  #CREATE OUTPUT TABLE
  
  #Rbind together & transpose, for glory
  q.data.sum <- rbind(q.full.sample.sum, 
                      q.gfca.sum,
                      q.sell.fish.sum,
                      q.highliner.sum,
                      q.pri.target.sum,
                      q.pri.target.sum.2,
                      q.boat.owner.sum) %>%
    t()


  #Set the column order to the output dataframe
  column.order <- c("V1", "V4", "V3", "V5", "V8", "V7", "V6", "V9", "V10", "V11",
                    "V12", "V14", "V13")
                    #Automatically creates a duplicate column (V2) for "full sample",
                    #so this line of code removes it


  #Convert to a data frame
  q.data.sum <- as.data.frame(q.data.sum)



  #Apply the column order to the output dataframe
  q.data.sum <- q.data.sum %>%
    select(any_of(column.order))

  
  #Save final table 
  write.csv(q.data.sum, na = "0.0", paste("Tables/", q.number, sep = "", 
                                          "_datasummaries.csv"))
  

  #-------------------------------------
  #-------------------------------------
  #Return final list of above objects
  q.data.sum.list <- list("q.data.sum.full.sample" = q.data.sum.full.sample,
                          "q.data.sum.gfca" = q.data.sum.gfca,
                          "q.data.sum.sell.fish" = q.data.sum.sell.fish,
                          "q.data.sum.highliner" = q.data.sum.highliner,
                          "q.data.sum.pri.target" = q.data.sum.pri.target,
                          "q.data.sum.pri.target.2" = q.data.sum.pri.target.2,
                          "q.data.sum.boat.owner" = q.data.sum.boat.owner)
  
  
  return(q.data.sum.list)
  
}

