##############
# Title: 2018 Guam small boat fishery cost-earnings survey
# Author: Crystal Dombrow
# Date: February-April 2024
##############

#----------------------------
# DESCRIPTION
#----------------------------
  # The purpose of this program is to clean the data, analyze the data, & 
  # generate the results for the 2018 Guam small boat survey report.
  # This readme script runs the entire program (code below).

# Note: Author manually checked each code chunk to ensure calculations are 
  # functioning as intended. 


#----------------------------
# R PROJECT FOLDER ORGANIZATION
#----------------------------
   #1. Main folder
     # Contains the analyses scripts, the R project file, and the folders 
       # supporting the program. 
     # The scripts ______ run the analyses, produce output tables for most
        # questions, and generate lists or R objects for all calculations.
        # In each script, the outputs are identified by survey question number.

   #2. "Data" Folder 
     # Contains data cleaning script, data cleaning documentation, & all data sets:
       # Marianas2018_SurveyData_checkedhardcopies.xlsx: raw survey data edited 
         # to match survey hard copies.
       # Marianas2018_SurveyData_checkedskippattern_OUTLIERSNOTES.xlsx: Notes on
         # outliers per calculations and the process of determining true outliers.
       # Marianas2018_SurveyData_checkedskippattern.csv: manually cleaned raw survey 
         # data as input for this R program. Data cleaning steps are described in: 
         # GuamCNMI_DataCleaningDocumentation.docx & 
         # Documentation for 2017 GuamCNMI Calculation.docx
       # GuamCNMI_SBF_20182019_DataCleaned.csv: cleaned survey data, output from  
         # this R program.
   
   #3. "Functions" folder
     # Contains scripts that run the functions for the data analysis scripts,
       # labeled by the description of their task.

   #4. "Tables" folder
     # Contains .csv files of output tables generated for most survey questions.
       # The type of calculation is indicated next to the survey question number.
    


#----------------------------
# INSTRUCTIONS
#----------------------------
# To run the program, run the following scripts in this order:

#Clear workspace
rm(list = ls())

#Run data cleaning script
source("Data/GuamCNMI_SBF_DataCleaning.R")


#Run analyses, by section 
source("01_Demographics.R")
source("02_VesselCharacteristics.R")
source("03_FishingActivity.R")
source("04_MarketParticipationAndAccess.R")
source("05_TripCosts.R")
source("06_AnnualFishingExpenditures.R")
source("07_LevelsOfInvestment.R")
source("08_CrewConsiderations.R")
source("09_SocialAspectsOfFishing.R")
source("10_FisherPerceptions.R")

