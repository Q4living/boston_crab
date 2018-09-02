# Boston_crab prediction program
This program will automatically intake a csv and scored the result + statistic + execution log

## Prerequisite
1. Install R on your local machine with the instruction below,
https://www.r-project.org/
2. Then install the packages from the install.r using cmd
R CMD BATCH --no-save --no-restore install.r
3. Make sure the model artifacts asiamiles.RData are download and saved next to amp1.R

## Run this program
Once all the installations are done, you can run below command in the bash console
R CMD BATCH --no-save --no-restore '--args /Users/XXX/crab_boston.csv /Users/XXXX/scored2.csv /Users/XXX/sLog.csv' /Users/XXXX/amp1.R

This command requires 4 inputs,
3 args input
  1. input csv
  2. out csv
  3. statstic log

1 script file input

Everytime this script is executed an execution log of amp1.Rout will be saved and it can be used for debugging purpose, thanks!
