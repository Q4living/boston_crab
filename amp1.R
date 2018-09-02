# Assume installed the R packages beforehand
library(data.table)
library(pdftools)
library(stringr)
library(ggvis)
library(xgboost)

# Check if the correct command has been provided
args = commandArgs(TRUE)
lapply(args,print)


if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else {
  # for(i in 1:length(args)){
  #   eval(parse(text = paste0("\"",args[[i]]),"\""),envir = .GlobalEnv)
  # }
}

lapply(args,print)
# args[1] : read file path
# args[2] : output file path
# args[3] : log file path

# load the model
load("asiamiles.RData")

# load the file
dt.score <- fread(args[1])
# dt.score <- fread("~/crab_boston.csv")

# Data preparation
dt.score$`F` <- 0
dt.score[sex == `F`]$`F` <- 1

dt.score$I <- 0
dt.score[sex == I]$I <- 1

dt.score$M <- 0
dt.score[sex == M]$M <- 1

dt.score$nLength <- (dt.score$length - lengthMin) / (lengthMax - lengthMin)
dt.score$nDiameter <- (dt.score$diameter - diameterMin) / (diameterMax - diameterMin)
dt.score$nHeight <- (dt.score$height - heightMin) / (heightMax - heightMin)
dt.score$nWeight <- (dt.score$weight - weightMin) / (weightMax - weightMin)
dt.score$nShucked_weight <- (dt.score$shucked_weight - shucked_weightMin) / (shucked_weightMax - shucked_weightMin)
dt.score$nViscera_weight <- (dt.score$viscera_weight - viscera_weightMin) / (viscera_weightMax - viscera_weightMin)
dt.score$nShell_weight <- (dt.score$shell_weight - shell_weightMin) / (shell_weightMax - shell_weightMin)
dt.score$nAge <- (dt.score$age - ageMin) / (ageMax - ageMin)
dt.score[,`:=`(mass = (height * diameter) / weight)]
dt.score[,`:=`(nMass = (mass - massMin) / (massMax - massMin))]

dt.score$predicted_age <- round(predict(dt.xgb.norm, data.matrix(dt.score[,c(10:21)])))

fwrite(dt.score[,c(1:9,23)],args[2])
# fwrite(dt.score[,c(1:9,23)],"scored.csv")

fwrite(dt.score[,.(accuracy = mean(1-abs(age-predicted_age)/age), exact_match = sum(age==predicted_age), total=.N)],args[3])
# fwrite(dt.score[,.(accuracy = mean(1-abs(age-predicted_age)/age), exact_match = sum(age==predicted_age), total=.N)],"statistic.csv")
