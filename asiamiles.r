library(pdftools)
library(stringr)
library(ggvis)
# install.packages("xgboost")
library(xgboost)


text <- pdf_text("~/Downloads/data.pdf")
text2 <- strsplit(text, "\n")
head(text2)

# head: Sex, Length, Diameter, Height, Weight, Shucked Weight, Viscera Weight, Shell Weight

# header: Sex, Length, Diameter, Height, Weight, Shucked Weight, Viscera Weight, Shell Weight, Age

header <- c("sex", "length", "diameter", "height", "weight", "shucked_weight", "viscera_weight", "shell_weight", "age")

# Private function for bind 2 dt objects into one
funcDTBind <- function(name, dt){
  if(exists(name)){
    eval(parse(text = paste0(name, "<<- ", "rbindlist(list(",name,", dt))")))
  } else {
    assign(name,dt,envir = .GlobalEnv)
  }
}

# rm(dt.crab)
# rm(dt.crab.tmp)
# rm(dt.crab.age)
# rm(dt.crab.age.tmp)
# rm(dt.line)
# rm(dt.str)
# rm(dt.crab.exceptional)
# rm(dt.crab.full)
# rm(dt.crab.full.clean)
# rm(dt.crab.full.dcast)

lapply(text2,function(sheet){
  # Param setting for house keeping
  if(!exists("pages")){
    pages <<- 0
  }
  pages <<- pages + 1
  sen <<- 0
  
  # Loop through individual line in each sheet
  lapply(sheet, function(l){
    sen <<- sen + 1
    
    print(paste0("Processing Sheet:", pages, " on Line: ", sen))
    
    # Rule 1: Check the header is sheetXX
    # Rule 2: Check the Foot is page xx
    # Rule 3: Make sure there is something to parse
    
    dt.line <<- gsub("\\s+", " ", str_trim(trimws(l)))
    
    if( grepl("sheet[0-9]",dt.line, ignore.case = T) || grepl("Page.[0-9]",dt.line, ignore.case = T) || length(dt.line) == 0 || l == "Sex Length Diameter Height Weight          Shucked Weight Viscera Weight  Shell Weight"){
      
      funcDTBind("dt.crab.exceptional",data.table(pages, sen, dt.line))
      
    } else if(!grepl("[a-z]",dt.line[1], ignore.case = T)) { 
      # ages operation
      dt.str <<- strsplit(str_trim(dt.line)," ")
      
      # Exceptional Handling 
      if(length(dt.str) != 1){
        funcDTBind("dt.crab.exceptional",data.table(pages, sen, dt.line))
      } else {
        funcDTBind("dt.crab.age",data.table(t(unlist(dt.str))))
      }
      
    } else {
      # print(dt.line)
      dt.str <<- strsplit(str_trim(dt.line)," ")
      
      # print(dt.str)
      
      # Exceptional Handling 
      if(length(dt.str[[1]]) != 8){
        funcDTBind("dt.crab.exceptional",data.table(pages, sen, dt.line))
      } else {
        funcDTBind("dt.crab",data.table(t(unlist(dt.str))))
      }
      
    }
    
    
  })
  
  # Clean up global variables
  if(length(text2) == pages){
    rm(pages, envir = .GlobalEnv)
    rm(sen, envir = .GlobalEnv)
  }
})

dt.crab.full <- data.table(dt.crab,dt.crab.age)

names(dt.crab.full) <- c("sex", "length", "diameter", "height", "weight", "shucked_weight", "viscera_weight", "shell_weight", "age")

str(dt.crab.full)

# Clearning the data
# 1. Convert everything into numeric
lapply(c("length", "diameter", "height", "weight", "shucked_weight", "viscera_weight", "shell_weight", "age"), function(n){
  eval(parse(text = paste0("dt.crab.full$",n, " <<- ", "as.numeric(dt.crab.full$",n,")")))
})


# dt.crab.full$sex <- as.factor(dt.crab.full$sex)

# 3. Check Summary
summary(dt.crab.full)

# 2. Remove NA, because just 1 na, dont bother to clean
# 4. Generate a clean state set for ML training
dt.crab.full.clean <- dt.crab.full[!is.na(weight)]

# 5. reshape the table for KNN
dt.crab.full.dcast <- dcast(dt.crab.full.clean, formula = sex + length + diameter + height + weight + shucked_weight + viscera_weight + shell_weight + age ~ sex, value.var = "sex")
dt.crab.full.dcast[sex=="F"]$`F` <- 1
dt.crab.full.dcast[sex!="F"]$`F` <- 0
dt.crab.full.dcast[sex=="M"]$M <- 1
dt.crab.full.dcast[sex!="M"]$M <- 0
dt.crab.full.dcast[sex=="I"]$I <- 1
dt.crab.full.dcast[sex!="I"]$I <- 0

dt.crab.full.dcast$`F` <- as.numeric(dt.crab.full.dcast$`F`)
dt.crab.full.dcast$M <- as.numeric(dt.crab.full.dcast$M)
dt.crab.full.dcast$I <- as.numeric(dt.crab.full.dcast$I)
dt.crab.full.dcast$sex <- as.factor(dt.crab.full.dcast$sex)

summary(dt.crab.full.dcast)

# Normalise the data for KNN
dt.crab.full.dcast$nLength <- (dt.crab.full.dcast$length - min(dt.crab.full.dcast$length)) / (max(dt.crab.full.dcast$length) - min(dt.crab.full.dcast$length))
dt.crab.full.dcast$nDiameter <- (dt.crab.full.dcast$diameter - min(dt.crab.full.dcast$diameter)) / (max(dt.crab.full.dcast$diameter) - min(dt.crab.full.dcast$diameter))
dt.crab.full.dcast$nHeight <- (dt.crab.full.dcast$height - min(dt.crab.full.dcast$height)) / (max(dt.crab.full.dcast$height) - min(dt.crab.full.dcast$height))
dt.crab.full.dcast$nWeight <- (dt.crab.full.dcast$weight - min(dt.crab.full.dcast$weight)) / (max(dt.crab.full.dcast$weight) - min(dt.crab.full.dcast$weight))
dt.crab.full.dcast$nShucked_weight <- (dt.crab.full.dcast$shucked_weight - min(dt.crab.full.dcast$shucked_weight)) / (max(dt.crab.full.dcast$shucked_weight) - min(dt.crab.full.dcast$shucked_weight))
dt.crab.full.dcast$nViscera_weight <- (dt.crab.full.dcast$viscera_weight - min(dt.crab.full.dcast$viscera_weight)) / (max(dt.crab.full.dcast$viscera_weight) - min(dt.crab.full.dcast$viscera_weight))
dt.crab.full.dcast$nShell_weight <- (dt.crab.full.dcast$shell_weight - min(dt.crab.full.dcast$shell_weight)) / (max(dt.crab.full.dcast$shell_weight) - min(dt.crab.full.dcast$shell_weight))
dt.crab.full.dcast$nAge <- (dt.crab.full.dcast$age - min(dt.crab.full.dcast$age)) / (max(dt.crab.full.dcast$age) - min(dt.crab.full.dcast$age))

dt.crab.full.dcast[,`:=`(mass = (height * diameter) / weight)]
dt.crab.full.dcast[,`:=`(nMass = (mass - min(mass)) / (max(mass) - min(mass)))]


dt.crab.kmean <- kmeans(dt.crab.full.dcast[,10:21],10)
dt.crab.full.dcast$cluster <- dt.crab.kmean$cluster
dt.crab.full.dcast$cluster <- as.numeric(dt.crab.full.dcast$cluster)

dt.crab.full.dcast[height < 1] %>% ggvis(x = ~age, y = ~height, fill = ~factor(cluster))
dt.crab.full.dcast[cluster == 4,cluster:=2]
dt.crab.full.dcast[cluster == 10,cluster:=6]
dt.crab.full.dcast[cluster == 7,cluster:=1]
dt.crab.full.dcast[cluster == 9,cluster:=1]
dt.crab.full.dcast[cluster == 5,cluster:=1]
dt.crab.full.dcast[cluster == 3,cluster:=1]
dt.crab.full.dcast[cluster == 8,cluster:=1]


dt.crab.full.dcast[height < 1] %>% ggvis(x = ~age, y = ~height, fill = ~factor(cluster)) 
dt.crab.full.dcast[height < 1] %>% ggvis(x = ~age, y = ~mass, fill = ~factor(cluster))
dt.crab.full.dcast[height < 1] %>% ggvis(x = ~age, y = ~diameter, fill = ~factor(cluster))

dt.crab.full.dcast[cluster > 1 ]
hist(dt.crab.full.dcast[cluster == 1]$age)
hist(dt.crab.full.dcast[cluster == 2]$age)
hist(dt.crab.full.dcast[cluster == 6]$age)

dt.crab.full.dcast[,.(uAge=mean(age),sdAge=sd(age),mAge=median(age)),by=.(cluster)]

plot(dt.crab.full.dcast[,.(weight,height,diameter, mass, age)], col = dt.crab.full.dcast$cluster)
legend("topleft", legend=(dt.crab.full.dcast$cluster), pch=16, col=unique(dt.crab.full.dcast$cluster))

# Full length
dt.crab.full.dcast[,.N]

# Prepare the data for ML training
# 40: test / 30: valid / 30: final
set.seed(1)
dt.test.sample <- sample(1:dt.crab.full.dcast[,.N],round(dt.crab.full.dcast[,.N] * .7))
dt.crab.full.dcast$set <- "final"
dt.crab.full.dcast[dt.test.sample]$set <- "train"
dt.final <- dt.crab.full.dcast[set == "final"]
dt.test <- dt.crab.full.dcast[set == "train"]
set.seed(1)
dt.valid <- sample(1:dt.test[,.N],round(dt.test[,.N] * .6))
dt.test[dt.valid]$set <- "valid" 


fit <- glm(age~nLength+nDiameter+nHeight+nWeight+nShucked_weight+nViscera_weight+nShell_weight+`F`+M+I,data=dt.test[set == "train"])
nFit <- glm(age~length+diameter+height+weight+shucked_weight+viscera_weight+shell_weight+`F`+M+I,data=dt.test[set == "train"])

dt.predict <- predict(fit,dt.test[set == "valid"])
dt.predict.norm <- predict(fit,dt.test[set == "valid"])

ggvis(dt.test[set == "valid"], x=~age, y=~dt.predict, fill=~factor(cluster))
ggvis(dt.test[set == "valid"], x=~age, y=~dt.predict.norm, fill=~factor(cluster))

dt.xgb <- xgboost(data = data.matrix(dt.test[set == "train",c(2:8,10:12)]),label = dt.test[set == "train"]$age, nrounds = 10)
dt.xgb.norm <- xgboost(data = data.matrix(dt.test[set == "train",10:21]),label = dt.test[set == "train"]$age, nrounds = 1000)

dt.xgb.predict <- predict(dt.xgb, data.matrix(dt.test[set == "valid",c(2:8,10:12)]))
dt.xgb.norm.predict <- predict(dt.xgb.norm, data.matrix(dt.test[set == "valid",10:21]))

ggvis(dt.test[set == "valid"], x=~age, y=~dt.xgb.predict, fill=~factor(cluster))
ggvis(dt.test[set == "valid"], x=~age, y=~dt.xgb.norm.predict, fill=~factor(cluster))

dt.test[set=="valid"][9]$age
dt.xgb.predict[9]
dt.xgb.norm.predict[9]

mean(1- abs(dt.test[set == "valid"]$age - dt.xgb.predict)/dt.test[set == "valid"]$age)
sum(dt.test[set == "valid"]$age == round(dt.xgb.predict))

dt.xgb.predict.norm.final <- predict(dt.xgb.norm, data.matrix(dt.crab.full.dcast[set=="final",c(10:21)]))
mean(1- abs(dt.crab.full.dcast[set=="final"]$age - dt.xgb.predict.norm.final)/dt.crab.full.dcast[set=="final"]$age)
sum(dt.crab.full.dcast[set=="final"]$age == round(dt.xgb.predict.norm.final))
ggvis(dt.crab.full.dcast[set=="final"], x=~age, y=~dt.xgb.predict.norm.final, fill=~factor(cluster))


dt.xgb.predict.final <- predict(dt.xgb, data.matrix(dt.crab.full.dcast[set=="final",c(2:8,10:12)]))
mean(1- abs(dt.crab.full.dcast[set=="final"]$age - dt.xgb.predict.final)/dt.crab.full.dcast[set=="final"]$age)
sum(dt.crab.full.dcast[set=="final"]$age == round(dt.xgb.predict.final))
ggvis(dt.crab.full.dcast[set=="final"], x=~age, y=~dt.xgb.predict.final, fill=~factor(cluster))

lapply(names(dt.crab.full.dcast)[2:9],function(n){
  print(paste(n,"max <<- ", max(getElement(dt.crab.full.dcast,n))))
  eval(parse(text = paste(n,"Max <<- ", max(getElement(dt.crab.full.dcast,n)),sep = "")))
  eval(parse(text = paste(n,"Min <<- ", min(getElement(dt.crab.full.dcast,n)),sep = "")))
})
