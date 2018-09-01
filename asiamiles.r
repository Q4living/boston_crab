library(pdftools)
library(stringr)
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



