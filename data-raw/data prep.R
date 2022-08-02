
library(reshape2)



#### load data set fresh water #####

mydata<-read.csv(file="data-raw/Microplasbial MP variants FW.csv", header=TRUE , dec="." , sep=";")
str(mydata)

# reshape data frame into more usable way
# wavenumber (WN) is the same for alle measurements (X-axis)
mydata <- melt(mydata, id.vars = "WN")

# rename columns and define polymer type etc.
colnames(mydata) <- c("wavenumber", "polV", "amp")

#levels(mydata$polV)
mydata$pol <- "x"
mydata$v <- "x"
for(i in 1:nrow(mydata)){
  temp <- strsplit(as.character(mydata$polV[i]), "\\.")
  mydata$pol[i] <- temp[[1]][1]
  mydata$v[i] <- temp[[1]][2]
}

str(mydata)
mydata$pol <- as.factor(mydata$pol)
mydata$v <- as.integer(as.numeric(as.factor(mydata$v)))

mydata$incWater <- "FW"
mydata$incWater <- as.factor(mydata$incWater)



##### load data set salt water ######

mydata.SW<-read.csv(file="data-raw/Microplasbial MP variants SW.csv", header=TRUE , dec="." , sep=";")
str(mydata.SW)

# reshape data frame into more usable way
# wavenumber (WN) is the same for alle measurements (X-axis)
mydata.SW <- melt(mydata.SW, id.vars = "WN")

# rename columns and define polymer type etc.
colnames(mydata.SW) <- c("wavenumber", "polV", "amp")

#levels(mydata.SW$polV)
mydata.SW$pol <- "x"
mydata.SW$v <- "x"
for(i in 1:nrow(mydata.SW)){
  temp <- strsplit(as.character(mydata.SW$polV[i]), "\\.")
  mydata.SW$pol[i] <- temp[[1]][1]
  mydata.SW$v[i] <- temp[[1]][2]
}

str(mydata.SW)
mydata.SW$pol <- as.factor(mydata.SW$pol)
mydata.SW$v <- as.integer(as.numeric(as.factor(mydata.SW$v)))

mydata.SW$incWater <- "SW"
mydata.SW$incWater <- as.factor(mydata.SW$incWater)


###### load references #########
mydata.ref<-read.csv(file="data-raw/References.csv", header=TRUE , dec="." , sep=";")
str(mydata.ref)

# reshape data frame into more usable way
# wavenumber (WN) is the same for alle measurements (X-axis)
mydata.ref <- melt(mydata.ref, id.vars = "WN")

# rename columns and define polymer type etc.
colnames(mydata.ref) <- c("wavenumber", "pol", "amp")

mydata.ref$v <- 1
mydata.ref$incWater <- "pristine"
mydata.ref$polV <- paste(mydata.ref$pol, ".V", mydata.ref$v, sep="")

mydata.ref$incWater <- as.factor(mydata.ref$incWater)
mydata.ref$polV <- as.factor(mydata.ref$polV)




######## merge data ##########
mydata <- rbind(mydata, mydata.SW)
# increase v by 1 as 1 should be pristine
mydata$v <- mydata$v +1
mydata <- rbind(mydata.ref, mydata)

mydata$polV <- paste(mydata$polV, mydata$incWater, sep=".")

levels(mydata$pol)
mydata <- droplevels(subset(mydata, mydata$pol != "X"))

# save data into file
write.table(mydata,
            file = "data-raw/prep_data",
            eol = "\r", 
            sep = ";",
            dec = "."
)


###### save data as usable data #####
# to provide further information about functional groups, we read another table with those information
components <- read.table(
  file = "data-raw/components", 
  sep = "\t",
  dec = ".",
  header = TRUE
)
components$rmin <- as.numeric(components$rmin)
components$rmax <- as.numeric(components$rmax)

mydata <- read.table(
  file = "data-raw/prep_data", 
  sep = ";",
  dec = ".",
  header = TRUE
)
mydata$polV <- as.factor(mydata$polV)
mydata$pol <- as.factor(mydata$pol)
mydata$incWater <- as.factor(mydata$incWater)
mydata$incWater <- factor(mydata$incWater, levels = c("pristine", "FW", "SW"))
mydata$v <- as.numeric(mydata$v)


#### put data into correct files #######
usethis::use_data(mydata, components, internal = TRUE, overwrite = TRUE)
specData <- mydata
usethis::use_data(specData, overwrite = TRUE)
