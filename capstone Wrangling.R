#Read in 1 excel file
read_input_data_file <- function(file_name){
  input.df <- read_excel(file_name.xlsx)
  return(input.df)
}
  
#Library functions
  library("xlsx", lib.loc="~/R/win-library/3.5")
  library("readxl", lib.loc="~/R/win-library/3.5")
  library("dplyr", lib.loc="~/R/win-library/3.5")

#Assign files based on demographics
UR.women <- read_excel("DataFinder-20190127172936.xlsx")
UR.men <- read_excel("DataFinder-20190201195336.xlsx")         
UR.16.19 <- read_excel("DataFinder-20190201195539.xlsx")
UR.20.24 <- read_excel("DataFinder-20190201195742.xlsx")                       
UR.25.34 <- read_excel("DataFinder-20190201200013.xlsx")                       
UR.35.44 <- read_excel("DataFinder-20190201200209.xlsx")
UR.45.54 <- read_excel("DataFinder-20190201200344.xlsx")
UR.55andOver <- read_excel("DataFinder-20190201200522.xlsx")
UR.Asian <- read_excel("DataFinder-20190127172218.xlsx")
UR.Hispanic <- read_excel("DataFinder-20190127172143.xlsx")
UR.Black <- read_excel("DataFinder-20190127172052.xlsx")
UR.White <- read_excel("DataFinder-20190127171845.xlsx")

#Remove unneccessary rows
UR.women1 <- UR.women[55:606,]
UR.women.2 <- UR.women1[,3:4]
UR.men.2 <- UR.men[55:606, 3:4]
UR.16.19.2 <- UR.16.19[55:606, 3:4]
UR.20.24.2 <- UR.20.24[55:606, 3:4]
UR.25.34.2 <- UR.25.34[55:606, 3:4]
UR.35.44.2 <- UR.35.44[55:606, 3:4]
UR.45.54.2 <- UR.45.54[55:606, 3:4]
UR.55andover.2 <- UR.55andOver[55:606, 3:4]
UR.Hispanic.2 <- UR.Hispanic[19:568, 3:4]
UR.Black.2 <- UR.Black[31:582, 3:4]
UR.Asian.2 <- UR.Asian[19:210, 3:4]
UR.White.2 <- UR.White[55:606, 3:4]

#Rename Columns
names(UR.women.2)[1] <- "Date"
names(UR.women.2)[2] <- "Women Unemployment Rate"
names(UR.men.2)[1] <- "Date"
names(UR.men.2)[2] <- "men Unemployment Rate"
names(UR.16.19.2)[1] <- "Date"
names(UR.16.19.2)[2] <- "16-19 Unemployment Rate"
names(UR.20.24.2)[1] <- "Date"
names(UR.20.24.2)[2] <- "20-24 Unemployment Rate"
names(UR.25.34.2)[1] <- "Date"
names(UR.25.34.2)[2] <- "25-34 Unemployment Rate"
names(UR.35.44.2)[1] <- "Date"
names(UR.35.44.2)[2] <- "35-44 Unemployment Rate"
names(UR.45.54.2)[1] <- "Date"
names(UR.45.54.2)[2] <- "45-54 Unemployment Rate"
names(UR.55andover.2)[1] <- "Date"
names(UR.55andover.2)[2] <- "55&Over Unemployment Rate"
names(UR.Hispanic.2)[1] <- "Date"
names(UR.Hispanic.2)[2] <- "Hispanic Unemployment Rate"
names(UR.Black.2)[1] <- "Date"
names(UR.Black.2)[2] <- "Black Unemployment Rate"
names(UR.Asian.2)[1] <- "Date"
names(UR.Asian.2)[2] <- "Asian Unemployment Rate"
names(UR.White.2)[1] <- "Date"
names(UR.White.2)[2] <- "White Unemployment Rate"

#Descriptive Statistics
mean(as.numeric(UR.men.2$`men Unemployment Rate`))

#Convert Date from chr to Date format using as.Date

#Create Plots
ggplot(UR.White.2, aes(x = "Date", y = "White Unemployment Rate")) +
  geom_line()


combine_input_data_files <- function(){
  #combine data sets (dplyr)
  total <- merge(UR.women.2, UR.men.2)
  total2 <- merge(total, UR.16.19.2)
  total3 <- merge(total2, UR.20.24.2)
  total4 <- merge(total3, UR.25.34.2)
  total5 <- merge(total4, UR.35.44.2)
  total6 <- merge(total5, UR.45.54.2)
  total7 <- merge(total6, UR.55andover.2)
  total8 <- merge(total7, UR.Hispanic.2)
  total9 <- merge(total8, UR.Black.2)
  total10 <- merge(total9, UR.White.2)
  total11 <- left_join(total10, UR.Asian.2)
  
##need to add in Asian........
  #left join
  
  #Convert to Date format
  str(total14)
  Date[1:550], format: "1973-04-01" "1973-08-01" "1973-12-01" "1973-07-01" "1973-06-01" "1973-03-01" "1973-05-01" "1973-11-01" "1973-10-01" ...
  total11$Date <- paste(total11$Date, 01)
  total11$Date <- as.Date(total11$Date, format = "%Y %b %d")
  str(total11$Date)
  
  #Convert to numeric
  total11$`Women Unemployment Rate`<- as.numeric(total11$`Women Unemployment Rate`)
  total11$`men Unemployment Rate` <- as.numeric(total11$`men Unemployment Rate`)
  total11$`16-19 Unemployment Rate` <- as.numeric(total11$`16-19 Unemployment Rate`)
  total11$`20-24 Unemployment Rate` <- as.numeric(total11$`20-24 Unemployment Rate`)
  total11$`25-34 Unemployment Rate`<- as.numeric(total11$`25-34 Unemployment Rate`)
  total11$`35-44 Unemployment Rate` <- as.numeric(total11$`35-44 Unemployment Rate`)
  total11$`45-54 Unemployment Rate` <- as.numeric(total11$`45-54 Unemployment Rate`)
  total11$`55&Over Unemployment Rate` <- as.numeric(total11$`55&Over Unemployment Rate`)
  total11$`Hispanic Unemployment Rate` <- as.numeric(total11$`Hispanic Unemployment Rate`)
  total11$`Black Unemployment Rate` <- as.numeric(total11$`Black Unemployment Rate`)
  total11$`White Unemployment Rate` <- as.numeric(total11$`White Unemployment Rate`)
  total11$`Asian Unemployment Rate` <- as.numeric(total11$`Asian Unemployment Rate`)
  
  ##Descriptive Statistics
  Summary(total11)
  
  
  
  ##GGplots
  ggplot(data=total11, aes(x=total11$Date, y=total11$`Women Unemployment Rate`)) +
     geom_line()
  ggplot(data=total11, aes(x=total11$Date, y=total11$`men Unemployment Rate`)) +
    geom_line()
  #Gender
  ggplot(total11, aes(Date)) + 
    geom_line(aes(y = total11$`Women Unemployment Rate`, colour = "Women Unemployment Rate")) + 
    geom_line(aes(y = total11$`men Unemployment Rate`, colour = "men Unemployment Rate"))+
    theme(axis.title.y=element_blank())+
    ggtitle("Unemployment by Gender")+
    scale_colour_discrete(name = "Gender")
  #Races
  ggplot(total11, aes(Date)) + 
    geom_line(aes(y = total11$`Hispanic Unemployment Rate`, colour = "Hispanic Unemployment Rate"))+
    geom_line(aes(y = total11$`Black Unemployment Rate`, colour = "Black Unemployment Rate"))+
    geom_line(aes(y = total11$`White Unemployment Rate`, colour = "White Unemployment Rate")) + 
    geom_line(aes(y = total11$`Asian Unemployment Rate`, colour = "Asian Unemployment Rate"))+
    theme(axis.title.y=element_blank())+
    ggtitle("Unemployment by Race")+
    scale_colour_discrete(name = "Race")
  
  #age Groups
  ggplot(total11, aes(Date)) + 
    geom_line(aes(y = total11$`16-19 Unemployment Rate`, colour = "16-19 Unemployment Rate"))+
    geom_line(aes(y = total11$`20-24 Unemployment Rate`, colour = "20-24 Unemployment Rate"))+
    geom_line(aes(y = total11$`25-34 Unemployment Rate`, colour = "25-34 Unemployment Rate")) + 
    geom_line(aes(y = total11$`35-44 Unemployment Rate`, colour = "35-44 Unemployment Rate"))+
    geom_line(aes(y = total11$`45-54 Unemployment Rate`, colour = "45-54 Unemployment Rate"))+
    geom_line(aes(y = total11$`55&Over Unemployment Rate`, colour = "55&Over Unemployment Rate"))+
    theme(axis.title.y=element_blank())+
    ggtitle("Unemployment by Age")+
    scale_colour_discrete(name = "Age Group")
    
  
  
  
  
  
}

wrangling_main <- function(){
  library(xlsx)
  D <- read_input_data_file("DataFinder20190201195336")
  return(D)
  
  
  
}
# create plots with ggplot 
# send data 