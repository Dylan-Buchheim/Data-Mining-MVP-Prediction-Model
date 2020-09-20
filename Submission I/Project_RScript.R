#libraries-------------------------------------
library(stringr)
library(plyr)
library(ggplot2)

#Datasets -------------------------------------
PlayerStatsRaw <- read.csv("Seasons_Stats.csv")
MVP_Winners <- read.csv("MVP_Winners.csv")

#Data Cleaning and Preparation-----------------
#Removing data from years where there was no MVP award.
PlayerStats <- PlayerStatsRaw[!(PlayerStatsRaw$Year %in% c("1950","1951","1952","1953","1954","1955")),]

#Removing the asterisks from the end of some players names.
PlayerStats$Player <- gsub("[[:punct:]]$","",PlayerStats$Player)

#Removing the completely null entries.
PlayerStats <- PlayerStats[-which(is.na(PlayerStats$Year)),]
#Removing completely null attributes.
PlayerStats$blanl <- NULL 
PlayerStats$blank2 <- NULL

#Re-Indexing the Data
names(PlayerStats)[names(PlayerStats)=="X"] <- "Index"
PlayerStats$Index <- c(1:nrow(PlayerStats))

#Adding the target variable MVP
PlayerStats$MVP <- "no"
PlayerStats$MVPPrevious <- "no"
#Using the data from MVP_Winners to get the correct values of for "MVP" in the PlayerStats df.
MVP_Winners$Season <- strtoi(substr(MVP_Winners$Season,1,4))+1
names(MVP_Winners)[names(MVP_Winners)=="Season"] <- "Year"
mvp.indexes <- match_df(PlayerStats,MVP_Winners,on = c("Player","Year"))$Index
PlayerStats[which(PlayerStats$Index %in% mvp.indexes),]$MVP <- "yes"
mvp.previous.indexes <- match_df(PlayerStats,MVP_Winners,on = c("Player"))$Index
PlayerStats[which(PlayerStats$Index %in% mvp.previous.indexes),]$MVPPrevious <- "yes"

#Re-Expressing the Position Attribute as a numeric value and simplifying the value to the players main position.
replacement <- c("C"=1,"F"=2,"G"=3,"PF"=4,"PG"=5,"SF"=6,"SG"=7,
                 "C-F"=1,"C-PF"=1,"C-SF"=1,"F-C"=2,"F-G"=2,"G-F"=3,
                 "PF-C"=4,"PF-SF"=4,"PG-SF"=5,"PG-SG"=5,"SF-PF"=6,"SF-PG"=6,
                 "SF-SG"=6,"SG-PF"=7,"SG-PG"=7,"SG-SF"=7)
pos.num <- revalue(x =PlayerStats$Pos,replace = replacement)
PlayerStats$Pos_Numeric <- as.numeric(levels(pos.num))[pos.num]

#Standardizing Certain Attributes
PlayerStats$G_z <- scale(x=PlayerStats$G)
PlayerStats$MP_z <- scale(x=PlayerStats$MP)
PlayerStats$PER_z <- scale(x=PlayerStats$PER)

#Removing players who played in very few games as they create outliers among many of the player statistics due to small sample size.
PlayerStats <- PlayerStats[-which(PlayerStats$G_z < -1),]

#Removing extraneous data that will not be used in modeling.
PlayerStats <- PlayerStats[,c(1,2,3,4,5,7,10,24,30,40,45,46,47,48,49,50,51,52,53,54,56)]

#Exploratory Data Analysis ------------------
#Graphs of major statistics with a MVP overlay.
ggplot(PlayerStats, aes(factor(Pos_Numeric))) + geom_bar(aes(fill = MVP), position ="fill")
ggplot(PlayerStats, aes(Age)) + geom_histogram(aes(fill = MVP),binwidth = 1)
ggplot(PlayerStats, aes(G)) + geom_histogram(aes(fill = MVP),binwidth = 1)
ggplot(PlayerStats, aes(PER)) + geom_histogram(aes(fill = MVP))
ggplot(PlayerStats, aes(WS)) + geom_histogram(aes(fill = MVP))
ggplot(PlayerStats, aes(FG)) + geom_histogram(aes(fill = MVP))
ggplot(PlayerStats, aes(FT)) + geom_histogram(aes(fill = MVP))
ggplot(PlayerStats, aes(TRB)) + geom_histogram(aes(fill = MVP),binwidth = 50)
ggplot(PlayerStats, aes(AST)) + geom_histogram(aes(fill = MVP),binwidth = 50)
ggplot(PlayerStats, aes(STL)) + geom_histogram(aes(fill = MVP))
ggplot(PlayerStats, aes(BLK)) + geom_histogram(aes(fill = MVP))
ggplot(PlayerStats, aes(TOV)) + geom_histogram(aes(fill = MVP))
ggplot(PlayerStats, aes(PF)) + geom_histogram(aes(fill = MVP))
ggplot(PlayerStats, aes(PTS)) + geom_histogram(aes(fill = MVP),binwidth = 100)


#Contingency table between position and MVP status
Pos_MVP_Table.v1 <- table(PlayerStats$Pos_Numeric,PlayerStats$MVP)
row.names(Pos_MVP_Table.v1) <- c("C","F","G","PF","PG","SF","SG")
Pos_MVP_Table.v2 <- round(prop.table(Pos_MVP_Table.v1,margin=2)*100,1)
row.names(Pos_MVP_Table.v2) <- c("C","F","G","PF","PG","SF","SG")
print(Pos_MVP_Table.v1)
print(Pos_MVP_Table.v2)

#Binning age to enhance its predictive value.
PlayerStats$Age_Binned <- cut(x=PlayerStats$Age,breaks=c(0,21.01,32.01,100),right=FALSE,
                              labels=c("21 and Under","22 to 32","Over 32"))
ggplot(PlayerStats, aes(Age_Binned)) + geom_bar(aes(fill = MVP),position = "fill")
#Binning g (games played) to enhance its predictive value.
PlayerStats$G_Binned <- cut(x=PlayerStats$G,breaks=c(0,64.01,77.01,100),right=FALSE,
                              labels=c("64 and Under","65 to 77","Over 77"))
ggplot(PlayerStats, aes(G_Binned)) + geom_bar(aes(fill = MVP),position = "fill")
#Binning TRB (Total Rebounds) to enhance its predictive value.
PlayerStats$TRB_Binned <- cut(x=PlayerStats$TRB,breaks=c(0,400.01,1000.01,3000),right=FALSE,
                            labels=c("400 and Under","401 to 1000","Over 1000"))
ggplot(PlayerStats, aes(TRB_Binned)) + geom_bar(aes(fill = MVP),position = "fill")
#Binning AST (Assists) to enhance predictive value.
PlayerStats$AST_Binned <- cut(x=PlayerStats$AST,breaks=c(0,200.01,600.01,3000),right=FALSE,
                              labels=c("200 and Under","201 to 600","Over 600"))
ggplot(PlayerStats, aes(AST_Binned)) + geom_bar(aes(fill = MVP),position = "fill")
#Binning PTS (Points) to enhance predictive value.
PlayerStats$PTS_Binned <- cut(x=PlayerStats$PTS,breaks=c(0,800.01,1500.01,2100.01,5000),right=FALSE,
                              labels=c("800 and Under","801 to 1500","1501 to 2100","Over 2100"))
ggplot(PlayerStats, aes(PTS_Binned)) + geom_bar(aes(fill = MVP),position = "fill")

#Deriving a new variable IPG (impact per game).
PlayerStats$IPG <- (PlayerStats$TRB + PlayerStats$AST + PlayerStats$PTS) / PlayerStats$G
#Graphing IPG
ggplot(PlayerStats, aes(IPG)) + geom_histogram(aes(fill = MVP),binwidth = 5)

PlayerStats$Year_Binned <- cut(x=PlayerStats$Year,breaks=c(0,1960.01,1970.01,1980.01,1990.01,2000.01,2010.01,2020.01),right=FALSE,
                              labels=c("1950s","1960s","1970s","1980s","1990s","2000s","2010s"))

write.csv(PlayerStats, file = "PlayerData.csv")

