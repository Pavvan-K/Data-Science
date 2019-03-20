library(XML)
library(dplyr)
setwd('E:\\Courses\\Jigsaw R\\JIGSAW\\DSR\\R Lab\\Data Science with R - files\\Data Science with R - files\\Assignments\\Graded Assignments\\Topic 6.2 - Data Visualization in R')
dir()
data<- readHTMLTable("The World's Most Valuable Brands List - Forbes.html")
brands<-data$the_list
names(brands)
head(brands)
### Removing # from Rank

brands$Rank<-gsub('#','',brands$Rank)
brands$Rank<-gsub('\\$','',brands$`Brand Value`)
