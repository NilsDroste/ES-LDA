########################################################################################
# Bibliometric Analysis of year subsets of Ecosystem Service Research 
# Script authors: N. Droste (nils.droste@ufz.de)
# an adaptation of the nails project source code: http://nailsproject.net/ 
########################################################################################

# -1 setting wd, loading packages, preps -----------------------------------------------

mainDir <- "/home/ubuntu/Documents/ES_LDA"
setwd(file.path(mainDir))

# Loading libraries
library(splitstackshape)
library(reshape)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(grid)
#library(igraph)
library(knitr)
#library(ldatuning)
library(SnowballC)
library(tm)
library(lda)
library(LDAvis)
library(rworldmap)
library(stringi)
library(sankey)
library(gplots)
library(XLConnect)

# Session Info
sessionInfo()

# Source helper functions
source("helperFunctions.R")

# 0 data preparation / cleaning --------------------------------------------------------
########################################################################################
# Input folder should contain full records and citations downloaded from
# Web of knowledge in tab-delimited (UTF-16) format. Working directory should
# contain the list of fieldtags for naming the variables (fieldtags.csv).
########################################################################################

# Load variable names
fieldtags <- read.csv(paste(getwd(), "/fieldtags.csv", sep=""), header = T, sep = ";")
filelist <- list()
literatureList <- list()

#Loop over times
for (i in c("1970_2000", "2001_2010", "2011_2016")){
  
  # List files in input folder
  filelist[[i]] <- list.files(paste(getwd(), "/input/", i, sep=""), full.names = T)
  
  # Load files in the input folder and merge into a single file
  for (file in filelist[[i]]) {
    literature <- read.delim2(file, header = T,
                              fileEncoding = "UTF-16", row.names = NULL,
                              quote = "", stringsAsFactors=FALSE )
    # Fix misplaced column names
    data.names <- names(literature)[2:length(names(literature))]
    literature <- literature[, 1:(ncol(literature) - 1)]
    names(literature) <- data.names
    # Merge data
    literatureList[[i]] <- rbind(literatureList[[i]], literature)
  }
  
  # Create and add id variable
  id <- c(1:nrow(literatureList[[i]]))
  literatureList[[i]] = cbind(as.data.frame(id), literatureList[[i]])
  
  # Cleaning data
  
  # Fix variable names
  tags <- names(literatureList[[i]])       # Extract column names
  # Match column names (acronyms) with full column names
  fields <- as.character(fieldtags$field[match(tags, fieldtags$tag)])
  fields[is.na(fields)] <- tags[is.na(fields)]     # Throws warnings but seems to be working
  fields <- gsub(" ", "", fields)         # Remove spaces
  
  # Change literature column names and fix weird names
  names(literatureList[[i]]) <- fields
  names(literatureList[[i]])[names(literatureList[[i]]) == "KeywordsPlus\xfc\xbe\x8e\x86\x84\xbc"] <- "KeywordsPlus"
  names(literatureList[[i]])[names(literatureList[[i]]) == "PublicationType(conference,book,journal,bookinseries,orpatent)"] <- "PublicationType"
  names(literatureList[[i]])[names(literatureList[[i]]) == "29-CharacterSourceAbbreviation"] <- "SourceAbbreviation"
  names(literatureList[[i]])[names(literatureList[[i]]) == "DigitalObjectIdentifier(DOI)" ] <- "DOI"
  
  #Format Data
  literatureList[[i]]$AuthorFullName <- toupper(literatureList[[i]]$AuthorFullName)
  literatureList[[i]]$AuthorFullName <- gsub("'", "", literatureList[[i]]$AuthorFullName)
  literatureList[[i]]$AuthorFullName <- gsub('"', "", literatureList[[i]]$AuthorFullName)
  
  literatureList[[i]]$AuthorKeywords <- tolower(literatureList[[i]]$AuthorKeywords)
  literatureList[[i]]$AuthorKeywords <- gsub("'", "", literatureList[[i]]$AuthorKeywords)
  literatureList[[i]]$AuthorKeywords <- gsub('"', "", literatureList[[i]]$AuthorKeywords)
  
  literatureList[[i]]$KeywordsPlus <- tolower(literatureList[[i]]$KeywordsPlus)
  literatureList[[i]]$KeywordsPlus <- gsub("'", "", literatureList[[i]]$KeywordsPlus)
  literatureList[[i]]$KeywordsPlus <- gsub('"', "", literatureList[[i]]$KeywordsPlus)
  
  literatureList[[i]]$YearPublished <- as.numeric(as.character(literatureList[[i]]$YearPublished))
  
  literatureList[[i]]$DocumentTitle <- gsub("'", "", literatureList[[i]]$DocumentTitle)
  literatureList[[i]]$DocumentTitle <- gsub('"', "", literatureList[[i]]$DocumentTitle)
  
  literatureList[[i]]$SubjectCategory <- tolower(literatureList[[i]]$SubjectCategory)
  literatureList[[i]]$SubjectCategory <- gsub("'", "", literatureList[[i]]$SubjectCategory)
  literatureList[[i]]$SubjectCategory <- gsub('"', "", literatureList[[i]]$SubjectCategory)
  
  literatureList[[i]]$CitedReferences <- gsub("'", "", literatureList[[i]]$CitedReferences)
  literatureList[[i]]$CitedReferences <- gsub('"', "", literatureList[[i]]$CitedReferences)
  literatureList[[i]]$CitedReferences <- toupper(literatureList[[i]]$CitedReferences)
  literatureList[[i]]$CitedReferences <- gsub("DOI DOI", "DOI", literatureList[[i]]$CitedReferences)
  
  literatureList[[i]]$TimesCited <- as.numeric(as.character(literatureList[[i]]$TimesCited))
  
  literatureList[[i]]$DOI <- toupper(literatureList[[i]]$DOI)
  
  #remove double entries (i.e. those with same first author AND same first 25 characters of title)
  
  literatureList[[i]]<-literatureList[[i]][!duplicated(data.frame(str_sub(tolower(literatureList[[i]]$DocumentTitle), 1, 25), stri_extract_first_words(tolower(literatureList[[i]]$Authors)))),]
  
  #remove empty abstracts
  literatureList[[i]]<-literatureList[[i]][nchar(literatureList[[i]]$Abstract) > 0,]
  
}

rm(list = c("data.names", "fields", "fieldtags", "file", "filelist", "i", "id", "tags"))

# 1 write seperate csv files for different analytical topics ---------------------------------

##LOCATIONS
for (i in c("1970_2000", "2001_2010", "2011_2016")){
  
  # Extract cities and countries
  literatureList[[i]]$Locations <- sapply(literatureList[[i]]$AuthorAddress, get_location)
  
  # Split locations by ";
  locationList <- unlist(lapply(literatureList[[i]]$Locations,
                                function(x) strsplit(x, ";")))
  
  locations <- data.frame(location = locationList)        # Create data frame
  locations$location <- as.character(locations$location)  # To chararcter type
  locations$city <- gsub(",.*", "", locations$location)   # Remove country from location
  locations$country <- gsub(".*,", "", locations$location) # Remove city from location
  
  # Save locations
  write.table(locations, paste(getwd(), "/output/", i, "/locations_", i, ".csv", sep=""),
              sep = ";", row.names = F, qmethod = "double")
  
  #remove temp data
  rm(list = c("locations", "locationList"))
}

##KEYWORDS
for (i in c("1970_2000", "2001_2010", "2011_2016")){
  # Create a new data frame, where each keyword is in a separate row.
  
  literatureByKeywords <- subset(literatureList[[i]],
                                 select = c("AuthorKeywords", "id"))
  literatureByKeywords <- literatureByKeywords[
    !is.na(literatureByKeywords$AuthorKeywords),]
  literatureByKeywords <- literatureByKeywords[
    literatureByKeywords$AuthorKeywords != "", ]
  using_KeywordsPlus = FALSE
  
  if (nrow(literatureByKeywords) == 0) {
    literatureByKeywords <- subset(literatureList[[i]],
                                   select = c("KeywordsPlus", "id"))
    names(literatureByKeywords)[1] <- "AuthorKeywords"
    literatureByKeywords <- literatureByKeywords[
      !is.na(literatureByKeywords$AuthorKeywords),]
    literatureByKeywords <- literatureByKeywords[
      literatureByKeywords$AuthorKeywords != "", ]
    using_KeywordsPlus = TRUE
  }
  
  if (nrow(literatureByKeywords) > 0) {
    literatureByKeywords <- cSplit(literatureByKeywords,
                                   splitCols = "AuthorKeywords",
                                   sep = ";", direction = "long")
    literatureByKeywords <- literatureByKeywords[
      !is.na(literatureByKeywords$AuthorKeywords),]
    literatureByKeywords <- subset(literatureByKeywords,
                                   select = c("id", "AuthorKeywords"))
    literatureByKeywords <- merge(literatureByKeywords,
                                  subset(literatureList[[i]], select = -c(AuthorKeywords)),
                                  by = "id")
  }
  
  # Save file
  write.table(literatureByKeywords, paste(getwd(), "/output/", i, "/literature_by_keywords_", i, ".csv", sep=""),
              row.names = F, sep = ';', qmethod = "double") 
  
  #remove temp data
  rm(list = c("literatureByKeywords", "using_KeywordsPlus"))
}  


#SAVE LITERATURE FILE
for (i in c("1970_2000", "2001_2010", "2011_2016")){
  # Save the literature as a single csv-file literature.csv.
  write.table(literatureList[[i]], paste(getwd(), "/output/", i, "/literature_", i, ".csv", sep=""),
              sep = ";", row.names = F, qmethod = "double")
}

# 2 Topic modelling ---------------------------------------------------------------------------------
# reading in saved data
setwd(file.path(mainDir))
literatureList = list()
for (i in c("1970_2000", "2001_2010", "2011_2016")){
  literatureList[[i]] <- read.csv( paste(getwd(), "/output/", i, "/literature_", i, ".csv", sep=""), sep = ";")
}
topdocsfortopic = list()
TopicModel = list()
topwords = list()
models =  list()
for (i in c("1970_2000", "2001_2010", "2011_2016")){  
  # Do topic modeling on abstracts using the lda libraries (adding them as a new column)
  source(paste(getwd(), "/topicmodel_years.R", sep = ""), chdir = T)
  
  outDir = paste(getwd(), "/output/", i, "/topicmodelvis_", i , sep = "")
  
  # HTML output
  serVis(json, out.dir = outDir, open.browser = FALSE)
  
  # Freeing up memory
  rm(list = c("json", "outDir", "tfdDF", "new.order"))
}

for (i in c("1970_2000", "2001_2010", "2011_2016")){ 
  TopicModel[[i]]$topic.frequency <- colSums(TopicModel[[i]]$theta * TopicModel[[i]]$doc.length)
  TopicModel[[i]]$topic.proportion <- TopicModel[[i]]$topic.frequency/sum(TopicModel[[i]]$topic.frequency) 
}

# 3 plotting -----------------------------------------------------------------------


#KEYWORDS
setwd(mainDir)
keywordDFList = list()
keywordPlotList <- list()
for (i in c("1970_2000", "2001_2010", "2011_2016")){
  #print(i)
  setwd(paste(getwd(), "/output/", i, sep = ""))
  
  keywordDFList[[i]] <- read.csv(paste("literature_by_keywords_", i, ".csv", sep = ""), sep = ';', stringsAsFactors=FALSE) 
  setwd(mainDir)
}

x1 <- keywordDFList[[1]] %>% group_by(AuthorKeywords) %>% summarize(freq = n()) %>% arrange(desc(freq))
x1 <- as.data.frame(head(x1, n = 12)[c(-1,-6),])
keywordPlot1<- ggplot(x1, aes (reorder(x1[,1], x1[,2]), freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Number of occurences", x = "Keywords") +
  ggtitle("1990 - 2000")

x2 <- keywordDFList[[2]] %>% group_by(AuthorKeywords) %>% summarize(freq = n()) %>% arrange(desc(freq))
x2 <- as.data.frame(head(x2, n = 12)[c(-1,-3),])
keywordPlot2<- ggplot(x2, aes (reorder(x2[,1], x2[,2]), freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Number of occurences", x = "Keywords") +
  ggtitle("2001 - 2010")


x3 <- keywordDFList[[3]] %>% group_by(AuthorKeywords) %>% summarize(freq = n()) %>% arrange(desc(freq))
x3 <- as.data.frame(head(x3, n = 12)[c(-1,-5),])
keywordPlot3<- ggplot(x3, aes (reorder(x3[,1], x3[,2]), freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Number of occurences", x = "Keywords") +
  ggtitle("2011 - 2016")

#keywordPlot <- grid.draw(cbind(ggplotGrob(keywordPlot1), ggplotGrob(keywordPlot2), ggplotGrob(keywordPlot3), size = "last"))
library(cowplot)
theme_set(theme_gray(base_size=5))#+theme(aspect.ratio=1))
#plot_grid(keywordPlot1,keywordPlot2,keywordPlot3,nrow=1)
setwd(paste(getwd(), "/output/years_plots/", sep = ""))
postscript("Fig3.eps", width=5.2, height= 2,paper="special")
plot_grid(keywordPlot1,keywordPlot2,keywordPlot3,nrow=1)
dev.off()
setwd(mainDir)

#LOCATIONS
locList = list()
for (i in c("1970_2000", "2001_2010", "2011_2016")){
  #print(i)
  setwd(paste(getwd(), "/output/", i, sep = ""))
  locList[[i]] <- read.csv(paste("locations_", i, ".csv", sep = ""), sep = ';', stringsAsFactors=FALSE) 
  setwd(mainDir)
}

for (i in c("1970_2000", "2001_2010", "2011_2016")){
  for (j in c("USA", "CA", "CT" ,"DC", "DE" ,"GA", "IL", "MD", "MI","MN", "NC", "NE", "NY", "PA", "TN", "VA", "WI", "WY")){
    locList[[i]][grep(j, locList[[i]]$country), "country"] <- "United States"
  }
  locList[[i]]$country=gsub("Peoples R China", "China", locList[[i]]$country)
  locList[[i]][grep("Scotland|England|Wales|North Ireland", locList[[i]]$country), "country"] <- "United Kingdom"
  locList[[i]][grep("Guadeloupe", locList[[i]]$country), "country"] <- "Guadeloupe"
  locList[[i]][grep("Polynesia", locList[[i]]$country), "country"] <- "French Polynesia"
  locList[[i]][grep("U Arab Emirates", locList[[i]]$country), "country"] <- "United Arab Emirates"
  #locList[[i]][grep("Trinid | Trinidad", locList[[i]]$country), "country"] <- 'Trinidad'
  locList[[i]][grep("Tobago", locList[[i]]$country), "country"] <- 'Tobago'
  locList[[i]][grep("Bosnia & Herceg", locList[[i]]$country), "country"] <- 'Bosnia and Herzegovina'
  locList[[i]][grep("Byelarus", locList[[i]]$country), "country"] <- 'Belarus'
  locList[[i]][grep("Cote Ivoire", locList[[i]]$country), "country"] <- 'Ivory Coast'
  locList[[i]][grep("Papua N Guinea", locList[[i]]$country), "country"] <- 'Mongolia'
  locList[[i]][grep("Mongol Peo Rep", locList[[i]]$country), "country"] <- 'Papua New Guinea'
  locList[[i]][grep("Surinam", locList[[i]]$country), "country"] <- 'Suriname'
  locList[[i]][grep("W Ind Assoc St", locList[[i]]$country), "country"] <- 'Turks and Caicos Islands'
  locList[[i]][grep("Dominican", locList[[i]]$country), "country"] <- 'Dominican Republic'
  locList[[i]][grep("Surinam", locList[[i]]$country), "country"] <- 'Suriname'
  locList[[i]][grep("Marshall", locList[[i]]$country), "country"] <- 'Marshall Islands'
  locList[[i]][grep("Martinique", locList[[i]]$country), "country"] <- 'Martinique'
  locList[[i]][grep("Micronesia", locList[[i]]$country), "country"] <- 'Micronesia'
  locList[[i]][grep("Reunion", locList[[i]]$country), "country"] <- 'Reunion'
  locList[[i]][grep("Sao Tome", locList[[i]]$country), "country"] <- 'Sao Tome and Principe'
  locList[[i]][grep("Zaire", locList[[i]]$country), "country"] <- 'Democratic Republic of the Congo'
  locList[[i]]$country = tolower(locList[[i]]$country)
  locList[[i]] <- na.omit(locList[[i]])
}

c1970_2000 <- as.data.frame(table(locList[[1]]$country)); colnames(c1970_2000)[1] <- "country"
c2001_2010 <- as.data.frame(table(locList[[2]]$country)); colnames(c2001_2010)[1] <- "country"
c2011_2016 <- as.data.frame(table(locList[[3]]$country)); colnames(c2011_2016)[1] <- "country"

# 
# sort(table(locList[[1]]$country), dec=T)
# sort(table(locList[[2]]$country), dec=T)
# sort(table(locList[[3]]$country), dec=T)
# sort(table(locList[[4]]$country), dec=T)


location_1970_2000 <- joinCountryData2Map(c1970_2000, joinCode = "NAME", nameJoinColumn = "country", verbose=TRUE)
location_2001_2010 <- joinCountryData2Map(c2001_2010, joinCode = "NAME", nameJoinColumn = "country", verbose=TRUE) #somehow missing three from "Trinidad & Tobago"
location_2011_2016 <- joinCountryData2Map(c2011_2016, joinCode = "NAME", nameJoinColumn = "country", verbose=TRUE)


setwd(paste(getwd(), "/output/years_plots/", sep = ""))
setEPS()
postscript("Fig2_untrimmed.eps", horizontal=F,width=5.2,height=12, onefile=T, paper="special")
#oldPar <- par(mar=c(0,0,1,0))
#nPanels <- layout(c(1,2,3), heights=c(1,1,1), respect=F)
#nPanels <- layout(c(0,1,2,3,4,5,0), heights=c(lcm(.5),1,lcm(.5),1,lcm(.5),1,lcm(.5)), respect=F)
op <- par(mfcol = c(4,1),mar = c(0,0,5,0), pty="m")

Map1970 <-
mapCountryData(location_1970_2000, nameColumnToPlot="Freq", mapTitle="1990-2000", addLegend=F, colourPalette="white2Black", catMethod=c(1,2,4,8,16,31,61,121))
do.call( addMapLegend, c(Map1970, legendWidth=1.5, legendShrink=.875, legendMar = 2, legendLabels="all", legendIntervals="page", tcl=-0.25))           

Map2001 <-
mapCountryData(location_2001_2010 , nameColumnToPlot="Freq", mapTitle="2001-2010", addLegend=F, colourPalette="white2Black", catMethod=c(1,3,9,28,87,264,805,2454))
do.call( addMapLegend, c(Map2001, legendWidth=1.5, legendShrink=.875, legendMar = 2, legendLabels="all", legendIntervals="page", tcl=-0.25))  

Map2010 <- 
mapCountryData(location_2011_2016, nameColumnToPlot="Freq", mapTitle="2011-2016", addLegend=F, colourPalette="white2Black", catMethod=c(1,4,14,52,194,724,2703,10090))
do.call( addMapLegend, c(Map2010, legendWidth=1.5, legendShrink=.875, legendMar = 2, legendLabels="all", legendIntervals="page", tcl=-0.25))  

#dev.print(file= "locationsPlot.png", device=png, width=550, height= 3000, res=300)

dev.off()
setwd(file.path(mainDir))

# use ps2epsi Fig2_untrimmed.eps Fig2.eps in terminal to trim the file.eps


rm(list=c("c1970_2000", "c2001_2010", "c2011_2016", "i", "j", "keywordDFList", "keywordPlot","keywordPlot1", "keywordPlot2", "keywordPlot3", "keywordPlot4", "keywordPlotList", "literature", "location_1970_2000", "location_2001_2010", "location_2011_2016", "location_13_16", "locList", "Map1970", "Map2001", "Map2010", "x1", "x2", "x3", "x4", "wb", "op"))


# sankey diagramm

load("TopicModel.RData")
detach("package:igraph")

# ES <- data.frame(cbind(c("marine", "freshwater", "ecosystem fcts.", "pollination", "soils", "forests", "land cover", "urban", "agriculture", "risk mgnt", "sust. mgnt", "role of science", "governance", "conservation", "valuation", "assessment", "global awaren."),c("navy", "royalblue", "darkolivegreen", "yellow", "sienna", "olivedrab4", "thistle", "lavender", "peru", "lightpink", "salmon", "palevioletred", "purple", "limegreen", "goldenrod", "firebrick", "mistyrose"), rbind(c(6,2,4), c(9,9,8), c(4,NA,NA), c(NA,5,6), c(NA,8,5), c(8,7,7), c(NA,4,3), c(NA,NA,9), c(NA,3,NA), c(1,NA,NA), c(2,NA,NA),  c(NA,1,NA), c(NA,NA,1), c(7,NA,NA), c(3,6,NA), c(NA,NA,2), c(5,NA,NA))))
# names(ES) <- c("Topic", "Color", "Period1", "Period2", "Period3")
# setwd(paste(file.path(mainDir), "/output/", sep = ""))
# write.csv(ES, "TopicDevelopment.csv")
# ES$colhex <- paste0(col2hex(ES$Color), "A8")

ES <- data.frame(cbind(c("marine", "freshwater", "ecosystem fcts.", "pollination", "soils", "forests", "land cover", "urban", "agriculture", "risk mgnt", "sust. mgnt", "role of science", "governance", "conservation", "valuation", "assessment", "global awaren."),c("navy", "royalblue", "darkolivegreen", "yellow", "sienna", "olivedrab4", "thistle", "lavender", "peru", "lightpink", "salmon", "palevioletred", "purple", "limegreen", "goldenrod", "firebrick", "mistyrose"), rbind(c(5,2,4), c(9,9,7), c(6,NA,NA), c(NA,4,6), c(NA,8,5), c(8,7,8), c(NA,5,3), c(NA,NA,9), c(NA,3,NA), c(1,NA,NA), c(2,NA,NA),  c(NA,1,NA), c(NA,NA,1), c(7,NA,NA), c(3,6,NA), c(NA,NA,2), c(4,NA,NA))))
names(ES) <- c("Topic", "Color", "Period1", "Period2", "Period3")
setwd(paste(file.path(mainDir), "/output/", sep = ""))
write.csv(ES, "TopicDevelopment.csv")
ES$colhex <- paste0(col2hex(ES$Color), "A8")

ESnodes <- data.frame(id=paste0(rep("N", 27), rep(1:27)),  x=rep(1:3, each=9), 
                      label=c(as.vector(ES[which(!is.na(ES$Period1)),1]), 
                              as.vector(ES[which(!is.na(ES$Period2)),1]), 
                              as.vector(ES[which(!is.na(ES$Period3)),1])), 
                      col=c(as.vector(ES[which(!is.na(ES$Period1)),6]), 
                            as.vector(ES[which(!is.na(ES$Period2)),6]), 
                            as.vector(ES[which(!is.na(ES$Period3)),6])), 
                      size=c(TopicModel[[1]]$topic.proportion[ES$Period1[which(!is.na(ES$Period1))]]*10, 
                             TopicModel[[2]]$topic.proportion[ES$Period2[which(!is.na(ES$Period2))]]*10, 
                             TopicModel[[3]]$topic.proportion[ES$Period3[which(!is.na(ES$Period3))]]*10), cex=rep(.8, 27))


ESedges <- data.frame(
  N1=c("N1" ,  "N2",  "N3",  "N3",  "N4",  "N5",  "N6",  "N7",  "N7",  "N8",  "N9", "N9", "N9", "N9",
       "N10", "N11", "N12", "N13", "N14", "N14", "N15", "N15", "N15", "N16", "N16", "N16", "N17", "N17", "N18"), 
  N2=as.character(ESnodes[c( c(10, 11, 12, 13, 14, 17, 17, 16, 17, 18, 18, 17, 11, 10), 
                             c(19, 20, 21, 22, 23, 25, 27, 24, 25, 26, 22, 21, 27, 26, 27)),1]), 
  weight=c(
    
    #1st period edges
    (TopicModel[[1]]$topic.proportion[5]/(TopicModel[[1]]$topic.proportion[4]+TopicModel[[1]]$topic.proportion[5]))*TopicModel[[2]]$topic.proportion[2], #marine
    (TopicModel[[1]]$topic.proportion[9]/(TopicModel[[1]]$topic.proportion[4]+TopicModel[[1]]$topic.proportion[9]))*TopicModel[[2]]$topic.proportion[9], #freshwater -> freshwater
    TopicModel[[2]]$topic.proportion[4], #community ecol. -> pollination
    TopicModel[[2]]$topic.proportion[8], #community ecol. -> soils
    TopicModel[[2]]$topic.proportion[7], #forests
    (TopicModel[[1]]$topic.proportion[1]/(TopicModel[[1]]$topic.proportion[1]+TopicModel[[1]]$topic.proportion[2]+TopicModel[[1]]$topic.proportion[4]+TopicModel[[1]]$topic.proportion[7]))*TopicModel[[2]]$topic.proportion[1], #risk mgnt -> role of science
    (TopicModel[[1]]$topic.proportion[2]/(TopicModel[[1]]$topic.proportion[1]+TopicModel[[1]]$topic.proportion[2]+TopicModel[[1]]$topic.proportion[4]+TopicModel[[1]]$topic.proportion[7]))*TopicModel[[2]]$topic.proportion[1], #sust. mgnt -> role of science
    TopicModel[[2]]$topic.proportion[3], #conserv -> agriculture
    (TopicModel[[1]]$topic.proportion[7]/(TopicModel[[1]]$topic.proportion[1]+TopicModel[[1]]$topic.proportion[2]+TopicModel[[1]]$topic.proportion[4]+TopicModel[[1]]$topic.proportion[7]))*TopicModel[[2]]$topic.proportion[1], #conservation -> role of science
    (TopicModel[[1]]$topic.proportion[3]/(TopicModel[[1]]$topic.proportion[3]+TopicModel[[1]]$topic.proportion[4]))*TopicModel[[2]]$topic.proportion[6], #valuation -> valuation
    (TopicModel[[1]]$topic.proportion[5]/(TopicModel[[1]]$topic.proportion[3]+TopicModel[[1]]$topic.proportion[5]))*TopicModel[[2]]$topic.proportion[6], #global awaren. -> valuation
    (TopicModel[[1]]$topic.proportion[4]/(TopicModel[[1]]$topic.proportion[1]+TopicModel[[1]]$topic.proportion[2]+TopicModel[[1]]$topic.proportion[4]+TopicModel[[1]]$topic.proportion[7]))*TopicModel[[2]]$topic.proportion[1], #global awaren. -> role of science
    (TopicModel[[1]]$topic.proportion[4]/(TopicModel[[1]]$topic.proportion[4]+TopicModel[[1]]$topic.proportion[9]))*TopicModel[[2]]$topic.proportion[9], #global awaren. -> freshwater
    (TopicModel[[1]]$topic.proportion[4]/(TopicModel[[1]]$topic.proportion[4]+TopicModel[[1]]$topic.proportion[6]))*TopicModel[[2]]$topic.proportion[2], #global awaren. -> marine
    
    #2nd period edges
    TopicModel[[3]]$topic.proportion[4], #marine
    TopicModel[[3]]$topic.proportion[7], #freshwater
    (TopicModel[[2]]$topic.proportion[4]/(TopicModel[[2]]$topic.proportion[3]+TopicModel[[2]]$topic.proportion[4]))*TopicModel[[3]]$topic.proportion[6], #pollination
    (TopicModel[[2]]$topic.proportion[8]/(TopicModel[[2]]$topic.proportion[3]+TopicModel[[2]]$topic.proportion[8]))*TopicModel[[3]]$topic.proportion[5], #soils -> soils
    TopicModel[[3]]$topic.proportion[8], #forests
    (TopicModel[[2]]$topic.proportion[7]/(TopicModel[[2]]$topic.proportion[5]+TopicModel[[2]]$topic.proportion[7]))*TopicModel[[3]]$topic.proportion[9], #forests -> urban
    (TopicModel[[2]]$topic.proportion[5]/(TopicModel[[2]]$topic.proportion[1]+TopicModel[[2]]$topic.proportion[5]+TopicModel[[2]]$topic.proportion[6]))*TopicModel[[3]]$topic.proportion[2], #land cover -> assess
    TopicModel[[3]]$topic.proportion[3], #land cover -> land cover
    (TopicModel[[2]]$topic.proportion[5]/(TopicModel[[2]]$topic.proportion[5]+TopicModel[[2]]$topic.proportion[7]))*TopicModel[[3]]$topic.proportion[9], #land cover -> urban
    (TopicModel[[2]]$topic.proportion[3]/(TopicModel[[2]]$topic.proportion[1]+TopicModel[[2]]$topic.proportion[3]))*TopicModel[[3]]$topic.proportion[1], #agriculture -> governance
    (TopicModel[[2]]$topic.proportion[3]/(TopicModel[[2]]$topic.proportion[3]+TopicModel[[2]]$topic.proportion[8]))*TopicModel[[3]]$topic.proportion[5], #agriculture -> soils
    (TopicModel[[2]]$topic.proportion[3]/(TopicModel[[2]]$topic.proportion[3]+TopicModel[[2]]$topic.proportion[4]))*TopicModel[[3]]$topic.proportion[6], #agriculture -> pollination
    (TopicModel[[2]]$topic.proportion[1]/(TopicModel[[2]]$topic.proportion[1]+TopicModel[[2]]$topic.proportion[5]+TopicModel[[2]]$topic.proportion[6]))*TopicModel[[3]]$topic.proportion[2], #role of science -> assess
    (TopicModel[[2]]$topic.proportion[1]/(TopicModel[[2]]$topic.proportion[1]+TopicModel[[2]]$topic.proportion[3]))*TopicModel[[3]]$topic.proportion[1], # role of science -> governance
    (TopicModel[[2]]$topic.proportion[6]/(TopicModel[[2]]$topic.proportion[1]+TopicModel[[2]]$topic.proportion[5]+TopicModel[[2]]$topic.proportion[6]))*TopicModel[[3]]$topic.proportion[2] #valuation -> assess
  )*10
)

ESnodes$id <- as.character(ESnodes$id)
ESnodes$col <- as.character(ESnodes$col)
ESedges$N1 <- as.character(ESedges$N1)
ESedges$N2 <- as.character(ESedges$N2)
ESnodes$label <- as.character(ESnodes$label)
ESedges$weight <- as.numeric(ESedges$weight)

setwd(paste(file.path(mainDir), "/output/years_plots/", sep = ""))

split.screen(matrix(c(0,0,  1,1,  0,0,  1,1), ncol=4))
screen(1)
sankey(make_sankey(ESnodes, ESedges, y="optimal", break_edges = T))
text(x = 2, y = 1.5, paste("1990-2000                                             2001-2010                                                2011-2016"), 
     cex = 1.0, col = "black", font = 2)

dev.print(file= "sankeyDiag.png", device=png, width=2800, height= 1800, res=300)
close.screen(all = TRUE) 
dev.off()
setwd(file.path(mainDir))

# 4 output -------

# Save the topic model topic descriptions via topwords
for (i in c("1970_2000", "2001_2010", "2011_2016")){
  if (i == "1970_2000") {
    write.table(topwords[[i]], paste(getwd(), "/output/", i, "/topicmodeltopics_", i , ".csv", sep=""),
                sep = ";", row.names = F, col.names = paste("topic: ", as.character(arrange(na.omit(ES[c(1,3)]), Period1)[,1])), qmethod = "double")
  }
  if (i == "2001_2010") {
    write.table(topwords[[i]], paste(getwd(), "/output/", i, "/topicmodeltopics_", i , ".csv", sep=""),
                sep = ";", row.names = F, col.names = paste("topic: ", as.character(arrange(na.omit(ES[c(1,4)]), Period2)[,1])), qmethod = "double")
  }
  if (i == "2011_2016") {
    write.table(topwords[[i]], paste(getwd(), "/output/", i, "/topicmodeltopics_", i , ".csv", sep=""),
                sep = ";", row.names = F, col.names = paste("topic: ", as.character(arrange(na.omit(ES[c(1,5)]), Period3)[,1])), qmethod = "double")
  }
}

rm("topwords")

# create a worksheet with the topics per document for each period
setwd(paste(file.path(mainDir), "/output/", sep = ""))
wb <- loadWorkbook(paste("DocumentTopics", ".xls", sep = ""), create = TRUE)

for (i in c("1970_2000", "2001_2010", "2011_2016")){
  createSheet(wb, i)
  if (i == "1970_2000") {writeWorksheet(wb, left_join(literatureList[[i]][,c(3,10,11,46:48,53:54,65)], ES[c("Topic","Period1")], by=c("TopModelTopic"="Period1"))[,-9], i)}
  if (i == "2001_2010") {writeWorksheet(wb, left_join(literatureList[[i]][,c(3,10,11,46:48,53:54,65)], ES[c("Topic","Period2")], by=c("TopModelTopic"="Period2"))[,-9], i)}
  if (i == "2011_2016") {writeWorksheet(wb, left_join(literatureList[[i]][,c(3,10,11,46:48,53:54,65)], ES[c("Topic","Period3")], by=c("TopModelTopic"="Period3"))[,-9], i)}
}
saveWorkbook(wb)
setwd(file.path(mainDir))

# create a worksheet with the top docs for topics for each period
for (i in c("1970_2000", "2001_2010", "2011_2016")){  
  for (j in 1:3){
    setwd(paste(file.path(mainDir), "/output/", names(topdocsfortopic[j]), "/", sep = ""))
    wb <- loadWorkbook(paste("topdocsfortopics_", names(topdocsfortopic[j]), ".xls", sep = ""), create = TRUE)
    if (i == "1970_2000" & j==1) {
      for (k in (1:K)){
        createSheet(wb, as.character(arrange(na.omit(ES[c(1,3)]), Period1)[k,1]))
        writeWorksheet(wb, as.data.frame(literatureList[j])[as.data.frame(topdocsfortopic[j])[,k], ][,c(3,46,10,11,53,54,56,23)], as.character(arrange(na.omit(ES[c(1,3)]), Period1)[k,1]))
      }
    }
    if (i == "2001_2010" & j==2) {
      for (k in (1:K)){
        createSheet(wb, as.character(arrange(na.omit(ES[c(1,4)]), Period2)[k,1]))
        writeWorksheet(wb, as.data.frame(literatureList[j])[as.data.frame(topdocsfortopic[j])[,k], ][,c(3,46,10,11,53,54,56,23)], as.character(arrange(na.omit(ES[c(1,4)]), Period2)[k,1]))
      }
    }
    if (i == "2011_2016"& j==3) {
      for (k in (1:K)){
        createSheet(wb, as.character(arrange(na.omit(ES[c(1,5)]), Period3)[k,1]))
        writeWorksheet(wb, as.data.frame(literatureList[j])[as.data.frame(topdocsfortopic[j])[,k], ][,c(3,46,10,11,53,54,56,23)], as.character(arrange(na.omit(ES[c(1,5)]), Period3)[k,1]))
      }
    }
    saveWorkbook(wb)
    setwd(file.path(mainDir))
  }
}

rm("topdocsfortopic")

#further output
for (i in c("1970_2000", "2001_2010", "2011_2016")){ 
  if (i == "1970_2000") {
    #save topic proportions in csv files
    write.table(as.data.frame(cbind(cbind(paste("topic: ", as.character(arrange(na.omit(ES[c(1,3)]), Period1)[,1]))),round(TopicModel[[i]]$topic.proportion*100,2))), paste(getwd(), "/output/", i, "/topicproportions_", i , ".csv", sep=""), sep = ";", row.names = F, col.names = F, qmethod = "double")
    
    #save topic centre coordinates in csv files
    write.table(TopicModel[[i]]$jsPCA, paste(getwd(), "/output/", i, "/topiccentrecoords_", i , ".csv", sep=""), sep = ";", row.names = paste("topic: ", as.character(arrange(na.omit(ES[c(1,3)]), Period1)[,1])), col.names = NA, qmethod = "double")
    
    #save topic distances in csv files
    dist <- (as.data.frame(as.data.frame(as.matrix(dist(TopicModel[[i]]$jsPCA))), row.names=paste("topic: ", as.character(arrange(na.omit(ES[c(1,3)]), Period1)[,1]))))
    names(dist) <- paste("topic: ", as.character(arrange(na.omit(ES[c(1,3)]), Period1)[,1]))
    write.table(dist, paste(getwd(), "/output/", i, "/topicdistances_", i , ".csv", sep=""), sep = ";", col.names = NA, qmethod = "double")
  }
  
  if (i == "2001_2010") {
    #save topic proportions in csv files
    write.table(as.data.frame(cbind(cbind(paste("topic: ", as.character(arrange(na.omit(ES[c(1,4)]), Period2)[,1]))),round(TopicModel[[i]]$topic.proportion*100,2))), paste(getwd(), "/output/", i, "/topicproportions_", i , ".csv", sep=""), sep = ";", row.names = F, col.names = F, qmethod = "double")
    
    #save topic centre coordinates in csv files
    write.table(TopicModel[[i]]$jsPCA, paste(getwd(), "/output/", i, "/topiccentrecoords_", i , ".csv", sep=""), sep = ";", row.names = paste("topic: ", as.character(arrange(na.omit(ES[c(1,4)]), Period2)[,1])), col.names = NA, qmethod = "double")
    
    #save topic distances in csv files
    dist <- (as.data.frame(as.data.frame(as.matrix(dist(TopicModel[[i]]$jsPCA))), row.names=paste("topic: ", as.character(arrange(na.omit(ES[c(1,4)]), Period2)[,1]))))
    names(dist) <- paste("topic: ", as.character(arrange(na.omit(ES[c(1,4)]), Period2)[,1]))
    write.table(dist, paste(getwd(), "/output/", i, "/topicdistances_", i , ".csv", sep=""), sep = ";", col.names = NA, qmethod = "double")
  }
  
  if (i == "2011_2016") {
    #save topic proportions in csv files
    write.table(as.data.frame(cbind(cbind(paste("topic: ", as.character(arrange(na.omit(ES[c(1,5)]), Period3)[,1]))),round(TopicModel[[i]]$topic.proportion*100,2))), paste(getwd(), "/output/", i, "/topicproportions_", i , ".csv", sep=""), sep = ";", row.names = F, col.names = F, qmethod = "double")
    
    #save topic centre coordinates in csv files
    write.table(TopicModel[[i]]$jsPCA, paste(getwd(), "/output/", i, "/topiccentrecoords_", i , ".csv", sep=""), sep = ";", row.names = paste("topic: ", as.character(arrange(na.omit(ES[c(1,5)]), Period3)[,1])), col.names = NA, qmethod = "double")
    
    #save topic distances in csv files
    dist <- (as.data.frame(as.data.frame(as.matrix(dist(TopicModel[[i]]$jsPCA))), row.names=paste("topic: ", as.character(arrange(na.omit(ES[c(1,5)]), Period3)[,1]))))
    names(dist) <- paste("topic: ", as.character(arrange(na.omit(ES[c(1,5)]), Period3)[,1]))
    write.table(dist, paste(getwd(), "/output/", i, "/topicdistances_", i , ".csv", sep=""), sep = ";", col.names = NA, qmethod = "double")
  }
}

rm("dist")

# edit the html files of the ldavis output by adding:
# <footer>
#   <text>
#     <font size="3">
#       <b>Topic names:</b>
#       as.character(arrange(na.omit(ES[c(1,3)]), Period1)[,1])
#       as.character(arrange(na.omit(ES[c(1,4)]), Period2)[,1])
#       as.character(arrange(na.omit(ES[c(1,5)]), Period3)[,1])
#     </font>
#   </text>

# edit the ldavis.js files of the ldavis output by adding:
# //create new svg ...
#   .attrb(... -80)


rm(list=c("i","j","K"))

save(TopicModel, file=c("TopicModel.RData"))

#done
