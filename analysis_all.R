########################################################################################
# Bibliometric Analysis of Circular Economy (CE), Green Economy (GE), Bioeconomy (BE)
# discourses for a systematic comparison between the content of research on these topics
# Script authors: N. Droste (nils.droste@ufz.de), D. D'Amato (dalia.damato@helsinki.fi), 
# Jess Joan ()
# an adaptation of the nails project source code: http://nailsproject.net/ 
########################################################################################

# -1 setting wd, loading packages, preps -----------------------------------------------

mainDir <- "/home/droste/Dropbox/Dokumente/doctorate/ecosystem services/data"
subDir1 <- "output"
subDir2 <- "all"

#dir.create(file.path(mainDir, subDir1, subDir2))
setwd(file.path(mainDir))

# Loading libraries
library(splitstackshape)
library(reshape)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(grid)
library(igraph)
library(knitr)
#library(ldatuning)
library(SnowballC)
library(tm)
library(lda)
library(LDAvis)
library(rworldmap)

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

  # # Check whether to use topic modeling
  # enableTM <- TRUE

# Load variable names
fieldtags <- read.csv(paste(getwd(), "/fieldtags.csv", sep=""), header = T, sep = ";")
filelist <- list()
literatureList <- list()

#Loop over three topics
for (i in c("ES")){

#List files in input folder
filelist[[i]] <- list.files(paste(getwd(), "/input/all/", sep=""), full.names = T)
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
  
}

rm(list = c("data.names", "fields", "fieldtags", "file", "filelist", "i", "id", "literature", "tags"))

# check number of obs
#lapply(literatureList, nrow)

# 1 write seperate csv files for different analytical topics ---------------------------------

##LOCATIONS
for (i in c("ES")){

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
  write.table(locations, paste(getwd(), "/output/all/locations_", i, ".csv", sep=""),
              sep = ";", row.names = F, qmethod = "double")
  
  #remove temp data
  rm(list = c("locations", "locationList"))
}

##KEYWORDS
for (i in c("ES")){
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
  write.table(literatureByKeywords, paste(getwd(), "/output/all/literature_by_keywords_", i, ".csv", sep=""),
              row.names = F, sep = ';', qmethod = "double") 
  
  #remove temp data
  rm(list = c("literatureByKeywords", "using_KeywordsPlus"))
}  


#SAVE LITERATURE FILE
for (i in c("ES")){
  # Save the literature as a single csv-file literature.csv.
  write.table(literatureList[[i]][nchar(literatureList[[i]]$Abstract) > 0,], paste(getwd(), "/output/all/literature_", i, ".csv", sep=""),
              sep = ";", row.names = F, qmethod = "double")
}

rm(literatureList)

# 2 Topic modelling ---------------------------------------------------------------------------------
setwd(file.path(mainDir))
for (i in c("ES")){  
    # Do topic modeling on abstracts using the lda libraries (adding them as a new column)
    source(paste(getwd(), "/topicmodel_all.R", sep = ""), chdir = T)
    
    # Add top topic to main document
    literature_ES$TopicModelTopic <- tfdDF$toptopic
    
    # Save the topic model topic descriptions
    write.table(topwords, "topicmodeltopics_ES.csv",
                sep = ";", row.names = F, qmethod = "double")
    
    outDir = paste(getwd(),"/output/all/topicmodelvis_", "ES" , sep = "")
    
    # HTML output
    serVis(json, out.dir = outDir, open.browser = FALSE)
    
    # Freeing up memory
    rm(list = c("json", "outDir", "topwords", "tfdDF"))
  }

# 3 plotting -----------------------------------------------------------------------

## Publication years

setwd(paste(getwd(), "/output/all/", sep = ""))
literature_ES <- read.csv("literature_ES.csv", sep=";")
setwd(mainDir)

yearPlot <- ggplot(literature_ES, aes(YearPublished)) +
  geom_histogram(binwidth = 1, fill = "darkgrey") +
  xlab("year") +
  ylab("article count") +
  scale_y_continuous(limits = c(0, 3000)) +
  scale_x_continuous(limits = c(1990, 2017), breaks=c(1990,2000,2010,2017))

#hist(literatureList$ES$YearPublished)

setwd(paste(getwd(), "/output/all/plots/", sep = ""))
png(file="yearPlot_ES.png", width=2000, height= 700, res=300)
yearPlot
dev.off()
setwd(mainDir)

#KEYWORDS

setwd(paste(getwd(), "/output/all/", sep = ""))
keywordPlotList <- list()
keywordDFList <- read.csv(paste("literature_by_keywords_", i, ".csv", sep = ""), sep = ';', stringsAsFactors=FALSE) 
setwd(mainDir)
  
x1 <- keywordDFList %>% group_by(AuthorKeywords) %>% summarize(freq = n()) %>% arrange(desc(freq))
x1 <- as.data.frame(head(x1, n = 10))
keywordPlot_ES<- ggplot(x1, aes (reorder(x1[,1], x1[,2]), freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Number of occurences", x = "Keywords") +
  ggtitle("ES top keywords")


setwd(paste(mainDir, "/output/all/plots/", sep = ""))
keywordPlot_ES
dev.print(file= "keywordPlot_ES.png", device=png, width=1000, height= 350)
dev.off()
setwd(mainDir)

#LOCATIONS
locList = list()
setwd(paste(mainDir, "/output/all/", sep = ""))
locList <- read.csv(paste("locations_", i, ".csv", sep = ""), sep = ';', stringsAsFactors=FALSE) 
for (j in c("USA", "CA", "CT" ,"DC", "DE" ,"GA", "IL", "MD", "MI","MN", "NC", "NE", "NY", "PA", "TN","VA", "WI", "WY")){
  locList[grep(j, locList$country), "country"] <- "United States"
  }
locList$country=gsub("Peoples R China", "China", locList$country)
locList[grep("Scotland|England|Wales|North Ireland", locList$country), "country"] <- "United Kingdom"
locList[grep("Guadeloupe", locList$country), "country"] <- "Guadeloupe"
locList[grep("Polynesia", locList$country), "country"] <- "French Polynesia"
locList[grep("U Arab Emirates", locList$country), "country"] <- "United Arab Emirates"
#locList[grep("Trinid | Trinidad", locList$country), "country"] <- 'Trinidad'
locList[grep("Tobago", locList$country), "country"] <- 'Tobago'
locList[grep("Bosnia & Herceg", locList$country), "country"] <- 'Bosnia and Herzegovina'
locList[grep("Byelarus", locList$country), "country"] <- 'Belarus'
locList[grep("Cote Ivoire", locList$country), "country"] <- 'Ivory Coast'
locList[grep("Papua N Guinea", locList$country), "country"] <- 'Mongolia'
locList[grep("Mongol Peo Rep", locList$country), "country"] <- 'Papua New Guinea'
locList[grep("Surinam", locList$country), "country"] <- 'Suriname'
locList[grep("W Ind Assoc St", locList$country), "country"] <- 'Turks and Caicos Islands'
locList[grep("Dominican", locList$country), "country"] <- 'Dominican Republic'
locList[grep("Surinam", locList$country), "country"] <- 'Suriname'
locList[grep("Marshall", locList$country), "country"] <- 'Marshall Islands'
locList[grep("Martinique", locList$country), "country"] <- 'Martinique'
locList[grep("Micronesia", locList$country), "country"] <- 'Micronesia'
locList[grep("Reunion", locList$country), "country"] <- 'Reunion'
locList[grep("Sao Tome", locList$country), "country"] <- 'Sao Tome and Principe'
locList[grep("Zaire", locList$country), "country"] <- 'Democratic Republic of the Congo'
locList$country = tolower(locList$country)
locList <- na.omit(locList)

es <- as.data.frame(table(locList$country)); colnames(es)[1] <- "country"

sort(table(locList$country), dec=T)

location_ES <- joinCountryData2Map(es, joinCode = "NAME", nameJoinColumn = "country", verbose=TRUE)
#MISSING COUNTRIES!"!!!

setwd(paste(mainDir, "/output/all/plots/", sep = ""))

mapCountryData(location_ES, nameColumnToPlot="Freq", mapTitle="ES", addLegend=T , colourPalette="white2Black", catMethod='logFixedWidth')
#do.call( addMapLegend, c(location_CE , legendWidth=0.5, legendMar = 2))
dev.print(file= "locMapES.png", device=png, width=800, height= 500)

dev.off()

rm(list=c("AuthorKeywords", "es", "i", "j", "keywordDFList", "keywordPlot_ES", "location_ES",
          "locList", "mainDir", "subDir1", "subDir2", "nPanels", "yearPlot"))
