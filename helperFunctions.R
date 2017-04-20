# Helper function to remove leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Helper function for extracting countries and cities from AuthorAddress
get_location <- function(x) {
  country <- NA
  city <- NA
  if (x != "") {
    x <- gsub("\\[.*?\\]", "", x)
    x <- unlist(strsplit(x, ";"))
    x <- x[x != " "]
    cities <- sapply(x, function(x) tail(unlist(strsplit(x, ",")), 2))
    city <- apply(cities, 2, function(x) gsub(".*[0-9]+ ", "", x[1]))
    city <- sapply(city, trim)
    #   country <- gsub(" ", "", cities[2, ])
    country <- sapply(cities[2, ], trim)
    return(paste(paste(city, country, sep = ","), collapse = ";"))
  }
  else {
    return(NA)
  }
}

# Helper function to construct strings
makeRef <- function(x) {
  refstring <- getName(x)
  if (!is.na(x["YearPublished"])) {
    refstring <- paste(refstring, x["YearPublished"], sep = ", ")
  }
  if (x["SourceAbbreviation"] != "") {
    refstring <- paste(refstring, x["SourceAbbreviation"], sep = ", ")
  }
  if (!is.na(x["Volume"])) {
    refstring <- paste(refstring, ", V", x["Volume"], sep = "")
  }
  if (!is.na(x["BeginningPage"])) {
    refstring <- paste(refstring, ", P", x["BeginningPage"], sep = "")
  }
  if (x["DOI"] != "") {
    refstring <- paste(refstring, ", DOI ", x["DOI"], sep = "")
  }
  return(refstring)
}

# Helper function to extract the name of first author
getName <- function(x) {
  name = NA
  try( {
    names <- unlist(strsplit(x["AuthorFullName"], ";"))
    names <- names[1]
    names <- unlist(strsplit(names, " "))
    name <- names[1]
    name <- gsub(",", "", name)
    if (length(names) > 1) {
      name <- paste(name, substring(names[2], 1, 1))
    }
    if (length(names) > 2) {
      name <- paste(name, substring(names[3], 1, 1), sep = "")
    }
  } )
  return(name)
}

# Helper function to extract DOIs
getDOIs <- function(x) {
  if (length(x) == 2) {
    return(x[2])
  } else {
    return(NA)
  }
}

# Helper function to extract years
getYear <- function(x) {
  year = NA
  if (length(x) > 1) {
    year = as.numeric(x[2])
  }
  return(year)
}
