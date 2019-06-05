# Load library functons
library(klassR)

# Print out lists of classification families
ListFamily()
ListFamily(codelists=TRUE)
ListFamily(family = 3)


# Print out list av all classifications
K <- ListKlass()
View(K)


# Search for a classification
SearchKlass("kommune")
SearchKlass("kommune", size = 5)
SearchKlass("municipality", size = 5)
SearchKlass("*stfold", size = 10) #øæå ikke mulig



# Search for correspondence tables for a classification
CorrespondList(klass = 7) #occupation



# Get classification codes
sn <- GetKlass(klass = 7) # Get classification for occupation classifications
sn <- GetKlass(klass = 7, language = "en") # Get classification for occupation classifications in English
sn <- GetKlass(klass = 7, output_level = 2) # Get classifications for level 2 only
sn <- GetKlass(klass = 131, date = "2007-01-01") # Get classification for municipality on a specified date (yyyy-mm-dd)
sn <- GetKlass(klass = 131, date = c("2007-01-01", "2018-01-01")) # Get classifications for valid between two dates
sn <- GetKlass(klass = 131, date = c("2015-01-01", "2018-01-01"), correspond = TRUE) # Get classifications for correspondence between two dates
sn <- GetKlass(klass = 7, correspond = 145, date = "2018-01-01") # Get correspondence table between two versions of occupation classifications

# Load example data
data(klassdata)
View(klassdata)

# Check which heirachy level data is
sn <- GetKlass(klass = "6", date = "2007-01-01")
lev <- levelCheck(x = klassdata$nace5, klass_data = sn)

# Check formatting of data compared with a classification
formattering(x = klassdata$nace5, input_level = lev, klass = 6, klass_data = sn)

# Municipality example
sn <- GetKlass(klass = "131", date = "2007-01-01")
lev <- levelCheck(x = klassdata$kommune2, klass_data = klass_data)
formattering(x = klassdata$kommune2, input_level = lev, klass = 131, klass_data = sn)

# Levels
klass_data <- GetKLASS(klass = "6")
Levels(input_level = 5, output_level = 2, klass_data = klass_data)

klass_data <- GetKLASS(klass = "6")
Levels(input_level = 5, output_level = c(2, 3), klass_data = klass_data)


# Apply a classification code to a variable
klassdata$nacenames <- ApplyKlass(klassdata$nace5, klass = 6)
klassdata$nacenames2 <- ApplyKlass(klassdata$nace5, klass = 6, date = "2015-01-01", language = "en")
klassdata$nacenames3 <- ApplyKlass(x=klassdata$nace5, klass = 6, output_level = 2) 

klassdata$kommunenames <- ApplyKlass(klassdata$kommune, klass = 131)
klassdata$kommunenames2 <- ApplyKlass(klassdata$kommune, klass = 131, date = "2015-01-01")
klassdata$kommunenames3 <- ApplyKlass(klassdata$kommune, klass = 131, date = c("2015-01-01", "2010-01-01"), correspond = TRUE)
klassdata$kommunecode2 <- ApplyKlass(klassdata$kommune, klass = 131, output = "code",
                                date = c("2015-01-01", "2019-01-01"), correspond = TRUE)

klassdata$occupation_names <- ApplyKlass(klassdata$occupation, klass = 7, date = "2015-01-01")
klassdata$occupation_names <- ApplyKlass(klassdata$occupation, klass = 7, format = F)
klassdata$occupation_old <- ApplyKlass(klassdata$occupation, klass = 7, correspond = 145, format = F)

klassdata$eduNames <- ApplyKlass(klassdata$education, klass = 66, date = "2015-01-01")

