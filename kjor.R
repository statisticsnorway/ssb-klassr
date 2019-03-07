library(klassR)

# hent ut liste ved familie
FamilyList()
FamilyList(codelists=TRUE)
FamilyList(family = 1)


# hent ut all klass
K <- KlassList()
View(K)


# søk på klass
SearchKlass("kommune")
SearchKlass("kommune", size = 5)
SearchKlass("municipality", size = 5)
SearchKlass("*fold", size = 10)


# correspondence tabell liste
CorrespondList(klass = 7)



# Hent ut kode
sn <- GetKLASS(klass = 7) # Get classification for occupation classifications
sn <- GetKLASS(klass = 7, language = "en") # Get classification for occupation classifications in English
sn <- GetKLASS(klass = 7, output_level = 2) # Get classifications for level 2 only
sn <- GetKLASS(klass = 131, date = "2007-01-01") # Get classification for municipality on a specified date
sn <- GetKLASS(klass = 131, date = c("2007-01-01", "2018-01-01")) # Get classifications for valid between two dates
sn <- GetKLASS(klass = 131, date = c("2015-01-01", "2018-01-01"), correspond = TRUE) # Get classifications for correspondence between two dates
sn <- GetKLASS(klass = 7, correspond = 145, date = "2018-01-01") # Get correspondence table between two versions of occupation classifications

# testdata
data(klassdata)
View(klassdata)

#levelCheck
sn <- GetKLASS(klass = "6", date = "2007-01-01")
levelCheck(x = klassdata$nace5, klass_data = sn)

sn <- GetKLASS(klass = "7", date = "2007-01-01")
levelCheck(x = klassdata$occupation, klass_data = sn)

# formattering
klass_data <- GetKLASS(klass = "6", date = "2007-01-01")
input_level <- levelCheck(x = klassdata$nace5, klass_data = klass_data)
formattering(x = klassdata$nace5, input_level = input_level, klass = 6, klass_data=klass_data)

klass_data <- GetKLASS(klass = "7", date = "2007-01-01")
input_level <- levelCheck(x = klassdata$occupation, klass_data = klass_data)
formattering(x = klassdata$occupation, input_level = input_level, klass = 7, klass_data=klass_data)

klass_data <- GetKLASS(klass = "131", date = "2007-01-01")
input_level <- levelCheck(x = klassdata$kommune2, klass_data = klass_data)
formattering(x = klassdata$kommune2, input_level = input_level, klass = 131, klass_data=klass_data)


# Bruk av hoved funksjon
klassdata$nacenames <- KLASS(klassdata$nace5, klass = 6)
klassdata$nacenames2 <- KLASS(klassdata$nace5, klass = 6, date = "2015-01-01")
klassdata$nacenames3 <- KLASS(klassdata$nace5, klass = 6, date = "2015-01-01",
                         output_level = 2, language = "en") ### Not wokring
klassdata$nacenames4 <- KLASS(klassdata$nace5, klass = 6, date = "2015-01-01",
                             output_level = 2, output = "both") ### Not wokring

klassdata$kommunenames <- KLASS(klassdata$kommune, klass = 131)
klassdata$kommunenames2 <- KLASS(klassdata$kommune, klass = 131, date = "2015-01-01")
klassdata$kommunenames3 <- KLASS(klassdata$kommune, klass = 131, date = c("2015-01-01", "2019-01-01"))
klassdata$kommunenames4 <- KLASS(klassdata$kommune, klass = 131, date = c("2015-01-01", "2010-01-01"), correspond = TRUE)
klassdata$kommunecode2 <- KLASS(klassdata$kommune, klass = 131, output = "code",
                                date = c("2015-01-01", "2019-01-01"), correspond = TRUE)
klassdata$kommunecode3 <- KLASS(klassdata$kommune, klass = 131, output = "code",
                                date = c("2015-01-01", "2010-02-01"))

klassdata$occupation_names <- KLASS(klassdata$occupation, klass = 7, date = "2015-01-01")
klassdata$occupation_names <- KLASS(klassdata$occupation, klass = 7, format = F)
klassdata$occupation_old <- KLASS(klassdata$occupation, klass = 7, correspond = 145, format = F)

klassdata$eduNames <- KLASS(klassdata$education, klass = 66, date = "2015-01-01")

