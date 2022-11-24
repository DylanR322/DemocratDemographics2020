#### PSCI 107
## Final Project: Cleaning the Election Data
## Dylan Radley

library(rio)
library(plyr)
library(dplyr)
library(tidyr)

rm(list = ls())

setwd("~/Desktop/GitHub/psci-107-project/Code & Data/Data/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.


## First, let's clean the 2020 Data

e20 <- import("2020-11-03.RDS") 
# note, the above address and all other addresses in import must be changed if this data is downloaded onto another computer 

e20 <- e20$cnty

# Drop Meta and keep Cand out of the two elements: Cand contains the info about election results we are interested in.

e20 <- e20$cand
e20.orig <- e20

# Break up the race.id column to extract useful information.

e20 <- separate(e20, race.id, into = c("date","state","race"), sep="~")

# Leave only house races

e20 <- e20[e20$race=="H",]

### (2020) Create a variable to merge the election years with --------------------
# First, break the state variable into state and then house district, using

unique(e20$state) # the state variable contains both states and house districts.

# Use regex to insert underscores after the state codes...

e20$state <- sub( '(?<=.{2})', '_', e20$state, perl=TRUE)

# ... then separate them into two new variables! 

e20 <- separate(e20, state, into = c("state","house.district"), sep="_")

# The below creates a new variable, district.id, a unique identifier to merge with later.

e20$district.id <- paste(e20$state, e20$house.district, sep="")

### (2020) Reorganize the data --------------------

# Now we need to reorganize this so it is organized by house districts instead of candidates.
# Cutting down the parties to just Dems and Republicans

e20 <- e20[e20$party=="Dem" | e20$party=="Rep",]

# I am going to spread by party... 

e20 <- spread(e20, 
              key="party",
              value="vote")

#...and then aggregate by district.

e20 <- ddply(e20, .(district.id), summarize, dem.20 = sum(Dem, na.rm = T), rep.20 = sum(Rep, na.rm = T))

### (2016) Follow the Same Process! --------------------
# No commenting, because the process is identical since the data is from the same source, in the same format.

e16 <- import("2016-11-08.RDS")

e16 <- e16$cnty

e16 <- e16$cand
e16.orig <- e16

e16 <- separate(e16, race.id, into = c("date","state","race"), sep="~")

e16 <- e16[e16$race=="H",]

unique(e16$state)

e16$state <- sub( '(?<=.{2})', '_', e16$state, perl=TRUE)
e16 <- separate(e16, state, into = c("state","house.district"), sep="_")

e16$district.id <- paste(e16$state, e16$house.district, sep="")

e16 <- e16[e16$party=="Dem" | e16$party=="Rep",]

e16 <- spread(e16, 
              key="party",
              value="vote")

e16 <- ddply(e16, .(district.id), summarize, dem.16 = sum(Dem, na.rm = T), rep.16 = sum(Rep, na.rm = T))

### (2018) Follow the Same Process! --------------------
# No commenting, because the process is identical since the data is from the same source, in the same format.

e18 <- import("2018-11-06.RDS")

e18 <- e18$cnty

e18 <- e18$cand
e18.orig <- e18

e18 <- separate(e18, race.id, into = c("date","state","race"), sep="~")

e18 <- e18[e18$race=="H",]

unique(e18$state)

e18$state <- sub( '(?<=.{2})', '_', e18$state, perl=TRUE)
e18 <- separate(e18, state, into = c("state","house.district"), sep="_")

e18$district.id <- paste(e18$state, e18$house.district, sep="")

e18 <- e18[e18$party=="Dem" | e18$party=="Rep",]

e18 <- spread(e18, 
              key="party",
              value="vote")

e18 <- ddply(e18, .(district.id), summarize, dem.18 = sum(Dem, na.rm = T), rep.18 = sum(Rep, na.rm = T))

### Merge All Three Elections + Calc Partisan Shift --------------------

e16and18 <- merge(e16, e18, by = "district.id", all.x = T)
elections <- merge(e16and18, e20, by = "district.id", all.x = T)

## Now to calculate the partisan shift for each house district.
# First, all 0's are coded as NA's; if a Dem or Rep candidate did not get any votes, it will be assumed that
# the other candidate was running unopposed.

elections$dem.16[elections$dem.16 == 0] <- NA
elections$rep.16[elections$rep.16 == 0] <- NA
elections$dem.18[elections$dem.18 == 0] <- NA
elections$rep.18[elections$rep.18 == 0] <- NA
elections$dem.20[elections$dem.20 == 0] <- NA
elections$rep.20[elections$rep.20 == 0] <- NA


# Calculate the vote percentages for each party in every district in each election.

elections$d.pct.2016 <- (elections$dem.16 / (elections$dem.16 + elections$rep.16)) * 100
elections$r.pct.2016 <- (elections$rep.16 / (elections$dem.16 + elections$rep.16)) * 100
elections$d.pct.2018 <- (elections$dem.18 / (elections$dem.18 + elections$rep.18)) * 100
elections$r.pct.2018 <- (elections$rep.18 / (elections$dem.18 + elections$rep.18)) * 100
elections$d.pct.2020 <- (elections$dem.20 / (elections$dem.20 + elections$rep.20)) * 100
elections$r.pct.2020 <- (elections$rep.20 / (elections$dem.20 + elections$rep.20)) * 100

# NOTE: partisan lean is demoratic vote share - 50%, so a partisan shift of 0 is no change. Negative values are shifts
# towards Republicans, and positve values are shifts towards the Democrats. 
# In the paper, changes in democratic vote share are discussed as well.
# Simply subtracting 50 does not affect coefficients in regression outputs, but it does affect intercepts. 

elections$plean.16 <- elections$d.pct.2016 - 50
elections$plean.18 <- elections$d.pct.2018 - 50
elections$plean.20 <- elections$d.pct.2020 - 50

# Next, calculating the partisan shift (pshift) in each house district between the three elections.

elections$pshift.16to18 <- elections$plean.18 - elections$plean.16
elections$pshift.18to20 <- elections$plean.20 - elections$plean.18
elections$pshift.16to20 <- elections$plean.20 - elections$plean.16

# Make a dataset with just the leans and the shifts

unique(elections$district.id)
names(elections)
leans.and.shifts <- elections[, c(1, 14:19)]

### Clean ACS Data --------------------

# Load in the data

acs <- read.csv("ACS2014-2018.csv")
acs.orig <- acs

## Get rid of extra columns that are blank or repeats.
summary(acs)
names(acs)

# Every column between Geo_STUSAB and SE_A00002_001, (6-55) is either irrelevant or totally empty. 
acs <- acs[, c(1:5, 56:124)]

# Next, get rid of redundant columns

names(acs)
redundant.cols <- names(acs[, c(9:18, 29:35, 44:46, 50:52, 54, 56:58, 60:63, 65:70, 72)])

acs <- subset(acs, select = -c(9:18, 29:35, 44:46, 50:52, 54, 56:58, 60:63, 65:70, 72))

# Now I am going to cut down those first few variables and try and hash them into a useful identifier variable. The 
# goal is to get results like "PA005"

unique(acs$Geo_STUSAB)

acs$Geo_STUSAB <- toupper(acs$Geo_STUSAB) # this is the state information

# GEOID contains fips codes, which can be used to pull out house districts

acs$Geo_GEOID <- sub( '(?<=.{9})', '_', acs$Geo_GEOID, perl=TRUE) # put a dash between the house district and the rest of fips

acs <- separate(acs, Geo_GEOID, into = c("val","house.district"), sep="_") # pull out house districts

# Add a zero to every house.district value and then combine it with the state abbreviations.

acs$house.district <- paste0('0', acs$house.district)
acs <- unite(acs, district.id, Geo_STUSAB, house.district, sep = '')

# This works for everything except for at-large districts, so I will just go in and fix Alaska, Delaware, Montana,
# North Dakota, South Dakota, Vermont, and Wyoming manually.

acs$district.id[acs$district.id == "AK000"] <- "AK001"
acs$district.id[acs$district.id == "DE000"] <- "DE001"
acs$district.id[acs$district.id == "MT000"] <- "MT001"
acs$district.id[acs$district.id == "ND000"] <- "ND001"
acs$district.id[acs$district.id == "SD000"] <- "SD001"
acs$district.id[acs$district.id == "VT000"] <- "VT001"
acs$district.id[acs$district.id == "WY000"] <- "WY001"

# Get rid of extra columns in the beginning.

names(acs)
acs <- acs[, c(3, 6:36)]

# Renaming all the variables. 

names(acs)
new.names <- c('district.id', 'population', 'pop.density', 'area.sq.miles', 'pct.not.hisp', 'pct.white', 'pct.black', 
               'pct.amer.ind', 'pct.asian', 'pct.hawaiian', 'pct.other.race', 'pct.two.or.more.races', 'pct.hisp', 
               'pop.>=.25', 'pct.<=.hs', 'pct.hs', 'pct.some.cldg', 'pct.bachelors', 'pct.masters', 'pct.professional', 
               'pct.doctorate', 'civ.pop.lbr.force.16.over', 'unemployment.rate', 'med.hshld.income',
               'avg.hshld.income', 'pct.hshhld.public.assist', 'income.per.capita', 'adult.poverty.rate', 
               'senior.poverty.rate', 'pct.uninsured','pct.public.ins', 'pct.priv.ins')
names(acs) <- new.names

# Collapse the education variables a little, by consolidating bachelor's degree or higher

bachelor.or.more.vars <- c('pct.bachelors', 'pct.masters','pct.professional', 'pct.doctorate')
names(acs)
acs <- acs %>%
  mutate(bachelor.or.more = select(., pct.bachelors:pct.doctorate) %>% rowSums(na.rm = TRUE))

# Remove the other educational attainment variables that make up bachelor.or.more now, and put bachelor or more with other
# educational attainment variables.

names(acs)
acs <- acs[, -c(18:21)]
acs <- acs[, c(1:4, 14:17, 29, 18:28, 5:13)]
names(acs)

### Merge ACS Data with Returns --------------------

census.and.elect <- merge(leans.and.shifts, acs, by = "district.id", all.x = T)
census.and.elect.full <- merge(elections, acs, by = "district.id", all.x = T)

# Save the datasets!

setwd("~/Desktop/GitHub/psci-107-project/Code & Data/Data/Clean Data/") # set it to save in the right location
save(census.and.elect, file="2016-2020Election&CensusData.Rdata")
save(census.and.elect.full, file="Full2016-2020Election&CensusData.Rdata")

# That's all for cleaning!