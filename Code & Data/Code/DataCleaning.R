#### PSCI 107
## Final Project: Cleaning the Election Data
## Dylan Radley

library(rio)
library(plyr)
library(dplyr)
library(tidyr)


## First, let's clean the 2020 Data

e20 <- import("~/Desktop/ThingsToGoInGitHub/psci-107-project/Code & Data/Data/2020-11-03.RDS")

e20 <- e20$cnty

# Drop Meta and keep Cand out of the two elements: Cand contains the info about election results we are interested in.

e20 <- e20$cand
e20.orig <- e20

# The above is what extracts all the useful data into dataframes.

e20 <- separate(
  e20,
  race.id,
  into = c("date","state","race"),
  sep="~"
)

# The above just cleans the data a little bit in a helpful way.
# What if, instead, I just looked at house races?

e20 <- e20[e20$race=="H",]

# Now, we want to be able to create a variable to merge the datasets. However, as it stands now, 
# the "state" variable is actually telling us congressional districts. We need to separate it into two new variables,
# one that says the house district, and one that says the state.

unique(e20$state)

# This code (totally not from stack exchange; I understand how it works) is going to insert a dash between the 
# letters (states) and the numbers (districts), then I can use separate to break them into two variables.
# Special Note: periods are 'special regex characters' that will break separate, so I use a dash instead so it works.

e20$state <- sub( '(?<=.{2})', '_', e20$state, perl=TRUE)
e20 <- separate(
  e20,
  state,
  into = c("state","house.district"),
  sep="_"
)

# The below creates a new variable, district.id, which will create a unique identifier to merge with later.

e20$district.id <- paste(e20$state, e20$house.district, sep="")

# Now we need to wrangle this so it is organized by house districts instead of candidates.
# Cutting down the parties to just Dems and Republicans

e20 <- e20[e20$party=="Dem" | e20$party=="Rep",]

# I am going to spread by party and then aggregate by district.

e20 <- spread(e20, 
              key="party",
              value="vote")



# Now we have all the data we need!

e20 <- ddply(e20, .(district.id), summarize, dem.20 = sum(Dem, na.rm = T), rep.20 = sum(Rep, na.rm = T))

##16: The exact same steps are followed for the e16 data.

e16 <- import("~/Dropbox/PSCI107/Final Project/Data/2016-11-08.RDS")

e16 <- e16$cnty

e16 <- e16$cand
e16.orig <- e16

e16 <- separate(
  e16,
  race.id,
  into = c("date","state","race"),
  sep="~"
)

e16 <- e16[e16$race=="H",]

unique(e16$state)

e16$state <- sub( '(?<=.{2})', '_', e16$state, perl=TRUE)
e16 <- separate(
  e16,
  state,
  into = c("state","house.district"),
  sep="_"
)

e16$district.id <- paste(e16$state, e16$house.district, sep="")

e16 <- e16[e16$party=="Dem" | e16$party=="Rep",]

e16 <- spread(e16, 
              key="party",
              value="vote")

e16 <- ddply(e16, .(district.id), summarize, dem.16 = sum(Dem, na.rm = T), rep.16 = sum(Rep, na.rm = T))


## And then, the same process for 2018

e18 <- import("~/Dropbox/PSCI107/Final Project/Data/2018-11-06.RDS")

e18 <- e18$cnty

e18 <- e18$cand
e18.orig <- e18

e18 <- separate(
  e18,
  race.id,
  into = c("date","state","race"),
  sep="~"
)

e18 <- e18[e18$race=="H",]

unique(e18$state)

e18$state <- sub( '(?<=.{2})', '_', e18$state, perl=TRUE)
e18 <- separate(
  e18,
  state,
  into = c("state","house.district"),
  sep="_"
)

e18$district.id <- paste(e18$state, e18$house.district, sep="")

e18 <- e18[e18$party=="Dem" | e18$party=="Rep",]

e18 <- spread(e18, 
              key="party",
              value="vote")

e18 <- ddply(e18, .(district.id), summarize, dem.18 = sum(Dem, na.rm = T), rep.18 = sum(Rep, na.rm = T))

## Now to merge all three elections!

e16and18 <- merge(e16, e18, by = "district.id", all.x = T)
elections <- merge(e16and18, e20, by = "district.id", all.x = T)


## Now to calculate the partisan shift for each house district.
# First, all 0's are going to be coded as NA's; if a Dem or Rep candidate did not get any votes, the only logical
# explanation is that the other candidate was running unopposed. I tried to do it with a for loop but got a little
# stuck so I am going to do it the simpler way because it's only a few columns.

elections$dem.16[elections$dem.16 == 0] <- NA
elections$rep.16[elections$rep.16 == 0] <- NA
elections$dem.18[elections$dem.18 == 0] <- NA
elections$rep.18[elections$rep.18 == 0] <- NA
elections$dem.20[elections$dem.20 == 0] <- NA
elections$rep.20[elections$rep.20 == 0] <- NA


# Now to calculate the vote percentages for each party in every district in each election.

elections$d.pct.2016 <- (elections$dem.16 / (elections$dem.16 + elections$rep.16)) * 100
elections$r.pct.2016 <- (elections$rep.16 / (elections$dem.16 + elections$rep.16)) * 100
elections$d.pct.2018 <- (elections$dem.18 / (elections$dem.18 + elections$rep.18)) * 100
elections$r.pct.2018 <- (elections$rep.18 / (elections$dem.18 + elections$rep.18)) * 100
elections$d.pct.2020 <- (elections$dem.20 / (elections$dem.20 + elections$rep.20)) * 100
elections$r.pct.2020 <- (elections$rep.20 / (elections$dem.20 + elections$rep.20)) * 100

# I think having the margin might be more useful, but what to call it?
# Also a note that while this is convenient for analysis, when reporting my results later on in the paper I just
# talk about changes in democratic vote share. However, since partisan lean is nothing more than democratic vote
# share minus 50, the interpretations of all coefficients is still valid. That being said, interpretations of 
# intercepts would have to keep that in mind.

elections$plean.16 <- elections$d.pct.2016 - 50
elections$plean.18 <- elections$d.pct.2018 - 50
elections$plean.20 <- elections$d.pct.2020 - 50

# Next, calculating the partisan shift (pshift) in each house district between the three elections.

elections$pshift.16to18 <- elections$plean.18 - elections$plean.16
elections$pshift.18to20 <- elections$plean.20 - elections$plean.18
elections$pshift.16to20 <- elections$plean.20 - elections$plean.16

# I am going to make a dataset with just the leans and shifts. 

unique(elections$district.id)
names(elections)
leans.and.shifts <- elections[, c(1, 14:19)]

# This means that a partisan shift of 0 is no change, and negative values are shifts towards Reps, and positives
# towards Dems. 

## Now, to clean the ACS Data!

# Let's load it.

acs <- read.csv("~/Dropbox/PSCI107/Final Project/Data/ACS2014-2018v2.csv")
acs.orig <- acs
## Let's start by getting rid of all the extra columns. There are like 50 in the beginning that are blank, and also
# cases where things like total population are repeated between variables.

summary(acs)
names(acs)

# First I am going to get rid of every column between Geo_STUSAB and SE_A00002_001, (6-55) because they either contain
# totally irrelevant information or are empty. 

acs <- acs[, c(1:5, 56:124)]

# Next I am going to get rid of columns that repeat, like population, and ones that are redundant, such as the # of men
# when I have the percent, which is what is relevant anyways.

names(acs)
redundant.cols <- names(acs[, c(9:18, 29:35, 44:46, 50:52, 54, 56:58, 60:63, 65:70, 72)])

acs <- subset(acs, select = -c(9:18, 29:35, 44:46, 50:52, 54, 56:58, 60:63, 65:70, 72))

# Now I am going to cut down those first few variables and try and hash them into a useful identifier variable. The 
# goal is to get results like "PA005"

unique(acs$Geo_STUSAB)

acs$Geo_STUSAB <- toupper(acs$Geo_STUSAB)

# With the above we have the state part, and working with the GEOID, which is made with fips codes, I can make the 005
# and so on.

acs$Geo_GEOID <- sub( '(?<=.{9})', '_', acs$Geo_GEOID, perl=TRUE)

acs <- separate(
  acs,
  Geo_GEOID,
  into = c("val","house.district"),
  sep="_"
)

# Now to just add a zero to every house.district value and then combine it with the state abbreviations.

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

# Now to get rid of all those useless columns in the beginning.

names(acs)
acs <- acs[, c(3, 6:36)]

# Now for the fun part: renaming all the variables. 

names(acs)
new.names <- c('district.id', 'population', 'pop.density', 'area.sq.miles', 'pct.not.hisp', 'pct.white', 'pct.black', 
               'pct.amer.ind', 'pct.asian', 'pct.hawaiian', 'pct.other.race', 'pct.two.or.more.races', 'pct.hisp', 
               'pop.>=.25', 'pct.<=.hs', 'pct.hs', 'pct.some.cldg', 'pct.bachelors', 'pct.masters', 'pct.professional', 
               'pct.doctorate', 'civ.pop.lbr.force.16.over', 'unemployment.rate', 'med.hshld.income',
               'avg.hshld.income', 'pct.hshhld.public.assist', 'income.per.capita', 'adult.poverty.rate', 
               'senior.poverty.rate', 'pct.uninsured','pct.public.ins', 'pct.priv.ins')
names(acs) <- new.names

# I did it! Now I am going to collapse the education variables a little, by just consolidating bachelor's degree or
# higher. 

bachelor.or.more.vars <- c('pct.bachelors', 'pct.masters','pct.professional', 'pct.doctorate')
names(acs)
acs <- acs %>%
  mutate(bachelor.or.more = select(., pct.bachelors:pct.doctorate) %>% rowSums(na.rm = TRUE))

# Now I am going to cut out the other variables for educ that make up bachelor.or.more, and also reorder the vars
# so that this new variable is with other like it. 

names(acs)
acs <- acs[, -c(18:21)]
acs <- acs[, c(1:4, 14:17, 29, 18:28, 5:13)]
names(acs)

## Now it's merging time!

census.and.elect <- merge(leans.and.shifts, acs, by = "district.id", all.x = T)
census.and.elect.full <- merge(elections, acs, by = "district.id", all.x = T)

# Now let's save these datasets!

save(census.and.elect, file="~/Dropbox/PSCI107/Final Project/Data/CleanData/2016-2020Election&CensusData.Rdata")
save(census.and.elect.full, file="~/Dropbox/PSCI107/Final Project/Data/CleanData/Full2016-2020Election&CensusData.Rdata")

rm(list=ls())

# That's all for cleaning!