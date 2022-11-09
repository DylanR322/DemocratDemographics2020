#### PSCI 107
## Final Project: Cleaning the Election Data
## Dylan Radley

library(rio)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(reactable)
library(RColorBrewer)
library(gghighlight)

setwd("~/Dropbox/PSCI107/Final Project/Data/CleanData/")
ce <- import("~/Dropbox/PSCI107/Final Project/Data/CleanData/2016-2020Election&CensusData.Rdata")
ce.full <- import("~/Dropbox/PSCI107/Final Project/Data/CleanData/Full2016-2020Election&CensusData.Rdata")

# First I am going to filter out Pennsylvania since they had redistricting in 2018, and NC for the same reason in
# 2019. 

ce$district.id
ce <- ce[-c(214:226, 302:316), ]
ce$district.id

# Now I am going to work with median household income, average household income, and income per capita. 

names(ce)
ce <- rename(ce,
             "med.hshld.inc.thousands" = "med.hshld.income",
             "avg.hshld.inc.thousands" = "avg.hshld.income",
             "income.per.capita.thousands" = "income.per.capita")

ce$med.hshld.inc.thousands <- ce$med.hshld.inc.thousands / 1000
ce$avg.hshld.inc.thousands <- ce$avg.hshld.inc.thousands / 1000
ce$income.per.capita.thousands <- ce$income.per.capita.thousands / 1000

# I think we wanna make a few regressions with the variables. I am going to find the coef, r-sq, and p-value of
# each variable, fitting models on pshift16to20. I will also fit models for just 2016, 2018, and 2020. 
# Let's for loop this thing! First, the lmp function (thanks internet) will extract the pvalue for each model.

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

names(ce)
variables <- names(ce[, c(12:15, 17:35)])
coefs.pshift <- rep(NA, length(variables))
r.sqs.pshift <- rep(NA, length(variables))
pvals.pshift <- rep(NA, length(variables))

for(i in 1: length(variables)){
  pshift.modelfl <- lm(pshift.16to20 ~ eval(as.symbol(variables[i])), ce)
  coefs.pshift[i] <- pshift.modelfl$coefficients[2]
  r.sqs.pshift[i] <- summary(pshift.modelfl)$r.squared
  pvals.pshift[i] <- lmp(pshift.modelfl)
}

coefs.plean.16 <- rep(NA, length(variables))
r.sqs.plean.16 <- rep(NA, length(variables))
pvals.plean.16 <- rep(NA, length(variables))

for(i in 1: length(variables)){
  pshift.modelfl <- lm(plean.16 ~ eval(as.symbol(variables[i])), ce)
  coefs.plean.16[i] <- pshift.modelfl$coefficients[2]
  r.sqs.plean.16[i] <- summary(pshift.modelfl)$r.squared
  pvals.plean.16[i] <- lmp(pshift.modelfl)
}

coefs.plean.18 <- rep(NA, length(variables))
r.sqs.plean.18 <- rep(NA, length(variables))
pvals.plean.18 <- rep(NA, length(variables))

for(i in 1: length(variables)){
  pshift.modelfl <- lm(plean.18 ~ eval(as.symbol(variables[i])), ce)
  coefs.plean.18[i] <- pshift.modelfl$coefficients[2]
  r.sqs.plean.18[i] <- summary(pshift.modelfl)$r.squared
  pvals.plean.18[i] <- lmp(pshift.modelfl)
}

coefs.plean.20 <- rep(NA, length(variables))
r.sqs.plean.20 <- rep(NA, length(variables))
pvals.plean.20 <- rep(NA, length(variables))

for(i in 1: length(variables)){
  pshift.modelfl <- lm(plean.20 ~ eval(as.symbol(variables[i])), ce)
  coefs.plean.20[i] <- pshift.modelfl$coefficients[2]
  r.sqs.plean.20[i] <- summary(pshift.modelfl)$r.squared
  pvals.plean.20[i] <- lmp(pshift.modelfl)
}

# Now to make a table out of all of these.

regressions <- cbind.data.frame(variables, coefs.plean.16, coefs.plean.18, coefs.plean.20, r.sqs.plean.16, 
                                pvals.plean.16, r.sqs.plean.18, pvals.plean.18, r.sqs.plean.20, pvals.plean.20,
                                variables, coefs.pshift, r.sqs.pshift, pvals.pshift)

# Now, for fun, let's add a variable to see the largest changes in the coefficients, both absolute and percent.

regressions$change.coef <- (regressions$coefs.plean.20 - regressions$coefs.plean.16)
regressions$change.coef.pct <- ((regressions$coefs.plean.20 - regressions$coefs.plean.16) / abs(regressions$coefs.plean.16)) * 100
options(scipen = 500)

# I am just going to reorder the variables a little to have them work better. 

names(regressions)
regressions <- regressions[, c(1:4, 15:16, 5:14)]

# Now it's time to make some plots, but first I have to make the data longer.

ce.g <- gather(ce, key = "year", value = "p.lean", plean.16, plean.18, plean.20)
ce.g$year[ce.g$year == 'plean.16'] <- 2016
ce.g$year[ce.g$year == 'plean.18'] <- 2018
ce.g$year[ce.g$year == 'plean.20'] <- 2020

ce.g %>% ggplot(aes(x = pct.white, y = p.lean, color = year)) + geom_point() + geom_smooth(method='lm', formula= y~x)
ce.g %>% ggplot(aes(x = income.per.capita.thousands, y = p.lean, color = year)) + geom_point() + geom_smooth(method='lm', formula= y~x)
ce.g %>% ggplot(aes(x = med.hshld.inc.thousands, y = p.lean, color = year)) + geom_point() + geom_smooth(method='lm', formula= y~x)

## Now, let's work at our example piece that is going to highlight things nicely. 

bachelor <- ce.g %>% ggplot(aes(x = bachelor.or.more, y = p.lean, color = year)) + 
  geom_point(pch = 1, alpha = 0.3) + ggtitle("") + xlab("Bachelor's Degree or Higher") + ylab("") + 
  geom_hline(yintercept= 0, linetype="dashed", alpha = 0.5) + scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_x_continuous(labels = function(x) paste0(x, "%")) + geom_smooth(method='lm', formula= y~x, alpha = 0.2) + 
  theme_bw() + theme(legend.position = 'none') + 
  annotate('text', label = c('2016', '2018', '2020'), x = c(65, 65, 65), y = c(-15, -20, -25), 
           color = c('red', 'green', 'blue'), size = 7.5)

median.income <- ce.g %>% ggplot(aes(x = med.hshld.inc.thousands, y = p.lean, color = year)) + 
  geom_point(pch = 1, alpha = 0.3) + ggtitle("") + xlab("Median Household Income in Thousands") + ylab("") + 
  geom_hline(yintercept= 0, linetype="dashed", alpha = 0.5) + scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  scale_x_continuous() + geom_smooth(method='lm', formula= y~x, alpha = 0.2) + theme_bw() + 
  theme(legend.position = 'none') + annotate('text', label = c('2016', '2018', '2020'), x = c(40, 40, 40), 
           y = c(-15, -20, -25), color = c('red', 'green', 'blue'), size = 7.5)

white <- ce.g %>% ggplot(aes(x = pct.white, y = p.lean, color = year)) + geom_point(pch = 1, alpha = 0.3) + 
  ggtitle("Predicting Partisan Lean in House Districts") + geom_hline(yintercept= 0, linetype="dashed", alpha = 0.5) +
  xlab("Percent White") + ylab("Partisan Lean of the District") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + scale_x_continuous(labels = function(x) paste0(x, "%")) +
  geom_smooth(method='lm', formula= y~x, alpha = 0.2) + theme_bw() + theme(legend.position = 'none') +
  annotate('text', label = c('2016', '2018', '2020'), x = c(15, 15, 15), 
           y = c(-15, -20, -25), color = c('red', 'green', 'blue'), size = 7.5)

grid.arrange(white, bachelor, median.income, ncol=3)
presentation.visual <- arrangeGrob(white, bachelor, median.income, ncol = 3)
ggsave(file="presentationvisual.png", presentation.visual, width = 13, height = 7.5, units = 'in')

# I am going to make a slightly different version that I think will highlight what I want to highlight a bit clearer. 
# First, I am going to use ce.full, and plot the democratic vote share on the y instead of partisan lean. So, I need 
# to lengthen that one.

ce.g.full <- gather(ce.full, key = "year", value = "d.pct", d.pct.2016, d.pct.2018, d.pct.2020)
ce.g.full$year[ce.g.full$year == "d.pct.2016"] <- 2016
ce.g.full$year[ce.g.full$year == 'd.pct.2018'] <- 2018
ce.g.full$year[ce.g.full$year == 'd.pct.2020'] <- 2020

# Now, I need to get rid of 2018.

names(ce.g.full)
ce.g.fullreduced <- ce.g.full[ce.g.full$year != 2018, ]

# Now to make the graph!

white.reduc <- ce.g.fullreduced %>% ggplot(aes(x = pct.white, y = d.pct, color = year)) + geom_point(pch = 1, alpha = 0.3) + 
  ggtitle("Predicting Democratic Vote Share in House Districts") + geom_hline(yintercept= 50, linetype="dashed", alpha = 0.5) +
  xlab("Percent White") + ylab("Democratic Vote Share") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + scale_x_continuous(labels = function(x) paste0(x, "%")) +
  geom_smooth(method='lm', formula= y~x, alpha = 0.2) + theme_bw() + theme(legend.position = 'none') +
  annotate('text', label = c('2016', '2020'), x = c(15, 15), 
           y = c(35, 38), color = c('red', 'dark cyan'), size = 7.5)

bachelor.reduc <- ce.g.fullreduced %>% ggplot(aes(x = bachelor.or.more, y = d.pct, color = year)) + geom_point(pch = 1, alpha = 0.3) + 
  ggtitle("") + geom_hline(yintercept= 50, linetype="dashed", alpha = 0.5) +
  xlab("Bachelor's Degree or Higher") + ylab("") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + scale_x_continuous(labels = function(x) paste0(x, "%")) +
  geom_smooth(method='lm', formula= y~x, alpha = 0.2) + theme_bw() + theme(legend.position = 'none') +
  annotate('text', label = c('2016', '2020'), x = c(65, 65), 
           y = c(40, 43), color = c('red', 'dark cyan'), size = 7.5)

grid.arrange(white.reduc, bachelor.reduc, ncol = 2)
presentation.visual.Rreduc <- arrangeGrob(white.reduc, bachelor.reduc, ncol = 2)
ggsave(file="presentationvisual.Rreduc.png", presentation.visual.Rreduc, width = 12, height = 7.5, units = 'in')


# Now to make the big super table that has all of the changes from 2016 to 2020. First I am going to select
# for the things that I need in the table.

regressions.t <- regressions[c(1:20, 22:23), c(1, 2, 4, 6) ]

vars.tablenames <- c("% Less Than HS Diploma", "% Only HS Diploma", "% Some College", "% Bachelor's Degree or higher",
                     "Unemployment Rate", "Median Household Income (thousands)", "Average Household Income (thousands)",
                     "% on Public Assistance", "Income per Capita (thousands)", "Adult Poverty Rate", "Senior Poverty Rate",
                     "% Uninsured", "% on Public Insurance", "% on Private Insurance", "% not Hispanic", "% White", "% Black",
                     "% American Indian & Alaska Native", "% Asian", "% Hawaiian", "% Two or More Races", "% Hispanic")
regressions.t$variables <- vars.tablenames

# Now to make our color palette and then the graph

RdWhBl <- function(x) rgb(colorRamp(c("red2", "#ffffff", "royalblue1"))(x), maxColorValue = 255)

reactable(regressions.t,  
          columns = list(
            variables = colDef(name = "Variable"),
            coefs.plean.16 = colDef(format = colFormat(digits = 3), name = "Correlation w/Democratic Vote Share 2016"),
            coefs.plean.20 = colDef(format = colFormat(digits = 3), name = "Correlation w/Democratic Vote Share 2020"),
            change.coef.pct = colDef(style = function(value) {
              if (!is.numeric(value)) return()
              normalized <- (value - min(regressions$change.coef.pct)) / (min(regressions$change.coef.pct) * -2) 
              color <- RdWhBl(normalized)
              list(background = color)
            },
            format = colFormat(digits = 2), name = "% Change 2016-2020")
          ), compact = TRUE, pagination = FALSE, bordered = TRUE)

# I'll export this table with dimensions 1150 by 655 to put it in the paper. Now to make another graph I 
# thought of.

# Now I am going to make the graphs of partisan shifts alongside education and median income, highlighting the
# districts that had the most extreme shifts. I will also make them so that they can be put together. 

# First I need to a way to find the median and standard deviation of the partisan shifts and the education and median
# income variables.

median(ce$pshift.16to20, na.rm = T)
sd(ce$pshift.16to20, na.rm = T)
median(ce$pshift.16to20, na.rm = T)
sd(ce$pshift.16to20, na.rm = T)

# Now to make the graphs themselves.

educandshift <- ce %>% ggplot(aes(x = bachelor.or.more, y = pshift.16to20)) + geom_point(pch = 1, alpha = 0.8) + 
  ggtitle("Higher Education and Partisan Shift") +
  xlab("% of Population with a Bachelor's Degree or Higher") + ylab("Partisan Shift 2016-2020") + geom_vline(xintercept = 51, linetype = "dashed") + 
  geom_vline(xintercept =  29.76, linetype = "dashed") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + scale_x_continuous(labels = function(x) paste0(x, "%")) +
  theme_bw() + theme(legend.position = 'none') + gghighlight(pshift.16to20 >= 12 | pshift.16to20 <= -10, label_key = district.id, use_direct_label = TRUE) +
  annotate(geom = "text",
           label = c("Median Level of Education", "Very High Levels of Education"), x = c(31, 53), y = c(0, 0), angle = 90, vjust = 1, size = 5)

ggsave(file="educandshiftv2.png", educandshift, width = 7.5, height = 7.5, units = 'in')

medincandshift <- ce %>% ggplot(aes(x = med.hshld.inc.thousands, y = pshift.16to20)) + geom_point(pch = 1, alpha = 1) + 
  ggtitle("Median Household Income and Partisan Shift") +
  xlab("Median Houshold Income in Thousands") + ylab("") + geom_vline(xintercept = 58.96, linetype = "dashed") + 
  geom_vline(xintercept =  93.22, linetype = "dashed") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_bw() + theme(legend.position = 'none') + gghighlight(pshift.16to20 >= 12 | pshift.16to20 <= -8.95, label_key = district.id, use_direct_label = TRUE) +
  annotate(geom = "text",
           label = c("Median Income", "Very High Income"), x = c(61, 95), y = c(0, 0), angle = 90, vjust = 1, size = 5)

ggsave(file="medincandshiftv3.png", medincandshift, width = 7.5, height = 7.5, units = 'in')

# Let's combine them now.

grid.arrange(educandshift, medincandshift, ncol = 2)
inceduccombined <- arrangeGrob(educandshift, medincandshift, ncol = 2)
ggsave(file="inceduccombined.png", inceduccombined, width = 15, height = 7.5, units = 'in')

# Now I am going to look a little bit more closely at a handful of house districts. 

ce.ext <- ce[ce$pshift.16to20 >= 12 | ce$pshift.16to20 <= -8.95, ]
ce.ext <- na.omit(ce.ext)

# I am going to focus in on Georgia 6th and Ohio 13th and compare them to the median district.

names(ce)
median.district <- rep(NA, 25)
median.district[1] <- "Median House District"
median.district[2:25] <- apply(ce[,12:35], 2, FUN = median)
std.devs <- rep(NA, 25)
std.devs[1] <- "standard.devs"
std.devs[2:25] <- apply(ce[,12:35], 2, FUN = sd)

GA.and.OH <- rbind.data.frame(median.district, std.devs, ce[ce$district.id == "GA006", c(1, 12:35)], 
                              ce[ce$district.id == "OH013", c(1, 12:35)])

# Now to make this into a table. I am going to modify GA.and.OH down to what I want.

names(GA.and.OH)
GA.and.OH <- GA.and.OH[c(1, 3:4) , c(1, 5, 8, 9, 11, 18:25)]

# I am going to make some vectors and put them in a new, longer table to fit the format I want to put in the table.

vars.gaoh <- c("% Bachelor's Degree or higher","Median Household Income (thousands)", "Average Household Income (thousands)",
               "Income per Capita (thousands)", "% White", "% Black", "% American Indian & Alaska Native", 
               "% Asian", "% Hawaiian", "% Other Race", "% Two or More Races", "% Hispanic")
ga06 <- as.numeric(GA.and.OH[2, 2:13])
oh13 <- as.numeric(GA.and.OH[3, 2:13])
meddist <- as.numeric(GA.and.OH[1, 2:13])

GA.and.OH.c <- cbind.data.frame(vars.gaoh, meddist, ga06, oh13)

reactable(GA.and.OH.c, 
          columns = list(
            vars.gaoh = colDef(name = ""),
            meddist = colDef(format = colFormat(digits = 2), name = "Median House District"),
            oh13 = colDef(format = colFormat(digits = 2), name = "Ohio 13th"),
            ga06 = colDef(format = colFormat(digits = 2), name = "Georgia 6th")
          ), pagination = FALSE, compact = TRUE, bordered = TRUE)

# I can't figure out how to save these, so I will just export it to my clipboard.

# Lastly, I am going to confirm how many of the extreme-shifting districts are in the top 40 in education
# and median income (which is above the 90th percentile)

names(ce)
dists.by.med.inc <- ce[rev(order(ce$med.hshld.inc.thousands)), c(1, 24)]
dists.by.educ <- ce[rev(order(ce$bachelor.or.more)), c(1, 21)]

extreme.dem.dists <- na.omit(ce$district.id[ce$pshift.16to20 >= 12])

dists.by.educ[c(1:13), ]
extreme.dem.dists %in% dists.by.educ[c(1:40), 1]
extreme.dem.dists %in% dists.by.med.inc[c(1:40), 1]

# So 6/13 of the top shifters are in the top 40 districts by % with bachelor's, and 5/13 (those same districts
# minus one) are also in the top 40 districts by income per capita.

# This was a project and a half, but it was a lot of fun to put together. That's all for this time, for the last time!
# Thanks so much to Nic (truly fantastic TA), and Dr. Pettigrew and Dr. Trussler, your class was a highlight of my week
# and taught me so much!


