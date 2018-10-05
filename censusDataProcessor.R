rm(list=ls())
library(foreign); library(rgdal)
library(tidycensus); library(tidyverse)

# https://api.census.gov/data/2016/acs/acs5/variables.html
# https://api.census.gov/data/2016/acs/acs5/subject/variables.html
# https://api.census.gov/data/2016/acs/acs5/profile/variables.html
# Unfortunately this only works for 2011-2015 and 2012-2016 ACS.
# I suspect API changes are the issue. May as well only use 2016 then

collect <- get_acs(geography = "tract",
                   state = c(34,42),
                   output = "wide",
                   variables = c(
                     povUniverse = "S1701_C01_001E",
                     povEst = "S1701_C01_042E",
                     femUniverse = "S0101_C01_001E",
                     femEst = "S0101_C03_001E",
                     disabUniverse = "S1810_C01_001E",
                     disabEst = "S1810_C02_001E",
                     lepUniverse = "S1601_C01_001E",
                     lepEst = "S1601_C05_001E"
                   ))
collect2 <- get_acs(geography = "tract",
                    state = c(34,42),
                    output = "wide",
                    variables = c(
                      ethUniverse = "B03002_001E",
                      ethEst = "B03002_012E", # Note that this will calc % Hisp. Was this the intent?
                      youthEst = "B09001_001E",
                      fornUniverse = "B05012_001E",
                      fornEst = "B05012_003E"
                    ))
collect3 <- get_acs(geography = "tract",
                    state = c(34,42),
                    output = "wide",
                    variables = c(
                      olderUniverse = "DP05_0001E", # note that Alex used S0101_C03_001E as universe here
                      olderEst = "DP05_0025E"
                    ))
# Add youthUniverse column in desired spot
collect2$youthUniverse <- collect2$ethUniverse
collect2 <- collect2[c(1:6, 13, 7:12)]
# Merge across
collect2[c("GEOID", "NAME")] <- NULL; collect3[c("GEOID", "NAME")] <- NULL
collect <- cbind(collect, collect2)
collect <- cbind(collect, collect3)
rm(collect2); rm(collect3)
# Clean up fields
collect <- collect[, -( grep("\\M$" , colnames(collect), perl = TRUE))]
# Get state/county info so we can split df by MSA
collect$st <- substr(collect$GEOID, 1, 2)
collect$cty <- substr(collect$GEOID, 3, 5)
collect$stcty <- paste0(collect$st, collect$cty)
# Subset out DVRPC counties
dvrpc <- c("34005", "34007", "34015", "34021",
           "42017", "42029", "42045", "42091", "42101")
collect <- subset(collect, stcty %in% dvrpc)
# Percentages
divisor <- function(i,j) i / j * 100
res <- as.data.frame(mapply(divisor,
                            collect[seq(4, 18, by = 2)],
                            collect[seq(3, 17, by = 2)]))
colnames(res) <- gsub("Est", "Perc", colnames(res))
res$year <- 2016

# Next: Get older data from NHGIS.
# Check out whether you can use 2010 Census API

