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
                     lepPerc = "S1601_C05_001E"
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
res$GEOID <- collect$GEOID; res$year <- 2016

# Next: Get older data from NHGIS.
# Check out whether you can use 2010 Census API
collect2 <- get_acs(geography = "tract",
                   state = c(34,42),
                   year = 2010,
                   output = "wide",
                   variables = c(
                     povUniverse = "B17024_001E",
                     povA.2 = "B17024_011E",
                     povA.3 = "B17024_012E",
                     povA.4 = "B17024_013E",
                     povA.5 = "B17024_014E",
                     povB.2 = "B17024_024E",
                     povB.3 = "B17024_025E",
                     povB.4 = "B17024_026E",
                     povB.5 = "B17024_027E",
                     povC.2 = "B17024_037E",
                     povC.3 = "B17024_038E",
                     povC.4 = "B17024_039E",
                     povC.5 = "B17024_040E",
                     povD.2 = "B17024_050E",
                     povD.3 = "B17024_051E",
                     povD.4 = "B17024_052E",
                     povD.5 = "B17024_053E",
                     povE.2 = "B17024_063E",
                     povE.3 = "B17024_064E",
                     povE.4 = "B17024_065E",
                     povE.5 = "B17024_066E",
                     povF.2 = "B17024_076E",
                     povF.3 = "B17024_077E",
                     povF.4 = "B17024_078E",
                     povF.5 = "B17024_079E",
                     povG.2 = "B17024_089E",
                     povG.3 = "B17024_090E",
                     povG.4 = "B17024_091E",
                     povG.5 = "B17024_092E",
                     povH.2 = "B17024_102E",
                     povH.3 = "B17024_103E",
                     povH.4 = "B17024_104E",
                     povH.5 = "B17024_105E",
                     povI.2 = "B17024_115E",
                     povI.3 = "B17024_116E",
                     povI.4 = "B17024_117E",
                     povI.5 = "B17024_118E",
                     povJ.2 = "B17024_128E",
                     povJ.3 = "B17024_129E",
                     povJ.4 = "B17024_130E",
                     povJ.5 = "B17024_131E",
                     ethUniverse = "B03002_001E",
                     ethEst = "B03002_012E",
                     youthEst = "B09001_001E",
                     fornUniverse = "B05012_001E",
                     fornEst = "B05012_003E",
                     femUniverse = "B01001_001E",
                     femEst = "B01001_026E",
                     lepUniverse = "B06007_001E",
                     lepA = "B06007_005E",
                     lepB = "B06007_008E",
                     lepC = "B06007_013E",
                     lepD = "B06007_016E",
                     lepE = "B06007_021E",
                     lepF = "B06007_024E",
                     lepG = "B06007_029E",
                     lepH = "B06007_032E",
                     lepI = "B06007_037E",
                     lepJ = "B06007_040E",
                     olderA.1 = "B01001_020E",
                     olderB.1 = "B01001_021E",
                     olderC.1 = "B01001_022E",
                     olderD.1 = "B01001_023E",
                     olderE.1 = "B01001_024E",
                     olderF.1 = "B01001_025E",
                     olderA.2 = "B01001_044E",
                     olderB.2 = "B01001_045E",
                     olderC.2 = "B01001_046E",
                     olderD.2 = "B01001_047E",
                     olderE.2 = "B01001_048E",
                     olderF.2 = "B01001_049E"
                   ))
# Doesn't work -- manual download? Blah
# collect2 <- get_acs(geography = "tract",
#                     state = c(34,42),
#                     year = 2010,
#                     output = "wide",
#                     variables = c(
#                       disabUniverse = "DP02_0070E",
#                       disabEst = "DP02_0070E"
#                     ))
# Clean up fields
collect2 <- collect2[, -( grep("\\M$" , colnames(collect2), perl = TRUE))]
# Get state/county info so we can split df by MSA
collect2$st <- substr(collect2$GEOID, 1, 2)
collect2$cty <- substr(collect2$GEOID, 3, 5)
collect2$stcty <- paste0(collect2$st, collect2$cty)
# Subset out DVRPC counties
dvrpc <- c("34005", "34007", "34015", "34021",
           "42017", "42029", "42045", "42091", "42101")
collect2 <- subset(collect2, stcty %in% dvrpc)
# Below 200% FPL
collect2$above200 <- rowSums(collect2[,4:43])
collect2[c(4:43)] <- NULL
collect2$povEst <- collect2$povUniverse - collect2$above200
# Youth
collect2$youthUniverse <- collect2$ethUniverse
# LEP
collect2$lepEst <- rowSums(collect2[,14:23])
collect2[c(14:23)] <- NULL
# Older
collect2$olderEst <- rowSums(collect2[,14:25])
collect2[c(14:25)] <- NULL
collect2$olderUniverse <- collect2$femUniverse
# Disabled
# Just checked, and Table DP02 was the only table with this var.
# Was (X) even for nationwide estimates.

# Rearrange columns to desired spots
collect2 <- collect2[c(1:3, 20, 4, 5, 21, 6:10, 13, 22, 24, 23, 16:18)]
res2 <- as.data.frame(mapply(divisor,
                             collect2[seq(4, 16, by = 2)],
                             collect2[seq(3, 15, by = 2)]))
colnames(res2) <- gsub("Est", "Perc", colnames(res2))
res2$GEOID <- collect2$GEOID; res2$year <- 2010

# Merge datasets
res$disabPerc <- NULL
fullRes <- rbind(res, res2)
