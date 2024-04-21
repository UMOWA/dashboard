########################## global.R ##########################
# Author: Melissa L Muradian
# Written May 2023
# Last updated April 2024
########################################################################
# GLOBALS
YEARS <- 2015:2023
nYR   <- 2023


# 
# Data and Variables for Macroinvertebrates
midat <- read.csv("Data/MI_2015-2023.csv", header = T)
                # colClasses = c("character", "integer", rep("numeric",9)) 
# miIndexName <- c(rep(NA,3),"Density", NA, "Richness", rep(NA,3), "% EPT", NA, 
#                  "% Non-Insect",NA, "% Midges", rep(NA,5), "HBI") # pretty silly
milabels <- c("Upstream of", "Downstream of", "Craig",
              "Upstream of", "Downstream of", 
              "Hardy Bridge", "Cascade") 
# miIndexCol <- c(4,6,10,12,20)
miSite <- c("LPPC US","LPPC DS","Craig","Dearborn US","Dearborn DS","Hardy","Cascade")
# season <- c("Springtime", "Summertime", "Fall" )
levelsHBI <- c(0,3.5, 3.51,4.5, 4.51,5.5, 5.51,6.5, 6.51,7.5, 7.51,8.5, 8.51,10)
colorsHBI <- c("steelblue1", 
               rgb(99,184,255, alpha=175, maxColorValue=255),
               rgb(99,184,255, alpha=100, maxColorValue=255), 
               "white", 
               rgb(205,133,63, alpha=100, maxColorValue=255),
               rgb(205,133,63, alpha=175, maxColorValue=255),
               "tan3")
cutoffHBI <- c(0, 3.51000, 4.51000, 5.51000, 6.51000, 7.51000, 8.51000, 10)
my_image <- readPNG("www/thumbnailmap2.png") 
# my_image <- readPNG("www/thumbnailmap4.png") # with site labels 
hbi_scale <- readPNG("www/HBIScale2.png")
# I created the 7 bins with cutoffHBI so .bincode will return the rank
# to reference the HBI color in the HBI Profiles!
# .bincode(10,cutoffHBI,right=T,include.lowest=T) 

levelsMMI.EPT    <- c(100,60.01, 60,40.01, 40,20.01, 20,0)
levelsMMI.Midges <- c(0,20,  20.01,30, 30.01,50, 50.01,100)
colorsMMI <- c(rgb(99,184,255, alpha=100, maxColorValue=255), 
               "white", 
               rgb(205,133,63, alpha=175, maxColorValue=255),
               "tan3")

# Create dataframes that subset by site only
LPPC_US     <- subset(midat, Sample.Site == "LPPC US")
LPPC_DS     <- subset(midat, Sample.Site == "LPPC DS")
Craig       <- subset(midat, Sample.Site == "Craig")
Dearborn_US <- subset(midat, Sample.Site == "Dearborn US")
Dearborn_DS <- subset(midat, Sample.Site == "Dearborn DS")
Hardy       <- subset(midat, Sample.Site == "Hardy")
Cascade     <- subset(midat, Sample.Site == "Cascade")

# These are subset by site, so result is they are a time series within site
# 2015; 2016; ...
locationTS <- list( LPPC_US = LPPC_US, LPPC_DS = LPPC_DS,
                    Craig = Craig, Dearborn_US = Dearborn_US,
                    Dearborn_DS=Dearborn_DS, Hardy=Hardy,
                    Cascade=Cascade )

 tc1 <- read.csv("Data/TotalComm_2022.csv", header = T, row.names = 1)
 tc2 <- read.csv("Data/TotalComm_2023.csv", header = T, row.names = 1)

 TotalComm <- list(tc1, tc2)
########################################################################

########################################################################
# 
# Data for Trout
# i <- 2 # Site  
# j <- 2 # Estimate (Abund or Biomass)  
# k <- 1 # Species

# Reading in data only needs to happen if Estimate changes (j)
Abundance <- read.csv(paste0("Data/Abundance.csv") , header = T, 
                      colClasses = c("integer", rep("numeric", 12)))
fishSiteNames <- c("Craig", "Cascade")   # i
fishSpec <- c("Rainbow", "Brown")    # k

# the list/order of params c("Rainbow Craig", "Brown Craig", "Rainbow Casc", "Brown Casc")
# fishYlim <- matrix(c(6000, 36000, 3000, 9000),2, byrow = T) 
fishSiteYlim <- c(6000, 3000)
fishSpecYlim <- c(6000, 1000)
#       fishYlim matrix looks like this: [i,j]
#  [1,1] Abund at Craig   [1,2] Biomass at Craig
#  [2,1] Abund at Cascade [2,2] Biomass at Cascade
fishCols <- ptCols <- c("darkgreen","tan3")
# fish4Cols <- c("darkgreen", "tan4", "darkolivegreen4", "tan3")

########################################################################

########################################################################
# 
# # Data for Hydrology (USGS Disharge)
# USGSsites <- c("Toston", "Holter", "LPPC", "Dearborn", "Cascade")
# dischargeDat <- list(Toston= NULL, Holter= NULL, LPPC= NULL, 
#                      Dearborn= NULL, Cascade= NULL)
# for(i in 1:5){
#   dischargeDat[[i]] <- read.csv(paste0("Data/USGSData/DisCFS_", USGSsites[i], ".CY.csv"), 
#                                 header = T)
# }
# monthLab <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
#               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# USGScolsYr <- c("#8B3800", "#8B8B00", "#008B1C", "#00538B", "#53008B")
# USGSfac <- c(5, 4, 1, 1, 4) # for scale of 1 cfs to be equal while yaxis limits differ
# USGSYlim <- c(25, 20, 1, 3, 20) # in thousands


########################################################################

########################################################################
# 
# Data for Water Quality
dat <- read.csv(paste0("Data/FullTS.csv") , header = T)
dat$Date <- as.Date(paste0(dat$Date,"-01"), format = "%Y-%m-%d")

WQ.colNames <- c("DisCFS","DisCFS_lm", "TempC", "TDS.mg.L", "TSS.mg.L", "NN.mg.L", "TN.mg.L", "TP.mg.L")
WQ.Labels <- c("DisCFS","DisCFS_lm", "TempC", "Total Dissolved Solids (mg/L)", "Total Suspended Solids (mg/L)",
               "Inorganic Nitrogen (mg/L)", "Total Nitrogen (mg/L)", "Total Phosphorus (mg/L)")

# Just UMOWA WQ data for user download option

justwqdat <- read.csv(paste0("Data/WQData.csv") , header = T)

# Create dataframes that subset by site only
WQ.LPPC_US     <- subset(dat, Site == "LPPC US")
WQ.LPPC_DS     <- subset(dat, Site == "LPPC DS")
WQ.Craig       <- subset(dat, Site == "Craig")
WQ.Dearborn_US <- subset(dat, Site == "Dearborn US")
WQ.Dearborn_DS <- subset(dat, Site == "Dearborn DS")
WQ.Hardy       <- subset(dat, Site == "Hardy")
WQ.Cascade     <- subset(dat, Site == "Cascade")

# These are subset by site, so result is they are a full time series within site
WQ.locationTS <- list( LPPC_US = WQ.LPPC_US, LPPC_DS = WQ.LPPC_DS,
                       Craig = WQ.Craig, Dearborn_US = WQ.Dearborn_US,
                       Dearborn_DS = WQ.Dearborn_DS, Hardy = WQ.Hardy,
                       Cascade = WQ.Cascade )

# Create dataframes that subset by season
WQ.Spring     <- rbind( subset(dat, (Site == "LPPC US"     & Season == "spr")),
                        subset(dat, (Site == "LPPC DS"     & Season == "spr")),
                        subset(dat, (Site == "Craig"       & Season == "spr")),
                        subset(dat, (Site == "Dearborn US" & Season == "spr")),
                        subset(dat, (Site == "Dearborn DS" & Season == "spr")),
                        subset(dat, (Site == "Hardy"       & Season == "spr")),
                        subset(dat, (Site == "Cascade"     & Season == "spr")) )

WQ.Summer     <- rbind( subset(dat, (Site == "LPPC US"     & Season == "sum")),
                        subset(dat, (Site == "LPPC DS"     & Season == "sum")),
                        subset(dat, (Site == "Craig"       & Season == "sum")),
                        subset(dat, (Site == "Dearborn US" & Season == "sum")),
                        subset(dat, (Site == "Dearborn DS" & Season == "sum")),
                        subset(dat, (Site == "Hardy"       & Season == "sum")),
                        subset(dat, (Site == "Cascade"     & Season == "sum")) )

WQ.Fall       <- rbind( subset(dat, (Site == "LPPC US"     & Season == "fal")),
                        subset(dat, (Site == "LPPC DS"     & Season == "fal")),
                        subset(dat, (Site == "Craig"       & Season == "fal")),
                        subset(dat, (Site == "Dearborn US" & Season == "fal")),
                        subset(dat, (Site == "Dearborn DS" & Season == "fal")),
                        subset(dat, (Site == "Hardy"       & Season == "fal")),
                        subset(dat, (Site == "Cascade"     & Season == "fal")) )


# These are subset by site, so result is they are a full time series within site
WQ.SeasonTS <- list( Spring = WQ.Spring, Summer = WQ.Summer,
                     Fall = WQ.Fall)


WQ.SprMeans <- rbind( apply(subset(dat, (Site == "LPPC US"     & Season == "spr"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "LPPC DS"     & Season == "spr"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Craig"       & Season == "spr"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Dearborn US" & Season == "spr"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Dearborn DS" & Season == "spr"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Hardy"       & Season == "spr"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Cascade"     & Season == "spr"))[5:11], 2, mean, na.rm = T))

WQ.SumMeans <- rbind( apply(subset(dat, (Site == "LPPC US"     & Season == "sum"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "LPPC DS"     & Season == "sum"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Craig"       & Season == "sum"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Dearborn US" & Season == "sum"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Dearborn DS" & Season == "sum"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Hardy"       & Season == "sum"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Cascade"     & Season == "sum"))[5:11], 2, mean, na.rm = T))

WQ.FalMeans <- rbind( apply(subset(dat, (Site == "LPPC US"     & Season == "fal"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "LPPC DS"     & Season == "fal"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Craig"       & Season == "fal"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Dearborn US" & Season == "fal"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Dearborn DS" & Season == "fal"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Hardy"       & Season == "fal"))[5:11], 2, mean, na.rm = T),
                      apply(subset(dat, (Site == "Cascade"     & Season == "fal"))[5:11], 2, mean, na.rm = T))

WQ.SeasonMeans <- list( Spring = WQ.SprMeans, Summer = WQ.SumMeans,
                        Fall = WQ.FalMeans)


WQ.ylim <- list(DisCFS = c(0,18000), TempC= c(0,24), TDS =c(0,400), 
                TSS= c(0,36), NN= c(0,0.27), TN= c(0,0.9), TP= c(0,0.15))
DisDiv <- c(1, 800, 50, 600, 72000, 30000, 180000)
WQ.colors <- c(1:6,8)


########################################################################

########################################################################
# TEXT VARIABLES
# LOCATIONS <- c("LPPC US"="LPPC US",  "LPPC DS"="LPPC DS",
#                "Craig"="Craig",      "Dearborn US"="Dearborn US",
#                "Dearborn DS"="Dearborn DS", 
#                "Hardy"="Hardy",    "Cascade"="Cascade")


# MAP VARIABLES -- scope, site, and set-up
long <- c(-111.96701, -111.83366, -111.69113 )
lat <- c( 47.05415, 47.16781, 47.28062)
long2 <- c(-112.01527, -112.01523 )
lat2 <- c(47.02281, 47.02345)
long3 <- c(-111.91174, -111.9109 )
lat3 <- c( 47.12819, 47.12791 )

# Pop-up location names
popLocs <- c("Craig", "Hardy Bridge", "Cascade")
popLocs2 <- c("Upstream of LPPC", "Downstream of LPPC")
popLocs3 <- c("Upstream of Dearborn R.", "Downstream of Dearborn R.")
# Abbreviatiopns
locAbbrev <- c("Craig","Hardy","Cascade")
locAbbrev2 <- c("LPPC US","LPPC DS")
locAbbrev3 <- c("Dearborn US","Dearborn DS")
########################################################################

########################################################################
