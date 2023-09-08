# Script Master Thesis
# Title: Potential collision risk of a threatened wader with wind turbines 
#        - an analysis based on high-resolution flight behaviour data
# Edited by Robin Maares
# 2023

# Packages
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(sp) 
library(sf)
library(writexl)



# Chapter 1 - Data input & selection
# ---------------------------------------------------------
# 1.1 Curlew
# 1.1.1 Breeding sites (nests)
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/Curlew")
nest <- read_sf("Nestkoordinaten_Gbv_2021.csv", header = T, sep = ",") # "tbl_df" "tbl" "data.frame"
nest <- as.data.frame(nest)
class(nest) # "data.frame"
nest

nest <- nest[!nest$Year == "2022", ] # remove data from 2022 
sum(duplicated(nest$Tag_ID)) # check for duplicates -> 3 

double <- nest %>% add_count(nest$Tag_ID) %>% filter(n > 1) # show duplicates
nest <- nest[!nest$ID %in% c("GBV 011", "GBV 023", "GBV 029"), ] # remove duplicates
rm(double)



# 1.1.2 Tracks 
# Curlew HB_21 (vector data)
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/Curlew/Curlew_breeding_season_2021_Movebank/European Curlew NDS_HB_2021")
HB_sp <- readOGR("points.shp")    # ESRI Shapefile (HB_21)
HB <- as.data.frame(HB_sp)        # "data.frame" 

# Curlew NI_M21 (vector data)
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/Curlew/Curlew_breeding_season_2021_Movebank/European Curlew NDS M 2021")
NIm_sp <- readOGR("points.shp")  # ESRI Shapefile (NI_M21)
NIm <- as.data.frame(NIm_sp)     # "data.frame"

# Curlew NI_N21 (vector data)
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/Curlew/Curlew_breeding_season_2021_Movebank/European Curlew NDS N 2021")
NIn_sp <- readOGR("points.shp")  # ESRI Shapefile (NI_N21)
NIn <- as.data.frame(NIn_sp)     # "data.frame" 

# Curlew NI_NW20 (vector data)
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/Curlew/Curlew_breeding_season_2021_Movebank/European Curlew NW Germany 2020")
NW_sp <- readOGR("points.shp")    # ESRI Shapefile (NI_NW20)
NW <- as.data.frame(NW_sp)        # "data.frame"

# combine tables (data.frames)
track <- rbind(HB,NIm,NIn,NW)
nrow(track) # 5729155 rows

######## Save: 1.1.2_tracks_nests_envi ############
rm(HB_sp,NIm_sp,NIn_sp,NW_sp) 


# Curlew flight tracks: check & count birds
length(table(HB$tag_ident))   #  9  birds
length(table(NIm$tag_ident))  # 18  birds
length(table(NIn$tag_ident))  # 20  birds
length(table(NW$tag_ident))   # 25  birds
sum(length(table(HB$tag_ident)),
    length(table(NIm$tag_ident)),
    length(table(NIn$tag_ident)),
    length(table(NW$tag_ident))) # 72 birds

# Compare both Curlew-datasets for same individuals (tracks & nests)
length(intersect(track$tag_ident,nest$Tag_ID)) # 72 birds - all individuals appear
length(setdiff(nest$Tag_ID, track$tag_ident))  # 31 birds = 103 - 72 
# number of birds with track data from breeding season 2021 (Movebank data)
length(table(track$tag_ident)) # 72 Ind. 

# adjust breeding bird nest data 
tmp <- as.data.frame(table(track$tag_ident))
tmp <- rename(tmp, tag_ident = Var1)
tmp$tag_ident <- as.character(tmp$tag_ident)

n72 <- merge(nest, tmp, by.x = "Tag_ID", by.y = "tag_ident")
rm(tmp,nest, track) 

# visual check
n72
# one birds without nest coordinates -> exclution: creation of buffer not possible
n71 <- subset(n72,nest_lat_2021 != '')
sum(is.na(n71$nest_lon_2021)) # 0
rm(n72)
# adjust coordinates 
n71$nest_lat_2021 <- as.numeric(gsub(",",".",n71$nest_lat_2021,fixed = F))
n71$nest_lon_2021 <- as.numeric(gsub(",",".",n71$nest_lon_2021,fixed = F))
# Export data
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/QGIS/Curlew")
#write.csv(n71, file = "Curlew_n71_2021.csv", row.names = F)



# 1.1.3 data selection 
# breeding season 2021 (cf. Currie et al. 2001 & Kaempfer et al. 2023)
HB <- subset(HB, timestamp >="2021-02-21" & timestamp <="2021-07-23")
NIm <- subset(NIm,timestamp >="2021-02-21" & timestamp <="2021-07-23")
NIn <- subset(NIn,timestamp >="2021-02-21" & timestamp <="2021-07-23")
NW <- subset(NW,timestamp >="2021-02-21" & timestamp <="2021-07-23")


# nest buffers (10km) 
n71_buf <- n71 %>% 
  st_as_sf(coords = c("nest_lat_2021","nest_lon_2021"), crs = 4326) %>% 
  st_transform(25832) %>% 
  st_buffer(dist = units::set_units(10, "kilometers")) %>% 
  lwgeom::st_snap_to_grid(10)
# select not important variables
n71_buf[,c(9:10,16:25,27:33)] <- NULL
# merge buffers
n71_buf_combi <- st_as_sf(st_union(n71_buf)) 
# ESRI Export
st_write(n71_buf, "C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/QGIS/buffer", "n71_buf", driver = "ESRI Shapefile")
st_write(n71_buf_combi, "C:/Users/maare/Documents/02_Studium/M.Sc.L?k/Masterarbeit/Auswertung/QGIS/buffer", "n71_buf_combi", driver = "ESRI Shapefile")


# tracks in nest-buffers (10 km)
HB_sf <- st_as_sf(HB %>% 
                      st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
                      st_transform(25832))  # 491483 rows / 32 columns - in sf
HB_buf <- st_intersection(HB_sf, n71_buf_combi) # 275629 rows / 31 columns - points in combi buffer 
rm(HB_sf, HB)
HB <- st_intersection(HB_buf,n71_buf)   # 1431253 rows / 51 columns - points in single buffers (contains duplicates in overlapping buffers)
rm(HB_buf)
HB <- HB %>% mutate(n71_buf = ifelse(tag_ident == Tag_ID, 1, 0)) # new column for next separation process 
HB <- HB[HB$n71_buf == "1", ]        # 262570 rows / 52 columns - remove duplicates of overlapping buffer zones 

NIm_sf <- st_as_sf(NIm %>% 
                        st_as_sf(coords = c("long", "lat"), crs =4326) %>% 
                        st_transform(25832))      # 700664 rows / 31 columns
NIm_buf <- st_intersection(NIm_sf, n71_buf_combi) # 376414 rows / 31 columns
rm(NIm_sf, NIm)
NIm <- st_intersection(NIm_buf, n71_buf)  # 2659647 rows / 51 columns
rm(NIm_buf)
NIm <- NIm %>% mutate(n71_buf = ifelse(tag_ident == Tag_ID, 1, 0))
NIm <- NIm[NIm$n71_buf == "1", ]        # 365247 rows / 52 columns

NIn_sf <- st_as_sf(NIn %>% 
                        st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
                        st_transform(25832))      # 947190 rows / 31 columns
NIn_buf <- st_intersection(NIn_sf, n71_buf_combi) #  484597 rows / 31 columns
rm(NIn_sf, NIn)
NIn <- st_intersection(NIn_buf, n71_buf)   # 2027539 rows / 51 columns
rm(NIn_buf)
NIn <- NIn %>% mutate(n71_buf = ifelse(tag_ident == Tag_ID, 1, 0))
NIn <- NIn[NIn$n71_buf == "1", ]        # 420192 rows / 52 columns

NW_sf <- st_as_sf(NW %>% 
                      st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
                      st_transform(25832))    # 3182694 rows / 31 columns
NW_buf <- st_intersection(NW_sf, n71_buf_combi) # 2745262 rows / 31 columns
rm(NW_sf, NW, n71_buf_combi)
NW <- st_intersection(NW_buf, n71_buf)  # 11993022 rows / 51 columns
rm(NW_buf)
NW <- NW %>% mutate(n71_buf = ifelse(tag_ident == Tag_ID, 1, 0))
NW <- subset(NW, n71_buf == "1")         # 2730518 rows / 52 columns

rm(n71_buf_combi)

######## Tabellen zusammenfuegen 
track_sf <- rbind(HB,NIm,NIn,NW)
nrow(track_sf) # 3778527 rows
class(track_sf) #  "sf"   "data.frame"


###### Save: "1.1.2.2_tracks_in_buf_envi" #######

rm(HB,NIm,NIn,NW)
track_sf$n71_buf <- NULL # column used to select data 
#n71_buf <- rename(n71_buf, coord_buf = geometry)
#track_sf <- rename(track_sf, coord_pts = geometry)

# check number of birds again
length(table(track_sf$tag_ident)) # data contains 71 birds  

# Check track data 
Ind1 <- track_sf[track_sf$tag_ident == "212446",]
#Ind1 <- rename(Ind1, coord_pts = geometry)
Ind2 <- track_sf[track_sf$tag_ident == "212444",]
#Ind2 <- rename(Ind2, coord_pts = geometry)
Ind3 <- track_sf[track_sf$tag_ident == "212447",]
#Ind3 <- rename(Ind3, coord_pts = geometry)
Ind <- rbind(Ind1,Ind2,Ind3)

Buf <- n71_buf[n71_buf$Tag_ID == "212446",]
Buf <- rbind(Buf,n71_buf[n71_buf$Tag_ID == "212444",])
Buf <- rbind(Buf,n71_buf[n71_buf$Tag_ID == "212447",])

# Visualisierung
plot(Buf$geometry)
plot(Ind1$geometry, pch = 4, col = 1, add = T)
plot(Ind2$geometry, pch = 4, col = 2, add = T)
plot(Ind3$geometry, pch = 4, col = 3, add = T)

rm(Ind,Ind1,Ind2,Ind3,Buf)



############# 1.2 digital terrain model (DGM10)
# 1.2.1 data input
# adjust format of Curlew data
track_sp <- as_Spatial(track_sf)

# DGM10 Niedersachsen
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/DigitalTerrainModel/Niedersachsen")
DGM10_NI <- raster("DGM101.tif") 
# add new column: "elev" (Elevation)  
track_sp$elev_NI <- extract(DGM10_NI,track_sp) # Angabe der Gelaendehoehen an den jeweiligen GPS-points 
range(track_sp$elev_NI, na.rm = T) # min: -3.05  max: 64.11 [meters above sea level]  
sum(is.na(track_sp$elev_NI)) # 604388   
# NA-values are located in NRW & HB (outside NI)

# DGM10 NRW
# previous step via QGIS: transformation of DGM1 into DGM10   
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/DigitalTerrainModel/NRW")
DGM10_NRW <- raster("DGM10_NRW.tif") 
# add new column: "elev" (Elevation)  
track_sp$elev_NW <- extract(DGM10_NRW,track_sp)
range(track_sp$elev_NW, na.rm = T) # min: 0  max: 66.1 [meters above sea level]  
sum(!is.na(track_sp$elev_NW)) # 390260   # number of affected fixes in NRW 

# DGM10 HB & BHV
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/DigitalTerrainModel/Bremen")
DGM10_HB <- raster("DGM10_2017_HB.tif")
DGM10_BHV <- raster("DGM10_2018_BHV.tif") 

# add new column: "elev" (Elevation) for Bremen
track_sp$elev_HB <- extract(DGM10_HB,track_sp)
range(track_sp$elev_HB, na.rm = T) # min: -0.82  max: 16.75 [meters above sea level]  
sum(!is.na(track_sp$elev_HB)) # 249236   # number of affected fixes in HB
# add new column: "elev" (Elevation) for Bremerhaven
track_sp$elev_BHV <- extract(DGM10_BHV,track_sp)
range(track_sp$elev_BHV, na.rm = T) # min: -2.71  max: 8.8 [meters above sea level]  
sum(!is.na(track_sp$elev_BHV)) # 548   # number of affected fixes in BHV


# 1.2.2 allocation of elevation inforamtion 
track_sf <- st_as_sf(track_sp) 
class(track_sf$elev_NI) # "numeric"
#track_sf <- rename(track_sf, coord_buf = geometry)

# transfer separated elev-columns (NI,NRW,HB,BHV) into one "elev"-column 
track_sf <- track_sf %>% mutate(elev = ifelse(!is.na(elev_NI), elev_NI, elev_NW))
track_sf$elev <- ifelse(!is.na(track_sf$elev_HB) & is.na(track_sf$elev), track_sf$elev_HB, track_sf$elev)
track_sf$elev <- ifelse(!is.na(track_sf$elev_BHV) & is.na(track_sf$elev), track_sf$elev_BHV, track_sf$elev)

check <- subset(track_sf, elev_NW == 0)
subset(track_sf, elev == 0 & elev_NW == 0) %>% View()

# shift "0"-bias out of NRW data into NA 
track_sf$elev <- ifelse(is.na(track_sf$elev_NI) &
                            is.na(track_sf$elev_HB) &
                            is.na(track_sf$elev_BHV) & 
                            track_sf$elev_NW == 0, NA, track_sf$elev)

range(track_sf$elev, na.rm = T) # min: -3.05  max: 66.1 [müNN]  
sum(is.na(track_sf$elev)) # 10867   # number of non-calculated elevations

# check GPS fixes without elevation information 
track_elev_na <- track_sf[is.na(track_sf$elev),] 
#st_write(track_elev_na, "C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/QGIS/data_check_exports", "Gbv_elev_na", driver = "ESRI Shapefile")

# selection of affected fixes
track_sf <- subset(track_sf, !is.na(elev))

rm(check)
# remove columns
track_sf[,c("elev_NI","elev_NW","elev_HB","elev_BHV")] <- NULL
track_sf[,c("import_mar","animal.id","Metal_ring_DEW","No_Eggs_2020")] <- NULL



# 1.2.3 flight altitude above ground
# calculate flight altitude above ground level (add new column "alti_ground" - altitude above ground)
track_sf$alti_ground <- track_sf$height_abo - track_sf$elev 
# flight altitude above ground = flight altitude above sea level - elevation


####### Save: 1.2.3_flight_altitude_above_ground ######

rm(DGM10_NI, DGM10_NRW, DGM10_HB, DGM10_BHV)
rm(track_sp, track_elev_na)


####### 1.3 wind turbines
# 1.3.1 data input
# Niedersachsen
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/Wind_turbines/Niedersachsen")
WKA_NI_sf <- st_read("WEA_Nds_AWZ_20191231.shp") 
class(WKA_NI_sf)   # "sf" "data.frame"
glimpse(WKA_NI_sf) # Rows: 8.393  Columns: 41 

WKA_NI_df <- as.data.frame(WKA_NI_sf)
rm(WKA_NI_sf)

# missing values (NA)
sum(is.na(WKA_NI_df$hoeheges)) # 1197 missing values
sum(is.na(WKA_NI_df$hoehenab)) # 922 missing values 
sum(is.na(WKA_NI_df$rotor)) # 1337 missing values     

# construction of missing information if possible 
WKA_NI_df$hoeheges <- ifelse(is.na(WKA_NI_df$hoeheges), 
                             WKA_NI_df[,"hoehenab"] + WKA_NI_df[,"rotor"]/2,
                             WKA_NI_df$hoeheges)
WKA_NI_df$hoehenab <- ifelse(is.na(WKA_NI_df$hoehenab), 
                             WKA_NI_df[,"hoeheges"] - WKA_NI_df[,"rotor"]/2,
                             WKA_NI_df$hoehenab)
WKA_NI_df$rotor <- ifelse(is.na(WKA_NI_df$rotor), 
                          (WKA_NI_df[,"hoeheges"] - WKA_NI_df[,"hoehenab"])*2,
                          WKA_NI_df$rotor)

sum(is.na(WKA_NI_df$hoeheges)) # 1186 missing values  
sum(is.na(WKA_NI_df$hoehenab)) # 879 missing values 
sum(is.na(WKA_NI_df$rotor)) # 1336 missing values   

# Null-values
sum(WKA_NI_df$hoeheges==0, na.rm = T) # 6 values 
sum(WKA_NI_df$hoehenab==0, na.rm = T) # 7 values 
sum(WKA_NI_df$rotor==0, na.rm = T) # 7 values    

# add new column: "minRTH" (minimum rotor tip height)   
WKA_NI_df$minRTH <- ifelse(WKA_NI_df$rotor != 0, 
                           WKA_NI_df[,"hoehenab"] - WKA_NI_df[,"rotor"]/2, 
                           NA)
sum(is.na(WKA_NI_df$minRTH)) # 1360 missing values 

# reconstruct missing coordinates 
sum(is.na(WKA_NI_df$x_etrs89)) # 1560 missing values  
sum(is.na(WKA_NI_df$y_etrs89)) # 1560 missing values  

WKA_NI_coords <- st_coordinates(WKA_NI_df$geometry)
WKA_NI_coords <- as.data.frame(WKA_NI_coords) # "data.frame"
WKA_NI_df$x_etrs89 <- ifelse(is.na(WKA_NI_df$x_etrs89), 
                             WKA_NI_coords[,"X"], 
                             WKA_NI_df$x_etrs89)
WKA_NI_df$y_etrs89 <- ifelse(is.na(WKA_NI_df$y_etrs89), 
                             WKA_NI_coords[,"Y"], 
                             WKA_NI_df$y_etrs89)

sum(is.na(WKA_NI_df$x_etrs89)) # 0 missing values
sum(is.na(WKA_NI_df$y_etrs89)) # 0 missing values
nrow(WKA_NI_df) # 8393 Anlagen (ges. Anz.) 
rm(WKA_NI_coords)


# NRW
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/Wind_turbines/NRW")
WKA_NW_sf <- st_read("WEA_in_Betrieb.shp") 
class(WKA_NW_sf)   # "sf" "data.frame"
glimpse(WKA_NW_sf) # Rows: 3808  Columns: 23 

WKA_NW_df <- as.data.frame(WKA_NW_sf)
rm(WKA_NW_sf)

# missing values (NA)
sum(is.na(WKA_NW_df$Ges_Hoeh)) # 0
sum(is.na(WKA_NW_df$Nab_Hoeh)) # 0
sum(is.na(WKA_NW_df$Durchmes)) # 0

# Null-values
sum(WKA_NW_df$Ges_Hoeh==0) # 102 values  
sum(WKA_NW_df$Nab_Hoeh==0) # 97 values 
sum(WKA_NW_df$Durchmes==0) # 94 values 

# Null-values into NA 
WKA_NW_df$Ges_Hoeh <- ifelse(WKA_NW_df$Ges_Hoeh == 0, NA, WKA_NW_df$Ges_Hoeh)
WKA_NW_df$Nab_Hoeh <- ifelse(WKA_NW_df$Nab_Hoeh == 0, NA, WKA_NW_df$Nab_Hoeh)
WKA_NW_df$Durchmes <- ifelse(WKA_NW_df$Durchmes == 0, NA, WKA_NW_df$Durchmes)

# missing values (NA)
sum(is.na(WKA_NW_df$Ges_Hoeh)) # 102
sum(is.na(WKA_NW_df$Nab_Hoeh)) # 97
sum(is.na(WKA_NW_df$Durchmes)) # 94

# add new column: "minRTH" (minimum rotor tip height)   
WKA_NW_df$minRTH <- ifelse(WKA_NW_df$Durchmes != 0, 
                           WKA_NW_df[,"Nab_Hoeh"] - WKA_NW_df[,"Durchmes"]/2, 
                           NA)
sum(is.na(WKA_NW_df$minRTH)) # 102 missing values due to Null-values

# nach fehlenden Koordinaten suchen 
sum(is.na(WKA_NW_df$geometry)) # 0 

# check status
table(WKA_NW_df$Status) # in Betrieb: 3801    Vorübergehend stillgelegt: 7 
# select decommissioned turbines 
WKA_NW_df <- WKA_NW_df[!WKA_NW_df$Status == "Vorübergehend stillgelegt",]


# Bremen
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/Wind_turbines/Bremen")
WKA_HB_sf <- st_read("2022_WEA_Standorte_Bremen.shp") 
class(WKA_HB_sf)   # "sf" "data.frame"
glimpse(WKA_HB_sf) # Rows: 89  Columns: 10 

WKA_HB_df <- as.data.frame(WKA_HB_sf)
rm(WKA_HB_sf)

# add column: calculate "Ges_Hoeh" (Gesamthoehe)
WKA_HB_df$Ges_Hoeh <- as.numeric(
  WKA_HB_df[,"Nab_Hoeh"] + WKA_HB_df[,"Durchmes"]/2)

# missing values (NA)
sum(is.na(WKA_HB_df$Ges_Hoeh)) # 0
sum(is.na(WKA_HB_df$Nab_Hoeh)) # 0
sum(is.na(WKA_HB_df$Durchmes)) # 0
# Null-values
sum(WKA_HB_df$Ges_Hoeh==0, na.rm = T) # 0 values  
sum(WKA_HB_df$Nab_Hoeh==0, na.rm = T) # 0 values 
sum(WKA_HB_df$Durchmes==0, na.rm = T) # 0 values

# add new column: "minRTH" (minimum rotor tip height)   
WKA_HB_df$minRTH <- ifelse(WKA_HB_df$Durchmes != 0, 
                           WKA_HB_df[,"Nab_Hoeh"] - WKA_HB_df[,"Durchmes"]/2, 
                           NA)
sum(is.na(WKA_HB_df$minRTH)) # 0 missing values 



# 1.3.2 combine and clean data  
# add variables 
WKA_NW_df$Land <- "NRW" # NRW-datensatz kennzeichnen
WKA_NI_df$Land <- "NI"  #  NI-datensatz kennzeichnen

# align column names  
WKA_NW_df$LANUV_ID <- gsub("WEA","NRW", WKA_NW_df[ ,"LANUV_ID"]) # NRW
WKA_NI_df$id_wea <- sub("^","NI_", WKA_NI_df[ ,"id_wea"]) # NI
WKA_HB_df$ID_WKA <- seq.int(nrow(WKA_HB_df)) # HB
WKA_HB_df$ID_WKA <- sub("^","HB_", WKA_HB_df[ ,"ID_WKA"]) # HB

# rename variables 
WKA_HB_df <- rename(WKA_HB_df, x_etrs89 = hoch)
WKA_HB_df <- rename(WKA_HB_df, y_etrs89 = rechts)
WKA_NW_df <- rename(WKA_NW_df, x_etrs89 = UTM32_O)
WKA_NW_df <- rename(WKA_NW_df, y_etrs89 = UTM32_N)
WKA_NI_df <- rename(WKA_NI_df, Ges_Hoeh = hoeheges)
WKA_NI_df <- rename(WKA_NI_df, Nab_Hoeh = hoehenab)
WKA_NI_df <- rename(WKA_NI_df, Durchmes = rotor)
WKA_NI_df <- rename(WKA_NI_df, Leist_kW = kw)
WKA_NI_df <- rename(WKA_NI_df, ID_WKA = id_wea)
WKA_NW_df <- rename(WKA_NW_df, ID_WKA = LANUV_ID)
WKA_NI_df <- rename(WKA_NI_df, Inb_Jahr = jahrinbe)
WKA_HB_df <- rename(WKA_HB_df, Inb_Jahr = Inbetriebn)
WKA_NI_df <- rename(WKA_NI_df, Anl_Typ = anlagtyp)
WKA_HB_df <- rename(WKA_HB_df, Anl_Typ = Anlagentyp)
WKA_NI_df <- rename(WKA_NI_df, Status = status_o)
WKA_HB_df <- rename(WKA_HB_df, Land = Region)
WKA_NI_df <- rename(WKA_NI_df, Kreis = landkr)
WKA_NI_df <- rename(WKA_NI_df, Gemeinde = gemeinde)
WKA_NI_df <- rename(WKA_NI_df, PLZ = plz)
WKA_NI_df <- rename(WKA_NI_df, En_Traeg = see_land)
WKA_NI_df <- rename(WKA_NI_df, Herstell = anl_hstl)
WKA_NI_df <- rename(WKA_NI_df, Standort = gemark)
WKA_NI_df <- rename(WKA_NI_df, Plan_Reg = arl)
WKA_NI_df <- rename(WKA_NI_df, Stand = datstand)

# remove variables
WKA_NI_df[,c("flurst","flur","dat_auss","id_wi_lk","rrop","id_vr_w","gebiet","geb_stat",
             "dat_plan","dat_gene","meldestel","stillleg","datmeld","netzbetr","windpark",
             "sch_gem","dat_inbe","anlagenr","name_wka","repower","id","Reg_Bezirk","GKZ",
             "Einspeisg","Inb_dat")] <- NULL
WKA_HB_df$Inb_Jahr <- as.numeric(WKA_HB_df$Inb_Jahr) # change format

# combine tables (NRW, NI, HB) into one data frame 
WKA_ges_df <- bind_rows(WKA_HB_df, WKA_NW_df, WKA_NI_df)
nrow(WKA_ges_df) # 12283 turbines
class(WKA_ges_df$geometry) # "sfc_GEOMETRY" "sfc"   

rm(WKA_HB_df, WKA_NI_df,WKA_NW_df)



# 1.3.3 Selection by construction type, location and operational status 
# delineate study area
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/Data/GBV_study_area_20_21")
WKA_stud_area <- st_read("GBV_study_area_poly_intersect.shp") 
class(WKA_stud_area)   # "sf" "data.frame"
glimpse(WKA_stud_area) # 29 rows ; 8 columns

WKA_stud_area <- st_transform(WKA_stud_area, crs = st_crs(25832))

# change WKA_ges_df into sf 
WKA_ges_sf <- st_as_sf(WKA_ges_df,crs=25832)

plot(WKA_ges_sf$geometry)
plot(WKA_stud_area$geometry,add=T)

# exclude all turbines out of the study area
WKA_UG_sf <- st_intersection(WKA_ges_sf, WKA_stud_area)
class(WKA_UG_sf) # "sf"   "data.frame"
plot(WKA_UG_sf$geometry)

nrow(WKA_UG_sf) # 7762 turbines
rm(WKA_ges_df, WKA_ges_sf)

# remove variables
WKA_UG_sf[,c("FID_bundes","COUNT","LAND_NAME","Id",
             "FID_New_Sh","ID_1","SUM_AREA")] <- NULL


# Status 
table(WKA_UG_sf$status_l)
# 4 = "ausser Betrieb"
tmp <- subset(WKA_UG_sf, status_l == "4") 
WKA_UG_sf <- setdiff(WKA_UG_sf, tmp) # selection of turbines which are "ausser Betrieb"

nrow(WKA_UG_sf) # 7396 turbines

# construction type
table(WKA_UG_sf$Technol)
tmp <- subset(WKA_UG_sf, Technol == "Vertikalläufer")
WKA_UG_sf <- setdiff(WKA_UG_sf, tmp)
tmp <- subset(WKA_UG_sf, Technol == "loeschen")
WKA_UG_sf <- setdiff(WKA_UG_sf, tmp)
# remove variables
WKA_UG_sf$Technol <- NULL
rm(tmp)

nrow(WKA_UG_sf) # 7363 Anlagen

# small wind turbines (Kleinwindanlagen)
# turbines <= 50m & Leistung > 50 kW
nrow(WKA_UG_sf[WKA_UG_sf$Ges_Hoeh <= 50 & 
                 WKA_UG_sf$Leist_kW < 50, ]) # 134

# Kleinwindanlagen filtern
WKA_UG_kl <- WKA_UG_sf %>% filter(Leist_kW < 50 | Ges_Hoeh <= 50) 
nrow(WKA_UG_kl) # 260 turbines
range(WKA_UG_kl$minRTH, na.rm = T) # -0.24 35.40 m

#### exclude small wind turbines 
WKA_UG_sf <- setdiff(WKA_UG_sf, WKA_UG_kl)

# min & max Werte pruefen
min(WKA_UG_sf$minRTH, na.rm = T)   # 11.85 
max(WKA_UG_sf$Ges_Hoeh, na.rm = T) # 241.0
summary(WKA_UG_sf[c("Ges_Hoeh", "minRTH")])

nrow(WKA_UG_sf)   #  7103 turbines
st_crs(WKA_UG_sf) # "ETRS89 / UTM zone 32N"
rm(WKA_UG_kl,WKA_stud_area)

# Export data: WKA_UG_sf
#st_write(WKA_UG_sf, "C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/QGIS/WKA", "WKA_UG_2021", driver = "ESRI Shapefile")

# check for missing wind turbine information 
WKA_UG_df <- as.data.frame(WKA_UG_sf)

sum(is.na(WKA_UG_df$Ges_Hoeh)) # 1153
sum(is.na(WKA_UG_df$Nab_Hoeh)) #  844
sum(is.na(WKA_UG_df$Durchmes)) # 1280
sum(is.na(WKA_UG_df$minRTH))   # 1296

WKA_tmp <- WKA_UG_df[,c("Ges_Hoeh","Nab_Hoeh","Durchmes","minRTH")]
WKA_tmp_NA <- WKA_tmp %>% filter_all(any_vars(is.na(.))) 
WKA_tmp_NA <- WKA_tmp %>% filter(rowSums(!is.na(.)) < 2) 
nrow(WKA_tmp_NA) # 1296 rows
# For the analysis of the vertical overlap, 1336 lines are omitted. 
# However, as locations for the horizontal overlap, these plants are to be considered!
rm(WKA_tmp,WKA_tmp_NA)


# Calculation of standard error (se)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
se(WKA_UG_sf$Ges_Hoeh) # 0.5827379 [m]  
se(WKA_UG_sf$minRTH)   # 0.2861798 [m]  



# 1.3.4 RHR - rotor height range
# minimun and maximum rotor tip height of turbines in the study area
WKA_rhr <- data.frame(minRTH=min(WKA_UG_sf$minRTH, na.rm = T),
                      maxRTH=max(WKA_UG_sf$Ges_Hoeh, na.rm = T))

# minimun and maximum rotor tip height of turbines built since 2018
tmp_WKA <- WKA_UG_df %>% filter(Inb_Jahr >= 2018)
nrow(tmp_WKA) # 365 turbines
WKA_rhr[nrow(WKA_rhr)+1,] <- c(min(tmp_WKA$minRTH),max(tmp_WKA$Ges_Hoeh))

# add vertical buffer of 5m in both directions
WKA_rhr <- WKA_rhr %>% mutate(buf_min = minRTH - 5, buf_max = maxRTH + 5) 
# add year 
WKA_rhr$Inb_Jahr <- c(NA,2018)
WKA_rhr <- WKA_rhr %>% select(Inb_Jahr,everything())
WKA_rhr <- round(WKA_rhr, digits = 0)
rm(tmp_WKA)


# add horizontal buffer of 200m around wind turbines  
WKA_buf200_sf <- st_buffer(WKA_UG_sf, 200)
class(WKA_buf200_sf) # "sf"     "data.frame"
class(WKA_buf200_sf$geometry) # "sfc_POLYGON" "sfc"

# Export data: WKA 200m buffer
#st_write(WKA_buf200_sf, "C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/QGIS/WKA", "WKA_buf_200", driver = "ESRI Shapefile")



############ Save: "1.3.7_WKA_envi" #################




############ Chapter 2 - preparation of calculations #######
# 2.1 Adaptation of data formats  
# variable "timestamp" (character -> POSIXct)
class(track_sf$timestamp)  # "character"
track_sf$timestamp <- as.POSIXct(track_sf$timestamp, 
                               "%Y-%m-%d %H:%M:%S", tz='UTC')        
class(track_sf$timestamp) # "POSIXct" "POSIXt" 

# add new column: "tdiff" (time difference) 
# time difference between consecutive fixes
track_sf <- track_sf %>%                               
  arrange(tag_ident,timestamp) %>%               
  group_by(tag_ident) %>%                 # gruppiert nach Individuen 
  mutate(tdiff=timestamp-lag(timestamp))  # Berechnung startet mit jedem Individuum neu

class(track_sf$tdiff)      # "difftime"
sum(is.na(track_sf$tdiff)) # 71 NA - in case of switching bird ID 

# replace NA with 0  
track_sf$tdiff[is.na(track_sf$tdiff)] <- 0
sum(is.na(track_sf$tdiff)) # 0
# first bird is always NA, as no previous reference value exists 

# change format: variable "tdiff" (difftime -> numeric)
track_sf$tdiff <- as.numeric(track_sf$tdiff, units='secs')
class(track_sf$tdiff)   # "numeric"
class(track_sf)         # "sf" "grouped_df" "tbl_df" "tbl" "data.frame"



# introduce hrd
# add new column: "hrd" (high-resolution-data) - hrd = 1 / 0 
track_sf <- track_sf %>% mutate(hrd = ifelse(tdiff <= 3 & tdiff != 0 & speed >= 3.6, 1, 0))
class(track_sf$hrd) # "numeric"
sum(track_sf$hrd==1, na.rm = T) # 1718080 rows hrd
sum(track_sf$hrd==0, na.rm = T) # 2049580 rows not hrd
sum(is.na(track_sf$hrd))        # 0 - no NA



# define track ID 
track <- as.data.frame(track_sf)
detach("package:raster", unload = T)
rm(track_sf)

track <- track %>% 
  mutate(hrd_shift = lag(track$hrd)) %>% 
  mutate(ind_shift = lag(track$tag_ident)) %>% 
  mutate(index = ifelse(hrd != hrd_shift | tag_ident != ind_shift, 1, 0))

track[1,"index"] <- 2 # to start, change first row value

# introduce counter
counter_hrd = 1
counter_lrd = 1

# nested for loop 
for (i in 1:length(track$hrd)) {
  if (track[i,"hrd"] == 1 & track[i,"speed"] >= 3.6 & track[i,"index"] != 0) {
    track$flight_ID[i] <- paste("A", counter_hrd, sep = "")
    counter_hrd = counter_hrd + 1
  }
  else if (track[i,"hrd"] == 0 & track[i,"speed"] < 3.6 & track[i, "index"] != 0) {
    track$flight_ID[i] <- paste("B", counter_lrd, sep = "")
    counter_lrd = counter_lrd + 1
  }
  else {track$flight_ID[i] <- NA
  }
} 

# fill up intermediate rows between first and last flight_ID
library(tidyr)
track <- track %>% fill(flight_ID)

####### individual flight tracks defined by ID #######

####### Save: 2.5_flight_ID #######



# add new column: "flight_nr (flight number) - consecutive number of each flight 
track <- track %>% 
  group_by(flight_ID) %>%
  mutate(flight_nr = row_number())

# clean data 
table(track$barometric) # 0
track$barometric <- NULL
table(track$import_mar) # F
track$import_mar <- NULL
table(track$sensor_typ) # gps
track$sensor_typ <- NULL
table(track$date) # 2022/11/01
track$date <- NULL
identical(track[['tag_ident']],track[['Tag_ID']]) # TRUE
track$Tag_ID <- NULL
track[,c("hrd_shift","ind_shift","Natura_Code","heading",
         "magnetic_f","magnetic1","magnetic2")] <- NULL
#track$accelerati <- NULL
#track$accelera1 <- NULL
#track$accelera2 <- NULL
#track$gps_time_t <- NULL
#track$external_t <- NULL
#track$light_leve <- NULL
track <- rename(track, Sex = Sex_Bill_Summers)
# change format (day)
track$day <- format(as.POSIXct(track$timestamp), format = "%D")
# change format (hour)
track$hour <- format(as.POSIXct(track$timestamp), format = "%H")


# 2.2 select hrd (! flight data !)
# condition: 
# hrd-variable == 1 (which means that time difference between consecultive rows is at max. 3 sec)
# speed-variable >= 3.6 km/h (threshold for flight movement)

hrd <- track %>% filter(hrd == 1 & speed >= 3.6) 
nrow(hrd) # 1718080 rows

hrd$flight_ID <- as.numeric(gsub('A','',hrd$flight_ID, fixed = F)) # remove "A"-letter of the ID
range(hrd$flight_ID) # 1  45307



# 2.3	Check arrivals and departures of the birds in the breeding sites
# using flight detection ! 
# Time of first registration of the birds (2020) in the nest buffer 
track20 <- track %>% filter(Year == "2020") # track-data, not hrd!
nrow(track20) # 2720565 
first_detec <- aggregate(timestamp~tag_ident, track20, min) # first arrival date for each of the birds (2020)
sum(duplicated(first_detec$timestamp)) # 0
mean(first_detec$timestamp) # "2021-02-27 13:20:44 UTC"
median(first_detec$timestamp) #  "2021-02-25 07:39:50 UTC"

# Time of first hrd transmission of the birds (2020) in the nest buffer
hrd20 <- hrd %>% filter(Year == "2020")
first_hrd <- aggregate(timestamp~tag_ident+day,hrd20,min) 
first_ind_hrd <- aggregate(timestamp~tag_ident, first_hrd,min) # flight detection activation 
first_ind_hrd # moment of first hrd transmission
# first flights / bird 
first_flight_ind <- subset(hrd, timestamp %in% first_ind_hrd$timestamp) # selecting whole row of according data 
# first flight fix / bird
# mean(first_flight_ind$timestamp) # 15.03.2021 
median(first_flight_ind$timestamp) # 08.03.2021: average of first hrd transmission 
min(first_flight_ind$timestamp)  # 23.02.2021: first hrd transmission
# Are arrivals included in hrd ?  
ankunft <- subset(first_flight_ind, timestamp %in% first_detec$timestamp)
nrow(ankunft) # 0 - no hrd on first transmission 
rm(first_flight_ind, first_hrd, first_ind_hrd, hrd20, ankunft, first_detec)


# take-off moments
last_detec <- aggregate(timestamp~tag_ident, track, max) # last registration in buffers (track data)
#mean(last_detec$timestamp) # "2021-06-15 14:44:56 UTC"
median(last_detec$timestamp) # "2021-06-15 18:50:28 UTC"

# check for hrd
last_hrd <- aggregate(timestamp~tag_ident+day,hrd,max)
last_ind_hrd <- aggregate(timestamp~tag_ident,last_hrd,max) # last hrd in buffers
#mean(last_ind_hrd$timestamp)   # "2021-06-14 02:25:08 UTC"
median(last_ind_hrd$timestamp)  # "2021-06-15 18:50:28 UTC"
# last flight / bird in hrd
last_flight_ind <- subset(hrd, timestamp %in% last_ind_hrd$timestamp)
nrow(last_flight_ind) # 65
length(table(last_flight_ind$tag_ident)) # 51 Ind
sum(duplicated(last_flight_ind$tag_ident)) # 14
last_flight_ind <- last_flight_ind[!duplicated(last_flight_ind$tag_ident,fromLast = T),]
nrow(last_flight_ind) # 51
# Are departuresincluded in hrd?
abzug <- subset(last_flight_ind, timestamp %in% last_detec$timestamp)
nrow(abzug) # 39
sum(duplicated(abzug$timestamp)) # 0
# identification of take-off moment with hrd-transmission for 39/51 birds 

# exclude take-off-tracks (migration not breeding season)  
hrd <- subset(hrd, !(flight_ID %in% abzug$flight_ID))
nrow(hrd) # 1701595
length(table(hrd$flight_ID)) # 45268 flights 

rm(last_detec, last_hrd, last_ind_hrd, last_flight_ind, abzug,track20)




# 2.4 detect and exclude outliers 
hrd$alti_ground <- round(hrd$alti_ground, digits = 0)

# add statistical information of each flight 
hrd_stat <- hrd %>% group_by(flight_ID) %>% 
  summarize(Median = median(alti_ground), 
            Mittelwert = mean(alti_ground), 
            sd = sd(alti_ground))

# merge statistical information to flights
hrd <- merge(hrd, hrd_stat, by = "flight_ID", all = T)
# add new columns: qnt1_l & qnt3_h (1.5*IQR) - additional threshold
hrd <- hrd %>% group_by(flight_ID) %>% 
  mutate(qnt1_l = quantile(alti_ground, 0.25, na.rm = T) - 1.5*IQR(alti_ground, na.rm = T)) %>% 
  mutate(qnt3_h = quantile(alti_ground, 0.75, na.rm = T) + 1.5*IQR(alti_ground, na.rm = T)) 
rm(hrd_stat)


# add new column: "hdiff" (height difference)
hrd <- hrd %>% 
  group_by(flight_ID) %>% 
  mutate(hdiff = alti_ground - lag(alti_ground))
hrd <- as.data.frame(hrd)
# first altitude value (hdiff) of each flight is NA, due to no prior comparable fix 

# second value (hdiff) of each flight should take value from variable (alti-ground) to compare
for (i in 2:nrow(hrd)) {
  if(is.na(hrd$hdiff[i-1])) {
    hrd$hdiff[i] = hrd$alti_ground[i]}
  else {hrd$hdiff[i] = hrd$hdiff[i]}
}


hrd$alti_clean <- NA

# replace outliers with NA
for (i in 2:nrow(hrd)) {
  if(hrd$flight_ID[i] == hrd$flight_ID[i-1] & 
     (hrd$hdiff[i] > 5 | hrd$hdiff[i] < (-5)) & 
     (hrd$alti_ground[i] < hrd$qnt1_l[i] | hrd$alti_ground[i] > hrd$qnt3_h[i])) { 
    hrd$alti_clean[i] <- NA
  }
  else if(hrd$flight_ID[i] != hrd$flight_ID[i-1] &
          (hrd$alti_ground[i] < hrd$qnt1_l[i] | hrd$alti_ground[i] > hrd$qnt3_h[i])) {
    hrd$alti_clean[i] <- NA
  }
  else {hrd$alti_clean[i] = hrd$alti_ground[i]}
}


# change value of first row (hdiff)
hrd[1,"alti_clean"] <- ifelse(hrd[1,"alti_ground"] > hrd[1,"qnt1_l"] & 
                              hrd[1,"alti_ground"] < hrd[1,"qnt3_h"], 
                            hrd[1,"alti_clean"],NA)

# number of identified outliers
sum(is.na(hrd$alti_ground))# 0 
sum(is.na(hrd$alti_clean)) # 8971 rows

#  position of affected fixes for individual flights
check <- hrd[is.na(hrd$alti_clean),c("alti_clean","flight_nr")]
# position of identified outliers in a flight
tmp <- as.data.frame(table(check$flight_nr))
sum(tmp[1:3,"Freq"])/sum(tmp$Freq)
# 95.9% identified outliers in one of the first three fix-positions of a flight after take-off
rm(tmp, check)

# exluding outliers (alti_clean == NA)
tmp_outliers <- hrd[is.na(hrd$alti_clean),]
hrd <- setdiff(hrd, tmp_outliers)
nrow(hrd) # 1692624
rm(tmp_outliers)
# Visualisierung
hist(hrd$alti_clean, breaks = 200,xlim = c(-100,200), xlab = "Height", main = "Distribution of flight altitude")
axis(side = 1,at=seq(-100,200,50))
range(hrd$alti_clean,na.rm = T) # -738   1156 [meters above ground level] - wide range! 



# set threshold for proper flights
# many short-flights due to wrong activation of flight detection 
fli_detec <- track %>% filter(tdiff <= 3 & speed < 3.6)
tmp <- aggregate(tdiff~flight_ID,fli_detec,sum)
tmp <- dplyr::rename(tmp, duration = tdiff) 
tmp$count <- 1
tmp1 <- aggregate(count~duration,tmp,sum) # number of performed flights, sorted by duration  
nrow(tmp1) # 364 - time classifications - large amount of fixes lasted 10 seconds!
# Proportion of activated flight detection with a duration of 10 seconds 
max(tmp1$count)/sum(tmp1$count) # 34%
# Proportion of activated flight detection with a duration of 1-10 seconds 
sum(tmp1[c(1:11),"count"]) # 23525 (incl. 0)
sum(tmp1[c(1:11),"count"])/sum(tmp1$count) # 53% of all performed flights had a duration of <= 10 sec

rm(tmp,tmp1,fli_detec)

# exluding flights with a duration of <=10 sec (threshold for proper flights)
tmp <- aggregate(hrd~flight_ID, hrd, sum) # number fixes / flight
tmp_selec <- tmp %>% filter(hrd <= 10)
tmp2 <- subset(hrd, flight_ID %in% tmp_selec$flight_ID) # select flight_ID of affected flights 
nrow(tmp2)/nrow(hrd) # 82689/1692624 = 4.9% 
hrd <- setdiff(hrd, tmp2) # exclude affected flights by ID from hrd 
nrow(hrd) # 1609935 - about 80000 fixes excluded
rm(tmp, tmp_selec, tmp2)

# Export data: hrd
# change into sf object 
hrd_sf <- st_as_sf(hrd, crs = 25832)
st_crs(hrd_sf)   # ETRS89 / UTM zone 32N
#st_write(hrd_sf, "C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/QGIS/Curlew", "hrd", driver = "ESRI Shapefile")
rm(hrd_sf)


# 2.5 Calculation of accuracy in recorded fixes 
# vertical position
# difference between position & elevation of stationary birds 
# add new column: "pdiff" (position difference)
check <- track_sf %>% mutate(pdiff = ifelse(speed < 3.6 & hrd == 0, abs(alti_ground - elev), NA))
sum(!is.na(check$pdiff)) # 2004097
#mean(check$pdiff,na.rm = T)   # 19.78 m 
median(check$pdiff,na.rm = T) # 11.96 m
range(check$pdiff,na.rm = T)  # 0.00   1158.94 m
sd(check$pdiff,na.rm = T)     # 18.61
rm(check)

# difference between position & elevation of stationary birds with high transmission rate 
# wrong hrd-values
hrd_wrong <- track_sf
hrd_wrong <- hrd_wrong %>% mutate(hrd_w = ifelse(tdiff <= 3 & tdiff != 0 & speed < 3.6, 1, 0))
hrd_wrong <- hrd_wrong[hrd_wrong$hrd_w == 1, ]
check <- hrd_wrong %>% mutate(pdiff = ifelse(speed <3.6, abs(alti_ground - elev), NA))
sum(!is.na(check$pdiff))      # 957380 rows 
#mean(check$pdiff,na.rm = T)   # 18.03 m 
median(check$pdiff,na.rm = T) # 10.86 m
range(check$pdiff,na.rm = T)  # 0.00   1158.12 m
sd(check$pdiff,na.rm = T)     # 17.48 m
rm(check, hrd_wrong)



# HDOP
#mean(track$hdop)  # 1.155581
median(track$hdop)# 1.1
sd(track$hdop)    # 0.3552097
summary(track$hdop)
table(hrd$hdop)




# 2.6 check battery charge level
# Calculation of fixes with low battery charge status
table(track$battery_ch <50)   #   FALSE 3762653    TRUE 5007
# 5007 rows < 50% charge level

low_bat <- track %>% filter(battery_ch < 50)
nrow(low_bat)/nrow(track) # 0.001328942
# 0.13% of whole dataset with charge level < 50%
rm(low_bat)



# 2.7 prepare further evaluation steps 
# exclude breeding site information of birds without hrd  (n71 -> n51)
tmp_ind <- as.data.frame(table(hrd$tag_ident))
tmp_ind <- rename(tmp_ind, tag_ident = Var1)

n51 <- subset(n71, Tag_ID %in% tmp_ind$tag_ident)
rm(tmp_ind)
# Export data: breeding sites of 51 birds which provided flight data (hrd) 
setwd("C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/QGIS/Curlew")
write.csv(n51, file = "Curlew_n51_2021.csv", row.names = F)

# create n51 buffer (10 km)
n51_buf <- n51 %>% 
  st_as_sf(coords = c("nest_lat_2021","nest_lon_2021"), crs = 4326) %>% 
  st_transform(25832) %>% 
  st_buffer(dist = units::set_units(10, "kilometers")) %>% 
  lwgeom::st_snap_to_grid(10)
# remove variables
n51_buf[,c(9:10,16:25,27:33)] <- NULL
rm(n71_buf)
# Export data: 10km buffer n51
#st_write(n51_buf, "C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/QGIS/buffer", "n51_buf", driver = "ESRI Shapefile")


########### Save: R_script_chapter_2_envi #############




##### Chapter 3 - calculation of results #####
library(tidyr)
hrd$count<-"1"
hrd$count<-as.numeric(hrd$count)
# add new column: row_id
hrd$row_id <- as.numeric(1:nrow(hrd))

# 3.1 general flight information
# 3.1.1 general results
length(table(hrd$flight_ID))    # 27919 flights
median(hrd$flight_nr)           # 40 fixes per flight on average 
range(hrd$flight_nr)            # 1   2184 fixes - range of flight length
range(hrd$alti_clean,na.rm = T) # -137  457 - range of flight altitudes
length(table(hrd$tag_ident))    # 51 - number of birds which transmitted hrd
nrow(hrd)                       # 1609935 rows - number of considered flight fixes

# flight speed  
median(hrd$speed) # 11.38 km/h
range(hrd$speed)  #  3.61   37.77 km/h
sd(hrd$speed)     #  3.86 km/h
# female
hrd_f <- subset(hrd, Sex == "F") 
summary(hrd_f$speed) # median: 11.94  # min: 3.61  # max: 36.66 
sd(hrd_f$speed) # 3.78 
# male 
hrd_m <- subset(hrd, Sex == "M")
summary(hrd_m$speed) # median: 11.11  # min: 3.61  # max: 37.77 
sd(hrd_m$speed) # 3.88 
# test 
wilcox.test(hrd_f$speed, hrd_m$speed, alternative = "two.sided")
# W = 3.0746e+11, p-value < 2.2e-16
# significant! 
rm(hrd_m, hrd_f)

# generated flight fixes per bird 
tmp_fixes <- aggregate(count~tag_ident+Sex, hrd, sum)
summary(tmp_fixes$count) # median: 22238  # min: 257  # max: 200465
sd(tmp_fixes$count) # 36563.44
# female
tmp_fixes_f <- subset(tmp_fixes, Sex == "F")
summary(tmp_fixes_f$count) # median: 17376  # min: 3101  # max: 47540
sd(tmp_fixes_f$count) # 13256.43
# male
tmp_fixes_m <- subset(tmp_fixes, Sex == "M")
summary(tmp_fixes_m$count) # median: 23380  # min: 257  # max: 200465
sd(tmp_fixes_m$count) # 46984.78
# test 
wilcox.test(tmp_fixes_f$count,tmp_fixes_m$count, alternative = "two.sided")
# W = 266, p-value = 0.2805
# not significant! 

# Export
#write.table(tmp_fixes,file="hrd_dat_Ind.csv", row.names = F, sep = ";")
rm(tmp_fixes_f, tmp_fixes_m, tmp_fixes)


# 3.1.2 distribution of flight altitudes 
tmp <- as.data.frame(table(hrd$alti_clean)) # number of fixes for each transmitted altitude 
tmp$Var1 <- as.character(tmp$Var1)
tmp$Var1 <- as.numeric(tmp$Var1)
# just negative altitude values
tmp_neg <- tmp[tmp$Var1 < 0,]
sum(tmp_neg$Freq)                   # 462675 - number of fixes below ground level 
sum(tmp_neg$Freq)/sum(tmp$Freq)*100 # 28.73% - proportion of fixes below ground level

# consider negative altitudes 
hrd_neg <- hrd[hrd$alti_clean < 0, ]
nrow(hrd_neg) # 462675 - number of fixes with altitude below ground level 
nrow(hrd_neg)/nrow(hrd)*100       # 28.73% - proportion towards hrd
summary(hrd_neg$alti_clean)       # median of negative altitudes: -5m

# consider accuracy of 11m towards distribution of fixes below ground level
hrd_neg11 <- hrd[hrd$alti_clean < -11, ]
nrow(hrd_neg11)                   # 47477 rows - number of fixes < -11m
nrow(hrd_neg11)/nrow(hrd)*100     # 2.94% - proportion of distribution in hrd 
nrow(hrd_neg11)/nrow(hrd_neg)*100 # 10.26% - proportion negative altitudes below -11m 
# conversely: 89.74% fixes in tolerance range between -11 - -1m below ground level 
rm(tmp,tmp_neg,hrd_neg,hrd_neg11)



# 3.1.3 residence time of the birds in their breeding sites 
# affected days of investigation  
birds21 <- aggregate(count~day+tag_ident,hrd,sum)
range(birds21$day) #  "02/23/21" "07/22/21"
tmp <- as.data.frame(table(birds21$day)) # number of birds generating data each day 
tmp$Var1 <- as.Date(tmp$Var1, "%m/%d/%y")
tmp <- dplyr::rename(tmp, date = Var1)
nrow(tmp) # 150 days with hrd transmission
# Visualisation 
barplot(tmp$Freq,breaks=50)
boxplot(tmp$date)

summary(tmp$date)

# duration of breeding season (just 2020 birds, because of whole season information!)
birds20 <- subset(hrd, Year == "2020")
birds20$day <- as.Date(birds20$day, "%m/%d/%y")   # chr into date
birds20$tag_ident <- as.factor(birds20$tag_ident) # chr into factor

days1 <- aggregate(count~day+tag_ident,birds20,sum) # number of fixes for each bird on each day
days1$countday <- 1
days2 <- aggregate(countday~tag_ident,days1,sum) # number of days with hrd transmission for each bird from 2020
median(days2$countday) # 92 days per bird - average duration of breeding season! 
summary(days2$countday)
sd(days2$countday) # 35.77
length(table(days2$tag_ident)) # 23 birds 

rm(birds21,birds20,days1,days2,tmp)



# 3.1.4 track length 
# add new column: ddist (distance difference)
library(geosphere)
class(hrd) # data.frame

hrd <- hrd %>% 
  group_by(flight_ID) %>% 
  mutate(ddist = c(NA, geosphere::distVincentyEllipsoid(cbind(coords.x1,coords.x2)))) %>% 
  ungroup()
tmp_dist <- aggregate(ddist~flight_ID+tag_ident+Sex,hrd,sum)
summary(tmp_dist$ddist) # median: 334.22 m # min: 43.13 m # max: 27051.30 m
sd(tmp_dist$ddist) # 1170.51 m
# female
tmp_dist_f <- subset(tmp_dist, Sex == "F")
summary(tmp_dist_f$ddist) # median: 349.38 m # min: 43.13 m # max: 27051.3 m
sd(tmp_dist_f$ddist) # 1339.41
# male
tmp_dist_m <- subset(tmp_dist, Sex == "M")
summary(tmp_dist_m$ddist) # median: 327.71 m # min: 43.82 m # max: 22644.33 m
sd(tmp_dist_m$ddist) # 1097.79
# test 
wilcox.test(tmp_dist_f$ddist,tmp_dist_m$ddist, alternative = "two.sided")
# W = 81729021, p-value = 3.952e-16
# significant! 
rm(tmp_dist,tmp_dist_f,tmp_dist_m)

hrd <- as.data.frame(hrd)




# 3.2 wind turbine information
# select minRTH
WKA_minRTH <- WKA_UG_df[,c("ID_WKA","minRTH")]
WKA_minRTH$Parameter <- "minRTH"
WKA_minRTH <- rename(WKA_minRTH,height=minRTH)
# select hugh heights
WKA_hub <- WKA_UG_df[,c("ID_WKA","Nab_Hoeh")]
WKA_hub$Parameter <- "hub"
WKA_hub <- rename(WKA_hub,height=Nab_Hoeh)
# select maxRTH
WKA_maxRTH <- WKA_UG_df[,c("ID_WKA","Ges_Hoeh")]
WKA_maxRTH$Parameter <- "maxRTH"
WKA_maxRTH <- rename(WKA_maxRTH,height=Ges_Hoeh)
# combine tables
WKA <- rbind(WKA_minRTH,WKA_maxRTH)
WKA$ID_WKA <- NULL
WKA$Parameter <- factor(WKA$Parameter,levels = c("minRTH","hub","maxRTH"), ordered = T)
# plot
WKA_pic <- ggplot(WKA,aes(x=Parameter,y=height))+
  xlab("")+
  ylab("height (m)")+
  scale_y_continuous(limits = c(-10,260),breaks = seq(0,250,50))+
  geom_hline(aes(yintercept = 7, color = "red"),lwd=0.8,linetype=2,show.legend = F)+
  geom_hline(aes(yintercept = 246, color = "red"),lwd=0.8,linetype=2,show.legend = F)+
  stat_boxplot(geom = "errorbar", width = 0.15)+
  geom_boxplot(width=0.6,lwd=0.8)+
  theme_minimal()

WKA_pic + theme(panel.grid.major.y = element_line(), 
                panel.grid.major.x = element_line(),
                panel.background = element_blank(),
                axis.line = element_line(color = "black",size = 0.6),
                axis.text.y = element_text(color = "black",size = 12),
                axis.ticks = element_blank(),
                axis.text.x = element_text(color = "black",vjust =-1,size = 16),
                axis.title.y = element_text(color = "black",vjust = 2, size = 16),
                panel.border = element_rect(colour = "black", fill=NA, size=0.6),
                plot.margin = unit(c(0.1,0.1,0.1,0.1),"inches"))





# 3.3 flight activity
# 3.3.1 activity per day
# number of days with hrd transmission / bird
acti_day <- aggregate(count~day+tag_ident+Sex+Year, hrd, sum) 

# average flight duration of a day / bird (1 GPS-fix == 1 sec)
acti_day_re <- aggregate(count~tag_ident+Sex+Year, acti_day, median)
acti_day_re <- dplyr::rename(acti_day_re, count_min = count) 
acti_day_re$count_min <- acti_day_re$count_min/60 # average flight duration / day and bird 

summary(acti_day_re$count_min) # median: 5.65  # min: 1.39  # max: 23.667  
sd(acti_day_re$count_min) # 5.41 minutes

# average flight minutes / sex / day
# female
acti_day_f <- subset(acti_day_re, Sex == "F") 
summary(acti_day_f$count_min) # median: 5.2  # min: 1.86  # max: 22.66
sd(acti_day_f$count_min)     # 4.21 
# male
acti_day_m <- subset(acti_day_re, Sex == "M") 
summary(acti_day_m$count_min) # median: 6.36  # min: 1.39  # max: 23.66
sd(acti_day_m$count_min)      # 6.12 
# test 
wilcox.test(acti_day_f$count_min,acti_day_m$count_min,alternative = "two.sided")
# W = 246.5, p-value = 0.1462
# not significant! 
rm(acti_day_f, acti_day_m)

# Export result: 
#write_xlsx(acti_day_re, "a_flight_day_hrd.xlsx")
rm(acti_day)


# 3.3.2 activity throughout the day
acti_h <- aggregate(count~day+hour+tag_ident+Sex, hrd, sum) # number of fixes / hour / day / bird
names(acti_h) # "day"  "hour"  "tag_ident"  "Sex"  count"  

acti_ind <- aggregate(count~tag_ident+hour+Sex, acti_h, mean) # average number of fixes / hour / bird
# reshape 
acti_ind_re <- reshape(acti_ind, idvar="tag_ident", timevar="hour", v.names="count", direction="wide", sep="")
acti_ind_re[is.na(acti_ind_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
acti_ind_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

acti_h_medi <- data.frame(apply(acti_ind_re,2,median))
acti_h_medi$hour <- c(0:23) # add hours 
acti_h_medi <- acti_h_medi %>% select(hour, everything()) # switch column positions 
acti_h_medi <- dplyr::rename(acti_h_medi, median = apply.acti_ind_re..2..median.)
acti_h_medi$median_min <- acti_h_medi$median/60 # average flight duration / day / bird
# standard deviation 
acti_sd <- data.frame(apply(acti_ind_re,2,sd))
acti_sd <- dplyr::rename(acti_sd, sd = apply.acti_ind_re..2..sd.)
acti_h_medi$sd_min <- acti_sd$sd/60
# standard error
#acti_se <- data.frame(apply(acti_ind_re,2,se))
#acti_se <- dplyr::rename(acti_se, se = apply.acti_ind_re..2..se.)
#acti_h_medi$se_min <- acti_se$se/60
barplot(acti_h_medi$median_min)
# Export: 
#write_xlsx(acti_h_medi, "acti_flight_hour.xlsx")
rm(acti_h, acti_ind_re, acti_sd)


# divided by sex 
# female
acti_ind_f <- acti_ind %>% filter(Sex == "F")
# reshape
acti_ind_f_re <- reshape(acti_ind_f, idvar="tag_ident", timevar="hour", v.names="count", direction="wide", sep="")
acti_ind_f_re[is.na(acti_ind_f_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation
acti_ind_f_re[,1:2] <- NULL # remove first two columns

acti_h_medi_f <- data.frame(apply(acti_ind_f_re,2,median))
acti_h_medi_f$hour <- c(0:23)

acti_h_medi_f <- acti_h_medi_f %>% select(hour, everything())
acti_h_medi_f <- dplyr::rename(acti_h_medi_f, median_f = apply.acti_ind_f_re..2..median.)
acti_h_medi_f$median_min_f <- acti_h_medi_f$median_f/60 # average flight duration / day / female bird
# standard deviation (female)
acti_f_sd <- data.frame(apply(acti_ind_f_re,2,sd))
acti_f_sd <- dplyr::rename(acti_f_sd, sd_f = apply.acti_ind_f_re..2..sd.)
acti_h_medi_f$sd_min_f <- acti_f_sd$sd_f/60
# standard error
#acti_f_se <- data.frame(apply(acti_ind_f_re,2,se))
#acti_f_se <- dplyr::rename(acti_f_se, se_f = apply.acti_ind_f_re..2..se.)
#acti_h_medi_f$se_min_f <- acti_f_se$se_f/60
barplot(acti_h_medi_f$median_min_f)
rm(acti_ind_f, acti_ind_f_re, acti_f_sd)

# male
acti_ind_m <- acti_ind %>% filter(Sex == "M")
# reshape
acti_ind_m_re <- reshape(acti_ind_m, idvar="tag_ident", timevar="hour", v.names="count", direction="wide", sep="")
acti_ind_m_re[is.na(acti_ind_m_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation
acti_ind_m_re[,1:2] <- NULL # remove first two columns

acti_h_medi_m <- data.frame(apply(acti_ind_m_re,2,median))
acti_h_medi_m$hour <- c(0:23)

acti_h_medi_m <- acti_h_medi_m %>% select(hour, everything())
acti_h_medi_m <- dplyr::rename(acti_h_medi_m, median_m = apply.acti_ind_m_re..2..median.)
acti_h_medi_m$median_min_m <- acti_h_medi_m$median_m/60 # average flight duration / day / male bird
# standard deviation (male)
acti_m_sd <- data.frame(apply(acti_ind_m_re,2,sd))
acti_m_sd <- dplyr::rename(acti_m_sd,sd_m = apply.acti_ind_m_re..2..sd.)
acti_h_medi_m$sd_min_m <- acti_m_sd$sd_m/60
# standard error
#acti_m_se <- data.frame(apply(acti_ind_m_re,2,se))
#acti_m_se <- dplyr::rename(acti_m_se,  se_m = apply.acti_ind_m_re..2..se.)
#acti_h_medi_m$se_min_m <- acti_m_se$se_m/60
barplot(acti_h_medi_m$median_min_m)
rm(acti_ind_m, acti_ind_m_re, acti_m_sd)

# merge tables (uni / female / male)
acti_h_medi <- cbind(acti_h_medi, acti_h_medi_f, acti_h_medi_m)
glimpse(acti_h_medi)
acti_h_medi[,c(5,9)] <- NULL # remove duplicated hour columns
rm(acti_h_medi_f, acti_h_medi_m)

acti_h_medi <- dplyr::rename(acti_h_medi, c(female = median_min_f, male = median_min_m))

# Visualisierung
library(reshape2)
acti_sex <- melt(acti_h_medi[,c('hour','female','male')],id.vars = "hour")
acti_sex <- dplyr::rename(acti_sex, c(minutes = value, sex = variable))
# sd
acti_sex_sd <- melt(acti_h_medi[,c('hour','sd_min_f','sd_min_m')],id.vars = "hour")
acti_sex_sd <- dplyr::rename(acti_sex_sd, c(sd_min = value, sex = variable))
# se
#acti_sex_se <- melt(acti_h_medi[,c('hour','se_min_f','se_min_m')],id.vars = "hour")
#acti_sex_se <- dplyr::rename(acti_sex_se, c(se_min = value, sex = variable))
# Tab zusammenfuehren
acti_plot <- cbind(acti_sex,acti_sex_sd)
acti_plot[,c(4,5)] <- NULL # remove duplicated columns
rm(acti_sex,acti_sex_sd)
# Export result:
acti_plot$minutes <- round(acti_plot$minutes, digits = 2)
acti_plot$sd_min <- round(acti_plot$sd_min, digits = 2)
#write.table(acti_plot, "Activity_throughout_day_tab.csv", row.names=F, sep=",", dec = ".")

# Visualisation 
acti_pic <- ggplot(acti_plot, aes(x = hour, y = minutes, fill = sex))+
  geom_bar(stat = "identity", position = "dodge",width = 0.6)+
  geom_errorbar(aes(ymin = minutes, ymax = minutes + sd_min),
                colour="black",width=.2,position=position_dodge(.6))+
  xlab("Time of day (UTC)")+
  ylab("Flight time (minutes)")+
  guides(fill=guide_legend(title="Sex"))+
  scale_fill_manual(values = c('dodgerblue4','deepskyblue'))+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(breaks = acti_plot$hour)

acti_pic + theme(panel.grid.major.y = element_line(color = "black",size=0.1,linetype = 2), 
               panel.grid.minor = element_blank(),
               panel.background = element_blank(), 
               axis.line = element_blank(),
               axis.text = element_text(color = "black",size = 13),
               axis.ticks = element_blank(),
               axis.text.x = element_text(color = "black",vjust =5),
               axis.title.x = element_text(color = "black",vjust =1),
               axis.title.y = element_text(color = "black",vjust =4),
               legend.text = element_text(color = "black",size = 14),
               legend.title = element_text(size = 20),
               text = element_text(size = 22),
               plot.margin = unit(c(0.1,0.1,0.1,0.2),"inches"))



# 3.4 Dependency of flight distance and flight altitude
# 3.4.1 Euclidean distance 
sum(is.na(hrd$geometry)) # 0

# divide multipoint & merge in table
euc_tmp <- data.frame(st_coordinates(st_cast(hrd$geometry, "MULTIPOINT")))
euc_tmp <- dplyr::rename(euc_tmp, row_id = L1)
hrd <- merge(hrd, euc_tmp, by = "row_id", all = T)
rm(euc_tmp)
# select first and last fix of each flight
euc_dep_arr <- hrd %>% 
  group_by(flight_ID) %>% 
  filter(row_number() == 1 | row_number() == n())
class(euc_dep_arr) # "grouped_df" "tbl_df" "tbl" "data.frame"
euc_tmp <- data.frame(table(euc_dep_arr$flight_ID))
table(euc_tmp$Freq) # take-off and landing for each flight is given 
rm(euc_tmp)

euc_dep_arr <- dplyr::rename(euc_dep_arr, long = X)
euc_dep_arr <- dplyr::rename(euc_dep_arr, lat = Y)
sum(is.na(euc_dep_arr$long)) # 0

# separate take-off and landing (separate even & odd rows)
row_odd <- seq_len(nrow(euc_dep_arr)) %% 2 # mark even & odd rows with 1 / 0

euc_depart <- euc_dep_arr[row_odd == 1, ]  # filter 1 (take-off)
sum(duplicated(euc_depart$flight_ID))         # 0 - no duplicates

euc_arrival <- euc_dep_arr[row_odd == 0, ] # filter 0 (landing)
sum(duplicated(euc_arrival$flight_ID))        # 0 - no duplicates

euc_depart_sf <- euc_depart %>% st_as_sf(coords = c("long", "lat"), crs = 25832)
euc_arrival_sf <- euc_arrival %>% st_as_sf(coords = c("long", "lat"), crs = 25832)

# calculation of Euclidean distance 
euc_dist <- as.data.frame(st_distance(euc_depart_sf, euc_arrival_sf, by_element = T, which = "Euclidean"))
euc_dist <- dplyr::rename(euc_dist, eucl_dist = `st_distance(euc_depart_sf, euc_arrival_sf, by_element = T, which = "Euclidean")`)
euc_dist$flight_ID <- euc_depart$flight_ID # add flight ID
nrow(euc_dist) # 27919 rows
euc_dist$eucl_dist <- round(euc_dist$eucl_dist, digits = 2)
# assign days
euc_tmp <- euc_arrival[,c("flight_ID","Sex")]
euc_dist <- merge(euc_dist,euc_tmp, by="flight_ID", all = T) # add variables
euc_dist$eucl_dist <- as.numeric(euc_dist$eucl_dist)

summary(euc_dist$eucl_dist) # median: 205.74   # min: 0.32  # max: 14406.59
sd(euc_dist$eucl_dist)     # 828.38 m
# female 
euc_dist_f <- euc_dist %>% filter(Sex == "F")
summary(euc_dist_f$eucl_dist) #  median: 220.54  # min: 4.6  # max: 14406.6 
sd(euc_dist_f$eucl_dist)     # 1025.37 m
# male
euc_dist_m <- euc_dist %>% filter(Sex == "M")
summary(euc_dist_m$eucl_dist) # median: 201.19  # min: 0.32  # max: 12475.84
sd(euc_dist_m$eucl_dist)     # 737.26 m
# test 
wilcox.test(euc_dist_f$eucl_dist,euc_dist_m$eucl_dist,alternative = "two.sided")
# W = 81649614, p-value = 1.173e-15
# significant! 
rm(euc_arrival_sf,euc_arrival,euc_depart_sf,euc_tmp,euc_dist_m,euc_dist_f)

#write_xlsx(euc_dist, "euc_eucl.dist_flight.xlsx")
#write.table(hrd, "dat_hrd.csv", row.names=F, sep=";", dec = ".")


# 3.4.2 GLMM
euc_dist$Sex <- NULL
hrd <- merge(hrd,euc_dist, by="flight_ID")
rm(euc_dist)

# average flight altitude of each flight (median)
euc_height <- aggregate(alti_clean~flight_ID+tag_ident+eucl_dist,hrd,median)

plot(euc_height$alti_clean~euc_height$eucl_dist)
range(euc_height$alti_clean) # -118  334

euc_height$tag_ident <- as.numeric(euc_height$tag_ident)
euc_height$eucl_dist <- as.numeric(euc_height$eucl_dist)

# GLMM 
# positive values only
euc_dat <- euc_height
rm(euc_height)
euc_dat0 <- subset(euc_dat, euc_dat$alti_clean >= 0)
euc_dat0$alti_clean <- round(euc_dat0$alti_clean, 0)

dim(euc_dat) # considering all flights
dim(euc_dat0) # exclude flights <0 
range(euc_dat0$alti_clean) #  0    334 m

# rescale eucl_dist
euc_dat0$euc_km <- euc_dat0$eucl_dist/1000
euc_dat0[order(euc_dat0$euc_km, decreasing=TRUE),]
range(euc_dat0$euc_km) # 0.0011   14.4 km

# rescale altitude
euc_dat0$alti_clean2 <- round(euc_dat0$alti_clean/10, 0)

# negative binomial scaled altitude ########## 
library(lme4)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

euc_mod_nb <- glmer.nb(alti_clean2 ~ euc_km+(1|tag_ident), 
                      data = euc_dat0, 
                      control = glmerControl(optimizer = "bobyqa", 
                                             optCtrl = list(maxfun = 2e5)))  # unidentifiable
summary(euc_mod_nb)
overdisp_fun(euc_mod_nb) # 1.38
library(MuMIn)
r.squaredGLMM(euc_mod_nb)
qqnorm(residuals(euc_mod_nb)) 
qqline(resid(euc_mod_nb))

library(ggeffects)
euc <- seq(min(euc_dat0$euc_km),max(euc_dat0$euc_km),length.out = 100)
euc_pred1 <- ggpredict(euc_mod_nb,"euc_km[euc]")
# Export
#write.table(euc_pred1, "Dependency_dist_alti_tab.csv", row.names=F, sep=",", dec = ".")

# plot GLMM (flight distance - flight altitude)
euc_pic <- ggplot(data=euc_pred1,aes(x=x, y=predicted,group=1))+
  geom_point(size=1)+
  xlab("Flight distance (km)")+
  ylab("Flight altitude (m)")+
  scale_x_continuous(limits = c(0,16),breaks = seq(0,15,5))+
  scale_y_continuous(limits = c(0,160),breaks = seq(0,150,50))

euc_pic + theme(panel.grid.major.y = element_line(color = "grey",size=0.05,linetype = 2), 
                 panel.grid.major.x = element_line(color = "grey",size=0.05,linetype = 2),
                 panel.background = element_blank(), 
                 axis.line = element_line(color = "black",size = 0.6),
                 axis.text = element_text(color = "black",size = 12),
                 axis.ticks = element_blank(),
                 axis.title.x = element_text(color = "black",vjust = 1, size = 16),
                 axis.title.y = element_text(color = "black",vjust = 2, size = 16),
                 panel.border = element_rect(colour = "black", fill=NA, size=0.6),
                 plot.margin = unit(c(0.1,0.1,0.1,0.1),"inches"))
rm(euc_dat,euc_dat0)



# 3.4.3 flight distances throughout the day 
# Distance between take-off and landing throughout the day
dist_ind <- aggregate(eucl_dist~tag_ident+hour+Sex,hrd,median)
# reshape 
dist_ind_re <- reshape(dist_ind, idvar="tag_ident", timevar="hour", v.names="eucl_dist", direction="wide", sep="")
dist_ind_re[is.na(dist_ind_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
dist_ind_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

dist_h_medi <- data.frame(apply(dist_ind_re,2,median,na.rm=T))
dist_h_medi$hour <- c(0:23) # add hours 
dist_h_medi <- dist_h_medi %>% select(hour, everything()) # switch column positions 
dist_h_medi <- dplyr::rename(dist_h_medi, median = apply.dist_ind_re..2..median..na.rm...T.)

dist_sd <- data.frame(apply(dist_ind_re,2,sd))
dist_sd <- dplyr::rename(dist_sd, sd = apply.dist_ind_re..2..sd.)
dist_h_medi$sd <- dist_sd$sd
rm(dist_sd,dist_ind_re)

# divided by sex 
# female
dist_ind_f <- dist_ind %>% filter(Sex == "F")
# reshape 
dist_ind_f_re <- reshape(dist_ind_f, idvar="tag_ident", timevar="hour", v.names="eucl_dist", direction="wide", sep="")
dist_ind_f_re[is.na(dist_ind_f_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
dist_ind_f_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

dist_h_medi_f <- data.frame(apply(dist_ind_f_re,2,median,na.rm=T))
dist_h_medi_f$hour <- c(0:23) # add hours 
dist_h_medi_f <- dist_h_medi_f %>% select(hour, everything()) # switch column positions 
dist_h_medi_f <- dplyr::rename(dist_h_medi_f, median_f = apply.dist_ind_f_re..2..median..na.rm...T.)

dist_f_sd <- data.frame(apply(dist_ind_f_re,2,sd))
dist_f_sd <- dplyr::rename(dist_f_sd, sd_f = apply.dist_ind_f_re..2..sd.)
dist_h_medi_f$sd_f <- dist_f_sd$sd
rm(dist_f_sd,dist_ind_f,dist_ind_f_re)

# male
dist_ind_m <- dist_ind %>% filter(Sex == "M")
# reshape 
dist_ind_m_re <- reshape(dist_ind_m, idvar="tag_ident", timevar="hour", v.names="eucl_dist", direction="wide", sep="")
dist_ind_m_re[is.na(dist_ind_m_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
dist_ind_m_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

dist_h_medi_m <- data.frame(apply(dist_ind_m_re,2,median,na.rm=T))
dist_h_medi_m$hour <- c(0:23) # add hours 
dist_h_medi_m <- dist_h_medi_m %>% select(hour, everything()) # switch column positions 
dist_h_medi_m <- dplyr::rename(dist_h_medi_m, median_m = apply.dist_ind_m_re..2..median..na.rm...T.)

dist_m_sd <- data.frame(apply(dist_ind_m_re,2,sd))
dist_m_sd <- dplyr::rename(dist_m_sd, sd_m = apply.dist_ind_m_re..2..sd.)
dist_h_medi_m$sd_m <- dist_m_sd$sd_m
rm(dist_m_sd,dist_ind_m,dist_ind_m_re)

# merge tables (uni / female / male)
dist_h_medi <- cbind(dist_h_medi, dist_h_medi_f, dist_h_medi_m)
glimpse(dist_h_medi)
dist_h_medi[,c(4,7)] <- NULL # remove duplicated hour columns
dist_h_medi <- dplyr::rename(dist_h_medi, c(female = median_f, male = median_m))
rm(dist_h_medi_f, dist_h_medi_m)

# Visualisierung
dist_sex <- melt(dist_h_medi[,c('hour','female','male')],id.vars = "hour")
dist_sex <- dplyr::rename(dist_sex, c(eucl_dist = value, Sex = variable))
# sd
dist_sex_sd <- melt(dist_h_medi[,c('hour','sd_f','sd_m')],id.vars = "hour")
dist_sex_sd <- dplyr::rename(dist_sex_sd, c(sd = value, Sex = variable))
# se
#dist_sex_se <- melt(dist_h_medi[,c('hour','median_se_f','median_se_m')],id.vars = "hour")
#dist_sex_se <- dplyr::rename(dist_sex_se, c(se = value, Sex = variable))
# Tab zusammenfuehren
dist_plot <- cbind(dist_sex,dist_sex_sd)
dist_plot[,c(4,5)] <- NULL # remove duplicated columns
rm(dist_sex,dist_sex_sd)

# Export result:
dist_plot$sd <- round(dist_plot$sd, digits = 2)
#write.table(dist_plot, "Distance_take-off_landing_throughout_day_tab.csv", row.names=F, sep=",", dec = ".")

# plot 
dist_pic <- ggplot(dist_plot, aes(x = hour, y = eucl_dist, fill = Sex))+
  geom_bar(stat = "identity", position = "dodge",width = 0.6)+
  geom_errorbar(aes(ymin = eucl_dist, ymax = eucl_dist + sd),
                colour="black",width=.2,position=position_dodge(.6))+
  xlab("Time of day (UTC)")+
  ylab("Euclidean distance (m)")+
  scale_fill_manual(values = c('dodgerblue4','deepskyblue'))+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(breaks = dist_plot$hour)

dist_pic + theme(panel.grid.major.y = element_line(color = "black",size=0.1,linetype = 2), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_blank(),
                 axis.text = element_text(color = "black",size = 13),
                 axis.ticks = element_blank(),
                 axis.text.x = element_text(color = "black",vjust =5),
                 axis.title.x = element_text(color = "black",vjust =1),
                 axis.title.y = element_text(color = "black",vjust =4),
                 legend.text = element_text(color = "black",size = 14),
                 legend.title = element_text(color = "black",size = 20),
                 text = element_text(color = "black",size = 22),
                 plot.margin = unit(c(0.1,0.1,0.1,0.2),"inches"))




# 3.5 flight duration
tmp_dur <- aggregate(timestamp~flight_ID+Sex,euc_dep_arr, diff)
tmp_dur <- tmp_dur[order(tmp_dur$flight_ID),]
tmp_dur <- dplyr::rename(tmp_dur, duration = timestamp)
tmp_dur$duration <- as.numeric(tmp_dur$duration)

summary(tmp_dur$duration) # median: 31  # min: 10  # max: 2183
sd(tmp_dur$duration) # 88.13 sec
# separated by sex 
# female
tmp_dur_f <- tmp_dur %>% filter(Sex == "F")
summary(tmp_dur_f$duration) # median: 31  # min: 10  # max: 2183
sd(tmp_dur_f$duration) # 101.22 sec
# male
tmp_dur_m <- tmp_dur %>% filter(Sex == "M")
summary(tmp_dur_m$duration) # median: 30  # min: 10  # max: 2010
sd(tmp_dur_m$duration) # 82.57 sec
# test 
wilcox.test(tmp_dur_f$duration,tmp_dur_m$duration, alternative = "two.sided")
# W = 79241838, p-value = 6.836e-05
# not significant! 

# add new column: duration
tmp_dur$Sex <- NULL
hrd <- merge(hrd, tmp_dur, by = "flight_ID")

rm(tmp_dur,tmp_dur_f,tmp_dur_m)



# 3.6 number of flights
# 3.6.1 number of flights per day
fli_day <- aggregate(count~day+tag_ident+Sex, euc_depart, sum)
summary(fli_day$count) # median: 7  # min: 1  # max: 91
sd(fli_day$count)     # 10.5

barplot(fli_day$count)

# separated by sex
fli_day_sex <- fli_day %>% 
  mutate(female = ifelse(Sex == "F", count, NA)) %>% 
  mutate(male = ifelse(Sex == "M", count, NA))
# females
summary(fli_day_sex$female,na.rm = T) # median: 5  # min: 1  # max: 34
sd(fli_day_sex$female,na.rm = T)     # 5.2
# males
summary(fli_day_sex$male,na.rm = T) # median: 10  # min: 1  # max: 91
sd(fli_day_sex$male,na.rm = T)      # 12.4
# test 
wilcox.test(fli_day_sex$female,fli_day_sex$male,alternative = "two.sided")
# W = 561737, p-value < 2.2e-16
# significant! 
rm(fli_day, fli_day_sex) 
rm(euc_dep_arr)



# 3.5.2 number of flights throughout the day
flights_h <- aggregate(count~day+hour+tag_ident+Sex, euc_depart, sum)
flights_ind <- aggregate(count~tag_ident+hour+Sex,flights_h,mean)
rm(flights_h)
# reshape 
flights_ind_re <- reshape(flights_ind, idvar="tag_ident", timevar="hour", v.names="count", direction="wide", sep="")
flights_ind_re[is.na(flights_ind_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
flights_ind_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

flights_h_medi <- data.frame(apply(flights_ind_re,2,median))
flights_h_medi$hour <- c(0:23) # add hours 
flights_h_medi <- flights_h_medi %>% select(hour, everything()) # switch column positions 
flights_h_medi <- dplyr::rename(flights_h_medi, median = apply.flights_ind_re..2..median.)

flights_sd <- data.frame(apply(flights_ind_re,2,sd))
flights_sd <- dplyr::rename(flights_sd, sd = apply.flights_ind_re..2..sd.)
flights_h_medi$median_sd <- flights_sd$sd
rm(flights_sd,flights_ind_re)

# divided by sex 
# female
flights_ind_f <- flights_ind %>% filter(Sex == "F")
# reshape 
flights_ind_f_re <- reshape(flights_ind_f, idvar="tag_ident", timevar="hour", v.names="count", direction="wide", sep="")
flights_ind_f_re[is.na(flights_ind_f_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
flights_ind_f_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

flights_h_medi_f <- data.frame(apply(flights_ind_f_re,2,median))
flights_h_medi_f$hour <- c(0:23) # add hours 
flights_h_medi_f <- flights_h_medi_f %>% select(hour, everything()) # switch column positions 
flights_h_medi_f <- dplyr::rename(flights_h_medi_f, median_f = apply.flights_ind_f_re..2..median.)

flights_f_sd <- data.frame(apply(flights_ind_f_re,2,sd))
flights_f_sd <- dplyr::rename(flights_f_sd, sd_f = apply.flights_ind_f_re..2..sd.)
flights_h_medi_f$sd_f <- flights_f_sd$sd_f

rm(flights_f_sd,flights_ind_f,flights_ind_f_re)

# male
flights_ind_m <- flights_ind %>% filter(Sex == "M")
# reshape 
flights_ind_m_re <- reshape(flights_ind_m, idvar="tag_ident", timevar="hour", v.names="count", direction="wide", sep="")
flights_ind_m_re[is.na(flights_ind_m_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
flights_ind_m_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

flights_h_medi_m <- data.frame(apply(flights_ind_m_re,2,median))
flights_h_medi_m$hour <- c(0:23) # add hours 
flights_h_medi_m <- flights_h_medi_m %>% select(hour, everything()) # switch column positions 
flights_h_medi_m <- dplyr::rename(flights_h_medi_m, median_m = apply.flights_ind_m_re..2..median.)

flights_m_sd <- data.frame(apply(flights_ind_m_re,2,sd))
flights_m_sd <- dplyr::rename(flights_m_sd, sd_m = apply.flights_ind_m_re..2..sd.)
flights_h_medi_m$sd_m <- flights_m_sd$sd_m

rm(flights_m_sd,flights_ind_m,flights_ind_m_re)

# merge tables (uni / female / male)
flights_h_medi <- cbind(flights_h_medi, flights_h_medi_f, flights_h_medi_m)
glimpse(flights_h_medi)
flights_h_medi[,c(4,7)] <- NULL # remove duplicated hour columns
flights_h_medi <- dplyr::rename(flights_h_medi, c(female = median_f, male = median_m))

rm(flights_h_medi_f, flights_h_medi_m)

# Visualisierung
flights_sex <- melt(flights_h_medi[,c('hour','female','male')],id.vars = "hour")
flights_sex <- dplyr::rename(flights_sex, c(flights = value, Sex = variable))
# sd
flights_sex_sd <- melt(flights_h_medi[,c('hour','sd_f','sd_m')],id.vars = "hour")
flights_sex_sd <- dplyr::rename(flights_sex_sd, c(sd = value, Sex = variable))
# se
#flights_sex_se <- melt(flights_h_medi[,c('hour','medi_se_f','medi_se_m')],id.vars = "hour")
#flights_sex_se <- dplyr::rename(flights_sex_se, c(se = value, Sex = variable))
# Tab zusammenfuehren
flights_plot <- cbind(flights_sex,flights_sex_sd)
flights_plot[,c(4,5)] <- NULL # remove duplicated columns

rm(flights_sex,flights_sex_sd)

# Export result:
flights_plot$sd <- round(flights_plot$sd, digits = 2)
#write.table(flights_plot, "Flights_throughout_day_tab.csv", row.names=F, sep=",", dec = ".")

# plot 
flights_pic <- ggplot(flights_plot, aes(x = hour, y = flights, fill = Sex))+
  geom_bar(stat = "identity", position = "dodge",width = 0.6)+
  geom_errorbar(aes(ymin = flights, ymax = flights + sd),
                colour="black",width=.2,position=position_dodge(.6))+
  xlab("Time of day (UTC)")+
  ylab("Number of flights")+
  scale_fill_manual(values = c('dodgerblue4','deepskyblue'))+
  scale_x_continuous(breaks = flights_plot$hour)

flights_pic + theme(panel.grid.major.y = element_line(color = "black",size=0.1,linetype = 2), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_blank(),
                    axis.text = element_text(color = "black",size = 13),
                    axis.ticks = element_blank(),
                    axis.text.x = element_text(color = "black",vjust =5),
                    axis.title.x = element_text(color = "black",vjust =1),
                    axis.title.y = element_text(color = "black",vjust =4),
                    legend.text = element_text(color = "black",size = 14),
                    legend.title = element_text(color = "black",size = 20),
                    text = element_text(color = "black",size = 22),
                    plot.margin = unit(c(0.1,0.1,0.1,0.2),"inches"))




#### 3.7 vertical overlap 
# 3.7.1 average flight altitude / bird
alti <- aggregate(alti_clean~tag_ident+Sex,hrd,median)
summary(alti$alti_clean)  # median: 8 m  # min: -2  # max: 46
sd(alti$alti_clean)      # 9.3 m
# female
alti_f <- subset(alti, Sex == "F")
summary(alti_f$alti_clean) # median: 13  # min: -1  # max: 46
sd(alti_f$alti_clean) # 10.9
# male
alti_m <- subset(alti, Sex == "M")
summary(alti_m$alti_clean) # median: 4  # min: -2  # max: 19
sd(alti_m$alti_clean) # 5.7
# test 
wilcox.test(alti_f$alti_clean,alti_m$alti_clean,alternative = "two.sided")
# W = 486.5, p-value = 0.002202
# significant! 
rm(alti,alti_f,alti_m)


# flight altitude range
range(hrd$alti_clean) #  -137  457 


# average flight altitude / bird / h
alti_ind <- aggregate(alti_clean~tag_ident+hour+Sex,hrd,median)
# reshape 
alti_ind_re <- reshape(alti_ind, idvar="tag_ident", timevar="hour", v.names="alti_clean", direction="wide", sep="")
#alti_ind_re[is.na(alti_ind_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
alti_ind_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

alti_h_medi <- data.frame(apply(alti_ind_re,2,median,na.rm=T))
alti_h_medi$hour <- c(0:23) # add hours 
alti_h_medi <- alti_h_medi %>% select(hour, everything()) # switch column positions 
alti_h_medi <- dplyr::rename(alti_h_medi, median = apply.alti_ind_re..2..median..na.rm...T.)

alti_sd <- data.frame(apply(alti_ind_re,2,sd,na.rm=T))
alti_sd <- dplyr::rename(alti_sd, sd = apply.alti_ind_re..2..sd..na.rm...T.)
alti_h_medi$sd <- alti_sd$sd
rm(alti_sd,alti_ind_re)

# divided by sex 
# female
alti_ind_f <- alti_ind %>% filter(Sex == "F")
# reshape 
alti_ind_f_re <- reshape(alti_ind_f, idvar="tag_ident", timevar="hour", v.names="alti_clean", direction="wide", sep="")
#alti_ind_f_re[is.na(alti_ind_f_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
alti_ind_f_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

alti_h_medi_f <- data.frame(apply(alti_ind_f_re,2,median,na.rm=T))
alti_h_medi_f$hour <- c(0:23) # add hours 
alti_h_medi_f <- alti_h_medi_f %>% select(hour, everything()) # switch column positions 
alti_h_medi_f <- dplyr::rename(alti_h_medi_f, median_f = apply.alti_ind_f_re..2..median..na.rm...T.)

alti_f_sd <- data.frame(apply(alti_ind_f_re,2,sd,na.rm=T))
alti_f_sd <- dplyr::rename(alti_f_sd, sd_f = apply.alti_ind_f_re..2..sd..na.rm...T.)
alti_h_medi_f$sd_f <- alti_f_sd$sd_f
rm(alti_f_sd,alti_ind_f,alti_ind_f_re)

# male
alti_ind_m <- alti_ind %>% filter(Sex == "M")
# reshape 
alti_ind_m_re <- reshape(alti_ind_m, idvar="tag_ident", timevar="hour", v.names="alti_clean", direction="wide", sep="")
#alti_ind_m_re[is.na(alti_ind_m_re)] <- 0 # NA to 0 , to avoid exclusion of evaluation 
alti_ind_m_re[,1:2] <- NULL # remove first two columns - just keep hours in table 

alti_h_medi_m <- data.frame(apply(alti_ind_m_re,2,median,na.rm=T))
alti_h_medi_m$hour <- c(0:23) # add hours 
alti_h_medi_m <- alti_h_medi_m %>% select(hour, everything()) # switch column positions 
alti_h_medi_m <- dplyr::rename(alti_h_medi_m, median_m = apply.alti_ind_m_re..2..median..na.rm...T.)

alti_m_sd <- data.frame(apply(alti_ind_m_re,2,sd,na.rm=T))
alti_m_sd <- dplyr::rename(alti_m_sd, sd_m = apply.alti_ind_m_re..2..sd..na.rm...T.)
alti_h_medi_m$sd_m <- alti_m_sd$sd_m
rm(alti_m_sd,alti_ind_m,alti_ind_m_re)

# merge tables (uni / female / male)
alti_h_medi <- cbind(alti_h_medi, alti_h_medi_f, alti_h_medi_m)
glimpse(alti_h_medi)
alti_h_medi[,c(4,7)] <- NULL # remove duplicated hour columns
alti_h_medi <- dplyr::rename(alti_h_medi, c(female = median_f, male = median_m))
rm(alti_h_medi_f, alti_h_medi_m)

# Visualisierung
alti_sex <- melt(alti_h_medi[,c('hour','female','male')],id.vars = "hour")
alti_sex <- dplyr::rename(alti_sex, c(altitude = value, Sex = variable))
# sd
alti_sex_sd <- melt(alti_h_medi[,c('hour','sd_f','sd_m')],id.vars = "hour")
alti_sex_sd <- dplyr::rename(alti_sex_sd, c(sd = value, Sex = variable))
# se
#alti_sex_se <- melt(alti_h_medi[,c('hour','median_se_f','median_se_m')],id.vars = "hour")
#alti_sex_se <- dplyr::rename(alti_sex_se, c(se = value, Sex = variable))
# Tab zusammenfuehren
alti_plot <- cbind(alti_sex,alti_sex_sd)
alti_plot[,c(4,5)] <- NULL # remove duplicated columns
rm(alti_sex,alti_sex_sd)

# Export result:
alti_plot$sd <- round(alti_plot$sd, digits = 2)
#write.table(alti_plot, "Flight_altitudes_throughout_day_tab.csv", row.names=F, sep=",", dec = ".")

# plot 
alti_pic <- ggplot(alti_plot, aes(x = hour, y = altitude, fill = Sex))+
  geom_bar(stat = "identity", position = "dodge",width = 0.6)+
  geom_errorbar(aes(ymin = altitude, ymax = altitude + sd),
                colour="black",width=.2,position=position_dodge(.6))+
  xlab("Time of day (UTC)")+
  ylab("Flight altitude (m)")+
  scale_fill_manual(values = c('dodgerblue4','deepskyblue'))+
  scale_x_continuous(breaks = alti_plot$hour)

alti_pic + theme(panel.grid.major.y = element_line(color = "black",size=0.1,linetype = 2), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_blank(),
                   axis.text = element_text(color = "black",size = 13),
                   axis.ticks = element_blank(),
                   axis.text.x = element_text(color = "black",vjust =5),
                   axis.title.x = element_text(color = "black",vjust =1),
                   axis.title.y = element_text(color = "black",vjust =4),
                   legend.text = element_text(color = "black",size = 14),
                   legend.title = element_text(color = "black",size = 20),
                   text = element_text(color = "black",size = 22),
                 plot.margin = unit(c(0.1,0.1,0.1,0.2),"inches"))



# 3.7.2 proportion of fixes in the rhr 
alti_rhr <- hrd %>% filter(alti_clean >= WKA_rhr[1,"buf_min"] & 
                               alti_clean <= WKA_rhr[1,"buf_max"])
nrow(alti_rhr) # 799281 rows
length(table(alti_rhr$flight_ID)) # 14784 flights
# proportion to hrd
nrow(alti_rhr)/nrow(hrd)*100 # 49.64% - fixes
length(table(alti_rhr$flight_ID))/length(table(hrd$flight_ID))*100 # 52.95% - flights
# time that was spend in the rhr of wind turbines 
# (average flight activity x proportion of fixes in RHR)
median(acti_day_re$count_min)*(nrow(alti_rhr)/nrow(hrd)) # 2.8 minutes


# proportion of fixes in the rhr, built since 2018
alti_rhr_2018 <- hrd %>% filter(alti_clean >= WKA_rhr[2,"buf_min"] & 
                                    alti_clean <= WKA_rhr[1,"buf_max"])
nrow(alti_rhr_2018) # 596888 rows
length(table(alti_rhr_2018$flight_ID)) # 9963 flights
# proportion to hrd
nrow(alti_rhr_2018)/nrow(hrd)*100 # 37.07% - fixes
length(table(alti_rhr_2018$flight_ID))/length(table(hrd$flight_ID))*100 # 35.68% - flights
# time that was spend in the rhr of wind turbines, built since 2018
median(acti_day_re$count_min)*(nrow(alti_rhr_2018)/nrow(hrd)) # 2.1 minutes


# Visualisation 
alti_tmp <- data.frame(hrd$count,hrd$alti_clean)
alti_tmp <- rename(alti_tmp, alti_clean = hrd.alti_clean)
alti_tmp <- rename(alti_tmp, count = hrd.count)
# devide into inverval of 10 
alti_tmp <- alti_tmp[order(alti_tmp$alti_clean),]
min(alti_tmp$alti_clean) # -137
max(alti_tmp$alti_clean) #  457
min_value <- -141
max_value <-  459 
int_range <- 10
intervals <- seq(min_value,max_value, by= int_range)
# add new column: interval
alti_tmp$interval <- cut(alti_tmp$alti_clean,breaks=intervals,labels = F, include.lowest=T)
# exlude fixes above 250m and below -10m 
alti_tmp <- alti_tmp %>% filter(alti_clean <= 250 & alti_clean >= -10)
nrow(alti_tmp) # 1527815
alti_tmp$interval <- (alti_tmp$interval - 15)*10

prop_plot <- aggregate(count~interval,alti_tmp,sum)
prop_plot$prop <- prop_plot$count/nrow(hrd)*100
# Export
#write.table(alti_plot, "Frequency_distribution_altitude_tab.csv", row.names=F, sep=",", dec = ".")

# plot
prop_pic <- ggplot(data=prop_plot,aes(x=prop, y=interval,group=1))+
  geom_step(lwd=0.8)+
  xlab("Proportion of fixes (%)")+
  ylab("Flight altitude (m)")+
  scale_x_continuous(limits = c(0,30),breaks = seq(0,30,5))+ 
  scale_y_continuous(limits = c(-10,260),breaks = seq(0,250,50))+ 
  geom_hline(aes(yintercept = 7, color = "red"),lwd=0.8,linetype = 2,show.legend = F)+
  geom_hline(aes(yintercept = 246, color = "red"),lwd=0.8,linetype = 2,show.legend = F)+
  theme_minimal()

prop_pic + theme(panel.grid.major.y = element_line(), 
             panel.grid.major.x = element_line(),
             panel.background = element_blank(), 
             axis.line = element_line(color = "black",size = 0.6),
             axis.text = element_text(color = "black",size = 12),
             axis.ticks = element_blank(),
             axis.title.x = element_text(color = "black",vjust = 1, size = 16),
             axis.title.y = element_text(color = "black",vjust = 2, size = 16),
             panel.border = element_rect(colour = "black", fill=NA, size=0.6),
             plot.margin = unit(c(0.1,0.1,0.1,0.1),"inches"))


# proportion of fixes beyond the threshold of -10 and 250 m 
rhr_above <- subset(hrd,alti_clean > 250)
nrow(rhr_above)/nrow(hrd)*100 # 0.22%
rhr_below <- subset(hrd, alti_clean < -10)
nrow(rhr_below)/nrow(hrd)*100 # 3.77%
rm(rhr_above,rhr_below)




# 3.8 horizontal overlap
# change format into sf  
hrd_sf <- st_as_sf(hrd, crs = 25832)
# check coordinate system of flight and WKA data 
st_crs(hrd_sf)     # ETRS89 / UTM zone 32N
st_crs(WKA_UG_sf)  # ETRS89 / UTM zone 32N

# 3.8.1 Intersection of flights and wind turbine buffers (200 m)
# combine buffers 
WKA_200_combi <- WKA_buf200_sf %>% 
  group_by(ID_WKA) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  ungroup() %>% 
  st_union() %>%   
  st_cast("POLYGON") 
# Export: buffer combined
#st_write(WKA_200_combi, "C:/Users/maare/Documents/02_Studium/M.Sc.Loek/Masterarbeit/Auswertung/QGIS/buffer", "WKA_200_combi", driver = "ESRI Shapefile")
WKA_200_combi <- st_as_sf(WKA_200_combi)

# overlapping fixes
h_overlap <- st_intersection(x = WKA_200_combi, y = hrd_sf)
# check for duplicated rows
sum(duplicated(h_overlap$row_id)) # 0

# add new column in hrd: WKA_buf200 (fixes inside 200m buffer 1/0) 
hrd_sf$WKA_buf200 <- 0
for (i in 1:nrow(hrd_sf)) {
  if (any(h_overlap$row_id == hrd_sf$row_id[i])) {
    hrd_sf$WKA_buf200[i] <- 1
  }
}
nrow(hrd_sf[hrd_sf$WKA_buf200 == 1, ]) # 101392


###### Save: 3.3_horizontal_overlap_envi ######

# Distribution of the number of flight fixes for each bird 
h_Ind200 <- as.data.frame(table(h_overlap$tag_ident))
h_Ind200 <- h_Ind200[order(h_Ind200$Freq),]
# large number of fixes by one bird
max(h_Ind200$Freq)/sum(h_Ind200$Freq)*100 # 95.09% fixes generated by one bird 
rm(h_Ind200)

# bird 201082
h_201082 <- hrd_sf %>% filter(tag_ident == 201082)
nrow(h_201082) # 200465 rows
h_201082_200 <- h_201082 %>% filter(WKA_buf200 == 1)
nrow(h_201082_200) # 96422 rows
# proportion of fixes inside 200m buffer of bird 201082
nrow(h_201082_200)/nrow(h_201082)*100 # 48.1%
# flight altitudes inside buffer
summary(h_201082_200$alti_clean) # median: 1m
# flight altitude outside buffer
h_201082_out <- setdiff(h_201082,h_201082_200)
summary(h_201082_out$alti_clean) # median: 8m 

rm(h_201082,h_201082_out,h_201082_200)


# number of flights inside 200m buffer
length(table(h_overlap$flight_ID)) # 2858 - no. of overlapping flights 
# birds inside 200m buffer
length(table(h_overlap$tag_ident)) # 16 - no. of involved birds 
# distribution of sexes
h_overlap_f <- subset(h_overlap, Sex == "F")
length(table(h_overlap_f$tag_ident)) # 10 - no. of involved females 
h_overlap_m <- subset(h_overlap, Sex == "M")
length(table(h_overlap_m$tag_ident)) # 6 - no. of involved males
rm(h_overlap_f,h_overlap_m)
# proportion of fixes in 200m buffer  
nrow(h_overlap) # 101392 rows - no. of overlapping fixes  
nrow(h_overlap)/nrow(hrd_sf)*100 # 6.29 % - proportion of overlapping fixes
length(table(h_overlap$flight_ID))/length(table(hrd$flight_ID))*100 # 10.13 % - proportion of overlapping flights
# proportion of flights in 200m buffer 
h_flights <- aggregate(count~flight_ID+tag_ident,h_overlap,sum)
h_flights$count <- 1
h_flights <- aggregate(count~tag_ident,h_flights,sum) # no. of flights per crossing bird 
max(h_flights$count)/sum(h_flights$count)*100 # 92.68 % - proportion of flights of bird with most flights in overlap 
rm(h_flights)

# proportion without 201082
h_overlap_excl <- h_overlap %>% filter(tag_ident != 201082) # exclude 201082
nrow(h_overlap_excl) # 4970 rows
length(table(h_overlap_excl$tag_ident)) # 15 birds
length(table(h_overlap_excl$flight_ID)) # 209 flights
# proportion of overlap without 201082 
hrd_tmp <- hrd_sf[hrd_sf$tag_ident != "201082", ] # exclude 201082 out of hrd
length(table(h_overlap_excl$flight_ID))/length(table(hrd_tmp$flight_ID))*100 # 0.84 % - proportion of overlapping flights without 201082
rm(hrd_tmp)

# consider whole tracks of birds crossing the buffers (n=16) 
hrd_crossings_16 <- subset(hrd_sf, flight_ID %in% h_overlap$flight_ID)
length(table(hrd_crossings_16$flight_ID)) # 2858 flights 
table(hrd_crossings_16$WKA_buf200)
aggregate(alti_clean~WKA_buf200,hrd_crossings_16,median) # inside: 2m   outside: 14m
alti_buf_in <- subset(hrd_crossings_16, WKA_buf200 == 1) 
alti_buf_out <- subset(hrd_crossings_16, WKA_buf200 == 0)
# test 
wilcox.test(alti_buf_in$alti_clean,alti_buf_out$alti_clean,alternative = "two.sided")
# W = 2946133054, p-value < 2.2e-16
# significant! 



# proportion of flight fixes of individual birds within the buffer 
buf_freq <- aggregate(count~tag_ident,h_overlap,sum)
hrd_freq <- aggregate(count~tag_ident,hrd,sum)

tmp_freq <- merge(hrd_freq,buf_freq, by="tag_ident",all=T)
tmp_freq <- rename(tmp_freq, Freq_hrd = count.x, Freq_buf = count.y)

tmp_freq$prop_hrd <- (tmp_freq$Freq_hrd/nrow(hrd))*100
tmp_freq$prop_buf <- (tmp_freq$Freq_buf/tmp_freq$Freq_hrd)*100
range(tmp_freq$prop_buf,na.rm=T)  # 0.0054  -  48.1%

# exclude 201082  
tmp_freq_excl <- subset(tmp_freq,tag_ident!=201082)
median(tmp_freq_excl$prop_buf,na.rm=T) # 0.17 % - average prop of fixes in buffer per bird
range(tmp_freq_excl$prop_buf,na.rm = T)# 0.0054  -  14.78 % 
#median(tmp_freq_excl$prop_cross,na.rm=T) # 4.29 %
#range(tmp_freq_excl$prop_cross,na.rm = T)# 0.06 %  -  46.63 %
rm(buf_freq,hrd_freq,tmp_freq,tmp_freq_excl)

# check flight altitude inside and outside buffers (n=15)
# consider whole track length of birds crossing the buffer (exclude 201082: breeding)
hrd_crossings_15 <- subset(hrd_sf, flight_ID %in% h_overlap_excl$flight_ID)
length(table(hrd_crossings_15$flight_ID)) # 209 flights 
aggregate(alti_clean~WKA_buf200,hrd_crossings_15,median) # inside: 11m   outside: 35m
alti_buf_in <- subset(hrd_crossings_15, WKA_buf200 == 1) 
alti_buf_out <- subset(hrd_crossings_15, WKA_buf200 == 0)
# U test 
wilcox.test(alti_buf_in$alti_clean,alti_buf_out$alti_clean,alternative = "two.sided")
# W = 36367768, p-value < 2.2e-16
# significant! 

rm(hrd_crossings_15,hrd_crossings_16)




# 3.9 vertical avoidance behaviour
buf_dat <- subset(hrd_sf, flight_ID %in% h_overlap$flight_ID)
length(table(buf_dat$tag_ident)) # 16 Individuen
rm(hrd_sf)

# delete NAs
sum(is.na(buf_dat$alti_clean)) # 0

buf_dat$WKA_buf200 <- as.factor(buf_dat$WKA_buf200)
buf_dat$tag_ident <- as.factor(buf_dat$tag_ident)

# delete negative values
buf_dat_0 <- subset(buf_dat, buf_dat$alti_clean > 0)

# 1 = inside buffer (200m), 0 = outside buffer

buf_Modell_0 <- glmer.nb(alti_clean~WKA_buf200+(1|tag_ident)+(1|flight_ID), data=buf_dat_0)

summary(buf_Modell_0)
overdisp_fun(buf_Modell_0)
qqnorm(residuals(buf_Modell_0))
qqline(resid(buf_Modell_0))
plot(fitted(buf_Modell_0),resid(buf_Modell_0))

# comparison of median: same result
library(multcomp)
buf_comparison<- glht(buf_Modell_0, linfct=mcp(WKA_buf200="Tukey"))
summary(buf_comparison)

# calculate se & sd and plot result
buf_data_summary <- aggregate(alti_clean ~ WKA_buf200, buf_dat_0,       # Create summary data
                          function(x) c(median = median(x),
                                        sd = sd(x)))
buf_data_summary <- data.frame(group = buf_data_summary[ , 1], buf_data_summary$alti_clean)
buf_data_summary <- buf_data_summary[order(buf_data_summary$median), ]
# Export
#write.table(d_data_summary, "Flight_altitude_buffers_tab.csv", row.names=F, sep=",", dec = ".")

par(mar=c(3,5,3,3))

buf_r_barplot <- barplot(buf_data_summary$median,ylim = c(0, 60), width=2, 
                       names.arg=c("inside", "outside"),
                       ylab="Flight altitude (m)",
                       cex.axis = 1.5, 
                       cex.lab=1.5,
                       cex.names=1.5,
                       lwd=1.5) # Draw and store Base R barplot
arrows(x0 = buf_r_barplot,                           # Add error bars
       y0 = buf_data_summary$median + buf_data_summary$sd,
       y1 = buf_data_summary$median,
       angle = 90,
       code = 1,
       length = 0.1,
       lwd = 1.5)

rm(buf_dat, buf_comparison)



