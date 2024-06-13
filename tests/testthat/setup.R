
## Load data
CV <- readRDS(test_path("data","CV.RDS"))
data <- readRDS(test_path("data","type_5_data.RDS"))
geos <- readRDS(test_path("data","geos.RDS"))
profile_tableID <- c("B01001","B02001")
profile_variables <- c("B01001_002","B01001_026","B02001_001","B02001_002","B02001_003","B02001_004","B02001_005","B02001_006","B02001_007","B02001_008")
usCompare <- readRDS(test_path("data","usCompare.RDS"))
statesCompare <- readRDS(test_path("data","statesCompare.RDS"))
year <- 2022 
st <- Sys.time()
stat_table <- readRDS(test_path("data","stat_table.RDS"))
spatial_mapDF <- readRDS(test_path("data","spatial_mapDF.RDS"))
markers <- readRDS(test_path("data","markers.RDS"))

## Address data
address <- "350 Fifth Avenue New York, NY 10118"
address2 <- "532 N 7th St, Philadelphia, PA 19123"
address3 <- "2600 Benjamin Franklin Pkwy, Philadelphia, PA 19130"
addy <- c(address,address2,address3)
addyf <- data.frame(address=addy)
addressList <- readRDS(test_path("data","addresses.RDS"))
buffer <- readRDS(test_path("data","test_buffer.RDS"))
ggrObject <- readRDS(test_path("data","ggrObject.RDS"))

verbose <- FALSE

set.seed(999)
