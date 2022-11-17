
data_file <- "foctr4.csv"

# foc_data <- function(data_file){
require(tidyverse)
require(magrittr)
data <- read.csv(paste0("data/",data_file)) %>%
  dplyr::select(geopoint,
                farm_code,
                farm_head_sex,
                region,
                district,
                education,
                banana_variety,
                plants_growing,
                farm_size,
                number_of_mats,
                age_of_plantation,
                soil_type,
                sell,
                yellowing,
                fusarium_panama,
                sample_number
                )

data[data == "n/a"] <- NA

data <- data %>%
  janitor::remove_empty("cols")

data$latitude <- as.numeric(stringr::str_split_fixed(data$geopoint," ", 3)[,1])
data$longitude <- as.numeric(stringr::str_split_fixed(data$geopoint," ", 3)[,2])
#data$altitude <- as.numeric(stringr::str_split_fixed(data$geopoint," ", 3)[,3])
data$geopoint <- NULL

# data types
data[,c("farm_head_sex", "region", "district", "education", "banana_variety", "plants_growing",
        "soil_type", "sell", "yellowing", "fusarium_panama")] %<>%
  dplyr::mutate_all(as.factor)
data[,c("farm_size", "number_of_mats", "age_of_plantation")] %<>%
  dplyr::mutate_all(as.numeric)

colnames(data) <- tools::toTitleCase(tolower(gsub("_", " ",colnames(data))))
colnames(data)[c(2,7, 14, 15)] <- c("Gender", "Plants Growning in", "Fusarium/ Panama", "SampleNo")

data$`Plants Growning in` <- ifelse(data$`Plants Growning in` == "commercial", "commercial plantation", data$`Plants Growning in`)

variety <- data.frame(do.call(rbind, stringr::str_split(data$`Banana Variety`, " ")))
colnames(variety) <- paste0("var", seq(1,7,1))
variety$`Farm Code` <- data$`Farm Code`
variety <- variety %>%
  tidyr::gather(id,Variety, starts_with("var"), na.rm = T)

ddata <- data %>%
  dplyr::full_join(variety)%>%
  .[!duplicated(.),]
ddata[,c("id","Banana Variety" )] <- NULL

ddata$District <- gsub("_", " ", ddata$District)
ddata$`Soil Type` <- gsub("_", " ", ddata$`Soil Type`)

dt <- ddata %>%
  dplyr::select(Region, District, "Farm Code", Gender, Education, "Farm Size", "Age of Plantation",
                "Soil Type", "Variety" , "Sell", "Yellowing", "Fusarium/ Panama", "SampleNo", "Latitude", "Longitude")
saveRDS(dt, "data/foctr4.rds")


