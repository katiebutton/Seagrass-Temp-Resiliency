library(NCBNAqua)
library(tidyverse))
library(fetchaquarius)

# establish connection to Aquarius
fetchaquarius::connectToAquarius("aqreadonly")

caco_seagrass <- get_wl_data(park_code = "CACO", protocol = "ENE_Seagrass") %>%
  filter(Unit == "degC") %>%
  filter(Identifier %in% c("Water Temp.A@CACO_Seagrass_MA20_1", "Water Temp.B@CACO_Seagrass_MA20_1", "Water Temp.C@CACO_Seagrass_MA20_1", "Water Temp.A@CACO_Seagrass_MA20_2", "Water Temp.B@CACO_Seagrass_MA20_2", "Water Temp.C@CACO_Seagrass_MA20_2" ))

caco_pt <- map("National Park Service.Northeast Coastal and Barrier Network", ~fetchaquarius::getLocationInfo(folder = .x)) %>%
  bind_rows() %>%
  filter(Identifier == "NCBN_OP_ERP_MA-PH") %>%
  distinct() %>%
  mutate(parameters = map(Identifier, ~fetchaquarius::getTimeSeriesInfo(.x))) %>%
  select(-c(Identifier, UniqueId, UtcOffset, LastModified, Publish, Tags)) %>%
  unnest(cols = c(parameters)) %>%
  mutate(timeseries = map(Identifier, ~fetchaquarius::getTimeSeries(.x))) %>%
  select(Name, Identifier, LocationIdentifier, Unit, Label, timeseries) %>%
  mutate(data = map(timeseries, ~.x$Points)) %>%
  select(-timeseries) %>%
  unnest(cols = data) %>%
  unnest(cols = Value) %>%
  rename("Value" = Numeric) %>%
  as.data.frame %>%
  separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
  mutate(Time = if_else(is.na(Time), "00:00:00", Time))

caco_eh <- map("National Park Service.Cape Cod National Seashore", ~fetchaquarius::getLocationInfo(folder = .x)) %>%
  bind_rows() %>%
  filter(Identifier == "CACO_EH_Lagoon") %>%
  distinct() %>%
  mutate(parameters = map(Identifier, ~fetchaquarius::getTimeSeriesInfo(.x))) %>%
  select(-c(Identifier, UniqueId, UtcOffset, LastModified, Publish, Tags)) %>%
  unnest(cols = c(parameters)) %>%
  mutate(timeseries = map(Identifier, ~fetchaquarius::getTimeSeries(.x))) %>%
  select(Name, Identifier, LocationIdentifier, Unit, Label, timeseries) %>%
  mutate(data = map(timeseries, ~.x$Points)) %>%
  select(-timeseries) %>%
  unnest(cols = data) %>%
  unnest(cols = Value) %>%
  rename("Value" = Numeric) %>%
  as.data.frame %>%
  separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
  mutate(Time = if_else(is.na(Time), "00:00:00", Time)) %>%
  filter(Unit == "degC")

fiis_seagrass <- get_wl_data(park_code = "FIIS", protocol = "ENE_Seagrass") %>%
  filter(Unit == "degC") %>%
  filter(Identifier %in% c("Water Temp.A@FIIS_Seagrass_MB", "Water Temp.B@FIIS_Seagrass_MB", "Water Temp.C@FIIS_Seagrass_MB", "Water Temp.A@FIIS_Seagrass_GSB", "Water Temp.B@FIIS_Seagrass_GSB", "Water Temp.C@FIIS_Seagrass_GSB"))

asis_seagrass <- get_wl_data(park_code = "ASIS", protocol = "ENE_Seagrass") %>%
  filter(Unit == "degC") %>%
  filter(Identifier %in% c("Water Temp.A@ASIS_Seagrass_Tingles", "Water Temp.B@ASIS_Seagrass_Tingles", "Water Temp.C@ASIS_Seagrass_Tingles"))

# get the 'passing' years for each site/transect
dat <- readxl::read_excel(here::here("data", "Site_Summary_Master_b.xlsx"), sheet = "Site_Year") %>%
  rename_with(~ gsub(" ", "_", .))

seagrass_temp <- bind_rows(list("CACO" = caco_seagrass, "CACO" = caco_pt, "CACO" = caco_eh, "FIIS" = fiis_seagrass, "ASIS" = asis_seagrass), .id = "park_code") %>%
  mutate(Transect = if_else(Name == "East Harbor", "NA", str_extract(Identifier, "(?<=\\.).*(?=@)")),
         Year = as.numeric(str_sub(Date, 1, 4)),
         Month = as.numeric(str_sub(Date, 6, 7)),
         Name = case_when(
           Name == "Moriches Bay" ~ "Moriches Bay (Great Gun)",
           Name == "Great South Bay" ~ "Great South Bay (Ho Hum Beach)",
           Name == "Provincetown Harbor, MA - Outside Park" ~ "Provincetown Harbor",
           T ~ Name
         ),
         id = paste(Name, Transect, Year)) %>%
  left_join(., dat, by = c("Name" = "Site_Name", "Transect", "Year")) %>%
  filter(Month %in% c(5, 6, 7, 8, 9)) %>%
  filter(!is.na(All_check))

seasonal_mean_temp <- seagrass_temp %>%
  group_by(park_code, Name, Transect, Month) %>%
  summarise(mean_temp_month = mean(Value, na.rm = TRUE))

max_mean_temp <- seasonal_mean_temp %>%
  group_by(park_code, Name) %>%
  summarise(max_mean_temp = max(mean_temp_month))

# split the difference between Moriches and GSB to find a FIIS-specific threshold
max_mean_temp$max_mean_temp[max_mean_temp$Name == "Moriches Bay (Great Gun)"] + (max_mean_temp$max_mean_temp[max_mean_temp$Name == "Great South Bay (Ho Hum Beach)"] - max_mean_temp$max_mean_temp[max_mean_temp$Name == "Moriches Bay (Great Gun)"])/2
# 26.27111
