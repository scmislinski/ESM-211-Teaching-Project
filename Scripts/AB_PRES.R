# Creating absence data with milkweed and monarchs 

library(tidyverse)

# round decimals to the second decimal on lat/long, like buffering in GIS 
milkweed_good$latitude = round(milkweed_good$latitude, digits = 2)

milkweed_good$longitude = round(milkweed_good$longitude, digits = 2)


monarch$latitude = round(monarch$latitude, digits = 2)

monarch$longitude = round(monarch$longitude, digits = 2)

joined_df = milkweed_good %>% 
  left_join(monarch, by = c("longitude", "latitude"),relationship = "many-to-many")

# Create a new column that tracks presence and absence

combo <- joined_df %>%
  mutate(pres_abs = case_when(
    scientific_name.y == "NA" ~ 0,
    scientific_name.y == "Danaus plexippus" ~ 1))

# For some reason the code above kept NAs as NAs, so the below makes them 0 in the new column
combo <- combo %>%
  mutate(pres_abs = replace_na(pres_abs, 0))

# result of above = presence is when milkweed is present and monarchs are present (1) 
# & absence is when milkweed is present and monarchs are not present (0).

# clean up data frame
final_df <- combo %>% select(latitude, longitude, pres_abs)

# export final data frame 
write.csv(final_df, "monarch_milkweed.csv", row.names = FALSE)


