# CLJ Temp code
# 5 Feb 2025
#####################

library(tidyverse)
library(here)
library(terra)
library(geodata)
#library(predicts)

#From : https://jcoliver.github.io/learn-r/011-species-distribution-models.pdf

#do this once and the data is downloaded
#Get the world clim data
#bioclim_data<-worldclim_global(var="bio",res=2.5, path="data/")

#load the data
obs_data<-read_csv(here("Data","Monarch.csv")) |>
  select(gbifID,decimalLatitude, decimalLongitude) |> drop_na()|>
  rename(latitude=decimalLatitude) |>
  rename(longitude=decimalLongitude)
#milkweed_data<-read_csv(here("data","monarch.csv"))

# Determine geographic extent of our data
max_lat <- ceiling(max(obs_data$latitude))
min_lat <- floor(min(obs_data$latitude))
max_lon <- ceiling(max(obs_data$longitude))
min_lon <- floor(min(obs_data$longitude))
# Store boundaries in a single extent object
geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))


#quick map
world_map <- world(resolution = 3,
                   path = "data/")
# Crop the map to our area of interest
my_map <- crop(x = world_map, y = geographic_extent)
# Plot the base map
plot(my_map,
     axes = TRUE,
     col = "grey95")
# Add the points for individual observations
points(x = obs_data$longitude,
       y = obs_data$latitude,
       col = "olivedrab",
       pch = 20,
       cex = 0.75)

#prepare the data for modeling
# Make an extent that is 25% larger
sample_extent <- geographic_extent * 1.25
# Crop bioclim data to desired extent
bioclim_data <- crop(x = bioclim_data, y = sample_extent)
# Plot the first of the bioclim variables to check on cropping
plot(bioclim_data[[1]])


#pseudo-absences (NOTE: I adjusted the number of pseudo-absences to be about the same as observations ~500)
# Set the seed for the random-number generator to ensure results are similar
set.seed(1234)

# Randomly sample points (same number as our observed points)
background <- spatSample(x = bioclim_data,
                         size = 500, # generate 500 pseudo-absence points
                         values = FALSE, # don't need values
                         na.rm = TRUE, # don't sample from ocean
                         xy = TRUE) # just need coordinates



# Pull out coordinate columns, x (longitude) first, then y (latitude) from
# saguaro data
presence <- obs_data[, c("longitude", "latitude")]
# Add column indicating presence
presence$pa <- 1
# Convert background data to a data frame
absence <- as.data.frame(background)
# Update column names so they match presence points
colnames(absence) <- c("longitude", "latitude")
# Add column indicating absence
absence$pa <- 0
# Join data into single data frame
all_points <- rbind(presence, absence)


#add the climate data
bioclim_extract <- extract(x = bioclim_data,
                           y = all_points[, c("longitude", "latitude")],
                           ID = FALSE) # No need for an ID column

# Add the point and climate datasets together
points_climate <- cbind(all_points, bioclim_extract)

#now you can go forward with teaching your package!
