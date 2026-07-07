# Load necessary libraries
library(terra)
library(tidyverse)

# Path to the multi-band raster .dat file
raster_path <- "path/to/your/Hyperspectral_Mosaic.dat"
hs_data <- rast(raster_path)
print(hs_data)

# Establish seed for reproducibility 
set.seed(42)

# Extract a random spatial sample of 10,000 pixels as a data frame
sampled_data <- spatSample(hs_data, size = 10000, method = "random", as.df = TRUE)

# Drop missing values to clean the statistical matrix
sampled_data <- na.omit(sampled_data)

# Compute PCA on standard-scaled pixel data
pca_model <- prcomp(sampled_data, scale. = TRUE)
summary(pca_model)

# Calculate the proportion of variance explained by each eigenvalue
variance_explained <- (pca_model$sdev^2) / sum(pca_model$sdev^2)

# Generate the Scree Plot via ggplot2
data.frame(PC = 1:length(variance_explained), Variance = variance_explained) %>%
  slice(1:10) %>% 
  ggplot(aes(x = PC, y = Variance)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Scree Plot: Explained Variance by PC",
       x = "Principal Component", 
       y = "Proportion of Variance") +
  theme_minimal()

# Project the PCA model back onto the original full-extent raster layer
hs_pca_raster <- predict(hs_data, pca_model, index = 1:3)
names(hs_pca_raster) <- c("PC1", "PC2", "PC3")

# Export the compressed principal components as an analysis-ready GeoTIFF
writeRaster(hs_pca_raster, 
            filename = "Output/hyperspectral_pca_compressed.tif",
            overwrite = TRUE)
