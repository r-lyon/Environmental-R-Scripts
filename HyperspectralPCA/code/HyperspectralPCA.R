# Load terra package

library(terra)

library(tidyverse)



# Load the 2025 multi-band raster .dat file, must have .hdr file in same directory

hs_sandia <- rast("Q:/GIS Data/Hyperspectral_Data/2025/MEDLEY_2025_Data_Delivery_20251212/Sandia/Full_Band_Hyperspectral_Mosaic/Sandia_2025_Geof.dat")

print(hs_sandia)



# Run a PCA to transform highly correlated bands into smaller set of Principal

# Components (PCs) to compress hundreds of bands into a few high-contrast layers



# Sample the data for efficiency



# Random sample of 10,000 pixels across the image

set.seed(42)

sampled_data <- spatSample(hs_sandia, size = 10000, method = "random", as.df = TRUE)

sampled_data <- na.omit(sampled_data)



# Run the PCA on the sampled pixel data

pca_model <- prcomp(sampled_data, scale. = TRUE)

summary(pca_model)



# Determine how many PCs contain useful information



# Calculate proportion of variance

variance_explained <- (pca_model$sdev^2) / sum(pca_model$sdev^2)



# Plot the screen plot, look for the elbow in the plot to determine number of PCs

data.frame(PC = 1:length(variance_explained), Variance = variance_explained) %>%
  
  slice(1:10) %>% #limit to top 10 PCs
  
  ggplot(aes(x = PC, y = Variance)) +
  
  geom_line(color = "darkgreen", linewidth = 1) +
  
  geom_point(size = 3) +
  
  labs(title = "Scree Plot: Explained Variance by PC",
       
       x = "Principal Component", y = "Proportion of Variance") +
  
  theme_minimal()



# Predict and Project back to original raster

# Apply the model weights back to every single pixel, specify index 1:3 to only use first 3 PCs

hs_pca_raster <- predict (hs_sandia, pca_model, index = 1:3)

names(hs_pca_raster) <- c("PC1", "PC2", "PC3")



# Write the compressed PCA layers to GeoTIFF

writeRaster(hs_pca_raster, filename = "Output/sandia_hyperspectral_pca_1.tif",
            
            overwrite = TRUE)