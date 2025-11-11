###############################################################################
# Title: Hardness-Dependent Metal Screening Tool
# Author: R. Lyon
# Date: 20241111
# Description:
#   This script screens surface water samples for hardness-dependent metals
#   against both acute and chronic aquatic life criteria using New Mexico
#   Environment Department (NMED) surface water quality standards
#   (20.6.4.900 NMAC). The criteria are expressed as a function of hardness
#   (mg CaCO3/L) and apply within specific hardness ranges:
#       • 0–400 mg/L for most metals (criteria for 400 mg/L applied above range)
#       • 0–220 mg/L for aluminum (criteria for 220 mg/L applied above range)
#
#   Acute and chronic criteria (in µg/L) are calculated using:
#       Acute:   exp(mA * ln(hardness) + bA) * CF
#       Chronic: exp(mC * ln(hardness) + bC) * CF
#
#   where mA/bA and mC/bC are metal-specific constants, and CF is the
#   conversion factor. These equations exclude the 2025 site specific water
#   quality copper criteria for Pajarito Plateau surface waters in the Rio 
#   Grande Basin as described in 20.6.4.900 NMAC.
#
# Data Source:
#   Surface water chemistry and hardness data were obtained from
#   Intellus New Mexico (https://www.intellusnm.com/), which provides public
#   access to environmental monitoring data for Los Alamos National Laboratory.
#
# Output:
#   An Excel table of site-specific hardness-adjusted metal criteria and
#   exceedance flags for both acute and chronic conditions.
###############################################################################

# ------------------------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------------------------
library(readr)        # Read CSV data
library(readxl)       # Read Excel workbooks
library(writexl)      # Write Excel outputs
library(lubridate)    # Handle and parse dates
library(dplyr)        # Data wrangling
library(janitor)      # Clean column names

# ------------------------------------------------------------------------------
# Define File Paths
# ------------------------------------------------------------------------------
gage_data_path <- "Data/StreamGages_Sandia_N3B_Metals_Gen_Chem.csv"
criteria_path  <- "Data/acute_chronic_hard_dep_calc.xlsx"
output_path    <- "Output/metal_hardness_screen.xlsx"

# ------------------------------------------------------------------------------
# Read and Prepare Gage Data (from IntellusNM)
# ------------------------------------------------------------------------------
gage <- read_csv(gage_data_path, col_types = cols(.default = col_character())) %>%
  clean_names() %>%
  mutate(
    sample_date   = mdy(sample_date),
    report_result = as.numeric(report_result)
  )

# ------------------------------------------------------------------------------
# Load Hardness-Dependent Metal Criteria (Acute & Chronic)
# ------------------------------------------------------------------------------

metal_criteria <- read_excel(criteria_path) %>%
  clean_names() %>%
  mutate(
    conversion_factor = as.numeric(na_if(conversion_factor, "")),
    criteria = str_trim(criteria)
  )

# Split into acute and chronic data frames
acute_criteria <- metal_criteria %>%
  filter(criteria == "Acute aquatic life") %>%
  select(metal, mA = m, bA = b, conversion_factor_A = conversion_factor)

chronic_criteria <- metal_criteria %>%
  filter(criteria == "Chronic aquatic life") %>%
  select(metal, mC = m, bC = b, conversion_factor_C = conversion_factor)

# ------------------------------------------------------------------------------
# Extract Hardness Measurements (Filtered Samples Only)
# ------------------------------------------------------------------------------
hardness <- gage %>%
  filter(parameter_name == "Hardness", filtered == "Y", detected == "Y") %>%
  select(
    location_id, sample_date, sample_type,
    field_sample_id, report_result, report_units
  ) %>%
  rename(
    hardness       = report_result,
    hardness_units = report_units
  ) %>%
  mutate(hardness = as.numeric(hardness))


# ------------------------------------------------------------------------------
# Define Hardness-Dependent Metals
# ------------------------------------------------------------------------------
hardness_dep_metals <- c(
  "Cadmium", "Chromium III", "Copper",
  "Lead", "Manganese", "Nickel", "Silver", "Zinc"
)
# Note: Aluminum is treated separately based on flow conditions.

# ------------------------------------------------------------------------------
# Filter and Join Data for Screening
# ------------------------------------------------------------------------------
metal_hardness_screen <- gage %>%
  filter(
    (parameter_name %in% hardness_dep_metals & field_preparation_code == "F") |
      (parameter_name == "Aluminum" & field_preparation_code == "UF" & sample_type == "WS") |
      (parameter_name == "Aluminum" & field_preparation_code == "F10u" & sample_type == "WT")
  ) %>%
  left_join(hardness, by = c("location_id", "sample_date", "sample_type")) %>%
  left_join(acute_criteria,  by = c("parameter_name"= "metal")) %>%
  left_join(chronic_criteria, by = c("parameter_name"= "metal"))

# ------------------------------------------------------------------------------
# Calculate Acute and Chronic Hardness-Dependent Criteria
# ------------------------------------------------------------------------------
metal_hardness_screen <- metal_hardness_screen %>%
  mutate(
    hardness = case_when(
      parameter_name == "Aluminum" & hardness > 220 ~ 220,
      parameter_name %in% hardness_dep_metals & hardness > 400 ~ 400,
      TRUE ~ as.numeric(hardness)
    ),
    # Acute criteria (µg/L)
    acute_criteria = case_when(
      parameter_name == "Cadmium" ~ exp(mA * log(hardness) + bA) *
        (1.136672 - (0.041838 * log(hardness))),
      parameter_name == "Lead" ~ exp(mA * log(hardness) + bA) *
        (1.46203 - (0.145712 * log(hardness))),
      TRUE ~ exp(mA * log(hardness) + bA) * conversion_factor_A
    ),
    # Chronic criteria (µg/L)
    chronic_criteria = case_when(
      parameter_name == "Cadmium" ~ exp(mC * log(hardness) + bC) *
        (1.101672 - (0.041838 * log(hardness))),
      parameter_name == "Lead" ~ exp(mC * log(hardness) + bC) *
        (1.46203 - (0.145712 * log(hardness))),
      TRUE ~ exp(mC * log(hardness) + bC) * conversion_factor_C
    ),
    # Exceedance flags
    acute_exceedance   = if_else(report_result > acute_criteria & detected == "Y", 1, 0),
    chronic_exceedance = if_else(report_result > chronic_criteria & detected == "Y", 1, 0),
    exceeds = if_else(acute_exceedance == 1 | chronic_exceedance == 1, 1, 0)
  )
# ------------------------------------------------------------------------------
# Export Results
# ------------------------------------------------------------------------------
write_xlsx(metal_hardness_screen, output_path)

message("Hardness-dependent metal screening complete. Results saved to: ", output_path)