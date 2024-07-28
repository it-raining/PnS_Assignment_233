# INSTALL PACKAGE
# install.packages("tidyr")
# install.packages("dplyr")

# ---------------------------
# Includes
library(tidyr)
library(dplyr)

# ---------------------------
# User Function
# ---------------------------
### CLEAN FUNCTIONS ###
# Brief: Remove "empty" rows from the table
# Arguments: data - table data; column_names - working column
# e.g: new_data <- CleanData_rm(new_data, Product_Collection)
CleanData_rm <- function(data, column_name) {
  data %>% filter(
    !is.na({{ column_name }}) &
      {{ column_name }} != "N/A" &
      {{ column_name }} != ""
  )
}
# Brief: Fill the missing values by same Sample_Vector name
# Arguments: data - table data; column_names - working column
# e.g: new_data <- CleanData_f_name(new_data, Cache, Product_Collection)
CleanData_f_name <- function(data, column_name, sample_column_name) {
  data %>%
    # groups the data frame by the Product_Collection column.
    group_by(sample_column_name) %>%
    # Replace NA values with the first non-NA value from
    # the Product_Collection column.
    mutate({{ column_name }} := ifelse(
      {{ column_name }} %in% c(NA, "N/A", ""),
      first({{ column_name }}[
        !is.na({{ column_name }}) &
          {{ column_name }} != "N/A" &
          {{ column_name }} != ""
      ]),
      {{ column_name }}
    ))
}
# Brief: Fill the missing values by average values
# Arguments: data - table data; column_name - working column
# e.g: new_data <- CleanData_f_avr(new_data, Product_Collection)
CleanData_f_avr <- function(data, column_name) {
  data %>%
    mutate({{ column_name }} := ifelse(
      {{ column_name }} %in% c(NA, "N/A", ""),
      mean(as.numeric({{ column_name }}, na.rm = TRUE), na.rm = TRUE),
      {{ column_name }}
    ))
}
#-----------------
### CONVERT FUNCTIONS ###
# Brief: Convert data into number
# Arguments: x - data to convert
# e.g: new_data$col_to_convert <- sapply(new_data$col_to_convert, get_num)
get_num <- function(x) {
  return(as.numeric(gsub("[^0-9.]", "", x)))
}
#-----------------
# Brief: Convert data into number
# Arguments: x - data to convert
# e.g: new_data$col_to_convert <- sapply(new_data$col_to_convert, get_num)
freq_to_mhz <- function(x) {
  # Check if the input is numeric (who knows what is it unit)
  if (is.numeric(x)) {
    return(x)
  }
  num <- as.numeric(gsub("[^0-9.]", "", x))
  # Determine the conversion factor based on the unit
  unit <- toupper(sub("[0-9.]", "", x))
  fac <- switch(unit,
    K = 1 / 1000,  # kHz to MHz
    M = 1,         # MHz to MHz
    G = 1000,      # GHz to MHz
    T = 1000000,   # THz to MHz
    1              # Default: MHz
  )
  return(num * fac)
}

#################################
#       Data Pre-processing
#################################
# ---------------------------
# Read data
data <- read.csv("Data//Intel_CPUs.csv")
# ---------------------------
# Extract data
new_data <- data[, c(
  "Product_Collection",
  "Bus_Speed",
  "Cache",
  "Graphics_Max_Dynamic_Frequency",
  "Graphics_Video_Max_Memory",
  "Max_Memory_Bandwidth",
  "Max_nb_of_Memory_Channels",
  "Max_Memory_Size",
  "nb_of_Cores",
  "Processor_Base_Frequency",
  "Recommended_Customer_Price",
  "Secure_Key",
  "TDP"
)]
# ---------------------------
### PROCESSING MISSING DATA ###
# Count missing data on new data label
missing_data <- new_data %>%
  sapply(
    function(x) {
      sum(
        x %>% is.na() | x %>% sapply(is.null) |
          x %>% sapply(function(x) (x == "" | x == "N/A"))
      )
    }
  ) %>%
  print()
# Count missing data frequency
missing_data_frequency <- missing_data %>%
  `/`(nrow(new_data)) %>%
  print()
# ---------------------------


### Paste code in here








# ---------------------------
### Max_nb_of_Memory_Channels ###
# UNIT: None
new_data <- new_data %>%
  mutate(
    Max_nb_of_Memory_Channels = sapply(Max_nb_of_Memory_Channels, get_num)
  ) # Convert into number
# ---------------------------
### Bus_Speed ###
# UNIT: MHz
# Transfer per second to MHz
tmp <- separate(new_data,
  col = Bus_Speed,
  into = c("Bus_Speed", "Speed_Unit", "Bus_Type"),
  sep = " ",
  fill = "right"
)

new_data$Bus_Speed <- ifelse(tmp$Speed_Unit == "MHz",
  as.numeric(tmp$Bus_Speed),
  as.numeric(tmp$Bus_Speed) * 100
  # Maybe improve when I know wtf is tranfer type and how can convert exactly
)
rm(tmp)
# ---------------------------
### Max Memory Bandwidth ###
# UNIT: GB/s
new_data <- new_data %>%
  mutate(
    Max_Memory_Bandwidth = sapply(Max_Memory_Bandwidth, get_num)
  )
# ---------------------------
#################################
#       Descriptive statistics
#################################
# ---------------------------
### Summary statistics ###
summary_stats <- new_data %>%
  summarise_all(funs(mean, median, sd, min, max))
print(summary_stats)
# ---------------------------
### Correlation matrix ###
