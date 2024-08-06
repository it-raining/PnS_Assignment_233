# INSTALL PACKAGE
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("statip")

# ---------------------------
# Includes
library(tidyr)
library(stringr)
library(dplyr)
library(statip)

# ---------------------------
# User Function
# ---------------------------
### CLEAN FUNCTIONS ###
# Brief: Remove "empty" rows from the table
# Arguments: data - table data; column_names - working column
# e.g: new_data <- CleanData_rm(new_data, Product_Collection)
CleanData_rm <- function(data, column_name) {
  data %>% filter( # Loc tat ca cac dong thoa man dieu kien
    # {{ vec }} acts as a placeholder
    # for any column name as arg
    !is.na({{ column_name }}) &
      {{ column_name }} != "N/A" &
      {{ column_name }} != ""
  )
}
# Brief: Fill the missing values by average values
# Arguments: data - table data; column_name - working column
# e.g: new_data <- CleanData_f_avr(new_data, Product_Collection)
CleanData_f_avr <- function(data, column_name) {
  data %>%
    mutate({{ column_name }} := ifelse(
      {{ column_name }} %in% c(NA, "N/A", ""),
      # na.rm: Skip the NA value in column_name
      {{ column_name }} %>%
        get_num() %>%
        mean(na.rm = TRUE),
      # else do nothing
      {{ column_name }}
    ))
}
# Brief: Fill the missing values by same Sample_Vector name
# Arguments: data - table data; column_names - working column;
#             sample_column_name - column to fill based on same value
# e.g: new_data <- CleanData_f_name(new_data, Cache, Product_Collection)
CleanData_f_name <- function(data, column_name, sample_column_name) {
  data %>%
    # groups the data frame by the sample column.
    group_by({{ sample_column_name }}) %>%
    mutate({{ column_name }} := ifelse(
      {{ column_name }} %in% c(NA, "N/A", ""),
      # Replace NA values with the first non-NA value from
      # the sample column.
      first({{ column_name }}[
        !is.na({{ column_name }}) &
          {{ column_name }} != "N/A" &
          {{ column_name }} != ""
      ]),
      # else just keep its value
      {{ column_name }}
    ))
}
# Brief: Fill the missing values by average of Sample_Vector name
# Arguments: data - table data; column_names - working column;
#             sample_column_name - column to fill based on same value
# e.g: new_data <- CleanData_f_name_avr(new_data, Cache, Product_Collection)
CleanData_f_name_avr <- function(data, column_name, sample_column_name) {
  data %>%
    # groups the data frame by the sample column.
    group_by({{ sample_column_name }}) %>%
    mutate({{ column_name }} := ifelse(
      {{ column_name }} %in% c(NA, "N/A", ""),
      # Replace NA values with the first non-NA value from
      # the sample column.
      {{ column_name }} %>%
        get_num() %>%
        mean(na.rm = TRUE),
      # else just keep its value
      {{ column_name }}
    ))
}
#-----------------
# Brief: Fill the missing values by the most repeated value
#         of Sample_Vector name
# Arguments: data - table data; column_names - working column;
#             sample_column_name - column to fill based on same value
# e.g: new_data <- CleanData_f_name_mod(new_data, Cache, Product_Collection)
CleanData_f_name_mod <- function(data, column_name, sample_column_name) {
  data %>%
    # groups the data frame by the sample column.
    group_by({{ sample_column_name }}) %>%
    mutate({{ column_name }} := ifelse(
      {{ column_name }} %in% c(NA, "N/A", ""),
      {{ column_name }} %>%
        get_num() %>%
        mfv1(na_rm = TRUE),
      {{ column_name }}
    ))
}
#-----------------
### OUTLIER ###
# Define: an outlier if it is 1.5 times the interquartile range greater than the
# third quartile (Q3) or 1.5 times the interquartile range less than the first
# quartile (Q1).
# Brief: Find the outlier of the data
# Arguments: data - data table
#             column_name - access data column name
# e.g: outlier_cache <- outlier_finder(new_data, new_data$Cache)
outlier_finder <- function(data, column_name) {
  q1 <- quantile(column_name, 0.25, na.rm = TRUE)
  q3 <- quantile(column_name, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  outliers <- data %>%
    subset(
      column_name < (q1 - 1.5 * iqr) |
        column_name > (q3 + 1.5 * iqr)
    )
  return(outliers)
}

### CONVERT FUNCTIONS ###
# Brief: Convert data into number
# Arguments: x - data to convert
# e.g: new_data$col_to_convert <- sapply(new_data$col_to_convert, get_num)
get_num <- function(x) {
  x %>%
    str_extract("[\\d]*[.]?[\\d]+") %>% # Get the first number
    as.numeric(na.rm = TRUE) # covert into number
}
#-----------------
# Brief: Convert data into same unit (Mega)
# Arguments: x - data to convert
# e.g: new_data$col_to_convert <- sapply(new_data$col_to_convert, unit_to_M )
unit_to_M <- function(x) {
  # Check if the input is numeric (why do you use this function)
  if (is.numeric(x)) {
    return(x)
  }
  num <- get_num(x)
  # Determine the conversion factor based on the unit
  unit <- x %>%
    gsub("[0-9. ]", "", .) %>% # remove all number digits
    substr(1, 1) %>% # get the first letter
    toupper() # uppercase the unit
  fac <- switch(unit,
    K = 1 / 1000, # kHz to MHz
    M = 1, # MHz to MHz
    G = 1000, # GHz to MHz
    T = 1000000, # THz to MHz
    1 # Default: MHz
  )
  return(num * fac)
}
SizeMemory <- function(x) {
  if (is.numeric(x)) {
    return(x)
  } else if (grepl("K", x)) {
    y <- 0.000001
  } else if (grepl("M", x)) {
    y <- 0.001
  } else if (grepl("G", x)) {
    y <- 1
  } else if (grepl("T", x)) {
    y <- 1000
  } else {
    y <- 1
  }
  return(y * get_num(x))
}
CacheMapper <- function(x) {
  if (is.numeric(x)) {
    return(x)
  } else if (grepl("K", x)) {
    y <- 1
  } else if (grepl("M", x)) {
    y <- 1000
  } else if (grepl("G", x)) {
    y <- 1000000
  } else if (grepl("T", x)) {
    y <- 1000000000
  } else {
    y <- 1
  }
  return(y * get_num(x))
}
#-----------------
### PLOT ###
# Brief: Create histogram for a given column
# Arguments:  - column_name: working column
#             - name: Name of the column
#             - xlabel: unit of working column
#             - max: maximum value of y axis
# e.g: hist_plot("Bo nho Cache", new_data$Cache, MB, 512)
hist_plot <- function(name, column_name, xlabel, max) {
  hist(column_name,
    main = name,
    xlab = xlabel,
    ylab = "Frequency",
    ylim = c(0, max),
    labels = TRUE,
    breaks = 15,
    col = "lightgreen"
  )
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
  "Vertical_Segment",
  "Bus_Speed",
  "Cache",
  "Lithography",
  "Max_Memory_Bandwidth",
  "Max_nb_of_Memory_Channels",
  "Max_Memory_Size",
  "nb_of_Cores",
  "Processor_Base_Frequency",
  "Recommended_Customer_Price",
  "TDP",
  "DirectX_Support",
  "PCI_Express_Revision"
)]
str(new_data)
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
### Bus_Speed ###
# UNIT: MHz
# Transfer per second to MHz
tmp <- separate(new_data,
  col = Bus_Speed,
  into = c("Bus_Speed", "Speed_Unit", "Bus_Type"),
  sep = " ",
  fill = "right"
)
new_data$Bus_Speed <- sapply(new_data$Bus_Speed, unit_to_M)
new_data <- new_data %>%
  CleanData_f_name_mod(Bus_Speed, Vertical_Segment)

new_data$Max_nb_of_Memory_Channels <- ifelse(tmp$Bus_Type == "FSB",
  1,
  new_data$Max_nb_of_Memory_Channels
)
new_data$Max_Memory_Bandwidth <- ifelse(tmp$Bus_Type == "FSB",
  new_data$Bus_Speed * 4 / 1000,
  new_data$Max_Memory_Bandwidth
)
rm(tmp)
# ---------------------------
### Max_nb_of_Memory_Channels ###
# UNIT: None
new_data <- new_data %>%
  mutate(
    Max_nb_of_Memory_Channels = sapply(Max_nb_of_Memory_Channels, get_num)
  ) %>%
  CleanData_f_name_mod(Max_nb_of_Memory_Channels, Vertical_Segment) %>%
  CleanData_rm(Max_nb_of_Memory_Channels)
# ---------------------------
### Max Memory Bandwidth ###
# UNIT: GB/s
new_data <- new_data %>%
  mutate(
    Max_Memory_Bandwidth = sapply(Max_Memory_Bandwidth, get_num)
  ) %>%
  CleanData_f_name_mod(Max_Memory_Bandwidth, Vertical_Segment) %>%
  CleanData_rm(Max_Memory_Bandwidth)
# ---------------------------
### Cache ###
# UNIT: KB
# Transfer per second to KB
new_data$Cache <- sapply(new_data$Cache, CacheMapper)
new_data <- CleanData_f_name_avr(new_data, Cache, Vertical_Segment)
new_data <- CleanData_rm(new_data, Cache)
# ---------------------------
### Max_Memory_Size ###
# UNIT: GB
# Transfer per second to GB
new_data$Max_Memory_Size <- sapply(new_data$Max_Memory_Size, SizeMemory)
new_data <- CleanData_f_name_avr(new_data, Max_Memory_Size, Vertical_Segment)
new_data <- CleanData_rm(new_data, Max_Memory_Size)
# ---------------------------
### TDP ###
# UNIT: W
new_data$TDP <- sapply(new_data$TDP, get_num)
new_data <- CleanData_f_name_mod(new_data, TDP, Vertical_Segment)
new_data <- CleanData_rm(new_data, TDP)
# ---------------------------
### Processor_Base_Frequency ###
# UNIT: GHz
new_data$Processor_Base_Frequency <- sapply(new_data$Processor_Base_Frequency, SizeMemory)
new_data$Processor_Base_Frequency <- round(new_data$Processor_Base_Frequency, digits = 2)
new_data <- CleanData_f_name_mod(new_data, Processor_Base_Frequency, Vertical_Segment)
# ---------------------------
### PCI_Express_Revision ###
# UNIT: None
new_data <- new_data %>%
  mutate(
    PCI_Express_Revision = sapply(PCI_Express_Revision, get_num)
  ) %>%
  CleanData_f_name_mod(PCI_Express_Revision, Vertical_Segment) %>%
  CleanData_rm(PCI_Express_Revision)
# ---------------------------
### DirectX_Support ###
# UNIT: None
new_data <- new_data %>%
  mutate(
    DirectX_Support = sapply(DirectX_Support, get_num)
  ) %>%
  CleanData_f_name_mod(DirectX_Support, Vertical_Segment) %>%
  CleanData_rm(DirectX_Support)
# ---------------------------
# Loc gia tri khong phu hop o cot PCI_Express_Revision va DirectX_Support#
check_PCI_Express_Revision <- function(x) {
  x <- get_num(x)
  return(pmin(pmax(x, 1.0), 6.0))
} # Loc cot PCI_Express_Revision
check_DirectX_Support <- function(x) {
  x <- get_num(x)
  return(pmin(pmax(x, 1.0), 12.2))
} # Loc cot DirectX_Support
new_data <- new_data %>%
  mutate(
    PCI_Express_Revision = sapply(PCI_Express_Revision, check_PCI_Express_Revision),
    PCI_Express_Revision = replace_na(as.character(PCI_Express_Revision), " "),
    DirectX_Support = sapply(DirectX_Support, check_DirectX_Support),
    DirectX_Support = replace_na(as.character(DirectX_Support), " ")
  ) # Loc va xoa cac gia tri khong phu hop
# ---------------------------
### nb_of_Cores ###
# Do_nothing
# ---------------------------
### Recommended_Customer_Price ###
new_data$Recommended_Customer_Price <- gsub("\\$", "", new_data$Recommended_Customer_Price)
new_data$Recommended_Customer_Price <- ifelse(new_data$Recommended_Customer_Price == "N/A", NA, new_data$Recommended_Customer_Price)
new_data$Recommended_Customer_Price <- sapply(new_data$Recommended_Customer_Price, function(x) {
  sep <- stringr::str_locate(x, "-")[, 1]
  if (is.na(sep)) {
    x
  } else {
    as.character(round(median(as.integer(stringr::str_sub(x, c(1L, sep + 1), c(sep - 1, -1L))))))
  }
})
new_data$Recommended_Customer_Price <- gsub(".00", "", new_data$Recommended_Customer_Price)
new_data$Recommended_Customer_Price <- as.numeric(new_data$Recommended_Customer_Price)

price_medium <- sum(new_data$Recommended_Customer_Price, na.rm = TRUE)
price_medium <- price_medium / 1301
print(price_medium)
new_data$Recommended_Customer_Price <- tidyr::replace_na(new_data$Recommended_Customer_Price, price_medium)
new_data$Recommended_Customer_Price <- new_data$Recommended_Customer_Price
#################################
#       Descriptive statistics
#################################
# ---------------------------
### Summary statistics ###
#  !Add more specific
summary_stats <- new_data[, c(
  "Bus_Speed",
  "Cache",
  # "Lithography",
  "Max_Memory_Bandwidth",
  "Max_nb_of_Memory_Channels",
  "Max_Memory_Size",
  "nb_of_Cores",
  "Processor_Base_Frequency",
  "Recommended_Customer_Price",
  "TDP"
)]
# !Note that not every vectors has to be summarized
# Calculate the average of each sample
mean <- apply(summary_stats, 2, mean)

# Calculate standard derive (correction)
sd <- apply(summary_stats, 2, sd)

# Calculate quartile scores
q1 <- apply(summary_stats, 2, quantile, probs = 0.25, na.rm = TRUE)
med <- apply(summary_stats, 2, median)
q3 <- apply(summary_stats, 2, quantile, probs = 0.75, na.rm = TRUE)

# Calculate min value
min <- apply(summary_stats, 2, min)

# Calculate max value
max <- apply(summary_stats, 2, max)

# Restore the features in tabular form
summary_stats <- data.frame(mean, sd, q1, med, q3, min, max) %>% print()
# ---------------------------
### Cache statistics ###
hist_plot("Bo nho Cache", new_data$Cache, "KB", 1500)
boxplot(new_data$Cache, main = "Boxplot of Cache", col = "green")
### Max_Memory_Size statistics ###
hist_plot("Max_Memory_Size", new_data$Max_Memory_Size, "GB", 2000)
boxplot(new_data$Max_Memory_Size, main = "Boxplot of MMS", col = "green")
### Bus_Speed ###
hist_plot("Bus speed", new_data$Bus_Speed, "MHz", 1500)
boxplot(new_data$Bus_Speed, main = "Boxplot of Bus speed", col = "green")
# ---------------------------
### Max_nb_of_Memory_Channels ###
hist_plot("Bus speed", new_data$Max_nb_of_Memory_Channels, "MHz", 1500)
boxplot(new_data$Max_nb_of_Memory_Channels, main = "Boxplot of Bus speed", col = "green")
# ---------------------------
### Max Memory Bandwidth ###
hist_plot("Bus speed", new_data$Max_Memory_Bandwidth, "MHz", 1500)
boxplot(new_data$Max_Memory_Bandwidth, main = "Boxplot of Bus speed", col = "green")
# ---------------------------