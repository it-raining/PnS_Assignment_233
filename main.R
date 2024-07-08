# INSTALL PACKAGE
# install.packages("tidyr")
# ---------------------------
# Includes
library(tidyr)


# ---------------------------
# User Function
  # Check missing data function
  count_miss_entries <- function(data){
    #check NA values
    na_check <- is.na(data)
    #check NULL values
    null_check <- sapply(data , is.null )
    #check empty string or "N/A"
    empty_na_check <- sapply(data , function(x) x== "" | x == "N/A")
    #combine all 
    missing_data_check <- na_check | null_check | empty_na_check
    
    return (sum(missing_data_check))
  }
  #-----------------
  get_num <- function(x) {
    return (as.numeric(gsub("[^0-9.]", "", x)))
  }
  #-----------------
  Freq_to_MHz <- function(x) {
    # Check if the input is numeric
    if (is.numeric(x)) {
      return(x)
    }
    
    num <- as.numeric(gsub("[^0-9.]", "", x))
    # Determine the conversion factor based on the unit
    if (grepl("K", x, ignore.case = TRUE)) {
      fac <- 1 / 1000  # kHz to MHz
    } else if (grepl("M", x, ignore.case = TRUE)) {
      fac <- 1        # MHz to MHz
    } else if (grepl("G", x, ignore.case = TRUE)) {
      fac <- 1000     # GHz to MHz
    } else if (grepl("T", x, ignore.case = TRUE)) {
      fac <- 1000000  # THz to MHz
    } else {
      fac <- 1  
    }
        return(num * fac)
  }
  
# ---------------------------
# Read data
data <- read.csv("Data//Intel_CPUs.csv")
# ---------------------------
# Extract data
new_data <- data[ , c("Product_Collection", 
                      "Bus_Speed",
                      "Instruction_Set",
                      "Cache",
                      "Graphics_Max_Dynamic_Frequency",
                      "Graphics_Video_Max_Memory",
                      "Max_Memory_Bandwidth",
                      "Max_Memory_Size",
                      "nb_of_Cores",
                      "Processor_Base_Frequency",
                      "Recommended_Customer_Price",
                      "Secure_Key",
                      "TDP")]
# ---------------------------
# Count missing data on new data label
missing_data <- sapply(new_data , count_miss_entries)
  # Print
print(missing_data)
missing_data_frequency <- missing_data / 2283
print(missing_data_frequency)
# ---------------------------
# Processing missing data

### Paste code in here

# ---------------------------
### Instruction_Set ###
  # UNIT: None
  # Remove vector contain empty Ins row
  new_data$Instruction_Set[new_data$Instruction_Set == ""] <- NA 
  new_data <- new_data[complete.cases(new_data$Instruction_Set), ]
  # Convert into num
  new_data$Instruction_Set <- sapply(new_data$Instruction_Set, get_num)
# ---------------------------
### Bus_Speed ###
  # UNIT: MHz
  # Transfer per second to MHz
  tmp <- separate(new_data, col = Bus_Speed, into = c("Bus_Speed", "Speed_Unit", "Bus_Type"), sep = " ", fill = "right") 
  # Maybe improve when I know wtf is tranfer type and how can convert exactly
  new_data$Bus_Speed <- ifelse(tmp$Speed_Unit == "MHz", 
                               as.numeric(tmp$Bus_Speed),
                               as.numeric(tmp$Bus_Speed) * 100)
  rm(tmp)
# ---------------------------
### Max Memory Bandwidth ###
  # UNIT: GB/s
  new_data$Max_Memory_Bandwidth <- as.numeric(gsub("[^0-9.]", "", new_data$Max_Memory_Bandwidth))
# ---------------------------  
  
  
  
