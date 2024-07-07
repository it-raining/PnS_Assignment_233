# Includes ------------------


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
# ---------------------------
# Read data
data <- read.csv("Data//Intel_CPUs.csv")
# ---------------------------
# Extract data
new_data <- data[ , c("Product_Collection", 
                      "Bus_Speed",
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
View(new_data)
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



