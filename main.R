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
tamdata <- new_data[,c("Recommended_Customer_Price", "Graphics_Video_Max_Memory")]
tamdata$Graphics_Video_Max_Memory<-ifelse(tamdata$Graphics_Video_Max_Memory =="256 MB","0.256 GB",tamdata$Graphics_Video_Max_Memory)
tamdata$Graphics_Video_Max_Memory<-gsub(" GB","",tamdata$Graphics_Video_Max_Memory)
tamdata$Graphics_Video_Max_Memory<-as.numeric(tamdata$Graphics_Video_Max_Memory)
tamdata$Recommended_Customer_Price<-gsub("\\$","",tamdata$Recommended_Customer_Price)
tamdata$Recommended_Customer_Price<-ifelse(tamdata$Recommended_Customer_Price== "N/A",NA,tamdata$Recommended_Customer_Price)
tamdata$Recommended_Customer_Price <- sapply(tamdata$Recommended_Customer_Price, function(x){
  sep <- stringr::str_locate(x, "-")[, 1]
  if(is.na(sep)) {
    x
  } else {
    as.character(round(median(as.integer(stringr::str_sub(x, c(1L, sep+1), c(sep-1, -1L))))))
  }
})
tamdata$Recommended_Customer_Price<-gsub(".00","",tamdata$Recommended_Customer_Price)
tamdata$Recommended_Customer_Price<-as.numeric(tamdata$Recommended_Customer_Price)

price_medium<-sum(tamdata$Recommended_Customer_Price,na.rm=TRUE)
price_medium<-price_medium / 1301
print(price_medium)
tamdata$Recommended_Customer_Price<-tidyr::replace_na(tamdata$Recommended_Customer_Price, price_medium)
new_data$Recommended_Customer_Price<-tamdata$Recommended_Customer_Price


### Paste code in here
# ---------------------------

