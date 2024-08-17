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
    )) %>%
    ungroup()
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
    )) %>%
    ungroup()
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
    )) %>%
    ungroup()
}
#-----------------
### OUTLIER ###
# Define: an outlier if it is 1.5 times the interquartile range greater than the
# third quartile (Q3) or 1.5 times the interquartile range less than the first
# quartile (Q1).
# Brief: Find the outlier of the data
# Arguments: data - data table
#             column_name - access data column name
# e.g: outlier_cache <- outlier_finder(new_data, Cache)
outlier_finder <- function(data, column_name) {
  # Convert into a string
  column_name <- deparse(substitute(column_name))
  # Calc quartile values
  q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  data %>%
    subset(
      data[[column_name]] < (q1 - 1.5 * iqr) |
        data[[column_name]] > (q3 + 1.5 * iqr)
    )
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
#################################
#       Data Pre-processing
#################################
# ---------------------------
# Read data
data <- read.csv("C:/Users/Admin/OneDrive - hcmut.edu.vn/Tải về/Intel_CPUs.csv")
# ---------------------------
# Extract data
new_data <- data[, c(
  "Product_Collection",
  "Vertical_Segment",
  "Launch_Date",
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
check_missing_data <- function(data){
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
### Product_Collection ###
new_data$Product_Collection <- gsub("[^a-zA-Z0-9/// ]", "",
                                    new_data$Product_Collection,
                                    ignore.case = TRUE
)
new_data <- CleanData_rm(new_data, Product_Collection)
# ---------------------------
### Launch_Date ###
years <- as.numeric(gsub("[^0-9]", "", new_data$Launch_Date)) %% 100
quart <- get_num(new_data$Launch_Date)
new_data$Launch_Date <- ifelse(years <= 22, years + 2000, years + 1900) + (3 * quart - 2) / 12
new_data <- CleanData_f_name_mod(new_data, Launch_Date, Product_Collection)
new_data <- CleanData_rm(new_data, Launch_Date)

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
  CleanData_f_name_mod(Bus_Speed, Product_Collection)

new_data$Max_nb_of_Memory_Channels <- ifelse(tmp$Bus_Type == "FSB",
                                             1,
                                             new_data$Max_nb_of_Memory_Channels
)
new_data$Max_Memory_Bandwidth <- ifelse(tmp$Bus_Type == "FSB",
                                        new_data$Bus_Speed * 4 / 1000,
                                        new_data$Max_Memory_Bandwidth
)
rm(tmp)
new_data <- CleanData_rm(new_data, Bus_Speed)

# ---------------------------
### Max_nb_of_Memory_Channels ###
# UNIT: None
new_data <- new_data %>%
  mutate(
    Max_nb_of_Memory_Channels = sapply(Max_nb_of_Memory_Channels, get_num)
  ) %>%
  CleanData_f_name_mod(Max_nb_of_Memory_Channels, Product_Collection) %>%
  CleanData_rm(Max_nb_of_Memory_Channels)
# ---------------------------
### Max Memory Bandwidth ###
# UNIT: GB/s
new_data <- new_data %>%
  mutate(
    Max_Memory_Bandwidth = sapply(Max_Memory_Bandwidth, get_num)
  ) %>%
  CleanData_f_name_mod(Max_Memory_Bandwidth, Product_Collection) %>%
  CleanData_rm(Max_Memory_Bandwidth)
# ---------------------------
### Cache ###
# UNIT: KB
# Transfer per second to KB
new_data$Cache <- sapply(new_data$Cache, CacheMapper)
new_data <- CleanData_f_name_avr(new_data, Cache, Product_Collection)
new_data <- CleanData_rm(new_data, Cache)
# ---------------------------
### Max_Memory_Size ###
# UNIT: GB
# Transfer per second to GB
new_data$Max_Memory_Size <- sapply(new_data$Max_Memory_Size, SizeMemory)
new_data <- CleanData_f_name_avr(new_data, Max_Memory_Size, Product_Collection)
new_data <- CleanData_rm(new_data, Max_Memory_Size)
# ---------------------------
### TDP ###
# UNIT: W
new_data$TDP <- sapply(new_data$TDP, get_num)
new_data <- CleanData_f_name_mod(new_data, TDP, Product_Collection)
new_data <- CleanData_rm(new_data, TDP)
# ---------------------------
### Processor_Base_Frequency ###
# UNIT: GHz
new_data$Processor_Base_Frequency <- sapply(new_data$Processor_Base_Frequency, SizeMemory)
new_data$Processor_Base_Frequency <- round(new_data$Processor_Base_Frequency, digits = 2)
new_data <- CleanData_f_name_mod(new_data, Processor_Base_Frequency, Product_Collection)
new_data <- CleanData_rm(new_data, Processor_Base_Frequency)
# ---------------------------
### PCI_Express_Revision ###
# UNIT: None
new_data <- new_data %>%
  mutate(
    PCI_Express_Revision = sapply(PCI_Express_Revision, get_num)
  ) %>%
  CleanData_f_name_mod(PCI_Express_Revision, Product_Collection)
# ---------------------------
### DirectX_Support ###
# UNIT: None
new_data <- new_data %>%
  mutate(
    DirectX_Support = sapply(DirectX_Support, get_num)
  ) %>%
  CleanData_f_name_mod(DirectX_Support, Product_Collection)
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
    PCI_Express_Revision = replace_na(as.character(PCI_Express_Revision), "0"),
    DirectX_Support = sapply(DirectX_Support, check_DirectX_Support),
    DirectX_Support = replace_na(as.character(DirectX_Support), "0")
  ) # Loc va xoa cac gia tri khong phu hop
new_data <- CleanData_rm(new_data, DirectX_Support)
new_data <- CleanData_rm(new_data, PCI_Express_Revision)

# ---------------------------
### nb_of_Cores ###
# Do_nothing
CleanData_f_name_mod(new_data ,nb_of_Cores, Product_Collection)
new_data <- CleanData_rm(new_data, nb_of_Cores)
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
new_data$Recommended_Customer_Price <- tidyr::replace_na(new_data$Recommended_Customer_Price, price_medium)
new_data <- CleanData_rm(new_data, nb_of_Cores)
### Lithography ###
# UNIT: mm
new_data <- new_data %>%
  mutate(
    Lithography = sapply(Lithography, get_num)
  ) %>%
  CleanData_f_name_mod(Lithography, Product_Collection) %>%
  CleanData_rm(Lithography)

new_mising_data <- sapply(new_data, check_missing_data)
print(new_mising_data)
# ---------------------------
#################################
#       Descriptive statistics
#################################
# ---------------------------
### Summary statistics ###
#  !Add more specific
summary_stats <- new_data[, c(
  "Bus_Speed",
  "Cache",
  "Lithography",
  "Max_Memory_Bandwidth",
  "Max_nb_of_Memory_Channels",
  "Max_Memory_Size",
  "nb_of_Cores",
  "Processor_Base_Frequency",
  "Recommended_Customer_Price",
  "TDP"
)]
Mean <- apply (summary_stats ,2 , mean ) 			# Tinh trung binh
SD <- apply (summary_stats ,2 , sd) 				# Tinh do lech chuan
Median <- apply (summary_stats ,2 , median ) 		# Tinh trung vi
Q1 <- apply (summary_stats ,2 , quantile , probs =0.25) 	# Tinh phan vi 25% (Q1)
Q3 <- apply (summary_stats ,2 , quantile , probs =0.75) 	# Tinh phan vi 75% (Q3)
Min <- apply (summary_stats ,2 , min ) 			# Tinh gia tri nho nhat
Max <- apply (summary_stats ,2 , max ) 			# Tinh gia tri lon nhat
#Tao dataframe
# Tạo một bản sao của tên hàng (rownames) trước khi áp dụng lapply
stats_df <- data.frame(Mean,SD,Q1,Median,Q3,Min,Max)
print(stats_df)
rownames_stats_df <- rownames(stats_df)

# Áp dụng formatC để rút gọn số 0 không cần thiết
stats_df <- data.frame(lapply(stats_df, function(x) formatC(x, format = "f", digits = 4, drop0trailing = TRUE)))

# Gán lại tên hàng ban đầu cho data frame sau khi định dạng
rownames(stats_df) <- rownames_stats_df

# Xem kết quả
print(stats_df)
new_data$Product_Collection<- gsub("[^0-9A-Za-z///' ]","" , new_data$Product_Collection ,ignore.case = TRUE)
new_data <- new_data %>%
  mutate(Product_Collection = gsub('.*Core.*', 'Intel Core Processors', Product_Collection),
         Product_Collection = gsub('.*Celeron.*', 'Intel Celeron Processor', Product_Collection),
         Product_Collection = gsub('.*Pentium.*', 'Intel Pentium Processor', Product_Collection),
         Product_Collection = gsub('.*Atom.*', 'Intel Atom Processors', Product_Collection),
         Product_Collection = gsub('.*Xeon.*', 'Intel Xeon Processors', Product_Collection),
         Product_Collection = gsub('.*Quark.*', 'Intel Quark Processors', Product_Collection),
         Product_Collection = gsub('.*Itanium.*', 'Intel Itanium Processors', Product_Collection))
table(new_data$Product_Collection)
table(new_data$Vertical_Segment)
table(new_data$DirectX_Support)
table(new_data$PCI_Express_Revision)
# ---------------------------
### Hist plot ###
# Brief: Create histogram for a given column
# Arguments:  - column_name: working column
#             - name: Name of the column
#             - xlabel: unit of working column
#             - max: maximum value of y axis
# e.g: hist_plot("Bo nho Cache", new_data$Cache, MB, 512)
hist_plot <- function(name, column_name, xlabel, x_max, y_max) {
  hist(column_name,
       main = name,
       xlab = xlabel,
       ylab = "Frequency",
       xlim = c(0, x_max),
       ylim = c(0, y_max),
       labels = TRUE,
       breaks = 15,
       col = "lightgreen"
  )
}
hist_plot("Cache", new_data$Cache, "KB", 1500)
hist_plot("TDP", new_data$TDP, "W", 200, 500)
hist_plot("Processor Base Frequency", new_data$Processor_Base_Frequency, "MHz", 5, 600)

## Boxplot ###
for (i in colnames(summary_stats)) {
  boxplot(new_data[[i]],
          xlab = i,
          col = topo.colors(10),
          main = paste("Boxplot of", i),
          horizontal = TRUE
  )
}

for (i in colnames(summary_stats)) {
  if (i != "Launch_Date") {
    boxplot(new_data[[i]] ~ new_data$Launch_Date,
            xlab = "Launch_Date",
            ylab = i,
            col = topo.colors(10),
            main = paste("Boxplot of", i, "with release date")
    )
  }
}

for (i in colnames(summary_stats)) {
  if (i != "Recommended_Customer_Price") {
    boxplot(new_data[[i]] ~ new_data$Recommended_Customer_Price,
            xlab = i,
            ylab = "Recommended Customer Price",
            col = topo.colors(10),
            main = paste("Boxplot of", i, "with recommended price"),
            horizontal = TRUE
    )
  }
}
########################ANOVA#############
print(name_table <- table(new_data$nb_of_Cores))
new_data_filtered <- new_data[new_data$nb_of_Cores %in% names(name_table[name_table > 50]), ]
print(name_table_filterd <- table(new_data_filtered$nb_of_Cores))
cores_1 <- subset(new_data_filtered, nb_of_Cores == "1")
shapiro.test(cores_1$Recommended_Customer_Price)
cores_2 <- subset(new_data_filtered, nb_of_Cores == "2")
shapiro.test(cores_2$Recommended_Customer_Price)
cores_4 <- subset(new_data_filtered, nb_of_Cores == "4")
shapiro.test(cores_4$Recommended_Customer_Price)
cores_6 <- subset(new_data_filtered, nb_of_Cores == "6")
shapiro.test(cores_6$Recommended_Customer_Price)
cores_8 <- subset(new_data_filtered, nb_of_Cores == "8")
shapiro.test(cores_8$Recommended_Customer_Price)
aov1 <- aov(Recommended_Customer_Price~as.factor(nb_of_Cores), new_data_filtered)
summary(aov1)
TukeyHSD(aov1)
frame()
plot.new()

##Setup and push viewport
vp0 <- viewport(x = .15, y = 0, just = c("left", "bottom"),
                width = .85, height = 1)
pushViewport(vp0)

##Add barplot
par(new = TRUE, fig = gridFIG())
plot(TukeyHSD(aov1), las = 1)
#######################ty le 2 mau#########
core1 = subset(new_data, nb_of_Cores == "1") $ TDP 
core4 = subset(new_data, nb_of_Cores == "4") $ TDP 
core1_50 = subset (core1, core1 > 50)
core4_50 = subset(core4, core4 > 50)
core1_size = length (core1)
core4_size = length (core4) 
core1_50_size = length (core1_50)
core4_50_size = length (core4_50)
print(core1_size)
print(core4_size)
print(core1_50_size)
print(core4_50_size)
result <- prop.test(x = c(core1_50_size, core4_50_size), 
                    n = c(core1_size, core4_size), 
                    alternative = "less",
                    correct = FALSE)
print(result)
