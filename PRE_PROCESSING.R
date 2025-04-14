#Step 1. 
#Set Up Git in R
usethis::use_git()

#Connect R to GitHub
usethis::use_github(protocol = "https", private = FALSE)

# Add all files
git2r::add(path = ".")

# Commit files
git2r::commit(message = "Initial commit")

# Push to GitHub
git2r::push(credentials = git2r::cred_user_pass("Mackendylsu", "Ayiticheri@20152"))

git --version
git2r::config()

install.packages("usethis")
library(usethis)
usethis::install_git()


##1.1.- Install Necessary Libraries.

library(vroom)
library(dplyr)
library(tidyr)
library(fs)
library(readr)  
library(data.table)
library(Hmisc)
library(zip)      
library(tools)
library(stringr)


##1.2.- Read Each Zip File and Generate a Combined File.

# Define the main folder path
main_folder <- "C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/RECORDER"

# Function to process a single zip file and load its content into the global environment
process_zip_to_env <- function(zip_path) {
  temp_dir <- tempdir() # Temporary directory to extract contents
  unzip(zip_path, exdir = temp_dir) # Unzip the file
  
  # Identify .txt files inside the zip
  txt_files <- list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE)
  
  # Check if there are any .txt files
  if (length(txt_files) > 0) {
    for (txt_file in txt_files) {
      # Read the .txt file
      table_data <- read_delim(txt_file, delim = "\t", show_col_types = FALSE)
      
      # Append the data frame to a global list
      data_list[[length(data_list) + 1]] <<- table_data
    }
  }
  
  # Cleanup temporary files
  unlink(temp_dir, recursive = TRUE)
}

# Initialize an empty list to store datasets
data_list <- list()

# List all zip files in the main folder and subfolders
zip_files <- list.files(main_folder, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)

# Process each zip file and load the tables into the list
lapply(zip_files, process_zip_to_env)

# Combine all datasets into one
combined_recorder <- rbind(data_list)

# Save the combined dataset as a .txt file
write.table(combined_recorder, file = "combined_recorder.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

# Print a success message
cat("All datasets have been combined and saved to:", main_folder)


##1.3.- Select Study Area and Necessary Columns. 

# Read data in chunks
combined_recorder <- fread("C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/combined_recorder.txt", sep = "\t", header = TRUE)

# Selection part of dataset depends on County FIPS
#Lafayette_LA <- recorder |> filter(recorder$DocumentRecordingCountyFIPs=="22055")

##1.4.- Inspect Dataset of Study Area.

# Display the first few rows of the data frame
head(combined_recorder)

# Verify the number of rows and columns
cat("Number of rows:", nrow(combined_recorder), "\n")
cat("Number of columns:", ncol(combined_recorder), "\n")

# Check missing values in all dataset
colSums(is.na(combined_recorder))

#Exploring the dimensions of dataset. 
dim(combined_recorder)

# Displaying columns names
variable_names <- names(combined_recorder)

# See types of variables (column)
str(combined_recorder)

##1.5.- Select Necessary Columns. 

# Remove columns where more than 95% of values are NA
combined_recorder <- combined_recorder |>
  select(where(~ mean(is.na(.)) <= 0.95))

# View the cleaned dataset
print(combined_recorder)

# Verify the number of rows and columns
cat("Number of rows:", nrow(combined_recorder), "\n")
cat("Number of columns:", ncol(combined_recorder), "\n")

##1.6.- Identify arm’s-length sales

# Remove rows where TransferAmount is NA or 0.
# TransferAmount: Sale Price
cleaned_recorder <- combined_recorder |>
  filter(!is.na(TransferAmount) & TransferAmount != 0)

# Remove rows where RecordingDate is NA or 0. 
# RecordingDate: Recordng date on document/instrument for the latest ownership change transaction.
cleaned_recorder <- cleaned_recorder |>
  filter(!is.na(RecordingDate) & RecordingDate != 0)

# Remove rows that are not satisfied arm's length condition (i.e. remove NA, 0, 2, 3, 4, 5 on column of ArmsLengthFlag).
# ArmsLengthFlag: Deed representing a transfer between two otherwise unrelated or affiliated parties.
# NULL:	Unknown or not provided
# 0:	Non Purchase or Unknown
# 1:	Arms Length Transaction
# 2:	Not at Arms-Length
# 3:	Not at Arms-Length - conveyance to Grantor's trust
# 4:	Not at Arms-Length - interfamily transfer
# 5:	Not at Arms-Length - dissolution
cleaned_recorder <- cleaned_recorder |>
  filter(ArmsLengthFlag == 1)

# Remove rows where QuitclaimFlag is True or equal 1.
# QuitclaimFlag: Indicates that the transaction is a Quit Claim.
# 0: Not a Quitclaim Document
# 1: Quitclaim Document
# NULL: Unknown or not provided
cleaned_recorder <- cleaned_recorder |>
  filter(QuitclaimFlag != 1)

# Remove rows where ForeclosureAuctionSale is True or equal 1 (bit 0 or 1).
# ForeclosureAuctionSale: Indicates that the transaction was the result of a foreclosure auction.
# 0: Transaction Was Not the Result of a Foreclosure Auction
# 1: Transaction Was the Result of a Foreclosure Auction
# NULL: Unknown or not provided
cleaned_recorder <- cleaned_recorder |>
  filter(ForeclosureAuctionSale != 1)

# Select only Residential as property type. 
# PropertyUseGroup: General property type description; residential, commercial, industrial, etc..
cleaned_recorder <- cleaned_recorder |>
  filter(PropertyUseGroup == "Residential")

# Select only Individual Grantor as Grantor1InfoEntityClassification. 
# Grantor1InfoEntityClassification: Derived field describing what type of entity the party is based on examination of the name and vesting. Sample values include individual / trust / company. 
# Select only Individual Grantee as Grantee1InfoEntityClassification. 
# Grantee1InfoEntityClassification: Derived field describing the type of entity based on examination of the name and vesting.
# NULL:	Unknown or not provided
# IND:	Individual
# NON:	Non-Individual
cleaned_recorder <- cleaned_recorder |>
  filter(Grantor1InfoEntityClassification != "NON" & Grantee1InfoEntityClassification != "NON")

# List of words to check (Nolte et al, 2024) provided a list of 
words_to_check <- c('LLC', 'LTD', 'LLP', 'LLLP', 'INC', 'EST',
                    'RET', 'INT', 'MTG', 'IRT', 'DEV', 'III', '2ND', '3RD',
                    'BANK', 'HOLDINGS', 'TRUST', 'REVOCABLE', 'ASSOCIATION',
                    'FAMILY', 'PROPERTIES', 'LIVING', 'COMPANY', 'ESTATE',
                    'PROPERTIES', 'NATIONAL', 'RANCH', 'DEVELOPMENT', 'CONDO',
                    'INVESTMENT', 'HOME', 'LOANS', 'INVESTMENTS', 'PARTNERS',
                    'PARTNERSHIP', 'LAND', 'CORPORATION', 'LIMITED',
                    'ENTERPRISES', 'GROUP', 'VENTURES', 'HOUSING', 'RESORT',
                    'RESORTS', 'CORP', 'ASSN', 'ASSOCIATES', 'IRREVOCABLE',
                    'REALTY', 'TRUSTS', 'VALLEY', 'CREEK', 'OWNERS', 'TOWNHOMES',
                    'RIDGE', 'LODGE', 'TRAILHEAD', 'MORTGAGE', 'COUNTY')

# Remove rows where any word from 'words_to_check' appears in Grantor1NameFull or Grantee1NameFull
cleaned_recorder <- cleaned_recorder |>
  filter(!(
    str_detect(Grantor1NameFull, paste(words_to_check, collapse = "|")) |
      str_detect(Grantee1NameFull, paste(words_to_check, collapse = "|"))
  ))


# Verify the number of rows and columns
cat("Number of rows:", nrow(cleaned_recorder), "\n")
cat("Number of columns:", ncol(cleaned_recorder), "\n")

## 1.7.- Recorder Data set with Essential Columns.

# Select necessary columns
selected_columns <- c("TransactionID", "[ATTOM ID]", "DocumentRecordingStateCode", 
                      "DocumentRecordingCountyName", "DocumentRecordingJurisdictionName", 
                      "DocumentRecordingCountyFIPs", "RecordingDate", 
                      "ForeclosureAuctionSale", "QuitclaimFlag", "TransferInfoMultiParcelFlag", 
                      "ArmsLengthFlag", "TransferAmount", "Grantor1NameFull",
                      "Grantor1InfoEntityClassification", "Grantee1NameFull",
                      "Grantee1InfoEntityClassification", "PropertyUseGroup")

# Recorder Data set after prior analysis 
cleaned_recorder_reduced <- cleaned_recorder |> select(all_of(selected_columns))

# Save the cleaned reduced data set as a .txt file
#write.table(cleaned_recorder_reduced, file = "cleaned_recorder_reduced.txt", 
            #sep = "\t", row.names = FALSE, col.names = TRUE)

#Check_out_1 <- cleaned_recorder_reduced |> group_by(cleaned_recorder_reduced$DocumentRecordingStateCode) |> tally()
#Check_out_2 <- cleaned_recorder |> group_by(cleaned_recorder$PropertyUseGroup) |> tally()
#Check_out_1 <- combined_recorder |> group_by(combined_recorder$ArmsLengthFlag) |> tally()




#Step 2.



##2.1.- Read Data set on local disk. 

# Read data in chunks
combined_assessor <- fread("C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/tax_assessor.txt", sep = "\t", header = TRUE)

##2.2.- Inspect Data set of Study Area.

# Display the first few rows of the data frame
head(combined_assessor)

# Verify the number of rows and columns
cat("Number of rows:", nrow(combined_assessor), "\n")
cat("Number of columns:", ncol(combined_assessor), "\n")

# Check missing values in all data set
colSums(is.na(combined_assessor))

#Exploring the dimensions of data set. 
dim(combined_assessor)

# Displaying columns names
variable_names <- names(combined_assessor)

# See types of variables (column)
str(combined_assessor)

## 2.5.- Assessor Data set with Essential Columns.

# Select necessary columns
selected_columns <- c("SitusStateCode", "[ATTOM ID]", "SitusCounty", "SitusStateCountyFIPS", 
                      "CBSAName", "CBSACode", "ParcelNumberRaw", "PropertyAddressFull", 
                      "PropertyAddressCity", "PropertyAddressZIP",
                      "PropertyLatitude", "PropertyLongitude", "PartyOwner1NameFull", 
                      "OwnerTypeDescription1",
                       "YearBuilt", "PropertyUseGroup", "AssessorLastSaleDate", 
                      "AssessorLastSaleAmount",
                      "AssessorPriorSaleDate", "AssessorPriorSaleAmount", 
                      "AreaBuilding", "AreaGross", "Area1stFloor",
                      "Area2ndFloor", "AreaUpperFloors", "AreaLotAcres", 
                      "AreaLotSF", "AreaLotDepth", "AreaLotWidth",
                      "BathCount", "BathPartialCount", "BedroomsCount", 
                      "RoomsCount", "StoriesCount", "ParkingSpaceCount", 
                      "Pool", "PoolArea")

# Assessor Data set after prior analysis 
cleaned_assessor_reduced <- combined_assessor |> select(all_of(selected_columns))

# Save the cleaned reduced data set as a .txt file
#write.table(cleaned_assessor_reduced, file = "cleaned_recorder_reduced.txt", 
            #sep = "\t", row.names = FALSE, col.names = TRUE)




#Step 3.


##3.1 Merge cleaned_recorder_reduced with cleaned_assessor_reduced

# Select the specific columns 
selected_columns_assessor <- c("PropertyAddressFull", "[ATTOM ID]",
                      "PropertyLatitude", "PropertyLongitude", "PartyOwner1NameFull",
                      "YearBuilt", "AssessorLastSaleDate", 
                      "AssessorLastSaleAmount",
                      "AssessorPriorSaleDate", "AssessorPriorSaleAmount", 
                      "AreaBuilding", "AreaGross", "Area1stFloor",
                      "Area2ndFloor", "AreaUpperFloors", "AreaLotAcres", 
                      "AreaLotSF", "AreaLotDepth", "AreaLotWidth",
                      "BathCount", "BathPartialCount", "BedroomsCount", 
                      "RoomsCount", "StoriesCount", "ParkingSpaceCount", 
                      "Pool", "PoolArea")
cleaned_assessor_reduced_selected <- cleaned_assessor_reduced |>
  select(selected_columns_assessor)

# Merge the datasets using common columns. 
merged_RecAss <- cleaned_recorder_reduced |>
  left_join(cleaned_assessor_reduced_selected, by = c("[ATTOM ID]"))

# Verify the number of rows and columns
cat("Number of rows:", nrow(merged_RecAss), "\n")
cat("Number of columns:", ncol(merged_RecAss), "\n")

# Save the cleaned reduced data set as a .txt file
write.table(merged_RecAss, file = "merged_RecAss.txt", 
sep = "\t", row.names = FALSE, col.names = TRUE)


##3.2.- Dealing with missing data on merged dataset.

# Check missing values in all data set
colSums(is.na(merged_RecAss))


summary_merged_RecAss <- merged_RecAss %>%
  group_by(DocumentRecordingStateCode) %>%
  summarise(across(where(is.numeric), list(
    mean = ~ ifelse(all(is.na(.)), NA, mean(., na.rm = TRUE)),
    sum = ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE)),
    min = ~ ifelse(all(is.na(.)), NA, min(., na.rm = TRUE)),
    max = ~ ifelse(all(is.na(.)), NA, max(., na.rm = TRUE))
  ), .names = "{col}_{fn}"))

print(summary_merged_RecAss)

setDT(merged_RecAss)[ , list(mean_county = mean(TransferAmount), 
                             min_county = min(TransferAmount),
                             max_county = max(TransferAmount),
                             sum_county = sum(TransferAmount)) , 
                      by = .(DocumentRecordingCountyFIPs)]













Check_out_2 <- merged_RecAss |> group_by(merged_RecAss$Grantee1InfoEntityClassification) |> tally()


Check_out_1 <- merged_RecAss |> group_by(merged_RecAss$TransferAmount == merged_RecAss$AssessorLastSaleAmount) |> tally()





































































































































library(vroom)
library(dplyr)
library(tidyr)
library(fs)
library(readr)  # Needed for write_csv()
library(data.table)
library(Hmisc)

# Set working directory
work_dir <- "C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/TAXASSESSOR"

# Get all zip files in the directory
zip_files <- dir_ls(work_dir, glob = "*.zip")

# Output file path for combined data
output_file <- file.path(work_dir, "combined_taxasssessor.csv")

# Check if output file exists and delete it to avoid appending old data
if (file.exists(output_file)) {
  file.remove(output_file)
}

# Process each ZIP file one at a time
for (zip_file in zip_files) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # Unzip files into the temp directory
  unzip(zip_file, exdir = temp_dir)
  
  # Find the TXT files inside the extracted folder
  txt_files <- dir_ls(temp_dir, glob = "*.txt")
  
  for (txt_file in txt_files) {
    # Read file using vroom for better memory handling
    combined_recorder <- vroom(txt_file, delim = "\t", show_col_types = FALSE, col_names = TRUE, altrep = TRUE, progress = FALSE)
    
    # Add source file name for reference
    combined_taxasssessor <- combined_taxasssessor %>% mutate(source_file = basename(txt_file))
    
    # Save data incrementally to avoid memory issues
    write_csv(combined_taxasssessor, output_file, append = TRUE)  # FIXED: This function now works
  }
  
  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)
}

print("Data processing complete. Combined data saved to:")
print(output_file)



#READIND DATA

# Read the file without specifying the header or separator to inspect its structure
combined_sales_data_Alabama <- read_csv(output_file)
# Read data in chunks
combined_sales_data_Alabama <- fread(output_file)
combined_sales_data_Alabama <- readLines(output_file)

# Verify the number of rows and columns
cat("Number of rows:", nrow(combined_sales_data_Alabama), "\n")
cat("Number of columns:", ncol(combined_sales_data_Alabama), "\n")



































# Read the file without specifying the header or separator to inspect its structure
work_dir <- "C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/TAXASSESSOR/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001"
#raw_tax_assessor <- (work_dir + "/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001.txt" )
raw_tax_assessor <- readLines("C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/TAXASSESSOR/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001.txt")
head(raw_tax_assessor, 30)


# Read data in chunks
recorder <- fread("C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/combined_recorder.txt", sep = "\t", header = TRUE)

# Selection part of dataset
Lafayette <- recorder |> filter(recorder$DocumentRecordingCountyFIPs=="22055")

Lafayette_Hist <- Lafayette |> group_by(Lafayette$`[ATTOM ID]`) |> tally()










# Verify the number of rows and columns
cat("Number of rows:", nrow(tax_assessor), "\n")
cat("Number of columns:", ncol(tax_assessor), "\n")



# Output file path for combined data
output_file <- file.path(work_dir, "tax_assessor.csv")

# Save data incrementally to avoid memory issues
write_csv(tax_assessor, output_file, append = TRUE)  # FIXED: This function now works

# Read csv file from directory
tax_assessor <- read.csv("C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/TAXASSESSOR/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001/tax_assessor.csv")

#raw_tax_assessor <- readLines("C:/Users/macke/OneDrive - Louisiana State University/Documents/PHD_LSU/SPRING_2025/WORK/EPSCOR PROJECT/DATA/CODING/PROPERTY_DATA_SAMPLE_TAB_TAXASSESSOR_0001.txt")
#head(tax_assessor, 30)

# Use fill = TRUE option to handle missing values
#tax_assessor <- read.table("C:/Users/macke/OneDrive - Louisiana State University/Documents/PHD_LSU/SPRING_2025/WORK/EPSCOR PROJECT/DATA/CODING/PROPERTY_DATA_SAMPLE_TAB_TAXASSESSOR_0001.txt", header = TRUE, sep = "\t", fill = TRUE)


# Display the first few rows of the data frame
head(tax_assessor)

library(tidyverse)
tax_assessor %>% select(RoomsCount, 
                        SitusCounty, 
                        SitusStateCode)%>% group_by(SitusStateCode)

tax_assessor %>% group_by(SitusStateCode)


# Count the number of building for each each state
tax_assessor %>% count(SitusStateCode)



# Install and load the necessary packages
#install.packages("sf")
#install.packages("dplyr")
library(sf)
library(dplyr)


# Check for missing values in the coordinate columns
missing_longitude <- sum(is.na(tax_assessor$PropertyLongitude))
missing_latitude <- sum(is.na(tax_assessor$PropertyLatitude))

cat("Missing Longitude Values:", missing_longitude, "\n")
cat("Missing Latitude Values:", missing_latitude, "\n")


# Remove rows with missing coordinates
tax_assessor <- tax_assessor %>% 
  filter(!is.na(PropertyLongitude) & !is.na(PropertyLatitude))


# Convert coordinates to numeric
tax_assessor$PropertyLongitude <- as.numeric(tax_assessor$PropertyLongitude)
tax_assessor$PropertyLatitude <- as.numeric(tax_assessor$PropertyLatitude)

# Check for any remaining NAs after conversion
remaining_nas_longitude <- sum(is.na(tax_assessor$PropertyLongitude))
remaining_nas_latitude <- sum(is.na(tax_assessor$PropertyLatitude))

cat("Remaining NAs in Longitude:", remaining_nas_longitude, "\n")
cat("Remaining NAs in Latitude:", remaining_nas_latitude, "\n")


# Select necessary columns
selected_columns <- c("SitusStateCode", "SitusCounty", "SitusStateCountyFIPS", "CBSAName", "CBSACode", "ParcelNumberRaw", "PropertyAddressFull", "PropertyAddressCity", "PropertyAddressZIP",
                      "PropertyLatitude", "PropertyLongitude", "PartyOwner1NameFull", "OwnerTypeDescription1",
                      "TaxYearAssessed", "TaxAssessedValueTotal", "TaxAssessedValueImprovements", "TaxAssessedValueLand",
                      "TaxAssessedImprovementsPerc", "PreviousAssessedValue", "TaxMarketValueYear", "TaxMarketValueTotal",
                      "TaxMarketValueLand", "TaxMarketImprovementsPerc", "TaxFiscalYear", "TaxRateArea", "TaxBilledAmount",
                      "LastAssessorTaxRollUpdate", "AssrLastUpdated", "TaxExemptionHomeownerFlag",
                      "TaxExemptionDisabledFlag", "TaxExemptionSeniorFlag", "TaxExemptionVeteranFlag", "TaxExemptionWidowFlag",
                      "TaxExemptionAdditional", "YearBuilt",
                      "PropertyUseGroup", "AssessorLastSaleDate", "AssessorLastSaleAmount",
                      "AssessorPriorSaleDate", "AssessorPriorSaleAmount", "AreaBuilding", "AreaGross", "Area1stFloor",
                      "Area2ndFloor", "AreaUpperFloors", "AreaLotAcres", "AreaLotSF", "AreaLotDepth", "AreaLotWidth",
                      "BathCount", "BathPartialCount", "BedroomsCount", "RoomsCount", "StoriesCount", "ParkingSpaceCount", "Pool", "PoolArea")

tax_assessor_reduced <- tax_assessor %>% select(all_of(selected_columns))

# Shorten column names to be 10 characters or less
colnames(tax_assessor_reduced) <- abbreviate(colnames(tax_assessor_reduced), minlength = 10)
print(colnames(tax_assessor_reduced))

# Create the sf object
sf_data <- st_as_sf(tax_assessor_reduced, coords = c("PrprtyLngt", "PrprtyLttd"), crs = 4326)



# Write the sf object to a shapefile (replace with the actual path to save your file)
#st_write(sf_data, "C:/Users/macke/OneDrive - Louisiana State University/Documents/PHD_LSU/SPRING_2025/WORK/EPSCOR PROJECT/DATA/CODING/Tax_Assessor_Property1.shp", 
#delete_layer = TRUE)


# Write the sf object to a GeoPackage
st_write(sf_data, "C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/TAXASSESSOR/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001/Tax_Assessor_dataset.gpkg", 
         delete_layer = TRUE, layer_options = "GEOMETRY_NAME=geometry")





#Data exploration 

# Check missing values in all dataset

colSums(is.na(tax_assessor))


#Exploring the dimensions of dataset. 
dim(tax_assessor)

# displaying columns names
variable_names <- names(tax_assessor)

# See types of variables (column)
str(tax_assessor)

# Describe data
describe(tax_assessor)

# Selection part of dataset
Lafayette <- subset(tax_assessor, SitusCounty=="Lafayette")


# Count the number of building for each each state
Lafayette <-  Lafayette %>% count(Lafayette, RoomsCount >0)
#Lafayette <- Lafayette %>% select(-c(RoomsCount>= 0, RoomsCount > 0))






tax_assessor <- tax_assessor %>% count()

























































































# Load necessary libraries
library(tidyverse) # For data manipulation
library(zip)       # For handling zip files
library(tools)     # For file path manipulation

# Define the main folder path
main_folder <- "C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/LOUISIANA"

# Function to process a single zip file and load its content into the global environment
process_zip_to_env <- function(zip_path) {
  temp_dir <- tempdir() # Temporary directory to extract contents
  unzip(zip_path, exdir = temp_dir) # Unzip the file
  
  # Identify .txt files inside the zip
  txt_files <- list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE)
  
  # Check if there are any .txt files
  if (length(txt_files) > 0) {
    for (txt_file in txt_files) {
      # Generate a name for the data frame based on the zip file name (without extension)
      zip_name <- file_path_sans_ext(basename(zip_path)) # Get zip file name without extension
      
      # Read the .txt file
      table_data <- read_delim(txt_file, delim = "\t", show_col_types = FALSE)
      
      # Assign the data frame to the global environment with the name from the zip file
      assign(zip_name, table_data, envir = .GlobalEnv)
    }
  }
  
  # Cleanup temporary files
  unlink(temp_dir, recursive = TRUE)
}

# List all zip files in the main folder and subfolders
zip_files <- list.files(main_folder, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)

# Process each zip file and load the tables into the global environment
lapply(zip_files, process_zip_to_env)

# Print a success message
cat("All zip files processed. Tables are available in the global environment!")



#combined_recorder <- rbind(LOUISIANA_STATE_UNIVERSITY_RECORDER_0001_001, LOUISIANA_STATE_UNIVERSITY_RECORDER_0001_001)


# Output file path for combined data
output_file <- file.path(main_folder, "combined_recorder.csv")

# Save data incrementally to avoid memory issues
write_csv(combined_recorder, output_file, append = TRUE)  # FIXED: This function now works















# Combine two datasets vertically or by rows. 

work_dir <- "C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/TAXASSESSOR/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001"
#raw_tax_assessor <- (work_dir + "/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001.txt" )
raw_tax_assessor <- readLines("C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/TAXASSESSOR/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001/LOUISIANA_STATE_UNIVERSITY_TAXASSESSOR_0001.txt")
head(raw_tax_assessor, 30)


# Read data in chunks

recorder_1 <- fread("C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/RECORDER/LOUISIANA_STATE_UNIVERSITY_RECORDER_0001_001.txt", sep = "\t", header = TRUE)

recorder_2 <- fread("C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/RECORDER/LOUISIANA_STATE_UNIVERSITY_RECORDER_0001_002.txt", sep = "\t", header = TRUE)

combined_recorder <- rbind(recorder_1, recorder_2)


# Save a dataset as a .txt file
write.table(combined_recorder, file = "combined_recorder.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

write.table(tax_assessor, file = "tax_assessor.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

write.table(merged_recorder_assesssor, file = "merged_recorder_assesssor.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

write.table(Lafayette_mar, file = "Lafayette_mar.txt", sep = "\t", row.names = FALSE, col.names = TRUE)


# Merge tax assessor and recorder datasets 
merged_recorder_assesssor <- merge(tax_assessor, combined_recorder, by = "[ATTOM ID]")

# Selection part of dataset
Lafayette_mar <- subset(merged_recorder_assesssor, SitusCounty=="Lafayette")



# Verify the number of rows and columns
cat("Number of rows:", nrow(recorder_2), "\n")
cat("Number of columns:", ncol(recorder_2), "\n")






















































# Load necessary libraries
library(tidyverse) # For data manipulation
library(zip)       # For handling zip files
library(tools)     # For file path manipulation

# Define the main folder path
main_folder <- "C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/HEDONIC/HEDONIC/SALES_DATA_AL_LA_MS/LOUISIANA"

# Function to process a single zip file and load its content into the global environment
process_zip_to_env <- function(zip_path) {
  temp_dir <- tempdir() # Temporary directory to extract contents
  unzip(zip_path, exdir = temp_dir) # Unzip the file
  
  # Identify .txt files inside the zip
  txt_files <- list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE)
  
  # Check if there are any .txt files
  if (length(txt_files) > 0) {
    for (txt_file in txt_files) {
      # Read the .txt file
      table_data <- read_delim(txt_file, delim = "\t", show_col_types = FALSE)
      
      # Append the data frame to a global list
      data_list[[length(data_list) + 1]] <<- table_data
    }
  }
  
  # Cleanup temporary files
  unlink(temp_dir, recursive = TRUE)
}

# Initialize an empty list to store datasets
data_list <- list()

# List all zip files in the main folder and subfolders
zip_files <- list.files(main_folder, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)

# Process each zip file and load the tables into the list
lapply(zip_files, process_zip_to_env)

# Combine all datasets into one
combined_Assessor_TS_LA <- rbind(data_list)

# Save the combined dataset as a .txt file
#output_file <- "C:/Users/mcerag1/OneDrive - Louisiana State University/Desktop/combined_data.txt"
write.table(combined_Assessor_TS_LA, file = "combined_Assessor_TS_LA.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

# Print a success message
cat("All datasets have been combined and saved to:", main_folder)




