# Load Packages
library(openxlsx)
library(tidyverse)
library(scales)
library(writexl)
library(janitor)
library(GGally)
library(sf)
library(moments)
library(ggpubr)

# Read Excel and CSV files
Dental_Workforce_and_Contracts_LAD <- read.csv("Reference files/FOI 01702.csv")
CarOwnership_LAD <- read.csv("Reference files/Car_Ownership_LAD23.csv")
IMD_Scores_LSOA11 <- read.xlsx("Reference files/IoD2019_Scores.xlsx", sheet = 2)
GPPS_LAD <- read.xlsx("Reference files/Table_1_LA_final.xlsx", sheet = 2)
Households_LAD22 <- read.csv("Reference files/Number_of_Households_LAD22.csv")
Populations_LAD <- read.csv("Reference files/Population_LAD23.csv")
Boundaries_LAD <- read_sf("Reference files/Local_Authority_Districts_December_2023_Boundaries_UK_BFC.gpkg")
LSOA11_to_LSOA21_to_LAD22 <- read.csv("Reference files/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2).csv")
LAD22_to_LAD23 <- read.csv("Reference files/Local_Authority_District_(2022)_to_Local_Authority_District_(2023)_Lookup_for_England_and_Wales.csv")
Health_Index <- read.xlsx("Reference files/healthindexscoresengland.xlsx", sheet = 5)

## First, work on the Demand indicators
# No cars
No_car_LAD23 <- CarOwnership_LAD %>%
  group_by(X2023.Lower.tier.local.authorities.Code) %>%
  filter(Car.or.van.availability..5.categories..Code == "0") %>%
  summarise(`Total no car` = sum(Observation)) %>%
  rename(LAD23CD = X2023.Lower.tier.local.authorities.Code) %>%
  filter(grepl("^E", LAD23CD))

# Average Income score
Income_LAD23 <- left_join(LSOA11_to_LSOA21_to_LAD22, 
                          IMD_Scores_LSOA11, 
                          by = c('LSOA11CD' = 'LSOA.code.(2011)')) %>%
  left_join(., LAD22_to_LAD23, by = 'LAD22CD') %>%
  filter(grepl("^E", LSOA11CD)) %>%
  .[, -c(1:12, 14:28, 31)] %>%
  group_by(LAD23CD) %>%
  summarise(`Average Income Deprivation Score` = mean(`Income.Score.(rate)`))

# GPPS 2yr Success Rates
GPPS_LAD23 <- GPPS_LAD[, -c(5:7, 9:12)] %>%
  left_join(., LAD22_to_LAD23, by = c('Code' = 'LAD22CD')) %>%
  group_by(LAD23CD,
           LAD23NM) %>%
  summarise(`Total Answered` = sum(total_2yrs),
            `Total Successful` = sum(Yes),
            `Total Unsuccessful` = sum(Excluding_CR)) %>%
  mutate(`Appointment Success Rate` = `Total Successful`/`Total Unsuccessful`)

# Merge these three variables and create the demand section of the index
df_list <- list(GPPS_LAD23, Income_LAD23, No_car_LAD23)      
Demand <- df_list %>% reduce(full_join, by = 'LAD23CD')

# Now remove rows containing Wales data and join Supply indicators to the Demand indicators
Supply_cleaned <- Dental_Workforce_and_Contracts_LAD %>%
  row_to_names(row_number = 1) %>%
  .[-c(296:330), -1] %>%
  rename(`Local Authority` = `Latest Local Authority Name`)
  
Supply_and_demand <- full_join(Supply_cleaned, 
                               Demand, 
                               by = c('Local Authority' = 'LAD23NM')) %>%
  relocate(LAD23CD, .before = `Local Authority`)

# Convert Contracted, Delivered UDAs and Performers class from 'Character' to 'Numeric'
Supply_and_demand$`UDA Performance Target` <- as.numeric(gsub(",",
                                                              "",
                                                              Supply_and_demand$`UDA Performance Target`))
Supply_and_demand$`UDA Delivered` <- as.numeric(gsub(",",
                                                     "",
                                                     Supply_and_demand$`UDA Delivered`))
Supply_and_demand$`Number of Performers` <- as.numeric(Supply_and_demand$`Number of Performers`)

# Remove Rows with NAs. These would be City of London and Isles of Scilly LADs
Supply_and_demand <- na.omit(Supply_and_demand)

# Sum the number of households per Local Authority District for 2023
Households_LAD23 <- Households_LAD22 %>%
  filter(grepl("^E", Lower.Tier.Local.Authorities.Code)) %>%
  rename(LAD22CD = Lower.Tier.Local.Authorities.Code) %>%
  left_join(., LAD22_to_LAD23, by = 'LAD22CD') %>%
  group_by(LAD23CD) %>%
  summarise(`Total Households` = sum(Observation))
  
# Sum the population per Local Authority District for 2023
Populations_LAD_total <- Populations_LAD %>%
  filter(grepl("^E", X2023.Lower.tier.local.authorities.Code)) %>%
  group_by(X2023.Lower.tier.local.authorities.Code) %>%
  summarise(`Total Population` = sum(Observation)) %>%
  rename(`LAD23CD` =  X2023.Lower.tier.local.authorities.Code)

# Create the full Supply and Demand table
df_list <- list(Supply_and_demand, Households_LAD23, Populations_LAD_total)
Supply_and_Demand_table <- df_list %>%
  reduce(full_join, by='LAD23CD')

Supply_and_Demand_table <- na.omit(Supply_and_Demand_table)

# Re-order the columns
Supply_and_Demand_table <- Supply_and_Demand_table %>%
  relocate(`Total Population`, .after = `Local Authority`) %>%
  relocate(`Total Households`, .after = `Total Population`) %>%
  relocate(`Number of Performers`, .after = `Total Households`) %>%
  relocate(`UDA Performance Target`, .after = `Number of Performers`)

# Calculate percentages
Supply_and_Demand_calc <- Supply_and_Demand_table %>%
  group_by(LAD23CD) %>%
  mutate(`Dentists per 10,000 Population` = `Number of Performers`/`Total Population` * 10000) %>%
  mutate(`Contracted UDAs per 10,000 Population` = `UDA Performance Target`/`Total Population` * 10000) %>%
  mutate(`Percentage of Delivered UDAs` = `UDA Delivered`/`UDA Performance Target` * 100) %>%
  mutate(`Percentage of Households with No Car` = `Total no car`/`Total Households` * 100) %>%
  mutate(`Appointment Success Rate` = `Appointment Success Rate`*100) %>%
  relocate(`Dentists per 10,000 Population`,
           .after = `Number of Performers`) %>%
  relocate(`Contracted UDAs per 10,000 Population`,
           .after = `UDA Performance Target`) %>%
  relocate(`Percentage of Delivered UDAs`,
           .after = `UDA Delivered`) %>%
  relocate(`Percentage of Households with No Car`,
           .after = `Total no car`)

# Check for multicollinearity after standardisation
X <- Supply_and_Demand_calc[, c(6,8,10,14,15,17)]

cor.matrix <- ggpairs(X, title = "Correlation Matrix",
                      upper = list(continuous = wrap('cor',size = 8)),
                      labeller = label_wrap_gen(20)) +
  theme(strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15),
        plot.title = element_text(size = 20,
                                  hjust = 0.5))

# Save this as an output
ggsave("Outputs/Updated/Pairwise Correlation Matrix.png",
        plot = cor.matrix,
        width = 14, 
        height = 14, 
        dpi = 600)

# Check for Skewness
Skew <- skewness(X)

head(Skew)

# Normalise function
normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Normalise the values
SD_Norm <- as.data.frame(lapply(Supply_and_Demand_calc[,c(6,8,10,14,15,17)],
                                normalise))

# Join the data
Supply_and_Demand_normalised <- cbind(Supply_and_Demand_calc, 
                                          SD_Norm) %>%
  rename(`Dentists per 10,000 (Normalised)` = 'Dentists.per.10.000.Population') %>%
  rename(`Contracted UDAs per 10,000 (Normalised)` = 'Contracted.UDAs.per.10.000.Population') %>%
  rename(`Delivered UDAs (Normalised)` = 'Percentage.of.Delivered.UDAs') %>%
  rename(`Appointment Success Rate (Normalised)` = 'Appointment.Success.Rate') %>%
  rename(`Income Deprivation Score (Normalised)` = 'Average.Income.Deprivation.Score') %>%
  rename(`Households with No Cars (Normalised)` = 'Percentage.of.Households.with.No.Car')

Supply_and_Demand_normalised$`Income Deprivation Score (Normalised)` <- 1 - Supply_and_Demand_normalised$`Income Deprivation Score (Normalised)`
Supply_and_Demand_normalised$`Households with No Cars (Normalised)` <- 1 - Supply_and_Demand_normalised$`Households with No Cars (Normalised)`

# Create the Index score
Supply_and_Demand_index <- Supply_and_Demand_normalised %>%
  group_by(LAD23CD) %>%
  mutate(`Total Score` = `Dentists per 10,000 (Normalised)` + 
           `Contracted UDAs per 10,000 (Normalised)` + 
           `Delivered UDAs (Normalised)` + 
           `Appointment Success Rate (Normalised)` + 
           `Income Deprivation Score (Normalised)` + 
           `Households with No Cars (Normalised)`)

# Normalise the Index scores
Index_Normalised <- as.data.frame(lapply(Supply_and_Demand_index[,24],
                                   normalise))

# Join the normalised values and multiply by 100
Supply_and_Demand_index_normalised <- cbind(Supply_and_Demand_index,
                                            Index_Normalised) %>%
  rename(`Total Score (Normalised)` = 'Total.Score') %>%
  mutate(`Public Dental Access Score` = `Total Score (Normalised)` * 100)

# Tidy up the table
Supply_and_Demand_index_table <- Supply_and_Demand_index_normalised %>%
  relocate(`Dentists per 10,000 (Normalised)`, 
           .after = `Dentists per 10,000 Population`) %>%
  relocate(`Contracted UDAs per 10,000 (Normalised)`, 
           .after = `Contracted UDAs per 10,000 Population`) %>%
  relocate(`Delivered UDAs (Normalised)`, 
           .after = `Percentage of Delivered UDAs`) %>%
  relocate(`Appointment Success Rate (Normalised)`, 
           .after = `Appointment Success Rate`) %>%
  relocate(`Income Deprivation Score (Normalised)`, 
           .after = `Average Income Deprivation Score`) %>%
  relocate(`Households with No Cars (Normalised)`, 
           .after = `Percentage of Households with No Car`)

# Apply ranking
Supply_and_Demand_index_table$`Public Dental Access Rank` <- rank(-Supply_and_Demand_index_table$`Public Dental Access Score`)

## Health Index for validation
# Clean the data
Health_Index_cleaned <- Health_Index %>%
  row_to_names(row_number = 2) %>%
  rename(LAD22CD = `Area Code`) %>%
  rename(`Health Score` = "2021") %>%
  mutate(`Health Score` = as.numeric(`Health Score`)) %>%
  left_join(., LAD22_to_LAD23, by = 'LAD22CD') %>%
  group_by(LAD23CD) %>%
  summarise(`Average Health Score` = mean(`Health Score`)) %>%
  na.omit()

# Apply the normalisation to the scores
Health_Norm <- as.data.frame(lapply(Health_Index_cleaned[,2],
                                    normalise))

# Join the scores to the main data.frame
Health_Index_Normalised <- cbind(Health_Index_cleaned, 
                                 Health_Norm) %>%
  rename(`Health Score (Normalised)` = 'Average.Health.Score') %>%
  mutate(`Health Score (Normalised)` = `Health Score (Normalised)`*100)

# Apply ranks to the scores
Health_Index_Normalised$`Health Index Rank` <- rank(-Health_Index_Normalised$`Health Score (Normalised)`)

# Join the health scores with the PDAS and calculate the difference
Supply_and_demand_difference <- Supply_and_Demand_index_table %>%
  left_join(., Health_Index_Normalised, by = "LAD23CD") %>%
  .[, -c(3:25)] %>%
  mutate(`Difference` = `Public Dental Access Score` - `Health Score (Normalised)`)

# Create a scatterplot using pearson correlation test
Scatterplot <- ggscatter(Supply_and_demand_difference, 
                         x = "Public Dental Access Score", 
                         y = "Average Health Score",
                         title = "Correlation between Public Dental Access Scores and Health Index Scores",
                         add = "reg.line", 
                         conf.int = TRUE, 
                         cor.coef = TRUE, 
                         cor.method = "pearson",
                         xlab = "Public Dental Access Score", 
                         ylab = "Health Index Score",
                         font.family = "Arial") +
  theme(plot.title = element_text(hjust = 0))

# Save the output
ggsave("Outputs/Updated/Scatterplot.png",
       plot = Scatterplot,
       width = 8, 
       height = 4.5, 
       dpi = 600)

# Remove old data.frames from environment
rm("CarOwnership_LAD", 
   "Demand", 
   "Dental_Workforce_and_Contracts_LAD", 
   "df_list", 
   "GPPS_LAD", 
   "GPPS_LAD23", 
   "Households_LAD22",
   "Households_LAD23",
   "IMD_Scores_LSOA11",
   "Income_LAD23",
   "LAD22_to_LAD23",
   "LSOA11_to_LSOA21_to_LAD22",
   "No_car_LAD23",
   "Populations_LAD",
   "Populations_LAD_total",
   "Supply_and_demand",
   "Supply_cleaned",
   "Index_Normalised",
   "SD_Norm",
   "Supply_and_Demand_calc",
   "Supply_and_Demand_index",
   "Supply_and_Demand_index_normalised",
   "Supply_and_Demand_normalised",
   "Supply_and_Demand_table",
   "X",
   "Skew",
   "cor.matrix",
   "normalise",
   "Health_Index",
   "Health_Index_cleaned",
   "Health_Norm",
   "Scatterplot")

# Export as Excel file
write_xlsx(Supply_and_Demand_index_table, 
           "Outputs/Updated/Supply_and_Demand_Table.xlsx")