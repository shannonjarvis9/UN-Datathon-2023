library(tidyverse)
library(readxl)
library(stringdist)
library(data.table)

setwd("C:/Users/jarvish/Downloads/")
 
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# Load Data 
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# Data from: https://unstats.un.org/unsd/snaama/downloads
GDP_per_capita_USD <- read_excel("Download-GDPPC-USD-countries.xlsx",  skip = 2) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "GDP per capita (current USD)")

GDP_USD <- read_excel("Download-GDPcurrent-USD-countries.xlsx",  skip = 2) %>% 
  filter(IndicatorName == "Gross Domestic Product (GDP)")  %>% 
  select(-"IndicatorName") %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "GDP (current USD)")

GrossNationalIncome_USD <- read_excel("Download-GNI-USD-countries.xlsx",  skip = 2)  %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "GNI (current USD)")

GrossNationalIncome_per_capita_USD <- read_excel("Download-GNIPC-USD-countries.xlsx",  skip = 2)  %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "GNI per capita (current USD)")

ShareGDP <- read_excel("Download-GDPcurrent-USD-countries.xlsx",  skip = 2)


# Data from: https://unstats.un.org/unsd/envstats/qindicators 
Municipal_waste_collected_thousandsoftonnes <- read_excel("Municipal waste collected.xlsx", sheet = "Data", na = "...")[1:133,] %>% 
  select(-c(contains("footnote"), "...30", "Source"))  %>% 
  mutate(CountryID = as.numeric(CountryID)) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Municipal Waste Collected (1000 tonnes)")

Municipal_waste_recycled <- read_excel("Percentage of municipal waste collected which is recycled.xlsx", sheet = "Data", na = "...") [1:85,] %>% 
  select(-c(contains("footnote"), "...30", "Source"))  %>% 
  mutate(CountryID = as.numeric(CountryID)) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Municipal Waste recycled (%)")

Municipal_waste_accessible_by_population <- read_excel("Total population served by municipal waste collection.xlsx", sheet = "data", na = "...")[1:76,]  %>% 
  select(-c(contains("footnote"), "...29"))  %>% 
  mutate(CountryID = as.numeric(CountryID)) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Total Population served by Municipal Waste collection")


Hazardous_Wasted_generated_tonnes <- read_excel("Hazardous waste generated.xlsx", sheet = "Data", na = "...")[1:99,] %>% 
  select(-c(contains("footnote"), "...30", "Source"))  %>% 
  mutate(CountryID = as.numeric(CountryID)) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Hazardous Waste Generated (tonnes)")

Hazardous_Wasted_generated_per_capita <- read_excel("Hazardous waste generated per capita.xlsx", sheet = "Data", na = "...")[1:102,] %>% 
  select(-c(contains("footnote"), "...27", "Source"))  %>% 
  mutate(CountryID = as.numeric(CountryID)) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Hazardous Waste Generated Per Capita")


Hazardous_Wasted_incinerated_tonnes <- read_excel("Hazardous waste incinerated.xlsx", sheet = "Data", na = "...")[1:78,] %>% 
  select(-c(contains("footnote"), "...30", "Source"))  %>% 
  mutate(CountryID = as.numeric(CountryID)) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Hazardous Waste Incinerated (tonnes)")

Hazardous_Wasted_landfilled_tonnes <- read_excel("Hazardous waste landfilled.xlsx", sheet = "Data", na = "...")[1:79,] %>% 
  select(-c(contains("footnote"), "...30", "Source"))  %>% 
  mutate(CountryID = as.numeric(CountryID)) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Hazardous Waste Landfilled (tonnes)")


Hazardous_Wasted_recycled_tonnes <- read_excel("Hazardous waste recycled.xlsx", sheet = "Data", na = "...")[1:75,] %>% 
  select(-c(contains("footnote"), "...30", "Source"))  %>% 
  mutate(CountryID = as.numeric(CountryID)) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Hazardous Waste Recycled (tonnes)")

Hazardous_Wasted_treated_tonnes <- read_excel("Hazardous waste treated or disposed.xlsx", sheet = "Data", na = "...")[1:86,] %>% 
  select(-c(contains("footnote"), "...30", "Source"))  %>% 
  mutate(CountryID = as.numeric(CountryID)) %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Hazardous Waste Treated or Disposed (tonnes)")

# non-yearly data 
Municipal_waste_composition <- read_excel("Composition of Municipal Waste (latest year).xlsx", sheet = "Data", na = "...")[1:98,] %>% 
  select(-c(contains("footnote")))  %>% 
  rename(Year = "Latest year available") %>% 
  mutate(CountryID = as.numeric(CountryID))

names(Municipal_waste_composition)[4:10] <- paste0("Municipal waste composition ", names(Municipal_waste_composition)[4:10], " (%)")

Municipal_waste_breakdown <- read_excel("Municipal waste treatment (latest year).xlsx", sheet = "Data", na = "...")[2:117,] %>% 
  select(-c(contains("footnote"), "Source", "Municipal waste collected"))   %>% 
  mutate(CountryID = as.numeric(CountryID))

names(Municipal_waste_breakdown)[4:8] <- paste0(names(Municipal_waste_breakdown)[4:8], " (%)")


# Data from:  https://data.worldbank.org/indicator/SP.POP.GROW
# has different population/country codes 
Population_Growth <- read_excel("API_SP.POP.GROW_DS2_en_excel_v2_5997106.xls", sheet = "Data", skip = 3) %>% 
  select(-c("Indicator Name", "Indicator Code"))  %>% 
  rename(Country = "Country Name") %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Population growth (annual %)")

Population_total <- read_excel("API_SP.POP.TOTL_DS2_en_csv_v2_6011311.xlsx", skip = 4) %>% 
  select(-c("Indicator Name", "Indicator Code"))  %>% 
  rename(Country = "Country Name") %>% 
  pivot_longer(cols = starts_with("1") | starts_with("2"),
               names_to = "Year", 
               values_to = "Population total")

country_group_region_incomegroup <- read_excel("Metadata_Country_API_SP.POP.TOTL_DS2_en_csv_v2_6011311.xlsx")

# Data from: 
sustainable_action_plan_SDG12_1_1 <- read_excel("country-scp-plan.xlsx") %>% 
  filter(!is.na(Code)) %>% 
  rename("Country" = Entity) %>% 
  select(-Code)

matrial_consumption_per_capita_SDG_12_2_2 <- read_excel("domestic-material-consumption-per-capita.xlsx") %>% 
  filter(!is.na(Code)) %>% 
  rename("Country" = Entity) %>% 
  select(-Code)

sustainable_development_in_school_SDG_4_1_1 <- read_excel("mainstreaming-sustainable-development-into-teacher-education.xlsx") %>% 
  filter(!is.na(Code)) %>% 
  rename("Country" = Entity) %>%  
  select(-Code)


# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# Reformat country names - there is difference in the data sets 
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# Not a great solution - getting the list of possible countrie names as there are variations 
country_list <- data.frame(CountryID = unique(dat$CountryID),
                           Country1 = NA, 
                           Counrty2 = NA)

for(id in unique(dat$CountryID)){
  country_names <- unlist(unique(c(unique(GDP_per_capita_USD %>% filter(CountryID == id) %>% select(Country)),  
                                   unique(GDP_USD%>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(GrossNationalIncome_USD%>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(GrossNationalIncome_per_capita_USD%>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(Municipal_waste_collected_thousandsoftonnes  %>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(Municipal_waste_recycled %>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(Municipal_waste_accessible_by_population  %>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(Hazardous_Wasted_generated_tonnes  %>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(Hazardous_Wasted_generated_per_capita  %>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(Hazardous_Wasted_incinerated_tonnes %>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(Hazardous_Wasted_landfilled_tonnes %>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(Hazardous_Wasted_recycled_tonnes  %>% filter(CountryID == id) %>% select(Country)) ,
                                   unique(Hazardous_Wasted_treated_tonnes %>% filter(CountryID == id) %>% select(Country)) )))
  
  country_list[which(country_list$CountryID == id), 2] <- country_names[1]
  country_list[which(country_list$CountryID == id), 3] <- country_names[2]
  
}

## manually adjust countries that coult not be matched 
#countries <- unique(c(Population_Growth$`Country`,
#                      Population_total$`Country`,
#                      country_group_region_incomegroup$`Country Code`, 
#                      sustainable_action_plan_SDG12_1_1$Country, 
#                      matrial_consumption_per_capita_SDG_12_2_2$Country, 
#                      sustainable_development_in_school_SDG_4_1_1$Country ))
#
#a <- 0
#missing_countries <- c()
#for(i in 1:length(countries)){
#  if(countries[i] %in% country_list$Country1 | countries[i] %in% country_list$Country2){
#    a <- a + 1 
#  } else{
#    missing_countries <- c(missing_countries, countries[i])
#  }
#} 
# write.xlsx(missing_countires, file = "countires.xlsx")

master_country_list <- country_list %>% 
  select(c(CountryID, Country1)) %>% 
  rename(Country = Country1)

format_names <- function(dat){
  countries_new_name <- read_excel("countries.xlsx") %>% filter(!is.na(new_name))
  countries_new_name2 <- country_list %>% filter(!is.na(Counrty2))
  if(! "CountryID" %in% names(dat)){dat$CountryID <- NA }
  
  for(i in 1:nrow(dat)){
    if(dat$`Country`[i] %in% countries_new_name$missing_countries){
      #print(paste0(i, dat$`Country`[i],  countries_new_name$new_name[which(dat$`Country`[i] == countries_new_name$missing_countries)]))
      dat$`CountryID`[i] <- countries_new_name$CountryID[which(dat$`Country`[i] == countries_new_name$missing_countries)]
      dat$`Country`[i] <- countries_new_name$new_name[which(dat$`Country`[i] == countries_new_name$missing_countries)]
    } 
    if(dat$`Country`[i] %in% countries_new_name2$Counrty2){
      #print(paste0(i, dat$`Country`[i],  countries_new_name2$Country1[which(dat$`Country`[i] == countries_new_name2$Counrty2)]))
      dat$`CountryID`[i] <- countries_new_name2$CountryID[which(dat$`Country`[i] == countries_new_name2$Counrty2)]
      dat$`Country`[i] <- countries_new_name2$Country1[which(dat$`Country`[i] == countries_new_name2$Counrty2)]
      
    }
    if(dat$`Country`[i] %in% master_country_list$Country & is.na(dat$`CountryID`[i])){
      dat$`CountryID`[i] <- master_country_list$CountryID[which(dat$`Country`[i] == master_country_list$Country)]

    }

  }
  return(dat)
}


# -------------------------------------------------------------------------------



# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# Combine all data sources together 
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
dat <- list(format_names(GDP_per_capita_USD) %>% select(-"Country") , 
            format_names(GDP_USD)%>% select(-"Country"), 
            format_names(GrossNationalIncome_USD)%>% select(-"Country"), 
            format_names(GrossNationalIncome_per_capita_USD)%>% select(-"Country"), 
            format_names(Municipal_waste_collected_thousandsoftonnes )%>% select(-"Country"), 
            format_names(Municipal_waste_recycled)%>% select(-"Country"), 
            format_names(Municipal_waste_accessible_by_population )%>% select(-"Country"), 
            format_names(Hazardous_Wasted_generated_tonnes )%>% select(-"Country"), 
            format_names(Hazardous_Wasted_generated_per_capita )%>% select(-"Country"), 
            format_names(Hazardous_Wasted_incinerated_tonnes)%>% select(-"Country"), 
            format_names(Hazardous_Wasted_landfilled_tonnes)%>% select(-"Country"), 
            format_names(Hazardous_Wasted_recycled_tonnes )%>% select(-"Country"), 
                         format_names(Hazardous_Wasted_treated_tonnes)%>% select(-"Country"))

dat <- dat %>% reduce(dplyr::full_join, by = c("CountryID","Year")) %>% 
  mutate(Year = as.numeric(Year))




# data for the latest year only 
# non-yearly data 


dat_latest_year <- list(format_names(Municipal_waste_composition) %>% select(-"Year"), 
                        format_names(Municipal_waste_breakdown) %>% select(-c("Year")))

dat_latest_year <- dat_latest_year %>% reduce(dplyr::full_join, by = c("Country", "CountryID"))

dat_latest_year <- list(dat_latest_year, 
                        full_join(format_names(sustainable_development_in_school_SDG_4_1_1 )%>% select(-c("Year", "Country")),
                        format_names(sustainable_action_plan_SDG12_1_1)%>% select(-c("Year",  "Country")), by = "CountryID")) 

dat_latest_year <- dat_latest_year %>% 
  reduce(dplyr::full_join, by = c("CountryID")) %>% 
  filter(!is.na(CountryID)) %>% 
  select(-"Country")

dat_latest_year <- left_join(dat_latest_year, master_country_list, by = "CountryID") %>% 
  relocate(Country, .after = CountryID)

write.xlsx(dat_latest_year, file = "Country data for only the latest year.xlsx")






pop_data <- list(format_names(Population_Growth) %>% filter(!is.na(CountryID)), 
                 format_names(Population_total) %>% select(-c("Country", "Country Code")) %>% filter(!is.na(CountryID)))
pop_data <- pop_data %>% reduce(dplyr::full_join, by = c("CountryID", "Year")) %>% 
  mutate(Year = as.numeric(Year))


dat2 <- list(pop_data,
             format_names(matrial_consumption_per_capita_SDG_12_2_2))

dat2 <- dat2 %>% reduce(dplyr::full_join, by = c("CountryID", "Year")) %>% 
  filter(!is.na(CountryID)) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  select(c("CountryID", "Country Code" , "Year" ,"Population growth (annual %)" ,
           "Population total" ,"12.2.2 - Domestic material consumption per capita, by type of raw material (tonnes) - EN_MAT_DOMCMPC - Total or no breakdown"))


# combine all back together 
yearly_data <- left_join(full_join(dat, dat2, by = c("CountryID", "Year")), master_country_list, by = "CountryID") %>% 
  relocate(c(Country, "Country Code"), .after = CountryID)

yearly_data <- left_join(yearly_data, country_group_region_incomegroup %>% select(-c("SpecialNotes", "TableName")))

write.xlsx(yearly_data, file = "Yearly population GDP and waste data.xlsx")
# add at end country_group_region_incomegroup
