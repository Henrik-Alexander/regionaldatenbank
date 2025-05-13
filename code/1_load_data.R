###
# Project: Regionaldatenbank der Statistischen Aemter
# Purpose:
# Author: Henrik-Alexander Schubert
# Date: 08.05.2025
###

## Structure:
# 1. Make the test login
# 2. Load the fertility rates
# 3. Load the population structures
# 4. Load the death rates
# 5. Load the migration rates

library(wiesbaden)
library(tidyverse)

# Load the account information
source("Account_information.R")

# Save the credentials
genesis_login <- c(user=username, password=password, db="regio")

## Functions ---------------------------------------------

# FUNCTION: Create the age-groups for fertility
create_age_group <- function(x) {
  age <- numeric(length=length(x))
  
  
  # Fertility ages
  age[x=="ALT000B20"] <- "0-19"
  age[x=="ALT020B25"] <- "20-24"
  age[x=="ALT025B30"] <- "25-29"
  age[x=="ALT030B35"] <- "30-34"
  age[x=="ALT035B40"] <- "35-39"
  age[x=="ALT040UM"] <- "40-49"
  
  
  return(age)
}

transform_age_death <- function(x=ages_death) {
  
  # Create the containers
  lower <- upper <- age_group <- numeric(length=length(x))
  
  # Extreme ages
  age_group[str_detect(x, "ALT000")] <- "0-1"
  age_group[str_detect(x, "ALT085UM")] <- "85+"
  age_group[!(x %in% c("ALT000", "ALT085UM"))] <- paste(str_sub(x, 5, 6), str_sub(x, 8, 9), sep="-")[nchar(x) == 9]
  
  # Create the lower interval
  lower <- as.numeric(str_extract(age_group, "^[0-9]+"))
  upper <- as.numeric(str_extract(age_group, "[0-9]+$"))
  
  # Reduce the upper by one to avoid overlapping age groups
  upper <- ifelse(upper %% 5 == 0, upper-1, upper)
  
  # Estiamte the age interval 
  n <- upper - lower
  
  
  # Create the data frame
  age_data <- data.frame(lower=lower,
                         upper=upper,
                         n = n,
                         age_group=age_group)
  
  return(age_data)
  
}


## Make test login ---------------------------------------

# Make a test login
test_login(genesis=genesis_login)

## Retre

# Load the data
retrieve_datalist(tableseries="14111*", genesis=genesis_login)

# 2. Load the fertility rates ---------------------------

# 12612-02-01-4:
# Lebendgeborene nach Geschlecht, Nationalität und Alter der
# Mütter - Jahressumme - regionale Tiefe: Kreise und krfr.
# Städte (bis 2017)

# Set the natural population
np_series <- "126*"

# Retrieve the data list
np_datalist <- retrieve_datalist(tableseries=np_series, genesis=genesis_login)

# Get the tablenumber for births by age
births_datalist <- np_datalist[str_detect(np_datalist$description, "Lebendgeborene.+Kreis.+Altersgruppen"), ]
births_datalist <- births_datalist[nchar(births_datalist$description) == min(nchar(births_datalist$description)), ]

# Load the birth data
births_data <- lapply(births_datalist$tablename, function(x) retrieve_data(x, genesis=genesis_login))

# Extract the fuller births data
longest_data <- which.max(sapply(births_data, nrow))
births_df <- births_data[[longest_data]]

# Retrieve the metadata for the births
births_metadata <- retrieve_metadata(births_datalist$tablename[longest_data], genesis=genesis_login)

# Change the column names for the data
names(births_df) <- c("country", "region", "age_code", "year", "births", "flag")

# Create the age groups
births_df$age_group <- create_age_group(x=births_df$age_code)
if(any(is.na(births_df$age_group))) stop("Conversion of age group did not work!")

# Save the birth data
save(births_df, file="data/births_age.Rda")

# 4. Load the death rates ------------------------------

# 12613-93-01-4:
# Gestorbene nach Altersgruppen - Jahressumme - regionale
# Tiefe: Kreise und krfr. Städte

# Extract the deaths data list
deaths_datalist <- np_datalist[str_detect(np_datalist$description, "Gestorbene.+Kreis.+Altersgruppen.+85.+Geschlecht"), ]
deaths_datalist <- deaths_datalist[nchar(deaths_datalist$description) == min(nchar(deaths_datalist$description)), ]

# Load the birth data
deaths_df <- retrieve_data(deaths_datalist$tablename, genesis=genesis_login)

# Retrieve the metadata for the deaths
deaths_metadata <- retrieve_metadata(deaths_datalist$tablename, genesis=genesis_login)

# Change the column names for the data
names(deaths_df) <- c("country", "region", "age_code", "sex", "year", "deaths", "flag")

# Create the age groups
deaths_df <- bind_cols(deaths_df, transform_age_death(x=deaths_df$age_code))
if(any(is.na(deaths_df$lower))) stop("Conversion of age group did not work!")

# Extract the sex
deaths_df$sex <- str_sub(deaths_df$sex, 4, 4)

# Save the birth data
save(deaths_df, file="data/deaths_age.Rda")


# 3. Load the population structures ---------------------

## NOTE:
# There exist two different series for the population structures. 
# 1) That ranges from 1995 to 2013 (12411-04-01-4):
# Bevölkerung nach Geschlecht und Altersjahren (78) - Stichtag 31.12. - (bis 2010) regionale Tiefe: Kreise und krfr. Städte
# 2) That ranges from 2011 to 2023 (12411-04-02-4):
# Bevölkerung nach Geschlecht und Altersjahren (79) - Stichtag 31.12. - (ab 2011) regionale Tiefe: Kreise und krfr. Städte

# Set the series to population
pop_series <- "124*"

# Retrieve the data list
pop_datalist <- retrieve_datalist(tableseries=pop_series, genesis=genesis_login)

# Get the tablenumber for pop by age
pop_datalist <- pop_datalist[str_detect(pop_datalist$description, "Bevölkerungsstand.+Kreis.+Geschlecht.+Altersjahre"), ]
pop_datalist <- pop_datalist[nchar(pop_datalist$description) == min(nchar(pop_datalist$description)), ]

# Load the birth data
#pop2 <- retrieve_data(pop_datalist$tablename[2], genesis=genesis_login)
pop_data <- lapply(pop_datalist$tablename, retrieve_data, genesis=genesis_login)

# Change the column names
pop_data <- lapply(pop_data, FUN = function(x) { names(x) <- c("country", "region", "sex", "age_code", "date", "pop", "flag")
return(x)})

# Combine the two datasets
pop_df <- bind_rows(pop_data)

# Retrieve the metadata for the pop
pop_metadata <- lapply(pop_datalist$tablename, retrieve_metadata, genesis=genesis_login)

# Clean the population data 
clean_pop_data <- function(pop) {
  
  # Create the age group
  pop$age <- as.numeric(str_sub(pop$age_code, start=5, end=6))
  
  # Make the date colmn
  pop$date <- as.Date(gsub("\\.", "-", pop$date), format="%d-%m-%Y")
  pop$year <- year(pop$date)
  
  # Create sex
  pop$sex <- str_sub(pop$sex, start=4, end=4)
  
  return(pop)

}

# Clean the population data
pop_df <- clean_pop_data(pop=pop_df)

# Save the birth data
save(pop_df, file="data/pop_age.Rda")

# 5. Load the migration rates ---------------------------

# Set the natural population
mig_series <- "127*"

# Retrieve the data list
mig_datalist <- retrieve_datalist(tableseries=mig_series, genesis=genesis_login)

# Get the tablenumber for mig by age
mig_datalist <- mig_datalist[str_detect(mig_datalist$description, "Zuzüge.+Fortzüge.+Kreis.+Altersgruppen.+Geschlecht"), ]

# Load the migration data
mig_data <- retrieve_data("12711KJ003", genesis=genesis_login)

# Retrieve the metadata for the mig
mig_metadata <- retrieve_metadata("12711KJ003", genesis=genesis_login)

# Change the column names for the data
# INmigration=within country
# Outcmigration=within country
# Immigration = outside the country
# Emigration = outside the country
mig_names <- as.vector(outer(c("value", "flag"), c("inmigration_gemeinde", "outmigration_gemeinde", "immigration_kreis", "emmigration_kreis", "inmigration", "emigration"), paste, sep="_"))
names(mig_data) <- c("country", "region", "age_code", "sex", "year", mig_names)

# Create the age groups
mig_data$age_group <- ""
mig_data$age_group[mig_data$age_ce=="ALT000B18"] <- "0-17"
mig_data$age_group[mig_data$age_ce=="ALT018B25"] <- "18-24"
mig_data$age_group[mig_data$age_ce=="ALT025B30"] <- "25-29"
mig_data$age_group[mig_data$age_ce=="ALT030B35"] <- "30-49"
mig_data$age_group[mig_data$age_ce=="ALT050B65"] <- "50-64"
mig_data$age_group[mig_data$age_ce=="ALT065UM"] <- "65+"

# Clean the sex data
mig_data$sex <- str_sub(mig_data$sex, start=4, end=4)

# Save the birth data
save(mig_data, file="data/mig_age.Rda")

## Load the map and regional names ----------------------------

map_series <- "11"

# Load the map data
retrieve_datalist(map_series, genesis=genesis_login)



### END ####################################################