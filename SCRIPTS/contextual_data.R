
## About this file ----

# Jamie Evans, 29/10/24
# The purpose of this code is to prepare various contextual datasets (primarily at LSOA-level) for use in our Mapping Energy Consumption project. The datasets are mostly from the 2021 Census and have been downloaded into DATA/Contextual/Raw from the ONS website and from Nomisweb. Some datasets are in long format and others are in wide format. We will produce a wide dataset of key variables at LSOA-level.


## Set-up ----

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)


## Import and manipulate files ----

### ~ Census 2021 ~ ----

#### ... Age ----

#Import data
age_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS007A_Agebands_wide.csv",
                    skip = 5)
# Copy dataset
age_dv <- age_raw

# Rename key columns
names(age_dv)[names(age_dv) == 'Total'] <- 'All usual residents'
names(age_dv)[names(age_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Deriving new categories
age_dv$`Aged 19 or under` <- age_dv$`Aged 4 years and under`+ age_dv$`Aged 5 to 9 years` + age_dv$`Aged 10 to 14 years` + age_dv$`Aged 15 to 19 years`

age_dv$`Aged 65 plus` <- age_dv$`Aged 65 to 69 years` + age_dv$`Aged 70 to 74 years`+ age_dv$`Aged 75 to 79 years` + age_dv$`Aged 80 to 84 years` + age_dv$`Aged 85 years and over`

# Determing percentages
age_dv$`Aged 19 or under (prop)` <- age_dv$`Aged 19 or under` / age_dv$`All usual residents`

age_dv$`Aged 65 plus (prop)` <- age_dv$`Aged 65 plus` / age_dv$`All usual residents`

age_dv$`Aged 20 to 64 (prop)` <- 1 - (age_dv$`Aged 19 or under (prop)` + age_dv$`Aged 65 plus (prop)`)

# Keep final columns
age_dv <- age_dv[,c("LSOA2021_code","All usual residents","Aged 19 or under (prop)","Aged 20 to 64 (prop)", "Aged 65 plus (prop)")]
rm(age_raw)


#### ... Economic activity ----

#Import data
econ_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS066_EconAct_wide.csv",
                    skip = 6)
# Copy dataset
econ_dv <- econ_raw

# Rename key columns
names(econ_dv)[names(econ_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Remove blank rows
econ_dv <- filter(econ_dv,!is.na(econ_dv$`LSOA2021_code`))

# Deriving new categories
econ_dv$`Student (employed or not)` <- econ_dv$`Economically active and a full-time student` + econ_dv$`Economically inactive: Student`

# Determining percentages
econ_dv$`In employment (exc students) (prop)` <- econ_dv$`Economically active (excluding full-time students):In employment` / econ_dv$`Total: All usual residents aged 16 years and over`

econ_dv$`Unemployed (exc students) (prop)` <- econ_dv$`Economically active (excluding full-time students): Unemployed` / econ_dv$`Total: All usual residents aged 16 years and over`

econ_dv$`Student (prop)` <- econ_dv$`Student (employed or not)` / econ_dv$`Total: All usual residents aged 16 years and over`

econ_dv$`Retired (prop)` <- econ_dv$`Economically inactive: Retired` / econ_dv$`Total: All usual residents aged 16 years and over`

econ_dv$`Carer (prop)` <- econ_dv$`Economically inactive: Looking after home or family` / econ_dv$`Total: All usual residents aged 16 years and over`

econ_dv$`Long-term sick or disabled (prop)` <- econ_dv$`Economically inactive: Long-term sick or disabled` / econ_dv$`Total: All usual residents aged 16 years and over`

econ_dv$`Other econ inactivity (prop)` <- econ_dv$`Economically inactive: Other` / econ_dv$`Total: All usual residents aged 16 years and over`

# Keep final columns
econ_dv <- econ_dv[,c("LSOA2021_code","Total: All usual residents aged 16 years and over","In employment (exc students) (prop)","Unemployed (exc students) (prop)", "Student (prop)", "Retired (prop)", "Carer (prop)", "Long-term sick or disabled (prop)", "Other econ inactivity (prop)")]
rm(econ_raw)


#### ... Disability ----

#Import data
disability_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS038_Disability_wide.csv",
                    skip = 6)
# Copy dataset
disability_dv <- disability_raw

# Rename key columns
names(disability_dv)[names(disability_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Remove blank rows
disability_dv <- filter(disability_dv,!is.na(disability_dv$`LSOA2021_code`))

# Determing percentages
disability_dv$`Disabled (prop)` <- disability_dv$`Disabled under the Equality Act` / disability_dv$`Total: All usual residents`


# Keep final columns
disability_dv <- disability_dv[,c("LSOA2021_code","Disabled (prop)")]
rm(disability_raw)


#### ... Ethnic group ----

#Import data
ethnic_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS021_Ethnicity_wide.csv",
                           skip = 6)
# Copy dataset
ethnic_dv <- ethnic_raw

# Rename key columns
names(ethnic_dv)[names(ethnic_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Remove blank rows
ethnic_dv <- filter(ethnic_dv,!is.na(ethnic_dv$`LSOA2021_code`))

# Determining key variables
ethnic_dv$`White (excluding other white) (prop)` <- ethnic_dv$`White (prop)` - ethnic_dv$`White: Other White (prop)`


# Divide proportions by 100 to align with other datasets
ethnic_dv$`Asian, Asian British or Asian Welsh (prop)` <- ethnic_dv$`Asian, Asian British or Asian Welsh (prop)` / 100
ethnic_dv$`Black, Black British, Black Welsh, Caribbean or African (prop)` <- ethnic_dv$`Black, Black British, Black Welsh, Caribbean or African (prop)` / 100
ethnic_dv$`Mixed or Multiple ethnic groups (prop)` <- ethnic_dv$`Mixed or Multiple ethnic groups (prop)` / 100
ethnic_dv$`White (excluding other white) (prop)` <- ethnic_dv$`White (excluding other white) (prop)` / 100
ethnic_dv$`White: Other White (prop)` <- ethnic_dv$`White: Other White (prop)` / 100


# Keep final columns
ethnic_dv <- ethnic_dv[,c("LSOA2021_code","Asian, Asian British or Asian Welsh (prop)", "Black, Black British, Black Welsh, Caribbean or African (prop)", "Mixed or Multiple ethnic groups (prop)", "White (excluding other white) (prop)", "White: Other White (prop)")]
rm(ethnic_raw)


#### ... Household composition ----

#Import data
hhcomp_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS003_HHComp_wide.csv",
                           skip = 6)
# Copy dataset
hhcomp_dv <- hhcomp_raw

# Rename key columns
names(hhcomp_dv)[names(hhcomp_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Remove blank rows
hhcomp_dv <- filter(hhcomp_dv,!is.na(hhcomp_dv$`LSOA2021_code`))

# Deriving key variables
hhcomp_dv$`Single person or lone parent` <- hhcomp_dv$`One-person household` + hhcomp_dv$`Single family household: Lone parent family`

# Determing percentages
hhcomp_dv$`Single person or lone parent households (prop)` <- hhcomp_dv$`Single person or lone parent` / hhcomp_dv$`Total: All households`


# Keep final columns
hhcomp_dv <- hhcomp_dv[,c("LSOA2021_code","Total: All households", "Single person or lone parent households (prop)")]
rm(hhcomp_raw)


#### ... Population density ----

#Import data
popdens_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS006_PopDens_wide.csv",
                           skip = 6)
# Copy dataset
popdens_dv <- popdens_raw

# Rename key columns
names(popdens_dv)[names(popdens_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Remove blank rows
popdens_dv <- filter(popdens_dv,!is.na(popdens_dv$`LSOA2021_code`))


# Keep final columns
popdens_dv <- popdens_dv[,c("LSOA2021_code","Usual residents per square kilometre")]
rm(popdens_raw)


#### ... Deprivation ----

#Import data
dep_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS011_Deprived_wide.csv",
                           skip = 6)
# Copy dataset
dep_dv <- dep_raw

# Rename key columns
names(dep_dv)[names(dep_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Remove blank rows
dep_dv <- filter(dep_dv,!is.na(dep_dv$`LSOA2021_code`))

# Determing percentages
dep_dv$`Deprived on any dimension (prop hh)` <- 1 - (dep_dv$`Household is not deprived in any dimension` / dep_dv$`Total: All households`)


# Keep final columns
dep_dv <- dep_dv[,c("LSOA2021_code","Deprived on any dimension (prop hh)")]
rm(dep_raw)


#### ... Occupancy rating ----

#Import data
occup_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS052_OccupRatingBed_wide.csv",
                           skip = 6)
# Copy dataset
occup_dv <- occup_raw

# Rename key columns
names(occup_dv)[names(occup_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Remove blank rows
occup_dv <- filter(occup_dv,!is.na(occup_dv$`LSOA2021_code`))

# Determing key variables
occup_dv$`Under-occupied` <- occup_dv$`Occupancy rating of bedrooms: +2 or more` + occup_dv$`Occupancy rating of bedrooms: +1`

occup_dv$`Overcrowded` <- occup_dv$`Occupancy rating of bedrooms: -2 or less` + occup_dv$`Occupancy rating of bedrooms: -1`

# Determing percentages
occup_dv$`Overcrowded (prop)` <- occup_dv$`Overcrowded` / occup_dv$`Total: All households`

occup_dv$`Under-occupied (prop)` <- occup_dv$`Under-occupied` / occup_dv$`Total: All households`


# Keep final columns
occup_dv <- occup_dv[,c("LSOA2021_code","Overcrowded (prop)", "Under-occupied (prop)")]
rm(occup_raw)



#### ... Second homes ----

#Import data
secondhomes_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_SecondAddresses_wide.csv",
                           skip = 3,
                           locale=locale(encoding="latin1"))
# Copy dataset
secondhomes_dv <- secondhomes_raw

# Rename key columns
names(secondhomes_dv)[names(secondhomes_dv) == 'Area Code'] <- 'LSOA2021_code'

# Convert redacted data to 0 (indicated by 'c')
secondhomes_dv$`Holiday home`[secondhomes_dv$`Holiday home` == "c"] <- 0
secondhomes_dv$`Another address when working away from home`[secondhomes_dv$`Another address when working away from home` == "c"] <- 0
secondhomes_dv$`Students term-time address`[secondhomes_dv$`Students term-time address` == "c"] <- 0
secondhomes_dv$`Students home address`[secondhomes_dv$`Students home address` == "c"] <- 0
secondhomes_dv$`Another parent or guardians address`[secondhomes_dv$`Another parent or guardians address` == "c"] <- 0
secondhomes_dv$`Partners address`[secondhomes_dv$`Partners address` == "c"] <- 0
secondhomes_dv$`Other (including armed forces)`[secondhomes_dv$`Other (including armed forces)` == "c"] <- 0

# Convert to numeric type
secondhomes_dv$`Holiday home` <- as.numeric(secondhomes_dv$`Holiday home`)
secondhomes_dv$`Another address when working away from home` <- as.numeric(secondhomes_dv$`Another address when working away from home`)
secondhomes_dv$`Other (including armed forces)` <- as.numeric(secondhomes_dv$`Other (including armed forces)`)

# Derive key variables
secondhomes_dv$`Holiday home, working away or other (inc armed forces)` <- secondhomes_dv$`Holiday home` + secondhomes_dv$`Another address when working away from home` + secondhomes_dv$`Other (including armed forces)`

# Add 'all usual residents'
secondhomes_dv <- merge(secondhomes_dv,age_dv[,c("LSOA2021_code","All usual residents")], by="LSOA2021_code", all.x=T)

# Determining percentages
secondhomes_dv$`Holiday home, working away or other (inc armed forces) (prop usual residents)` <- secondhomes_dv$`Holiday home, working away or other (inc armed forces)` / secondhomes_dv$`All usual residents`


# Keep final columns
secondhomes_dv <- secondhomes_dv[,c("LSOA2021_code","Holiday home, working away or other (inc armed forces)","Holiday home, working away or other (inc armed forces) (prop usual residents)")]
rm(secondhomes_raw)




#### ... Tenure ----

#Import data
tenure_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS054_Tenure_wide.csv",
                           skip = 6)
# Copy dataset
tenure_dv <- tenure_raw

# Rename key columns
names(tenure_dv)[names(tenure_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Remove blank rows
tenure_dv <- filter(tenure_dv,!is.na(tenure_dv$`LSOA2021_code`))


# Divide proportions by 100 to align with other datasets
tenure_dv$`Owned (prop)` <- tenure_dv$`Owned (prop)` / 100
tenure_dv$`Owned: Owns outright (prop)` <- tenure_dv$`Owned: Owns outright (prop)` / 100
tenure_dv$`Owned: Owns with a mortgage or loan (prop)` <- tenure_dv$`Owned: Owns with a mortgage or loan (prop)` / 100
tenure_dv$`Social rented (prop)` <- tenure_dv$`Social rented (prop)` / 100
tenure_dv$`Social rented: Rents from council or Local Authority (prop)` <- tenure_dv$`Social rented: Rents from council or Local Authority (prop)` / 100
tenure_dv$`Social rented: Other social rented (prop)` <- tenure_dv$`Social rented: Other social rented (prop)`/ 100
tenure_dv$`Private rented (prop)` <- tenure_dv$`Private rented (prop)` / 100

# Keep final columns
tenure_dv <- tenure_dv[,c("LSOA2021_code","Owned (prop)","Owned: Owns outright (prop)", "Owned: Owns with a mortgage or loan (prop)", "Social rented (prop)", "Social rented: Rents from council or Local Authority (prop)","Social rented: Other social rented (prop)","Private rented (prop)")]
rm(tenure_raw)



#### ... Home-working ----

#Import data
wfh_raw <- read_csv("DATA/Contextual/Raw/EW_Census2021_LSOA_TS058_Homeworking_wide.csv",
                       skip = 6)
# Copy dataset
wfh_dv <- wfh_raw

# Rename key columns
names(wfh_dv)[names(wfh_dv) == 'mnemonic'] <- 'LSOA2021_code'

# Remove blank rows
wfh_dv <- filter(wfh_dv,!is.na(wfh_dv$`LSOA2021_code`))

# Divide proportions by 100 to align with other datasets
wfh_dv$`Works mainly from home (prop)` <- wfh_dv$`Works mainly from home (prop)` / 100
wfh_dv$`Works mainly at an offshore installation, in no fixed place, or outside the UK (prop)` <- wfh_dv$`Works mainly at an offshore installation, in no fixed place, or outside the UK (prop)` / 100

# Keep final columns
wfh_dv <- wfh_dv[,c("LSOA2021_code","Total: All usual residents aged 16 years and over in employment the week before the census","Works mainly from home (prop)","Works mainly at an offshore installation, in no fixed place, or outside the UK (prop)")]
rm(wfh_raw)




## Join all census datasets ----

# Join all data frames in list
list_df = list(age_dv, dep_dv, disability_dv, econ_dv, ethnic_dv, hhcomp_dv, occup_dv, popdens_dv,secondhomes_dv,tenure_dv,wfh_dv)
merged_census <- list_df %>% reduce(inner_join, by='LSOA2021_code')
rm(list_df)

# Change variable order
merged_census <- merged_census %>% relocate("Total: All households", .after = "LSOA2021_code")
merged_census <- merged_census %>% relocate("Total: All usual residents aged 16 years and over", .after = "All usual residents")
merged_census <- merged_census %>% relocate("Total: All usual residents aged 16 years and over in employment the week before the census", .after = "Total: All usual residents aged 16 years and over")

# Export merged dataset
write.csv(merged_census,"DATA/Contextual/merged_census21_lsoa.csv",row.names=F)