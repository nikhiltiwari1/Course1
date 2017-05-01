install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
#install.packages("gdata")
#library(gdata)
install.packages("tidyr")
library(tidyr)

# Load the data
companies <- read.delim("companies.txt", header =  TRUE , stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)
#load the mapping file
mapping <- read.csv("mapping.csv", header =  TRUE, stringsAsFactors = FALSE)
missing_mappings <- read.csv("mapping_missing_sectors.csv", header =  TRUE, stringsAsFactors = FALSE)

#clean the permalink column
companies <- mutate(companies, permalink1 = str_to_lower(companies$permalink))
companies <- companies[,-1]

rounds2 <- mutate(rounds2, permalink1 = str_to_lower(rounds2$company_permalink))
rounds2 <- rounds2[,-1]

# Find the distinct values
n_distinct(companies$permalink1)
n_distinct(rounds2$permalink1)


# Are there any companies in the rounds2 file which are not present in companies? 
setdiff(rounds2$permalink1, companies$permalink1)



#merge companies and rounds2
master_frame <- inner_join(companies, rounds2, by = "permalink1")
#master_frame_left <- left_join(companies, rounds2, by = "permalink1")

# checking the number of NA values
sum(is.na(master_frame$raised_amount_usd))

#replacing NA values with numeric 0
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0


#Grouping on Fund type 
master_fund_group <- group_by(master_frame, funding_round_type)
fund_investment <- summarise(master_fund_group, mean(raised_amount_usd))
colnames(fund_investment) <- c("fund_type", "avg_raised_amt")

#group by country for Venture
master_country_group <- filter(group_by(master_frame, country_code), funding_round_type == "venture")
top9 <- summarise(master_country_group, sum(raised_amount_usd))
colnames(top9) <- c("Country_Code","Total_Sum")
top9 <- head(arrange(top9, desc(Total_Sum)),9)

#master_frame[] <- lapply(master_frame, as.character)

#Primary Sector extract
primary_sector_list <- str_split(master_frame$category_list, pattern="\\|")
primary_sector <- sapply(primary_sector_list, function(x) x[1][1])
master_frame[,"primary_sector"] <- primary_sector

#master_frame$primary_sector[is.na(master_frame$primary_sector)] <- master_frame$category_list[is.na(master_frame$primary_sector)]

#Take a backup of the master frame
mapping_bkp <- mapping
#Backup of the master frame
master_frame_bkp <- master_frame

# 0 -> na
mapping$category_list <- gsub("0", "na",mapping$category_list)
mapping$category_list <- gsub("2.na", "2.0", mapping$category_list)

#to_lower
master_frame$primary_sector <- str_to_lower(master_frame$primary_sector)
mapping$category_list <- str_to_lower(mapping$category_list)

#Wide to long
mapping_long <- gather(mapping, main_sector, nval, 2:10)
mapping_long <- mapping_long[!(mapping_long$nval == 0), ]
mapping_long <- mapping_long[,-3]

mapping_long_bkp <- mapping_long

mapping_long <- rbind(mapping_long,missing_mappings)


#merge the mapping and master frame on primary sector
final_master <- merge(master_frame, mapping_long, by.x = "primary_sector", by.y = "category_list")

#Creating the data frames each of the 3 countries

india_investment <- filter(final_master, country_code == "IND", funding_round_type == "venture")
usa_investment <- filter(final_master, country_code == "USA", funding_round_type == "venture")
gbr_investment <- filter(final_master, country_code == "GBR", funding_round_type == "venture")

#create data frames with groupings on main sector
group_main_sector <- function(p)
{
  sector_group <- group_by(p, main_sector)
}

india_invest_grp <- group_main_sector(india_investment)
usa_invest_grp <- group_main_sector(usa_investment)
gbr_invest_grp <- group_main_sector(gbr_investment)


# Summarises the main sectors with Avg raised amount, number of investments
# Also selects the main sectors where investments are between 5 and 15 million
avg_raised_amt <- function(p)
{
   country_main_sector <- summarise(p, mean(raised_amount_usd), n())
   colnames(country_main_sector) <- c("main_sector","avg_raised_amt_usd","no. of investments")
   country_main_sector <- subset(country_main_sector, avg_raised_amt_usd > 5000000 & avg_raised_amt_usd < 15000000)
   return(country_main_sector)
}

# calling the avg_raised_amt function 
india_main_sector <- avg_raised_amt(india_invest_grp)
usa_main_sector <- avg_raised_amt(usa_invest_grp)
gbr_main_sector <- avg_raised_amt(gbr_invest_grp)
