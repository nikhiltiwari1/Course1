library(dplyr)
library(stringr)
library(gdata)
library(tidyr)
# Load the data
companies <- read.delim("companies.txt", header =  TRUE , stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)

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

#load the mapping file
mapping <- read.csv("mapping.csv", header =  TRUE, stringsAsFactors = FALSE)

master_frame[] <- lapply(master_frame, as.character)

#String extract function
primary_sector_extract <- function(p)
{
  if(str_detect(p$category_list, pattern = "\\Q|\\E") == TRUE)
  {
    primary_sector <- substr(p$category_list,1,
           str_locate(pattern = "\\Q|\\E", p$category_list)-1)
  } else {
    primary_sector <- substr(p$category_list,1,nchar(p$category_list))
  }
  return(primary_sector)
}
#Extract the category List 
for (i in 1:nrow(master_frame))
{
  mf <- master_frame[i,]
  master_frame[i,"primary_sector"] <- primary_sector_extract(mf)
}
#master_frame$primary_sector[is.na(master_frame$primary_sector)] <- master_frame$category_list[is.na(master_frame$primary_sector)]

#Take a backup of the master frame
mapping_bkp <- mapping

#Clean the mapping data frame 0-> o
mapping$category_list <- gsub("0","o", mapping$category_list)

# o -> na
mapping$category_list <- gsub("otive ", "native", mapping$category_list)

mapping$category_list <- gsub("oge", "nage", mapping$category_list)
mapping$category_list <- gsub("olytics", "nalytics", mapping$category_list)

mapping$category_list <- gsub("ool", "onal", mapping$category_list)
mapping$category_list <- gsub("ionc", "inanc", mapping$category_list)

mapping$category_list <- gsub("once", "nance", mapping$category_list)
mapping$category_list <- gsub("otu", "natu", mapping$category_list)

mapping$category_list <- gsub("ono", "nano", mapping$category_list)
mapping$category_list <- gsub("ovi", "navi", mapping$category_list)


#to_lower
mapping$category_list <- str_to_lower(mapping$category_list)

#Backup of the master frame
master_frame_bkp <- master_frame
#to_lower
master_frame$primary_sector <- str_to_lower(master_frame$primary_sector)

#Wide to long
mapping_long <- gather(mapping, main_sector, nval, 2:10)
mapping_long <- mapping_long[!(mapping_long$nval == 0), ]
mapping_long <- mapping_long[,-3]

#merge the mapping and master frame on primary sector
final_master <- merge(master_frame, mapping_long, by.x = "primary_sector", by.y = "category_list")
final_master$raised_amount_usd <- as.numeric(final_master$raised_amount_usd)
#Creating the data frames each of the 3 countries

india_investment <- filter(final_master, country_code == "IND", funding_round_type == "venture")
usa_investment <- filter(final_master, country_code == "USA", funding_round_type == "venture")
gbr_investment <- filter(final_master, country_code == "GBR", funding_round_type == "venture")


group_main_sector <- function(p)
{
  sector_group <- group_by(p, main_sector)
}

india_invest_grp <- group_main_sector(india_investment)
usa_invest_grp <- group_main_sector(usa_investment)
gbr_invest_grp <- group_main_sector(gbr_investment)



avg_raised_amt <- function(p)
{
   summarise(p, mean(raised_amount_usd), n())
}

india_main_sector <- avg_raised_amt(india_invest_grp)
usa_main_sector <- avg_raised_amt(usa_invest_grp)
gbr_main_sector <- avg_raised_amt(gbr_invest_grp)

