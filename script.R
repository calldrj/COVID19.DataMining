# **** Data Mining  **** #
# 1. Webpage's url for data of CV19 in the US mining
cv19_url <- 'https://en.wikipedia.org/w/index.php?title=2020_coronavirus_outbreak_in_the_United_States&oldid=944107102'
# 2. Scrap the webpage then store in an object
library(rvest)      # read_html
library(magrittr)   # pipe %>%
cv19_wiki <- read_html(cv19_url)
# 3. Save the raw data of CV19 in the US in a table
cv19_raw <- cv19_wiki %>% html_nodes('table') %>% .[[5]] %>% html_table(fill=TRUE)
# 4. Visible review the structure and content of the raw data table
str(cv19_raw)
View(cv19_raw)
# 5. Save the data of confirmed, deaths, and recovered cases from the raw table
# Set border of the states' data table (top, bottom [the last updated row], left, right) 
# Row #93 corresponds with date 05/09/2020 
# Set order of the US' data table (top, bottom [the last updated row], left, right)
t_us <- c(2,93,57,63)
t_st <- c(2,93,1,56)

cv19_us <- cv19_raw[c(t_us[1]:t_us[2]), c(t_us[3]:t_us[4])]
cv19_st <- cv19_raw[c(t_st[1]:t_st[2]), c(t_st[3]:t_st[4])]
# 6. Visible review the structure and content of the saved data table
str(cv19_us)
View(cv19_us)
str(cv19_st)
View(cv19_st)
# 7. Remove some irrelevant rows (17, 18, 50, 51), then reset the row index of the data table
cv19_us <- cv19_us[-c(17:18, 50:51, 82:83), ]
cv19_st <- cv19_st[-c(17:18, 50:51, 82:83), ]
rownames(cv19_us) <- NULL
rownames(cv19_st) <- NULL

# 8. Assign proper column names in 2 data tables
colnames(cv19_us)[c(3, 5, 7)] <- c('Cumulated_Confirmed', 'Cumulated_Deaths', 'Cumulated_Recovered')
colnames(cv19_st) <- c('Date','AK','AZ','CA','CO','HI','ID','MT','NM','NV','OR','UT','WA','WY','IA','IL','IN','KS',
                       'MI','MN','MO','ND','NE','OH','OK','SD','WI','AL','AR','FL','GA','KY','LA','MS','NC','SC','TN',
                       'TX','VA','WV','CT','DC','DE','MA','MD','ME','NH','NJ','NY','PA','RI','VT','GU','MP','PR','VI')

# 9. Process missing data
# *** Check unrepoted number in DC and DE
cv19_st$DC
cv19_st$DC[47]
cv19_st$DE
cv19_st$DE[66]
cv19_st$KY
cv19_st$KY[79]
cv19_st$CT
cv19_st$CT[80]
# *** Fill DC unrepoted number with average of day before (94) and day after (91)
cv19_st$DC[47] <- 92
cv19_st$DE[66] <- 211
cv19_st$KY[79] <- 176
cv19_st$CT[80] <- 669
# *** Double check DC column
cv19_st$DC
cv19_st$DE
cv19_st$KY
cv19_st$CT
# Fill each of emtpy cells with 0
col <- c(2:7)
cv19_us[ , col][cv19_us[ , col]==''] <- 0
col <- c(2:56)
cv19_st[ , col][cv19_st[ , col]==''] <- 0

# 10. Test sum of a column, but it fails due to improper data type
sum(cv19_us$Confirmed)
sapply(cv19_us[1:4], class)
# 11. Convert data in columns, except Date of both tables to number
col <- c(2:7)
cv19_us[ , col] <- apply(cv19_us[ , col], 2, function(x) as.numeric(x))
col <- c(2:56)
cv19_st[ , col] <- apply(cv19_st[ , col], 2, function(x) as.numeric(x))
# 12. Retest column data type
sapply(cv19_us[1:4], class)
str(cv19_st)
sum(cv19_us$Deaths)
sum(cv19_st$CA)
# 13. Fix and format the Date column in proper form for manipulations
d <- as.Date('01/21/2020', format='%m/%d/%Y')
# Last update date
ld <- t_us[2] + 16
i <- c(0,3:5,9:12,15,25,30,35:ld)
Date <- c(d + i)
View(Date)
cv19_us$Date <- Date
cv19_st$Date <- Date
# 14. Double check column data types for both tables
str(cv19_us)
str(cv19_st)
# 15. Convert 2 tables to csv data frame, then double check
us.csv <- as.data.frame(cv19_us)
View(us.csv)
state.csv <- as.data.frame(cv19_st)
View(state.csv)
# 16. Save the csv dataset to a local drive
write.csv(us.csv, 'us.csv', row.names=FALSE)
write.csv(state.csv, 'state.csv', row.names=FALSE)
