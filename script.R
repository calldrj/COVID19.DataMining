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

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #

# **** Data Visualization  **** #
# 1. Import data tables from local storage
df_us <- read.csv('us.csv')
df_st <- read.csv('state.csv')

str(df_us)
str(df_st)
df_us$Date <- as.Date(as.character(df_us$Date, format='%m/%d/%Y'))
df_st$Date <- as.Date(as.character(df_st$Date, format='%m/%d/%Y'))
str(df_us)
str(df_st)
View(df_us)
View(df_st)
# Assign the last row of the data frame df_us / df_st
lr <- 86

# 2. Pie chart shows all cases ('Positive': p, 'Death': d, 'Recovered'; r)
p <- sum(df_us$Cumulated_Recovered[lr])
d <- sum(df_us$Cumulated_Deaths[lr])
r <- sum(df_us$Cumulated_Confirmed[lr])
case <- c('Recovered','Death', 'Positive')
case_number <- c(p, d, r)
pie <- data.frame(Case=case, Value=case_number)
head(pie)
library(ggplot2)
plot_title <- 'Coronavirus Cases In the United States'
plot_subtitle <- paste('Total: ', prettyNum(sum(pie$Value), big.mark=','))
plot_caption <-  paste('Update date:', format(df_us$Date[lr], '%a, %B %d, %Y'))
pie_theme <- theme( plot.title = element_text(hjust=0.5, color='#E95D0F', size=16, face='bold.italic'),
                    plot.subtitle = element_text(hjust=0.5, color='#0D1B2E', size=12, face='bold'),
                    plot.caption = element_text(hjust=0, color='#B51C35', size=10, face='italic'),
                    axis.line=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    panel.background=element_blank(),
                    panel.border=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank())
palette_color <- c('Death'='#0D1B2E', 'Recovered'='#429890', 'Positive'='#B51C35')
pie_chart <- ggplot(pie, aes(x='', y=Value, fill=Case)) +
  geom_bar(stat='identity', width=1) + coord_polar('y', start=0) +
  geom_text(aes(label=paste(round(Value/sum(Value)*100, 1),'%'), x=1.3),
            color='white', position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=palette_color) +
  labs(title=plot_title, subtitle=plot_subtitle, caption=plot_caption) +
  pie_theme
pie_chart

# 2. Point charts show all the progression of cases ('Positive', 'Deaths' and 'Recovered'; r) over time
library(scales)
library(plotly)
subtitle_con <- paste('Total positive: ', prettyNum(sum(df_us$Confirmed), big.mark=','))
subtitle_dea <- paste('Total Deaths: ', prettyNum(sum(df_us$Deaths), big.mark=','))
subtitle_rec <- paste('Total Recovered: ', prettyNum(sum(df_us$Recovered), big.mark=','))
plot_con <- ggplot(df_us, aes(x=Date, y=Confirmed)) +
  geom_point(color='#B51C35') +
  geom_smooth( color='#0D1B2E') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab('') +
  scale_x_date(labels=date_format('%b %d')) +
  ylab('Confirmed Positives') +
  scale_y_continuous(labels=function(x) format(x, scientific=FALSE)) 
ggplotly(plot_con) %>% layout(title=list(text=plot_title, y=1), font=list(family='sans serif', size=18, color='#E95D0F')) %>%
  add_annotations(text=subtitle_con, font=list(size=18, color='#0D1B2E'),
                  xref='paper', yref='paper', x=0.5, y=0.995, showarrow=FALSE)  %>% 
  add_annotations(text=plot_caption, font=list(family='sans serif', size=14, color='#B51C35'),
                  xref='paper', yref='paper', x=0, y=0, showarrow=FALSE)
plot_dr <- ggplot(df_us, aes(x=Date)) + 
  geom_point(aes(y=Deaths)) + 
  geom_smooth(aes(y=Deaths, color='Deaths')) +
  geom_point(aes(y=Recovered)) +
  geom_smooth(aes(y=Recovered, color='Recovered')) +
  scale_color_manual(name='', values=c('Deaths'='#B51C35', 'Recovered'='#429890')) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ylab('') +
  xlab('') +
  scale_x_date(labels=date_format('%b %d'))
ggplotly(plot_dr) %>% layout(title=list(text=plot_title, y=1), font=list(family='sans serif', size=18, color='#E95D0F')) %>%   
  add_annotations(text=plot_caption, font=list(family='sans serif', size=14, color='#B51C35'),
                  xref='paper', yref='paper', x=0, y=0, showarrow=FALSE) %>%
  add_annotations(text=subtitle_dea, font=list(family='sans serif', size=20, color='#B51C35'),
                  xref='paper', yref='paper', x=0, y=1, showarrow=FALSE) %>%
  add_annotations(text=subtitle_rec, font=list(family='sans serif', size=20, color='#429890'),
                  xref='paper', yref='paper', x=0, y=0.96, showarrow=FALSE)

# 3. Aggregrate data points by week to graph barchart of weekly number of cases
library(reshape2)   # melt()
library(lubridate)  # week()
weekly_count <- aggregate(df_us[ , c(2,4,6)], by=list(Week=week(df_us$Date)), sum)
weekly_count <- melt(weekly_count, id.vars='Week', variable.name='Case', value.name='Count')
View(weekly_count)
star_week <- 9
end_week <- 20
plot_weekly <- ggplot(weekly_count, aes(x=Week, y=Count, fill=Case)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_x_continuous(breaks=seq(star_week, end_week + 1, 2), limits=c(star_week, end_week + 1), expand=c(0,0)) +
  scale_fill_manual(name='', values = c('#B51C35','#E95D0F', '#429890')) +
  theme(panel.grid.major.x=element_blank())
ggplotly(plot_weekly) %>% layout(title=list(text=plot_title, y=1), font=list(family='sans serif', size=18, color='#E95D0F')) %>%   
  add_annotations(text=plot_subtitle, font=list(size=18, color='#0D1B2E'),
                  xref='paper', yref='paper', x=0.5, y=0.995, showarrow=FALSE)  %>% 
  add_annotations(text=plot_caption, font=list(family='sans serif', size=14, color='#B51C35'),
                  xref='paper', yref='paper', x=0, y=0, showarrow=FALSE)

# 4. Calenda heatmap of daily number of cases
library(gridExtra)    # multiplot stack
library(ggTimeSeries) # heatmap

cal_theme <- theme( plot.title = element_text(hjust=0.5, color='#0D1B2E', size=16, face='bold.italic'),
                    plot.subtitle = element_text(hjust=0.5, color='#0D1B2E', size=12, face='bold'),
                    plot.caption = element_text(hjust=0, color='#B51C35', size=10, face='italic'),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank() )
# Calenda for Confirmed cases
cal_con <- ggplot_calendar_heatmap(df_us, 'Date', 'Confirmed') +
  scale_fill_continuous(low='yellow', high='#B51C35') +
  labs(title=plot_title, subtitle=subtitle_con, caption=plot_caption) + cal_theme
cal_con
# Calenda for Deaths cases
cal_dea <- ggplot_calendar_heatmap( df_us, 'Date', 'Deaths' ) +
  scale_fill_continuous(low='blue', high='black') +
  labs(title=plot_title, subtitle=subtitle_dea, caption=plot_caption) + cal_theme
cal_dea
# Calenda for Recovered cases
cal_rec <- ggplot_calendar_heatmap( df_us, 'Date', 'Recovered' ) +
  scale_fill_continuous(low = 'green', high = '#429890') +
  labs(title=plot_title, subtitle=subtitle_rec, caption=plot_caption) + cal_theme
cal_rec
grid.arrange(cal_con, cal_dea, cal_rec, ncol=1)

# 5. State-by-state distribution map of cases
# Read latitude & longitude data for each state
df_state <- read.csv('statelatlong.csv')
df_state <- df_state[order(df_state$State), ]
rownames(df_state) <- NULL
View(df_state)
# Process data for map plot
library('dplyr')
df_st.count <- df_st %>% summarize_if(is.numeric, sum, na.rm=TRUE) %>% select(sort(names(.))) 
col_ex <- names(df_st.count) %in% c('GU', 'MP', 'PR', 'VI')
df_st.count <- df_st.count[!col_ex]
View(df_st.count)
for (i in c(1:51)) { df_state$Count[i] <- df_st.count[1,i] }
View(df_state)
str(df_state)
# Save clean data in local storage
write.csv(df_state, 'state_count.csv', row.names=FALSE)
# Plot a map of total confirmed cases of each state
library(leaflet)
map_count <- leaflet(width=2400, height=900) %>% 
  setView(-75,38,4.5) %>%
  addTiles() %>%
  addCircleMarkers(df_state$Longitude, df_state$Latitude, 
                   radius=log2(df_state$Count), color='#B51C35', fillOpacity=0.5, 
                   popup=paste0(df_state$State,' ', prettyNum(df_state$Count, big.mark=','), ' cases')) 
map_count

# 6. States with highest and lowest numbers of confirmed cases
df_top <- df_state %>% filter(rank(desc(Count)) <= 10)
df_top <- df_top[order(df_top$Count, decreasing=TRUE), ]
rownames(df_top) <- NULL
df_top$Rank <- seq.int(nrow(df_top))
View(df_top)
df_bot <- df_state %>% filter(rank(Count) <= 10)
df_bot <- df_bot[order(df_bot$Count, decreasing=TRUE), ]
rownames(df_bot) <- NULL
df_bot$Rank <- 41 + seq.int( nrow(df_bot))
View(df_bot)
theme_hilo <- theme( plot.title = element_text(hjust=0.5, color='#E95D0F', size=16, face='bold.italic'),
                     plot.caption = element_text(hjust=0, color='#B51C35', size=10, face='italic'),
                     axis.text.y=element_text(color='#B51C35', size=12, face='bold'), 
                     axis.text.x=element_text(color='#0D1B2E', size=12, face='bold'),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     panel.grid.major.x=element_blank(),
                     panel.grid.minor=element_blank() )
title_hi <- 'States with Highest Number of Confirmed Cases'
plot_top <- ggplot(df_top, aes(x=reorder(State, -Count), y=Count)) + 
  geom_bar(stat='identity', fill='#0D1B2E') + 
  coord_flip() +
  geom_text(aes(label=paste(prettyNum(Count, big.mark=','))), size=4, hjust=1.2, color='white', fontface='bold') +
  geom_text(aes(label=Rank), size=4, hjust=-0.6, color='#B51C35', fontface='bold') +
  scale_y_continuous(labels=function(x) format(x, scientific=FALSE)) + 
  labs(title=title_hi, caption=plot_caption) +
  theme_hilo
plot_top
title_lo <- 'States with Lowest Number of Confirmed Cases'
plot_bot <- ggplot(df_bot, aes(x=reorder(State, - Count), y=Count)) + 
  geom_bar(stat='identity', fill='#0D1B2E') +
  coord_flip() +
  geom_text(aes(label=paste(prettyNum(Count, big.mark=','))), size=4, hjust=1.2, color='white', fontface='bold') +
  geom_text(aes(label=Rank), size=4, hjust=-0.6, color='#B51C35', fontface='bold') +
  scale_y_continuous(labels=function(x) format(x, scientific=FALSE)) + 
  labs(title=title_lo, caption=plot_caption) +
  theme_hilo
plot_bot
grid.arrange(plot_top, plot_bot, ncol=2)

# 7. Compare the infection rate between New York and California
library(readr)
# Read daily confirmed cases from NY and CA from states.csv
df_NYCA <- read_csv('state.csv', col_types=cols_only('Date'=col_date(format=''),
                                                      'NY'=col_integer(), 'CA'=col_integer()))
# Remove date with single-digit cases in both NY and CA
df <- df_NYCA[(df$NY > 9 & df$CA > 9), ]
# Smooth the data in a Gaussian-window of one week interval for both NY and CA
library(smoother)
df_NYCA <- df_NYCA %>% mutate(NY_smth=round(smth(NY, window=7, method='gaussian', tails=TRUE))) %>% 
  mutate(CA_smth=round(smth(CA, window=7, method='gaussian', tails=TRUE))) 
View(df_NYCA)
title_NYCA <- 'Daily Confirmed Cases (Original & Smoothed) in New York and California'
plot_NYCA <- df_NYCA %>%  ggplot(aes(x=Date, y=NY)) +
                          geom_line(linetype='dotted', color='#429890') + 
                          geom_line(aes(y=NY_smth), color='#429890') +
                          geom_line(aes(y=CA), linetype='dotted', color='#E95D0F') +
                          geom_line(aes(y=CA_smth), color='#E95D0F') +
                          scale_color_manual(name=NULL, values=c('NY', 'CA')) +
                          annotate(geom='text',x=as.Date('2020-01-10'),
                                   y=11500, label='--- New York', fontface='bold', color='#429890') +
                          annotate(geom='text',x=as.Date('2020-01-10'),
                                   y=11000, label='--- California', fontface='bold', color='#E95D0F') +
                          theme(axis.title.x=element_blank(),
                                axis.title.y=element_blank())

ggplotly(plot_NYCA) %>% layout(title=list(text=title_NYCA, x=0.5, y=1), font=list(family='sans serif', size=18, color='#E95D0F')) %>%   
  add_annotations(text=plot_caption, font=list(family='sans serif', size=14, color='#B51C35'),
                  xref='paper', yref='paper', x=0, y=0, showarrow=FALSE)
