library(tidyverse)
library(lubridate)

# import data - we're using the FDNY dataset
# https://data.cityofnewyork.us/Public-Safety/Incidents-Responded-to-by-Fire-Companies/tm6d-hbzd
master_tbl = read.csv('../../Data/Data_Wrangling/Incidents_Responded_to_by_Fire_Companies.csv',
    header = T, stringsAsFactors = F)

# Define some constants that will be used for this portion
BFIRE = '111 - Building fire'
SMOKE = '651 - Smoke scare, odor of smoke'
MALIC = '710 - Malicious, mischievous false call, other'
COOKF = '113 - Cooking fire, confined to container'
# unique(master_tbl$BOROUGH_DESC)
BOROS = c('1 - Manhattan', '2 - Bronx', '3 - Staten Island', '4 - Brooklyn',  '5 - Queens')

# For all answers, I'm going to report up to 5 decimals, same as the coding challenge
# Q1 - most common call proportion
counter = master_tbl %>% count(INCIDENT_TYPE_DESC) %>% arrange(desc(n))
# head(counter)
print(paste0('The most common type is: ', counter$INCIDENT_TYPE_DESC[1]))
# Incident type 300 takes up about 36% of all rows
counter$n[1] / nrow(master_tbl)

# Q2 - differences in number of units arriving by incident type
# Note - for calculation of means, NA's were omitted
# (default assumption for missing data with no need for imputation)
sub_tbl = master_tbl %>% drop_na(UNITS_ONSCENE)
# sum(sub_tbl$INCIDENT_TYPE_DESC == BFIRE)
# sum(sub_tbl$INCIDENT_TYPE_DESC == SMOKE)
# head(sub_tbl)
mu_1 = mean((sub_tbl %>% filter(INCIDENT_TYPE_DESC == BFIRE))[['UNITS_ONSCENE']])
mu_2 = mean((sub_tbl %>% filter(INCIDENT_TYPE_DESC == SMOKE))[['UNITS_ONSCENE']])
# average for BFIRE is about 11.2, while for SMOKE it is 4.0
mu_1 / mu_2

# Q3 - false call in diff. boroughs
# sum(master_tbl$BOROUGH_DESC == BOROS[1])
# sum(master_tbl$BOROUGH_DESC == BOROS[5])
# sum(master_tbl$INCIDENT_TYPE_DESC == MALIC)
sub_tbl_5 = master_tbl %>% filter(BOROUGH_DESC == BOROS[5])
prop_5 = sum(sub_tbl_5$INCIDENT_TYPE_DESC == MALIC) / nrow(sub_tbl_5)
sub_tbl_1 = master_tbl %>% filter(BOROUGH_DESC == BOROS[1])
prop_1 = sum(sub_tbl_1$INCIDENT_TYPE_DESC == MALIC) / nrow(sub_tbl_1)
# false call rate for BORO[5] is about 0.556, for BORO[1] is about 0.0429
prop_5 / prop_1

# Q4 - distribution of amount of time it takes for the first unit to arrive
# define a function that calculates the number of minutes between two dt entries from our datasets
# uses int_length() and interval() from lubridate
int_minutes = function(dt1, dt2) {
    # Assumes that the dt1, dt2 strings come in the right format and timezone is EST,
    # as supplied by this dataset. Also assumes dt2 is after dt1
    return(int_length(
        interval(mdy_hms(dt1, tz = 'EST'), mdy_hms(dt2, tz = 'EST'))
    ) / 60)
}
# dt1 = '01/01/2013 12:58:10 AM'
# dt2 = '01/01/2013 01:00:50 AM'
# int_minutes(dt1, dt2)

sub_tbl = master_tbl %>% filter(INCIDENT_TYPE_DESC == BFIRE) %>% select(
    c('IM_INCIDENT_KEY', 'INCIDENT_TYPE_DESC', 'INCIDENT_DATE_TIME', 'ARRIVAL_DATE_TIME'))
sub_tbl$int_minutes = do.call(int_minutes, sub_tbl %>% 
    rename(dt1 = 'INCIDENT_DATE_TIME', dt2 = 'ARRIVAL_DATE_TIME') %>% select(c('dt1', 'dt2')))
# Some rows are missing entries and thus yielding NA
sub_tbl = sub_tbl %>% drop_na(int_minutes)
# hist(sub_tbl$int_minutes)
# there was one quirky data point where dt2 was before dt1?
# obtain the third quartile (75%), as directed
quantile(sub_tbl$int_minutes)

# Q5 - cooking hours inferred by incident type
# calculate the hour of the incidents using hour() function of lubridate
master_tbl$hr = hour(mdy_hms(master_tbl$INCIDENT_DATE_TIME, tz = 'EST'))
# head(master_tbl)
# Calculate total number of incidents by hour
hour_counts = master_tbl %>% count(hr)
# Calculate cooking fire incidents by hour
hour_counts$COOKF_n = sapply(hour_counts$hr, function(x) {nrow(master_tbl %>% 
    filter(hr == x, INCIDENT_TYPE_DESC == COOKF))})
hour_counts$proportion = hour_counts$COOKF_n / hour_counts$n
hour_counts %>% arrange(desc(proportion))
# we see the spike during dinner hours - esp. hours 17, 18, 19
# qplot(hour_counts$hr, hour_counts$proportion)

# Q6 - r^2 between population and building fires
# read in the parsed table:
# this part was run by a python snippet. The code is in Scripts/simple_web_scraper.py and also copied below:
# # python snippet to import data table for part 2
# # from https://www.newyork-demographics.com/zip_codes_by_population
# # lxml was installed along with python for nice parsing of the table
# import urllib.request as urllib2
# import pandas as pd
# link = 'https://www.newyork-demographics.com/zip_codes_by_population'
# response = urllib2.urlopen(link)
# contents = response.read()
# tbl = pd.read_html(contents)[0]
# # However, some zip code entries need to be parsed manually
# # Since this table is small (~1600 rows) and main processing is being done in another script,
# # I'm going to write the rows manually
# out_file = '../../Table/zip_pop.tsv'
# out_fhandle = open(out_f, 'w')
# out_fhandle.write('ZIP_CODE\tPopulation\n')
# for i in range(len(tbl)-1):
#     zip_array = tbl['Zip Code'][i].replace(' and ', ', ').replace(',,', ',').split(', ')
#     for z in zip_array:
#         out_fhandle.write('%s\t%s\n' % (z, tbl['Population'][i]))
# out_fhandle.close()

# Now we can read in the processed zip code table
sub_tbl = master_tbl %>% filter(INCIDENT_TYPE_DESC == BFIRE)
zip_pop = read.table('../../Table/zip_pop.tsv', sep = '\t', header = T, stringsAsFactors = F)
zip_counts = sub_tbl %>% count(ZIP_CODE)
# convert strings into numeric type
zip_counts$ZIP_CODE = as.numeric(zip_counts$ZIP_CODE)
# do a left_join() to merge the metadata table
# some zip codes are missing in the table imported from website - remove these rows
zip_counts = zip_counts %>% left_join(zip_pop, by = 'ZIP_CODE') %>% drop_na(Population)
# head(zip_counts)
# qplot(zip_counts$Population, zip_counts$n)
cor(zip_counts$n, zip_counts$Population)

# Q7 - CO detector and fire duration, sig. testing
# unique(master_tbl$CO_DETECTOR_PRESENT_DESC)
with_CO = master_tbl %>% filter(CO_DETECTOR_PRESENT_DESC == 'Yes')
without_CO = master_tbl %>% filter(CO_DETECTOR_PRESENT_DESC == 'No')
# construct contingency table
threshold = 3600
tbl = c(c(
        sum(without_CO_detect$TOTAL_INCIDENT_DURATION > threshold),
        sum(with_CO_detect$TOTAL_INCIDENT_DURATION > threshold)), 
    c(nrow(without_CO_detect), nrow(with_CO_detect)))
# tbl
chisq.test(tbl)
# X-squared = 51799, df = 3, p-value < 2.2e-16