library(haven)
library(tidyverse)

# first notebook - https://github.com/bjo/TDI_coding_challenge/blob/master/Analysis/Notebooks/TDI_capstone_BRFSS_CSPP_preprocessing.ipynb
# 1. Clean up BRFSS datasets and select exploratory variables
survey_repo = list()
for (file in survey_files) {
    year = substr(file, 5, 8)
    tbl = read_xpt(paste0('../../Data/BRFSS_dataset/', file))
    print(dim(tbl))
    survey_repo[[year]] = tbl
}

match_cols = c('MMSANAM', '_AGEG5YR', '_INCOMG', '_TOTINDA', '_FL', '_RFBING', '_RFDRHV', '_SMOKER', '_RFSMOK')
survey_clean = list()
for (file in survey_files) {
    year = substr(file, 5, 8)
    matched_cols = sapply(match_cols, function(x) {which(grepl(x , names(survey_repo[[year]])))})
    cleaned_table = cbind(survey_repo[[year]][, c('_MMSA')], survey_repo[[year]][, matched_cols])
    print(dim(cleaned_table))
    colnames(cleaned_table) = c('mmsa', 'mmsa_name', 'age', 'income', 'exercise', 'flushot',
        'binging', 'heavy_drink', 'smoker', 'smoking_now')
    cleaned_table$year = as.numeric(year)
    survey_clean[[year]] = cleaned_table
    # need to write .tsv, since MMSA names have commas in them
    write.table(cleaned_table, file = paste0('../../Table/BRFSS/cleaned_10cols_', year, '.tsv'),
        row.names = F, quote = F, sep = '\t')
}

# let's save some memory:
rm(survey_repo)
total_tbl = do.call('rbind', survey_clean)
rm(survey_clean)

# 2. Matching MSAs to state, and combining another dataset with MSA available (HPI data)
# Let's extract the state variable (two-letter) from each row:
extract_state = function(name) {
    return(
        strsplit(
            strsplit(
                strsplit(name, ', ')[[1]][2],
            ' ')[[1]][1],
        '-')[[1]][1]
    )
}
total_tbl$state = sapply(total_tbl$mmsa_name, function(x) {extract_state(x)})

# Meanwhile, we can also attach the Housing Price Index (HPI) values to each entry
hpi_tbl = read.csv('../../Data/HPI_master.csv', stringsAsFactors = F)
hpi_sub_tbl = hpi_tbl %>% filter(level == 'MSA') %>% distinct(place_id, yr, .keep_all = T)

# Let's do a left_join() with index_nsa:
hpi_join_tbl = hpi_sub_tbl %>% select(c('place_id', 'yr', 'index_nsa')) %>% rename(
    mmsa = place_id, year = yr, hpi = index_nsa)
hpi_join_tbl$mmsa = as.numeric(hpi_join_tbl$mmsa)
head(hpi_join_tbl)
total_tbl_hpi = total_tbl %>% left_join(hpi_join_tbl, by = c('mmsa', 'year'))

write.table((total_tbl_hpi %>% select(-c('mmsa_name'))), 
    file = '../../Table/BRFSS/BRFSS_cleaned_master.tsv', quote = F, row.names = F, sep = '\t')

# I'm also going to save the MSA names into a separate file, since it's a bit clunky:
mmsa_tbl = total_tbl_hpi %>% distinct(mmsa, .keep_all = T) %>% 
    select(c('mmsa', 'state', 'mmsa_name')) %>% arrange(state, mmsa_name)
write.table(mmsa_tbl, file = '../../Table/US_MMSA.tsv', quote = F, row.names = F, sep = '\t')

# 3. Clean up MSU state policy dataset and select exploratory variables
# main file
msu_tbl = read.csv('../../Data/MSU_CSPP/cspp_june_2021.csv', header = T, stringsAsFactors = F)
# metadata file
codebook = read.csv('../../Data/MSU_CSPP/codebook.csv', header = T, stringsAsFactors = F) %>% drop_na(category)

# This is a pretty extensive dataset with > 2000 correlates. Let's narrow it down a bit
# First, we're only doing correlation, so all years before 2003 are not used:
cleaned_tbl = msu_tbl %>% filter(year >= 2003) %>% distinct(year, st, .keep_all = T)

# Let's have some focus groups - first that comes to mind is taxation:
# Let's narrow it down further to a few categories
categories = c('environment', 'healthcare', 'drug-alcohol', 'misc. regulation')
codebook_subset = codebook %>% filter(category %in% categories)
tax_columns = codebook_subset[which(sapply(codebook_subset$long_desc, function(x) {grepl('tax', x)})), 'variable']

# We can also look at % of income spending:
spend_columns = codebook[which(sapply(codebook$long_desc, function(x) {grepl('% of income', x)})), 'variable']

# Let's save these two files:
cleaned_tbl_subset = cleaned_tbl[, c('year', 'st', 'state', tax_columns)]
# remove columns that are all NA
cleaned_tbl_subset = cleaned_tbl_subset[, 
    sapply(names(cleaned_tbl_subset), function(x) {sum(is.na(cleaned_tbl_subset[, x])) < nrow(cleaned_tbl_subset)})]
write.table(cleaned_tbl_subset, 
    file = '../../Table/state_policy_tax.tsv', quote = F, row.names = F, sep = '\t')

cleaned_tbl_subset = cleaned_tbl[, c('year', 'st', 'state', spend_columns)]
# remove columns that are all NA
cleaned_tbl_subset = cleaned_tbl_subset[, 
    sapply(names(cleaned_tbl_subset), function(x) {sum(is.na(cleaned_tbl_subset[, x])) < nrow(cleaned_tbl_subset)})]
write.table(cleaned_tbl_subset, 
    file = '../../Table/state_policy_spending.tsv', quote = F, row.names = F, sep = '\t')

# -----------------------------------------------------------------------

# second notebook - https://github.com/bjo/TDI_coding_challenge/blob/master/Analysis/Notebooks/TDI_capstone_BRFSS_CSPP_part1.ipynb
# Now that we've pre-processed the data, let's go into the exploratory data analysis portion
library(tidyverse)
library(ggplot2)

brfss_table = read.table('../../Table/BRFSS/BRFSS_cleaned_master.tsv', 
    stringsAsFactors = F, sep = '\t', header = T)

## Question: does income play a role in behavioral risk factors?
# 1 - exercise
tbl = brfss_table %>% filter(exercise < 9, income < 9)
summary(lm(exercise ~ income, data = tbl))
# 2- flushot
tbl = brfss_table %>% filter(flushot < 9, income < 9)
summary(lm(flushot ~ income, data = tbl))
# 3 - binging
tbl = brfss_table %>% filter(binging < 9, income < 9)
summary(lm(binging ~ income, data = tbl))
# 4 - heavy_drink
tbl = brfss_table %>% filter(heavy_drink < 9, income < 9)
summary(lm(heavy_drink ~ income, data = tbl))
# 5 - smoker
tbl = brfss_table %>% filter(smoker < 9, income < 9)
summary(lm(smoker ~ income, data = tbl))
# 6 - smoking_now
tbl = brfss_table %>% filter(smoking_now < 9, income < 9)
summary(lm(smoking_now ~ income, data = tbl))

# Example visualization: income and smoking
# options(repr.plot.width=12, repr.plot.height=6)
# plot total number of reports by category
plot_df = brfss_table %>% filter(smoker < 9, income < 9)
ggplot(plot_df, aes(x = factor(income), fill = factor(smoker))) + geom_bar(position = 'dodge')

# also plot proportions for each income bracket:
plot_list = list()
counts_df = plot_df %>% count(income)
for (i in c(1:4)) {
    plot_list[[i]] = data.frame(income = c(1:5), smoker = i,
        proportion = sapply(c(1:5), function(x) {
        	nrow(plot_df %>% filter(income == x, smoker == i))
        }) / counts_df$n
    )
}
proportion_plot_df = do.call('rbind', plot_list)
ggplot(proportion_plot_df, aes(x = factor(income), y = proportion, fill = factor(smoker))) + 
	geom_bar(position = 'dodge', stat = 'identity')

## Question: are Americans engaging in more healthy behaviors over time?

# 1 - exercise
tbl = brfss_table %>% filter(exercise < 9)
summary(lm(exercise ~ year, data = tbl))
# 2- flushot
tbl = brfss_table %>% filter(flushot < 9)
summary(lm(flushot ~ year, data = tbl))
# 3 - binging
tbl = brfss_table %>% filter(binging < 9)
summary(lm(binging ~ year, data = tbl))
# 4 - heavy_drink
tbl = brfss_table %>% filter(heavy_drink < 9)
summary(lm(heavy_drink ~ year, data = tbl))
# 5 - smoker
tbl = brfss_table %>% filter(smoker < 9)
summary(lm(smoker ~ year, data = tbl))
# 6 - smoking_now
tbl = brfss_table %>% filter(smoking_now < 9)
summary(lm(smoking_now ~ year, data = tbl))

# Let's look at exercise first
plot_df = brfss_table %>% filter(exercise < 9)
ggplot(plot_df, aes(x = year, fill = factor(exercise))) + geom_bar(position = 'dodge')
plot_list = list()
counts_df = plot_df %>% count(year)
for (i in c(1:2)) {
    plot_list[[i]] = data.frame(year = counts_df$year, exercise = i,
        proportion = sapply(counts_df$year, function(x) {nrow(plot_df %>% filter(year == x, binging == i))}) / counts_df$n
    )
}
proportion_plot_df = do.call('rbind', plot_list)
ggplot(proportion_plot_df, aes(x = year, y = proportion, fill = factor(exercise))) + geom_bar(position = 'dodge', stat = 'identity')

# Let's also look at binging
# AGEG5YR of 1-3 corresponds to ages 18 to 34
plot_df = brfss_table %>% filter(binging < 9, age < 4)
plot_list = list()
counts_df = plot_df %>% count(year)
for (i in c(1:2)) {
    plot_list[[i]] = data.frame(year = counts_df$year, binging = i,
        proportion = sapply(counts_df$year, function(x) {nrow(plot_df %>% filter(year == x, binging == i))}) / counts_df$n
    )
}
proportion_plot_df = do.call('rbind', plot_list)
ggplot(proportion_plot_df, aes(x = year, y = proportion, fill = factor(binging))) + geom_bar(position = 'dodge', stat = 'identity')


## Question: are there certain metro areas more prone to specific behavioral risks?
# Let's filter out MSAs with low counts:
count_df = sub_tbl %>% count(mmsa) %>% arrange(desc(n))
# We're going to require at least 1500 responses
count_df = count_df %>% filter(n >= 1500)

# let's rank these 129 metro areas by proportion reporting vaccinated (over the whole period)
get_proportion = function(city) {
    tbl_top = c(nrow(sub_tbl %>% filter(flushot == 1, mmsa == city)),
               nrow(sub_tbl %>% filter(flushot == 2, mmsa == city)))
    return(tbl_top[1] / sum(tbl_top))
}
count_df$proportion = sapply(count_df$mmsa, function(x) {get_proportion(x)})

# let's also attach MMSA names to IDs:
mmsa_tbl = read.table('../../Table/US_MMSA.tsv', header = T, sep = '\t', stringsAsFactors = F)
count_df = count_df %>% arrange(desc(proportion)) %>% left_join(mmsa_tbl, by = 'mmsa')

# Now we're aggregating at state-level
count_df = sub_tbl %>% count(state)
# We're going to require at least 1500 responses
# let's rank these 129 metro areas by proportion reporting vaccinated (over the whole period)
get_proportion = function(st) {
    tbl_top = c(nrow(sub_tbl %>% filter(flushot == 1, state == st)),
               nrow(sub_tbl %>% filter(flushot == 2, state == st)))
    return(tbl_top[1] / sum(tbl_top))
}
count_df$proportion = sapply(count_df$state, function(x) {get_proportion(x)})

library(maps)
us_states <- map_data("state")
# The HPI_master table has mapping from two-letter state to state name
hpi_table = read.csv('../../Data/HPI_master.csv')
hpi_table = hpi_table %>% filter(level == 'State') %>% distinct(place_id, .keep_all = T)
hpi_table$region = tolower(hpi_table$place_name)
hpi_table$state = hpi_table$place_id
us_states = us_states %>% left_join(hpi_table[, c('region', 'state')], by = 'region')
us_states_vax_data = us_states %>% left_join(count_df, by = 'state')

g = ggplot(data = us_states_vax_data, 
        mapping = aes(x = long, y = lat, group = group, fill = n))
g = g + geom_polygon(color = "gray90", size = 0.1) + 
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_gradient(low = 'blue', high = 'red')

g = ggplot(data = us_states_vax_data, 
        mapping = aes(x = long, y = lat, group = group, fill = proportion))
g = g + geom_polygon(color = "gray90", size = 0.1) + 
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_gradient(low = 'red', high = 'blue')

# We can also plot state bins:
library(statebins)
g = ggplot(data = us_states_vax_data,  aes(state = state, fill = proportion)) + geom_statebins()

# -----------------------------------------------------------------------

# third notebook - https://github.com/bjo/TDI_coding_challenge/blob/master/Analysis/Notebooks/TDI_capstone_BRFSS_CSPP_part2.ipynb
# Are there any state-level policies that seem to correlate with the behavioral risks?

# I've processed two documents: state_policy_spending.tsv and state_policy_tax.tsv
state_spending = read.table('../../Table/state_policy_spending.tsv', stringsAsFactors = F, header = T, sep = '\t')
state_tax = read.table('../../Table/state_policy_tax.tsv', stringsAsFactors = F, header = T, sep = '\t')

## Question: is there a positive correlation between a state's spending on parks and recreation and exercise for people in the state?
# the matched columns in brfss_table is: year, state
# the matched columns in state_spending and state_tax is year, st
test_tbl = brfss_table %>% rename(st = state) %>% 
    left_join(state_spending[, c('year', 'st', 'aparkpi')]) %>%
    drop_na(aparkpi)

# 1 - exercise
tbl = test_tbl %>% filter(exercise < 9)
summary(lm(exercise ~ aparkpi, data = tbl))

options(repr.plot.width=12, repr.plot.height=6)
ggplot(test_tbl %>% filter(exercise < 9), aes(x = aparkpi, color = factor(exercise))) + geom_density()

## Question: can we find a similar relationship between alcohol taxes and prevalence of binging and heavy drinkers?

alcol_tax_names = c('cbeertex', 'cbeerp', 'cbeert', 'cbeertav',
        'cwinetex', 'cwinep', 'cwinet', 'cwinetav',
        'cspirtex', 'cspirp', 'cspirt', 'cspirtav')

test_tbl = brfss_table %>% rename(st = state) %>% 
    left_join(state_tax[, c('year', 'st', alcol_tax_names)])
tbl = test_tbl %>% filter(binging < 9)

# generate formula text for binging
form_texts = sapply(alcol_tax_names, function(x) {paste('binging', '~', x)})
# conduct linear mapping
data_points = sapply(form_texts, function(x) {
    summary(lm(x, data = tbl))$coefficients[2, 3]
})

binge_tbl = data.frame(type = c(rep('beer', 4), rep('wine', 4), rep('spirit', 4)), 
    stat = data_points, tax_type = rep(c('excise', 'net', 'per_g', 'ad_valorem'), 3))

# generate formula text for heavy drinking
form_texts = sapply(alcol_tax_names, function(x) {paste('heavy_drink', '~', x)})
# conduct linear mapping
data_points = sapply(form_texts, function(x) {
    summary(lm(x, data = tbl))$coefficients[2, 3]
})

heavy_tbl_tbl = data.frame(type = c(rep('beer', 4), rep('wine', 4), rep('spirit', 4)), 
    stat = data_points, tax_type = rep(c('excise', 'net', 'per_g', 'ad_valorem'), 3))

ggplot(binge_tbl, aes(x = factor(type), y = -stat, fill = factor(tax_type))) + 
    geom_bar(stat = 'identity', position = 'dodge')

ggplot(heavy_tbl, aes(x = factor(type), y = -stat, fill = factor(tax_type))) + 
    geom_bar(stat = 'identity', position = 'dodge')

## Question: are there any interventions that appear to affect behavioral risk factors?

max_spend = state_spending %>% drop_na(aparkpi) %>% group_by(st) %>% summarize(max(aparkpi))
min_spend = state_spending %>% drop_na(aparkpi) %>% group_by(st) %>% summarize(min(aparkpi))
combined = cbind(max_spend, min_spend)[, c(1, 2, 4)]
combined$diff = combined[,2] - combined[,3]
combined = combined %>% arrange(desc(diff))

ggplot(state_spending %>% drop_na(aparkpi) %>% filter(st == 'AZ'), aes(x = year, y = aparkpi)) + geom_line()
ggplot(state_spending %>% drop_na(aparkpi) %>% filter(st == 'ND'), aes(x = year, y = aparkpi)) + geom_line()

plot_df = brfss_table %>% filter(exercise < 9)
counts_df = plot_df %>% count(year)
counts_df_AZ = plot_df %>% filter(state == 'AZ') %>% count(year)
counts_df_ND = plot_df %>% filter(state == 'ND') %>% count(year)
proportion_plot_df = rbind(
    data.frame(area = 'US', year = counts_df$year,
        proportion = sapply(counts_df$year, function(x) {nrow(plot_df %>% filter(year == x, exercise == 1))}) / counts_df$n),
    data.frame(area = 'AZ', year = counts_df_AZ$year,
        proportion = sapply(counts_df_AZ$year, function(x) {nrow(plot_df %>% filter(year == x, state == 'AZ', exercise == 1))}) / counts_df_AZ$n),
    data.frame(area = 'ND', year = counts_df_ND$year,
        proportion = sapply(counts_df_ND$year, function(x) {nrow(plot_df %>% filter(year == x, state == 'ND', exercise == 1))}) / counts_df_ND$n)
)
ggplot(proportion_plot_df, aes(x = year, y = proportion, color = factor(area))) + geom_line()