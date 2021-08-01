library(haven)
library(tidyverse)

# first notebook - 
survey_repo = list()
for (file in survey_files) {
    year = substr(file, 5, 8)
    tbl = read_xpt(paste0('../../Data/BRFSS_dataset/', file))
    print(dim(tbl))
    survey_repo[[year]] = tbl
}

# description for each of these variables are detailed in the notebook
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

# # some basic stats on each column - we have 3 range variables and 5 binary variables
# length(unique(total_tbl$mmsa)) # data available from 265 MSA's
# unique(total_tbl$age) # range (1 to 14)
# unique(total_tbl$income) # range (1 to 5)
# unique(total_tbl$exercise) # binary (1 or 2)
# unique(total_tbl$flushot) # binary (1 or 2)
# unique(total_tbl$binging) # binary (1 or 2)
# unique(total_tbl$heavy_drink) # binary (1 or 2)
# unique(total_tbl$smoker) # range (1 to 4)
# unique(total_tbl$smoking_now) # binary (1 or 2)
# unique(total_tbl$year) # years 2003 to 2019, but some gaps in between, including 4 years between 2008 and 2013

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