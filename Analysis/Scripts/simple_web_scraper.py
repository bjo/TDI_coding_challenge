# python snippet to import data table for part 2
# from https://www.newyork-demographics.com/zip_codes_by_population
# lxml was installed along with python for nice parsing of the table
import urllib.request as urllib2
import pandas as pd
link = 'https://www.newyork-demographics.com/zip_codes_by_population'
response = urllib2.urlopen(link)
contents = response.read()
tbl = pd.read_html(contents)[0]
# However, some zip code entries need to be parsed manually
# Since this table is small (~1600 rows) and main processing is being done in another script,
# I'm going to write the rows manually
out_file = '../../Table/zip_pop.tsv'
out_fhandle = open(out_f, 'w')
out_fhandle.write('ZIP_CODE\tPopulation\n')
for i in range(len(tbl)-1):
    zip_array = tbl['Zip Code'][i].replace(' and ', ', ').replace(',,', ',').split(', ')
    for z in zip_array:
        out_fhandle.write('%s\t%s\n' % (z, tbl['Population'][i]))
out_fhandle.close()