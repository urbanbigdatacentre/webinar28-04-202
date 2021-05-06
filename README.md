# UBDC webinar (28 April 2021)
# Exploring and analysing open transport data 
# (with a focus on SCOOT data and bike share data)

This repository contains the materials used in the UBDC webinar on the 28th April 2021: <br>
1. webinar.pdf (presentation)
2. openData.R (R script containing the examples of obtaining data)
3. The html files are the maps created using the data obtained in 2
4. london.R (R script containing the example of downloading, loading and combining London bikeshare trip data with covid restrictions dataset) 

## Examples in openData.R ##
Here we have three examples of traffic count (Brussels, Hull, Glasgow)
### Brussels: more information; no api key
### Hull: more straighforward; no api key
### Glasgow: count, events, parking, vms; require api key; (written in a more cumbersome way but perhaps easier to see how the lists work)

The lines of codes for setting up a database and scheduling the task for regular interval after the Glasgow example. <br>
Two simple examples for bike availability are included (Citibike, Nextbike).<br>

## London.R ##
This is slightly different from the version I used in the webinar. The script includes:
1. generating a list of links for file download (e.g., all csv files ending in '2020.csv'), instead of downloading a zip file
2. reading the list of files that are unzipped
3. converting the date column 
4. reading the covid19 restriction time series
5. combining trip data and covid restriction data

## view the maps here ##
https://chaumanfung.github.io/webinar/bru1.html <br>
https://chaumanfung.github.io/webinar/hull1.html <br>
https://chaumanfung.github.io/webinar/combined.html <br>
