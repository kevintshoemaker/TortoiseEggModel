
This repository contains the code and data used in the publication &quot;&#39;Unscrambling&#39; the drivers of egg production in Agassiz&#39;s desert tortoise: climate and individual attributes predict reproductive output&quot; by Mitchell et al.

## Data

All data necessary for running the analyses presented in the manuscript can be found in the &quot;Final data for paper&quot; folder. This folder contains three files:

##### tortoise_egg_data.csv

This CSV contains the tortoise egg-laying data collected from x-rays of female tortoises from 1997-2018. For more details about data collection methodology, see the manuscript. Each row represents an observation on a single tortoise for a single year. The data has the following columns:
- **Site** - the name of the site at which the tortoise was located
- **Tort** - the ID of the tortoise. Note that these ID's are unique *within a single site*, but not necessarily among all sites. For a unique identifier, use the 'TortSite' column.
- **Year** - The year in which the data was collected
- **ReproStatus** - either 0 or 1, indicating whether or not the tortoise reproduced in the given year
- **MCL** - mean carapace length, in millimeters. Note that tortoises were remeasured in each year, so these values may vary slightly across years for the same tortoise.
- **TE** - total eggs; the total number of eggs laid in the given year
- **TortSite** - a unique identifier for each tortoise, formed by concatenating the &#39;Tort&#39; and &#39;Site&#39; columns.

##### prism_data.csv

This CSV contains the monthly climate data for each site from 1995-2018, extracted from 800m PRISM data. Each row provides the climate observations for a single month at a single site. The data has the following columns:
- **site** - the name of the site
- **year** - the year of the observation
- **month** - the month of the observation
- **ppt** - cumulative precipitation for the given month
- **tmax** - average daily maximum temperature for the given month
- **tmin** - average daily minimum temperature for the given month

##### daymet_data.csv

This CSV contains the daily maximum temperature for each site from 1996-2018, retrieved from Daymet, a dataset that contains gridded daily climate estimates. The data has the following columns:
- **Year** - the year of the observation
- **Jdate** - the Julian day of the observation (1-365)
- **tmax** - the maximum temperature for the given day
- **Date** - the date of the observation, in &#39;mm/dd/yy&#39; format. Note that the date is fully described by the &#39;Year&#39; and &#39;Jdate&#39; columns. This column is provided for convenience only.
- **Site** - the name of the site

## Code

Two scripts are included:
- **EDM_FUNCTIONS_1_5.R** - contains a series a functions that are used in for data manipulation and model fitting. This script does not need to be run separately, as &#39;EDM_MAIN_SCRIPT_1_5.R&#39; imports the functions contained in this file.
- **EDM_MAIN_SCRIPT_1_5.R** - makes use of the functions in the previous script to fit and assess the Bayesian hierarchical model in JAGS.

To run the model, ensure that the two R scripts and the three CSVs are located in the same folder and set this folder as the working directory using `setwd()`. Run &#39;EDM_MAIN_SCRIPT_1_5.R&#39; to run the model. All model output will be saved in the working directory.

A working JAGS installation is necessary to run the code. The following packages must also be installed:
- coda, HDInterval, loo, plyr, R2jags, ROCR, runjags, zoo
