# incR 2.1.1
* Package anchors in links updated

# incR 2.1.0

# incR 2.0.1.9000
* maptools functionality replaced by suncalc

# incR 2.0.0.9000
* New function added, incRscan_v2. This new function applies a fixed temperature threshold to assign on/off-bouts. incRscan_v2 may handle differences in data quality among days worse than the original incRscan function but it recovers on/off-bouts information for every day in the original data set (unlike incRscan which starts the analysis in the first day with complete data).
* incRenv function re-coded to process data much faster
* Small bug in incRscan fixed

# incR 1.1.5.9000
* incRscan code amended to handle missing data better
* incRatt code amended to confirm adequate date format

# incR 1.1.2.9000
* incRprep code amended to include sampling intervals < 1min
* Bug incRt fixed
* Bug in incRprep example sorted

# incR 1.1.1.9000
* incRscan code amended to remove useless warning message
* incRt code amended to remove useless warning message

# incR 1.1.0 (.9000)
* New function for visualisation of incRscan results: incRplot
* Updated vignette
* temp.diff in incRscan deprecated. New argument name is temp.diff.threshold
* Typo in DESCRIPTION corrected

# incR 1.0.2
* Description of the package available on bioRxiv
* Updated package vignette
* Updated documentation for incRt and incRbouts

# incR 1.0.1
* bug in incRt fixed
* major version release to CRAN

# incR 1.0.0.9000
* incRconstancy changed by incRatt
* incRactivity changed by incRact

# incR 0.3.1.9000
* Version for development available on GitHub

# incR 0.3.1
* 20/07/2016 v0.3.1 available on CRAN

* Most of incRbouts has been coded again using a different approach to increase speed
* New functionality included in incRbouts: per day and per bout outputs
* Example data re-arranged to increase speed when running examples
* New vignette describing a suggested work flow

# incR 0.2.1.9000
* Checked on Travis CI



