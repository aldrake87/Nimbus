# Nimbus
An internal tool to monitor the London Bus Network in near real-time.

Work done whilst at Transport for London to compliment the Bus Network Disruption Monitor, focussing on data retrieval and munging to deliver to a front-end application (detailing summary stats, map and table of key disruptions).

Functions created to connect and query [Amazon Redshift in an R environment](https://aws.amazon.com/blogs/big-data/connecting-r-with-amazon-redshift/) to retrieve live bus data based on the route, run, registration, last stop passed and any deviations in scheudle. Difficulties include:

* Detecting and handling buses that are stuck/waiting at a stop over several minutes.
* Calculating delay from changes to schedule deviation.
* Weighting data within a time-window to detect current delays that have built up over time (but have not recently cleared).
* Sequencing the bus data to provide detail for each bus run.
* Transforming the data from stop level to a stop-to-stop measure.
* Handling missing values ie where there have not been any returns for particular stops in a sequence.
* Providing summary statistics to determine data validity.

This work mainly uses the R packages for the `tidyverse`, `RJDBC`, `zoo` and `data.table`.

* `tidyverse` for data manipulation such as selecting, grouping and summarising.
* `RJDBC` to connect RStudio to Redshift.
* `zoo` to handle `NA` values (interpolate with `na.approx()`).
* `data.table` to identify where changes have taken place within the data set using `uniquelist()`, eg:
  + stop changes
  + reg/route changes

The `microbenchmark` package is also useful to identify opportunities to increase efficiency in sections of the code.
