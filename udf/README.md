# UDF

User Defined Function are function created to process the data for Nimbus. These inclucde:

* `getData()` for pulling the data from Amazon Redshift.
  + requires connection details to specific Redshift environment (not provided).
  + requires appropriate [JDBC driver](http://docs.aws.amazon.com/redshift/latest/mgmt/configure-jdbc-connection.html).
* `processData()` for initial processing of data to a route, run, stop-to-stop summary.
  + requires bus sequence csv file (not provided) and data from `getData()` pull.
  + this function could be improved by using `data.table` instead of `dplyr` to decrease run time.
