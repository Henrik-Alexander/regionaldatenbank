# Regionaldatenbank
This repository contains the code to interact with the German Regionaldatenbank upheld by the German Federal Statistical Office and the State statistical offices. The code draws on the package `Wiesbaden`, which streamlines the interaction in R.

# Objective
This repository loads population data on population age-sex structures, migration, births and deaths at the regional level (dt. Kreise und Kreisfreie Städte). 

# Preparations
In order to reproduce the results, the user may register to the [Regionaldatenbank](https://www.regionalstatistik.de/genesis/online?Menu=Registrierung#abreadcrumb). Upon registration, the user receives an e-mail with information on the username and the password. Please store these information in an R-file called `Account_information.R` in the root-directory. The password may be assigned to the object `password` the `username` may be assigned to the object username. See the code below for an Illustration:


```
username <- "INSERT_USERNAME"
password <- "INSERT_PASSWORD"
```