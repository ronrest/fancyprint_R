# fancyprint_R
Convenience packages for pretty formatting of ascii console output

## Installing 
```r
library(devtools)
install_github("ronrest/fancyprint_R/fancyprint")
```

##Loading Package
```r
library("fancyprint")
```

##Using Package Functions
```r
name = "Joe Blogs"
printkv("name", name)
```
```
"name: Joe Blogs""
```

```r
mu = 36.32348572837228473
sd = 2.953421344345134454
printkv("Mean", mu, fill=25, fill_char=".", round=2)
printkv("Standard Deviation", sd, fill=25, fill_char=".", round=2)
```
```
"Mean.....................: 36.32"
"Standard Deviation.......: 2.95"
```
