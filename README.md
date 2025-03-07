# These are my personal Packages from R

## How to Install this package.

In your R console, please add the following line:

```
devtools::install_github("adrianArimany/mypackageR")
```

then in your R script add the following:

```
library(mypackageR)
```

If it gives an error, the issue is likely because you don't have devtools, install, in such case:

```
install.packages("devtools")
library(devtools)
```



### my Packages

Goto:

````
cd R
````

Here you can find the following files:

* functions_anova.R
  These are the packages about doing ANOVA tests
* functions_general.R
  These are the packages about doing general work in R
* functions_graph.R
  These are the packages about doing graphs in R
* functions_linearRegression.R
  These are the packages about doing linear regression in R


### How to update (self-note):

In r console:

```
devtools::document()
```

In description change the version

Version: X.Y.Z

Where:

X : Major update

Y : Medium update

Z : Small updates


In terminal:

```
git add .
git commit -m "Updated package: added new function X, fixed bug Y"
git push origin main
```
In r console:
```
devtools::install_github("adrianArimany/mypackageR", force = TRUE)
```

To verify version:
In r console
```
library(mypackageR)
packageVersion("mypackageR")
```
