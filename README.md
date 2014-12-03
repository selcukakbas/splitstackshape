# splitstackshape

R functions to split concatenated data, stack columns of your datasets, and convert your data into different shapes.

## Core Functions

* `cSplit`: A core function that collects the functionality of several of the  `concat.split` family of functions.
* `cSplit_f`: A fast way to split columns of data where you know each row would result in the same number of values after being split.
* `concat.split`: A set of functions to split strings where data have been concatenated into a single value, as is common when getting data collected with tools like Google Forms. (`cSplit_l` to return a `list`; and `cSplit_e` to return an "expanded" view of the input data.)
* `Stacked`: A function to create a list of `stack`ed sets of variables. Similar to `melt` from "reshape2", but doesn't put everything into one very long `data.frame`.
* `Reshape`: A function to allow base R's `reshape` function to work with "unbalanced" datasets.
* `stratified`: A function to take random row samples by groups.

## Utilities

* `getanID`: A function for creating a secondary ID when duplicated "id" variables are present.
* `expandRows`: "Expands" the rows of a dataset.
* `listCol_l` and `listCol_w`: Unlists (long) or flattens (wide) a column in a `data.frame` or a `data.table` stored as a `list`.

## Install

The package [is on CRAN](http://cran.r-project.org/web/packages/splitstackshape/index.html). You can install it using:

```r
install.packages("splitstackshape")
```

To install the development version, use:

```r
library(devtools)
install_github("mrdwab/splitstackshape", ref = "devel")
```

To install the version that uses "stringi" (>= version 0.3.1), use:

```r
library(devtools)
install_github("mrdwab/splitstackshape", ref = "stringi")
```

Current devel version: 1.4.3
Current stringi version: 1.5.2
