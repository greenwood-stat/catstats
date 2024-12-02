# catstats

This is an R package used in our [Introduction to Statistics](https://math.montana.edu/courses/s216/) (STAT 216) and [Intermediate Statistics with Introduction to Statistical Computing](https://math.montana.edu/courses/s217/) (STAT 337) courses at Montana State University.  Currently, it is only available on GitHub.  

This package contains interactive vignettes, code, and data sets to support STAT 216 and STAT 337 at Montana State University and is publicly available for others to use.

Our set of simulation-based inference (SBI) R functions mimic the output of online applets such as those found at [www.rossmanchance.com/applets/](https://www.rossmanchance.com/applets/), performing permutation/randomization tests and percentile bootstrapped confidence intervals:

| **Parameter** 	| **Simulation-based Test** 	| **Bootstrap CI** 	|
|-	|-	|-	|
| Single proportion 	| `one_proportion_test` 	| `one_proportion_bootstrap_CI` 	|
| Difference in two proportions 	| `two_proportion_test` 	| `two_proportion_bootstrap_CI` 	|
| Single mean 	| `one_mean_test` 	| `one_mean_bootstrap_CI` 	|
| Paired mean difference 	| `paired_test` 	| `paired_bootstrap_CI` 	|
| Difference in two means 	| `two_mean_test` 	| `two_mean_bootstrap_CI` 	|
| Regression slope or correlation 	| `regression_test`  	| `regression_boostrap_CI` 	|


## Installation 
You must install the `remotes` package first.

```
install.packages("remotes")
```

Then, use `install_github` to install this package.

```
remotes::install_github("greenwood-stat/catstats")
```

## Open Source Textbooks

STAT 216 and STAT 337 both use open source textbooks and resources hosted on Github:

* [_Montana State Introductory Statistics with R_](https://mtstateintrostats.github.io/IntroStatTextbook/) by Stacey Hancock, Nicole Carnegie, Elijah Meyer, Jade Schmidt, and Melinda Yager ([Github repository](https://github.com/MTstateIntroStats/IntroStatTextbook))
* [_Intermediate Statistics with R_](https://greenwood-stat.github.io/GreenwoodBookHTML/) by Mark Greenwood ([Github repository](https://github.com/greenwood-stat/GreenwoodBookHTML))
