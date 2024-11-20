
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nurserysurveyrap

<!-- badges: start -->
<!-- badges: end -->

**Under development**

The `nurserysurveyrap` package will produce the report and accompanying
spreadsheet ready for publication. It processes the input data and
prepares for presentation in publication format.



## Installation

You can install the development version of forestrystats from
[GitHub](https://github.com/forestresearch/nurserysurveyrap) with:

``` r
# install.packages("devtools")
devtools::install_github("forestresearch/nurserysurveyrap")
```
Some users may not be able to use the `devtools::install_github()`
function as a result of their network security settings. If this is the
case, `nurserysurveyrap` can be installed by downloading a zip of the
repository and installing the package locally via
`devtools::install_local(<file path>)`.

## Development

To contribute to development, use the following steps:

1.  Set up a new branch with a name relating to the feature you will be
    developing e.g. “dev_trade_plots”.

2.  Commit and push your changes back to your branch, and ensure that
    functions have roxygen2 documentation
    (<https://roxygen2.r-lib.org/>).

3.  Once you are happy that your contributions are ready to be merged
    into the main branch, use devtools::document() and devtools::check()
    (unit tests are under development, currently very low coverage) to
    update documentation and check the package.

4.  If you have no issues from step 3, open Git Bash and use:
    `git checkout main && git merge your_branch_name`.

Only merge with main branch when package successfully installs and is
working as intended with no errors.

## Licence

This information is licenced under the conditions of the [Open
Government
Licence](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3).

The following attribution statement MUST be cited in your products and
applications when using this information.

> Contains public sector information licensed under the Open Government
> licence v3.

### About the licence

The Open Government Licence (OGL) was developed by the Controller of His
Majesty’s Stationery Office (HMSO) to enable information providers in
the public sector to licence the use and re-use of their information
under a common open licence.

It is designed to encourage use and re-use of information freely and
flexibly, with only a few conditions.
