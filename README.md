
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nurserysurveyrap

<!-- badges: start -->
<!-- badges: end -->

**Stable version**

The `nurserysurveyrap` package produces the report and accompanying
spreadsheet ready for Quality Assurance. Like our other RAPs for Official Statistics it will still require manual updating of headers, a content page and outputting to PDF. It processes the survey returns for the latest year, appends these to the back series, corrects for disaggregation errors which can occur when nurseries report results for Scotland which are implicitly inaccurate due to their return for Great Britain, formats tables and charts, produces publication ready supplementary tables and a Word Document of the report.



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

## Design

The design architecture is set out in the [[system map]].

Like our other RAP products the entire pipeline can be triggered and all outputs created from a single wrapper function `nurserysurveyrap::output_nursery()`.

## Desk Instruction

Every planting year (runs October to September) we run a survey of sales of improved nursery stock by nurseries in Great Britain. The data cover sales of Sitka spruce and Scots pine to public and private sector clients in Scotland. Sales of these species in Great Britain are also reported.

This is a voluntary survey of nursery businesses of Great Britain, run on behalf of Scottish Forestry, to monitor the level of improved nursery stock. 

This publication is an Official Statistics release requiring full compliance with the Code of Practice for Statistics. As part of this we pre-announce the publication a year in advance (usually 2nd/3rd Thursday in October), conduct a formal Quality Assurance process and pre-release statistics at 09:00 on the Wednesday.

### Data requests

We request data for the previous planting year (i.e. in October 2024 we published new data for the 2022/23 planting year for the first time).

#### Tasks to begin early July

- Review the distribution list for the previous run. Consult with lead statistician to ensure all nurseries are still in operation and sell Sitka spruce and/or Scots pine.

- Action Administrative Officer to update master email draft and questionnaire for next run. 

- Once distribution list is confirmed, action Administrative Officer to produce questionnaires and emails. Aim to send these out on the 2nd Monday of July with a deadline of the next again Friday.

- Once you have full returns, the RAP is ready to run. The only function required is `nurserysurveyrap::output_nursery()` which triggers the processes needed to produce the full outputs. This requires several parameters to be specified.
  - `dir_path` = this is the path to the folders containing the returns as `.xlsx` workbooks
  - `hist_data` = this is the backseries as a `.rds` file created by the final run of the program for the previous years' publication
  - `nursery_names` = this is a workbook stored in the Nursery Survey Data folder in the file share, it is a list of the corrected Nursery Names to handle discrepancies in survey returns year on year.
  - `out_path` = this is the folder R will output the report, tables and backseries to
  - `out_name_doc` = this is the name for the output word document, this could be fixed to "nursery-survey", however you may wish to do a preliminary version and this can be helpful for flagging these
  - `pub_date` = this is the date of the publication, for our 2024 publication this was "2024-11-17"
  - `next_update` = this is the set date for publication in the next year, for our 2024 publication this was "2025-10-16"
  - `stat_name` = this is the name of the Responsible Statistician
  - `ref_year` = this is the year of publication. Could set this to default to `lubridate::year(sys.Date())`

To reproduce the 2024 publication run the following:


```r
nurserysurveyrap::output_nursery(dir_path = r"(Z:\IFOS\Statistics\Data\Nursery Survey\2022-23\4_Surveys returned)",
                                 hist_data = r"(Z:\IFOS\Statistics\Data\Nursery Survey\braby_dev\nursery\nursery_survey-2024-10-07.rds)",
                                 nursery_names = r"(Z:\IFOS\Statistics\Data\Nursery Survey\braby_dev\nursery\names.csv)",
                                 ref_year = 2024,
                                 out_path = r"(Z:\IFOS\Statistics\Data\Nursery Survey)",
                                 out_name_doc = "nursery-survey",
                                 pub_date = "2024-10-17",
                                 next_update = "2025-11-16,
stat_name = "Daniel Braby")
```


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
