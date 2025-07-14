# nurserysurveyrap <img src="inst/img/FRlogo_linear_colour_transparent.png" align="right" alt="Forest Research logo" width="120" />

<!-- badges: start -->
[![R build status](https://github.com/forestresearch/nurserysurveyrap/workflows/R-CMD-check/badge.svg)](https://github.com/forestresearch/nurserysurveyrap/actions)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Overview

The **nurserysurveyrap** package is a Reproducible Analytical Pipeline (RAP) for processing UK nursery survey data on improved stock sales. It generates official statistics on genetically improved Sitka spruce and Scots pine sales, supporting forest policy development and industry monitoring.

<img src="inst/img/accredited_official_statistics_logo_english_hq.png" align="center" alt="Official Statistics logo" width="200" />

### Key Features

🌲 **Species Coverage**: Sitka spruce and Scots pine genetically improved stock  
📊 **Geographic Scope**: Scotland and Great Britain totals  
🔄 **Data Processing**: Automated disaggregation error correction  
📈 **Publication Outputs**: Word documents and accessible Excel workbooks  
✅ **Quality Assurance**: Built-in validation and error checking  

## Survey Background

Every planting year (October to September), Forest Research conducts a voluntary survey of nursery businesses in Great Britain to monitor sales of improved nursery stock. This survey is run on behalf of Scottish Forestry and provides critical data for:

- **Forest Policy**: Informing decisions on genetic improvement programs
- **Industry Monitoring**: Tracking adoption of improved forestry stock
- **Research Planning**: Supporting breeding and improvement research
- **Market Analysis**: Understanding supply chains for quality stock

### Data Collected

| Category | Details |
|----------|---------|
| **Species** | Sitka spruce, Scots pine |
| **Stock Types** | Genetically improved (GI) vs. non-improved |
| **Production Methods** | Seedlings, transplants, cuttings |
| **Geography** | Scotland, England & Wales, Great Britain totals |
| **Time Period** | Annual data by planting year (Oct-Sep) |

## Installation

### Development Version

```r
# Install from GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("forestresearch/nurserysurveyrap")
```

### Dependencies

The package requires several specialized dependencies:

```r
# Core data processing
install.packages(c("dplyr", "tidyr", "readr", "readxl", "afcharts"))

# Forest Research specific packages
devtools::install_github("forestresearch/frpubutils")
devtools::install_github("forestresearch/afcharts")

# Publication formatting
install.packages(c("flextable", "officer", "aftables"))
```

### Network Installation

Some users may not be able to use `devtools::install_github()` due to network security settings. In this case:

```r
# Download zip file from GitHub and install locally
devtools::install_local("path/to/nurserysurveyrap.zip")
```

## Quick Start

### Single Function Pipeline

The entire analysis pipeline can be triggered with one function call:

```r
library(nurserysurveyrap)

# Generate 2024 nursery survey publication
output_nursery(
  dir_path = "Z:/IFOS/Statistics/Data/Nursery Survey/2022-23/4_Surveys returned",
  hist_data = "Z:/IFOS/Statistics/Data/Nursery Survey/nursery_survey-2023-10-15.rds",
  nursery_names = "Z:/IFOS/Statistics/Data/Nursery Survey/names.csv",
  ref_year = 2024,
  out_path = "Z:/IFOS/Statistics/Data/Nursery Survey",
  out_name_doc = "nursery-survey",
  pub_date = "2024-10-17",
  next_update = "2025-11-16",
  stat_name = "Daniel Braby"
)
```

### Step-by-Step Processing

For development or troubleshooting, you can run individual steps:

```r
# 1. Read all survey returns
returns <- read_returns("path/to/survey_returns/")

# 2. Load nursery name reference data
nursery_names <- read_nursery_names("path/to/nursery_names.csv")

# 3. Apply corrections and standardized names
corrected_returns <- fix_returns(returns, nursery_names)

# 4. Quality assurance check
message <- check_returns(corrected_returns)
print(message)

# 5. Load historical data and combine
historical_data <- readr::read_rds("path/to/historical_data.rds")
combined_data <- dplyr::bind_rows(corrected_returns, historical_data)
```

## Data Processing Pipeline

### 1. Data Reading
- **Input**: Excel workbooks (.xlsx) with "return" sheets
- **Process**: Recursive search of specified directory
- **Output**: Combined tibble of all survey returns

### 2. Data Cleaning
- **Disaggregation correction**: Fixes cases where Scotland totals exceed GB totals
- **Name standardization**: Applies consistent nursery names across years
- **Data validation**: Ensures proper data types and ranges

### 3. Quality Assurance
- **Error detection**: Identifies negative volumes (indicates corrections made)
- **Consistency checks**: Validates data integrity across time series
- **Manual review prompts**: Alerts users to cases requiring attention

### 4. Analysis & Reporting
- **Time series construction**: Combines new data with historical backseries
- **Statistical calculations**: Generates volume totals and improvement percentages
- **Publication formatting**: Creates tables meeting accessibility standards

## Disaggregation Error Correction

A key feature of the pipeline is automatic correction of disaggregation errors:

### The Problem
Nurseries sometimes report Scotland figures that don't properly disaggregate from their Great Britain totals, resulting in negative "England & Wales" values.

### The Solution
```r
# Automatic correction algorithm:
# 1. Identify negative E&W non-GI volumes
# 2. Calculate correction amount
# 3. Redistribute between GI and non-GI categories
# 4. Maintain overall volume totals
```

### Quality Check
```r
# After processing, check for corrections
check_returns(processed_data)
# Returns: "A disaggregation error has been corrected..." or "All is well."
```

## Output Structure

### Excel Workbook
- **Cover sheet**: Publication metadata and contact information
- **Contents sheet**: Navigation and table descriptions  
- **Notes sheet**: Methodology and data source information
- **6 Supplementary tables**: 
  - S1: Sales of nursery stock, Scotland
  - S2: Sales of improved nursery stock, Scotland  
  - S3: Percentage improved stock, Scotland
  - S4: Sales of nursery stock, Great Britain
  - S5: Sales of improved nursery stock, Great Britain
  - S6: Percentage improved stock, Great Britain

### Word Document
- **Executive summary**: Key findings and trends
- **Data tables**: Publication-ready formatted tables
- **Charts and figures**: Visual representation of trends
- **Methodology**: Data collection and processing notes
- **Quality assurance**: Validation results and notes

### Data Files
- **Updated RDS file**: Combined time series for next year's baseline
- **File naming**: `nursery_survey-YYYY-MM-DD.rds`

## Publication Schedule

### Annual Timeline

| Period | Activity |
|--------|----------|
| **Early July** | Review distribution list, update questionnaires |
| **Mid July** | Send surveys to nurseries (2nd Monday) |
| **August** | Follow-up and data collection |
| **September** | Data processing and analysis |
| **October** | Publication (typically 2nd/3rd Thursday) |

### Pre-release Process
1. **Quality Assurance**: Comprehensive data validation
2. **Pre-announcement**: One year advance notice
3. **Pre-release access**: 09:00 Wednesday before publication
4. **Official release**: Thursday morning publication

## Planting Year Convention

The survey follows UK forestry planting year conventions:

```r
# Planting years run October to September
planting_year(2024)    # Returns "2024/25"
planting_year(2023)    # Returns "2023/24" 

# Date conversion
winter_date <- as.Date("2024-01-15")
in_planting_year(winter_date)  # 2023/24 planting year
```

## Function Reference

### Main Pipeline Functions
- `output_nursery()` - Complete survey processing pipeline
- `read_returns()` - Read all survey returns from directory
- `fix_returns()` - Apply corrections and standardized names
- `check_returns()` - Quality assurance validation

### Data Processing Functions
- `read_return()` - Read individual survey return
- `read_nursery_names()` - Load standardized nursery names
- `files_matching()` - Find files by pattern in directory

### Utility Functions
- `planting_year()` - Convert year to planting year format
- `in_planting_year()` - Get planting year interval from date
- `to_planting_year()` - Convert year to interval
- `with_pivot()` - Specialized pivot operations

### Publication Functions
- `pub_a11y_prep()` - Prepare accessibility metadata
- `ns_figures()` - Generate publication charts
- `ft_theme_pub()` - Publication table formatting

## Development

### Contributing

To contribute to development:

1. **Create feature branch**: `git checkout -b feature/your-feature-name`
2. **Make changes**: Add code with roxygen2 documentation
3. **Test thoroughly**: Run `devtools::check()` and test with real data
4. **Update documentation**: `devtools::document()`
5. **Submit pull request**: Ensure all checks pass

### Code Standards
- Follow [tidyverse style guide](https://style.tidyverse.org/)
- Use roxygen2 for all function documentation
- Include examples in documentation
- Write comprehensive tests for new functions
- Maintain backward compatibility

### Package Check
```r
# Before submitting changes
devtools::document()  # Update documentation
devtools::check()     # Run R CMD check
devtools::test()      # Run unit tests (when implemented)
```

## Data Security

### File Handling
- **Network drives**: Package designed for Z: drive file shares
- **Sensitive data**: Survey responses handled according to data protection policies
- **File paths**: Use `r"(raw strings)"` for Windows paths with backslashes

### Quality Standards
- **Official Statistics**: Full compliance with Code of Practice for Statistics
- **Reproducibility**: All outputs generated programmatically
- **Audit trail**: Complete documentation of data processing steps
- **Version control**: Git-based development and release management

## Troubleshooting

### Common Issues

| Problem | Solution |
|---------|----------|
| **Network access errors** | Use `install_local()` instead of `install_github()` |
| **Negative volumes in output** | Normal after disaggregation corrections - review flagged cases |
| **Missing survey files** | Check directory path and file permissions |
| **Inconsistent nursery names** | Update names.csv reference file |

### Error Messages
```r
# If disaggregation errors corrected:
"A disaggregation error has been corrected. Run View(returns) and sort by volume to identify which cases were affected."

# If data validation passes:
"All is well."
```

### Support
- **Issues**: [GitHub Issues](https://github.com/forestresearch/nurserysurveyrap/issues)
- **Email**: daniel.braby@forestresearch.gov.uk
- **Documentation**: `?function_name` for detailed help

## License

This information is licensed under the conditions of the [Open Government Licence](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3).

### Attribution Statement

> Contains public sector information licensed under the Open Government licence v3.

### About the Licence

The Open Government Licence (OGL) was developed by the Controller of His Majesty's Stationery Office (HMSO) to enable information providers in the public sector to licence the use and re-use of their information under a common open licence. It is designed to encourage use and re-use of information freely and flexibly, with only a few conditions.

---

**Forest Research** | Alice Holt Lodge | Farnham | Surrey | GU10 4LH | UK

**Crown Copyright** © 2024 Forest Research
