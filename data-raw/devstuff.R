library(devtools)
library(usethis)
library(desc)

# Remove default DESC
unlink("DESCRIPTION")
# Create and clean desc
my_desc <- description$new("!new")

# Set your package name
my_desc$set("Package", "nurserysurveyrap")

#Set your name
my_desc$set("Authors@R", "person('Dan', 'Braby', email = 'daniel.braby@forestresearch.gov.uk', role = c('cre', 'aut'))")

# Remove some author fields
my_desc$del("Maintainer")

# Set the version
my_desc$set_version("0.0.0.9000")

# The title of your package
my_desc$set(Title = "nurserysurveyrap")
# The description of your package
my_desc$set(Description = "Read, analyse and report for our Nursery Survey")
# The urls
my_desc$set("URL", "http://github.com/forestresearch/nurserysurveyrap.com")
my_desc$set("BugReports", "http://github.com/forestresearch/nurseysurveyrap/issues.com")
# Save everyting
my_desc$write(file = "DESCRIPTION")


# Get the dependencies
use_package("tidyr")
use_package("readr")
use_package("magrittr")
use_package("purrr")
use_package("readxl")
use_package("lubridate")


# Clean your description
use_tidy_description()
