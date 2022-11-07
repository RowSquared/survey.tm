
# r2.tms

<!-- badges: start -->
<!-- badges: end -->

Collection of wrappers for Translation Management for surveys/censuses.
TBC.

## Installation

You can install the development version of r2.tms through

``` r
devtools::install_github("petbrueck/r2.tms")
```

## Usage

### Pre-requisites

- Google Account to use [Google
  Sheets](https://www.google.com/sheets/about/)

### Set up

Before working with one or more questionnaires, you need to get a google
worksheet that will serve as the “Translation Master Sheet”.

Please note: If you have not used any functionality of package
`googlesheets4` before, at first run of any `r2.tms` function, you will
need to generate an access token. Follow instructions in console and
browser.

``` r
#Optional: Indicate to `googlesheets4` which mail you will use. Avoids selecting pre-authorised account manually. 
googlesheets4::gs4_auth(email = "your.email@address.com")
#Set up sheet, specify: 
# - Name to assign to new google worksheet
# - Translations/Languages needed. Can be one or multiple as in this example. Needs to be ISO 639-1 name.
setup_tsheet(
    name.ssheet="Project X: Translation Sheet",
    translation.languages=c("German","French")
)
```

### Workflow
