
# survey.tm

<!-- badges: start -->
<!-- badges: end -->

Collection of wrappers for Translation Management for surveys/censuses.
TBC.

## Installation

You can install the development version of survey.tm through

``` r
devtools::install_github("petbrueck/survey.tm")
```

## Pre-requisites

- Google Account to use [Google
  Sheets](https://www.google.com/sheets/about/)

## Usage

### Set up

Before working with one or more questionnaires, you need to run once the
`setup_tdb()` function to get a google worksheet that will serve as the
“Translation Database Sheet”.

Please note: If you have not used any functionality of package
`googlesheets4` before, at first run of any `survey.tm` function, you
will need to generate an access token. Follow instructions in console
and browser.

``` r
# Optional: Indicate to `googlesheets4` which mail you will use. Avoids selecting pre-authorised account manually.
# googlesheets4::gs4_auth(email = "your.email@address.com")

setup_tdb(
  ssheet_name = "Project X: Translation Sheet", # Name to assign to new google worksheet
  lang_names = c("German", "French") # Translations/Languages needed. Needs to be ISO 639-1 language name.
)
```

### Workflow

#### A.1) Pull the Questionnaire Template

<details>
<summary>
Click to see detailed description
</summary>

To retrieve the current version and text items of the questionnaire(s)
you’re working with, you can use the `get_suso_tfiles()` function. This
function downloads the [questionnaire
templates](https://docs.mysurvey.solutions/questionnaire-designer/toolbar/multilingual-questionnaires/)
in Excel format from the Survey Solutions Designer and stores them as
nested lists in your environment. You can specify the questionnaires by
using their questionnaire ID, which is a 32-character alphanumeric
identifier.

You can find this ID by logging in to the Survey Solutions Designer and
accessing your questionnaire. The URL in your browser should look like
`https://designer.mysurvey.solutions/questionnaire/details/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx`,
where the combination of x’s after `/details/` represents your
questionnaire ID.

</details>

``` r
#One needs to pull the Questionnaire Template(s) ('Translation File') from the Survey Solutions Designer

# Define the questionnaire IDs you want to retrieve translations for
questionnaires <- c("a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6", "b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6a7")

# Retrieve the 'Source Questionnaire' template files
suso_trans_templates <- get_suso_tfiles(
  questionnaires = questionnaires,
  user = "your_email@example.com", # Registered with Survey Solutions Designer
  password = "your_password",
  sheets = c("Translations", "@@_myreusable_category")
)

# Access the 'Source Questionnaire' of the first questionnaire template
translations_first_questionnaire <- suso_trans_templates[["NAME OF YOUR QUESTIONNAIRE"]]
# Access the "Translations" sheet for the first questionnaire
translations_sheet_first_questionnaire <- translations_first_questionnaire[["Translations"]]
# Note: The last four rows are an example of how to access the returned object "suso_trans_templates". In most cases, you will not need to access the object in this way.
```

##### A.2) Parse Questionnaire Templates

<details>
<summary>
Click to see detailed description
</summary>
Text items to be translated are currently distributed across
questionnaires and sheets in the object returned by `get_suso_tfiles()`.
To aggregate all these items into a single consolidated data.table, use
the `parse_suso_titems()` function.
</details>

``` r
#Aggregate and consolidate text items from multiple questionnaires and sheets into a single data.table using parse_suso_titems().
source_titems <- parse_suso_titems(
  tmpl_list = suso_trans_templates, #Nested list as returned by `get_suso_tfiles()`
  collapse = TRUE # Keep only unique text items across questionnaires
)
```

#### B) Pull ‘Translation Database’ Google Sheet data

<details>
<summary>
Click to see detailed description
</summary>

In order to add translations to the ‘Source Questionnaires’, it’s
necessary to pull any existing translations (if available) specified by
the Translator from the ‘Translation Database’ Google Sheet.

`get_tdb_data()` is a simple wrapper for `googlesheets4::read_sheet()`
and returns a list where each element is a sheet in the Google Sheet,
representing translation data for a specific language.

At first run after `setup_tdb()`, all elements of the list returned will
be empty data.table’s.
</details>

``` r
# googlesheets4::gs4_auth(email = "your.email@address.com")

#Pull all columns and rows found in all sheets in 'Translation Database' Google Sheet
tdb_data <- get_tdb_data(ss = "GOOGLE-SHEET-IDENTIFIER")
```

#### C) Update ‘Translation Database’ object

<details>
<summary>
Click to see detailed description
</summary>

Compares ‘Source Questionnaire’ text items (object returned by
`parse_odk_titems()` or `parse_suso_titems()`) against the ‘Translation
Database’ list returned by `get_tdb_data()`.

Removes any text item in any sheet in the translation database object
that no longer is part of the source questionnaire(s). Adds any new text
item from source questionnaire(s) not yet found in the database.

List returned will be used for subsequent processes, including creating
‘Translated Questionnaires’ for uplöad to CAPI system as well as pushed
to the ‘Translation Database’

</details>

``` r
#Update the 'Translation Database' object based on the text items in current version of 'Source Questionnaire(s)'
new_tdb <- update_tdb(
  tdb = tdb_data,
  source_titems = source_titems
)
```

## TODO

### General / High-Level

- [ ] Cleanup of function names & argument naming convention to make
  clear between translation and questionnaire
- [ ] Documentation (Functions + Google Translation Sheet)
- [ ] Unit Tests
- [ ] ODK Stream
- [ ] pkgdown
- [ ] `remove_coding()` as part of parse_xx_titems()?
- [ ] TODOs in functions (:
- [ ] devtools::check() tests dont pass locally while devtools::tests()
  does

### Software Text Mismatch

- [ ] Color coding
- [ ] Check HTML-Tags

### GoogleAPI/Deepl

- [ ] Move to Deepl2 4 free account

### Get SuSo Designer Template

- [ ] Issue if special unicodes & readxl (e.g with
  “953faa24e13144ac984e1ad62593aab5”)
