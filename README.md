
# survey.tm

A collection of functions that aims to streamline the management and
updating of translations in multilingual surveys using CAPI software.

The package automatically extracts text items from survey
questionnaires, consolidates them, and cross-references them with an
existing translation database hosted on Google Sheets, ensuring that
translations stay up-to-date and synchronized with the latest version of
the questionnaire(s).

The ‘Translation Database’, which is a Google Sheet, serves as the
central hub where translators can add translations to text items of
respective questionnaires, offering a simple and intuitive user
interface that also allows them to track their progress and flag text
items for future review or revision.

## Installation

You can install the development version of survey.tm through

``` r
devtools::install_github("petbrueck/survey.tm")
```

## Set up

Before working with one or more questionnaires for a data collection
project, you need to run the `setup_tdb()` function **once** to get a
google worksheet that will serve as the “Translation Database Sheet”.

It is usually recommended to have one “Translation Management” Project
(i.e. Google Sheet) for one project (which itself can consist of
multiple Questionnaires and languages).

Please note: To work with [Google
Sheets](https://www.google.com/sheets/about/), you must have a valid
Google account. If you have not used any functionality of package
`googlesheets4` before, you will need to generate an access token.
Follow instructions in console and browser after you ran `setup_tdb()`.

``` r
# Optional: Indicate to `googlesheets4` which mail you will use. Avoids selecting pre-authorised account manually.
# googlesheets4::gs4_auth(email = "your.email@address.com")
setup_tdb(
  ssheet_name = "Project X: Translation Sheet", # Name to assign to new google worksheet
  lang_names = c("German", "French") # Translations/Languages needed. Needs to be ISO 639-1 language name.
)
```

## Basic Workflow

To get started with the essential features of the ‘Translation
Management’, follow the basic steps outlined below, which will guide you
through the necessary steps for a smooth ride.

### Step 1: <span style="font-weight: normal"> Pull all data sources</span>

Pull the Questionnaire Template(s) and current ‘Translation Database’,
compare and produce an updated translation database object.

#### 1.1 Pull the Questionnaire Template(s)

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
  password = "your_password"
  )

# Access the 'Source Questionnaire' of the first questionnaire template
translations_first_questionnaire <- suso_trans_templates[["NAME-OF-QUESTIONNAIRE"]]
# Access the "Translations" sheet for the first questionnaire
translations_sheet_first_questionnaire <- translations_first_questionnaire[["Translations"]]
# Note: The last four rows are an example of how to access the returned object "suso_trans_templates". In most cases, you will not need to access the object in this way.
```

#### 1.2 Parse Questionnaire Templates

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

#### 1.3 Pull ‘Translation Database’ Google Sheet data

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

Please note: At first run after `setup_tdb()`, all elements of the list
returned will be empty data.table’s.
</details>

``` r
# googlesheets4::gs4_auth(email = "your.email@address.com")

#Pull all columns and rows found in all sheets in 'Translation Database' Google Sheet
tdb_data <- get_tdb_data(ss = "GOOGLE-SHEET-IDENTIFIER")
```

#### 1.4 Update ‘Translation Database’ object

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

### Step 2: <span style="font-weight: normal"> Generate outputs for CAPI program and update the Translation Database</span>

#### 2.1 Push new ‘Translation Database’ to Google Sheet

<details>
<summary>
Click to see detailed description
</summary>

Object `new_tdb` contains the most recent text items from the source
questionnaire along with the previously added translations (if any) for
all languages in the ‘Translation Database’. As a next step, one should
update the Google Worksheet ‘Database’ so that Translators can continue
working on the most recent version of the questionnaire(s). To this end,
use `write_tdb_data()` which writes one language from `new_tdb` object
to one particular sheet in the Google Worksheet.

Please note: It is advisable to coordinate with the Translator(s) to not
overwrite translations that she is adding to the Database sheet while
you are about to push new ones.

</details>

``` r
#Write one particular (updated) language to the 'Translation Database' Google Sheet
write_tdb_data(new_tdb[["German"]],
    ss = "GOOGLE-SHEET-IDENTIFIER",
    sheet = "German"
  )

#Or write all languages found in 'Translation Database' object to the Google Sheet
purrr::walk(
  .x = names(new_tdb),
  .f = ~ write_tdb_data(new_tdb[[.x]],
    ss = "GOOGLE-SHEET-IDENTIFIER",
    sheet = .x
  )
)
```

#### 2.2 Create ‘Translation File’ for upload to CAPI software

<details>
<summary>
Click to see detailed description
</summary>

Using object `new_tdb`, one can now add the most recent translation to a
questionnaire source template (as returned by `get_suso_tfiles()`) of
your choice.

To this end, use `create_suso_file()` which will write a .xlsx file that
one can use to upload to the Survey Solutions Designer.

</details>

``` r
# Take the 'German' Translation from database and merge into the source questionnaire as returned by \code{\link{get_suso_tfiles}}
# Consider only text items of particular statuses defined by Translator
create_suso_file(
  tdb.language = new_tdb[["German"]],
  source_questionnaire = suso_trans_templates[["NAME-OF-QUESTIONNAIRE"]],
  statuses = c("Machine", "reviewed", "translated"),
  path = "your-path/German_NAME-OF-QUESTIONNAIRE.xlsx"
)
```

## Utility Functions

In addition to the basic workflow functions, there are/will be a number
of additional wrappers that help in translation as well as validating
the translation.

### Query Translations

TO BE DOCUMENTED & CLEANED UP

``` r
# Query Google API
add_gl_translation(new_tdb)

# Query Deepl
add_deepl_translation(new_tdb,
  API_key = Sys.getenv("deepl_key")
)
```

### Validate Translation

``` r
new_tdb <- identify_sw_issues(new_tdb)
```

## TODO

### Before sharing with wider audience

- [ ] Revise create_suso_file() (simplify, better documentaton)
- [ ] question_coding() revise approach/improve docs (as part of
  parse_xx_titems()?)
- [ ] identify_sw_issues() revise approach/improve docs
- [ ] add_deepl_translation() / add_gl_translation() revise
  approach/improve docs

### General / High-Level

- [ ] Cleanup of function names & argument naming convention to make
  clear between translation and questionnaire
- [ ] Documentation (Functions + Google Translation Sheet)
- [ ] Unit Tests
- [ ] pkgdown
- [ ] TODOs in functions (:
- [ ] devtools::check() tests dont pass locally while devtools::tests()
  does
- [ ] ODK Stream
- [ ] If new questionnaire added for which text item exist, not
  reflected in “QUestionnaire(s)” cell (sticks with old questionnaire)
- [ ] Remove the value.unique in Google Sheet?

### Software Text Mismatch

- [ ] Color coding
- [ ] Check HTML-Tags

### GoogleAPI/Deepl

- [ ] Move to Deepl2 4 free account

### Get SuSo Designer Template

- [ ] Issue if special unicodes & readxl (e.g with
  “953faa24e13144ac984e1ad62593aab5”)
