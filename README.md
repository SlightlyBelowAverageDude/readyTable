
# readyTable

`readyTable` helps you analyze and clean data frames more easily.  
It provides functions to detect issues such as missing values, constant
columns, numeric‑like strings, and mixed types — and then apply cleaning
actions in a controlled way.

------------------------------------------------------------------------

## ✨ Installation

You can install the development version from GitHub with:

``` r
#install.packages("devtools")
devtools::install_github("SlightlyBelowAverageDude/readyTable")

library(readyTable)

#create a bad data frame
df <- data.frame(
  age = c(25, 30, NA, 45),
  income = c(1000, NA, 2000, 1500),
  const_col = rep("x", 4),
  stringsAsFactors = FALSE
)

#Step 1: Analyzing issues and get suggestions
log <- suggest_cleaning(df)
log
#a tibble with suggested actions for each problematic column

#Step 2: Deciding which columns to fix
cols_to_fix <- c("age", "income", "const_col")

#Step 3: Applying cleaning
cleaned_df <- apply_cleaning(df, columns = cols_to_fix)
cleaned_df
#Returns the cleaned data frame
```
