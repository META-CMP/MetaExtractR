> [!NOTE]
> # MetaExtractR
> 
> ## About:
>
> The `MetaExtractR` R package was developed for internal use in the [full text screening and data extraction phase](https://github.com/META-CMP/data?tab=readme-ov-file#full-text-screening) of our [meta-study on the macroeconomic effects of conventional monetary policy](https://github.com/META-CMP/data). We make the source code public for transparency and reproducibility purposes. Below, we provide guidance on the workflow.

> [!IMPORTANT]
> Please note that in our research project, we extracted effect sizes from "impulse response functions" which usually implies multiple effect sizes across the periods of the response horizon for a single model. We also integrated the effect size standardization which is customized to the specific requirements of our meta-analysis project. These features will not work "out of the box" for your project. But some higher level functionality, like the creation of `JSON` files from a codebook may still be interesting for you. So while this package was developed and customized for our [specific research project]([data](https://github.com/META-CMP/data)), we welcome adaptations. Please feel free to get inspired or even fork the repository and modify it for your own meta-analysis needs.

## Overview:

`MetaExtractR` facilitates systematic data extraction from academic studies by using `JSON` files as the primary data format. This approach offers several advantages over traditional spreadsheet-based coding:
- Individual files per study enable granular version control through Git
- Clear documentation of all changes through commit history
- Reduced risk of accidental data corruption
- Support for hierarchical data structures (e.g., multiple models per study, ensuring [_single-point-of-truth_](https://en.wikipedia.org/wiki/Single_source_of_truth) principle)
- Automated validation against a predefined codebook

## Key Features:

- **Automated file generation**: Creates pre-structured JSON files based on your codebook
- **Validation**: Ensures consistency between coded data and predefined variables
- **Multi-model support**: Handles studies with multiple specifications efficiently
- **Data transformation**: Converts `JSON` with study coding and `csv` files with effect sizes to analysis-ready dataframes
- **Error checking**: Provides error messages for coding inconsistencies

## Installation:

1. Clone this repository to your local machine
2. In RStudio, install the package using:

```r
install.packages("devtools")
devtools::build()
devtools::install()
```

Load the package with `library(MetaExtractR)`.

## Basic Workflow:

1. Create a new study file

```r
MetaExtractR::create_json_file(
  key = "study_identifier",
  rid = "researcher_id",
  codebook = "path/to/codebook.xlsx",
  folder_path = "path/to/json/folder"
)
```

2. Code the study
Open the generated JSON file and replace `null` values with:

- `true`/`false` for boolean variables
- `"text_value"` for string variables
- `"null"` (with quotes!) for unavailable information
- Numeric values without quotes

For studies with multiple models, use nested structures:
```r
"variable_name": {
    "model_1": "value1",
    "model_2": "value2"
}
```

3. Validate and parse
```
df <- MetaExtractR::parse_study_json("path/to/study.json")
```

This function validates the JSON against your codebook and returns a dataframe where each row represents an effect size observation.

> [!NOTE]
>
> You can look at the actual application of the package in our project to get a clearer picture of how it works. For example, you might want to look at the [codebook](https://github.com/META-CMP/data/blob/main/codebook.csv) and the [`JSON` files](https://github.com/META-CMP/data/tree/main/data/full_text_screening/JSON_files) from our research project.

## How MetaExtractR Works:

### Data Structure and Integration

The package is designed around a two-component data structure:

1. **Study metadata (JSON files)**: Contains all coded study characteristics, identification strategies, estimation methods, and model specifications
2. **Effect size data (CSV files)**: Stores extracted impulse response functions with separate files for point estimates and confidence bounds

### Core Functions and Processing Pipeline

The package implements a hierarchical function structure centered around `final_join()`, which orchestrates the complete data processing workflow:

#### 1. Data Parsing and Validation
- `parse_study_json()`: Reads JSON files and validates them against the codebook structure, ensuring all required variables are present
- Converts JSON's hierarchical structure (studies → models → variables) into a rectangular dataframe format

#### 2. Effect Size Integration
- `join_irf_json()`: Matches JSON data with corresponding IRF CSV files based on study keys and model identifiers
- `irf_csv_path()`: Constructs file paths to locate the correct CSV files for each model and outcome variable

#### 3. Effect Size Standardization
The package implements transformations through `trans_study_data()` and its helper functions:

- `effect_trans_se_function()`: Applies case-specific transformations based on how variables are defined in the original study (e.g., log levels vs. growth rates)
- `get_shock_size()`: Extracts and standardizes the monetary policy shock size to 100 basis points
- `adjust_shock_periodicity()`: Converts quarterly or monthly interest rate changes to annualized rates
- `get_axis_scale()`: Parses axis transformation instructions to convert graph units to percentages
- `get_conf_level()` and `get_crit_val()`: Determine confidence levels and calculate critical values for standard error approximation

The package handles several transformation cases based on variable specifications. Each case applies transformations to ensure all effect sizes represent comparable percentage changes following a standardized monetary policy shock.

#### 4. Standard Error Approximation
For studies reporting only confidence intervals, the package approximates standard errors by:
- Calculating the distance between confidence bounds and point estimates
- Dividing by the appropriate critical value (e.g., 1.96 for 95% CI)
- Handling asymmetric confidence intervals by calculating separate upper and lower standard errors

### Error Handling and Validation

The package provides informative error messages pinpointing specific studies and models when issues arise.

### Output

The final output is a unified dataframe where:
- Each row represents one effect size at a specific response horizon
- All effects are standardized to percentage changes from a 100 basis point contractionary shock
- Standard errors are consistently approximated across different reporting formats
