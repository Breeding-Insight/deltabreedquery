# deltabreedquery


This is an R package to pull data from Breeding Insight's [DeltaBreed](https://sandbox.breedinginsight.net/) platform into R via [BrAPI](https://brapi.org/) calls. It offers basic functions to retrieve four types of data:

- **Experiments/Environments**. Location, year, trial type, etc.
- **Germplasm**. Name, pedigree, etc.
- **Observation variables**. Trait ontology definitions. What is being measured/estimated, min/max/units (for numerical traits), accepted response levels (for categorical traits), etc.
- **Observations**. Metadata about the experimental units (experiment, location, entry/germpasm, row, block, etc.) and the phenotypes themselves.

The fetched data is then formatted in a consistent format that closely matches how it appears on DeltaBreed itself.

You can use this library to greatly expedite data import. Instead of manually downloading and reading in a series of CSVs, just run:

```
login_deltabreed()
pheno <- get_observations()
```

## Table of Contents  

1. [Installation](#installation)  
2. [Authentication](#authentication)
3. [Retrieving data](#retrieving-data)
4. [Filtering observations](#filtering-observations)
5. [Adjusting page size size](#page-size)


## Installation

To install the latest version, first make sure the `remotes` package is installed, then run:

```
remotes::install_github("breeding-insight/deltabreedquery")
```

The library can then be loaded per usual with `library(deltabreedquery)`.

## Authentication
To retrieve data from your DeltaBreed instance, you will need two things:
1. The *BrAPI Base URL*. This is unique to each program and does not change.
2. A temporary *Access Token* that authenticates your BrAPI calls for 24 hours.

Both of these can be found on the **BrAPI** tab of your DeltaBreed instance:



While you can simply run `login_deltabreed()` and enter the Base URL at the time of login, it's generally easiest to add the URL to the `login_deltabreed()` call at the start of your R script, since the URL for a given program will never change:

```
login_deltabreed("https://rel-test.breedinginsight.net/v1/programs/07ffcd99-c0ff-4cbb-9b18-d05ae70d10fa")
```

After you supply the URL, the terminal will prompt you for an Access Token. To generate this, hit the **Generate Access Token** button at right and copy-paste the token in into your terminal:


You should then be able fetch data as described below.

You can run `check_auth()` at any time to check whether you have valid credentials stored, which program you are currently logged into, and when the current access token will expire.

## Retrieving data

The library has four main functions to retrieve all of the data of a given type from your DeltaBreed instance:

```
get_experiments()
get_germplasm()
get_variables()
get_observations()
```

Each of these functions returns a data frame designed to resemble how the data appears on DeltaBreed itself, in the templates used to upload data, or the `.csv`/`.xlsx` files downloaded from DeltaBreed.

Column names have remained the same (with spaces and special characters removed) where possible. Some column names and orderings deviate from the data view on DeltaBreed in order to create [tidy](https://tidyr.tidyverse.org/articles/tidy-data.html#defining) data frames with R-compliant column names.

## Filtering observations

Observation data is typically the largest data type by volume for any given program. For programs with a large amount of phenotype data, this can make `get_observations()` somewhat slow. If you only want to import a subset of data, you can filter the request like so:

```
ith <- filter_observations(location = "Ithaca")
ayt <- filter_observations(exp_name = "AYT",
                           year = c(2022:2025))
```

For a full description of the available filters, run `?filter_observations`.

All filters correspond to one of the columns returned by `get_experiment()`, so it is useful to run this function first to double-check spelling and which experiments have been uploaded to DeltaBreed. All filtering is case-sensitive.


## Adjusting page size for faster calls
For most BrAPI requests, the response sent by the BrAPI server is *paginated*. Instead of returning a single massive JSON document, the server returns a series of JSON documents, each with a given number of records (the page size), which the client program sequentially retrieves and reads. When you send a BrAPI request, you can usually request that the server use a specific page size.

`get_germplasm()`, `get_observations()`, and `filter_observations()` all take a `page_size` argument, set to 5000 by default. Increasing the page size can speed up your requests, but setting it too high can cause server errors. If you find that one of these functions is taking too long, try increasing this value to 10000 or higher.

`get_experiments()` and `get_variables()` do not take a `page_size` argument, since these responses are usually several orders of magnitude smaller than germplasm/observations and do not need adjustment.