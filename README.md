# deltabreedquery


This is a small R package to pull data from Breeding Insight's [DeltaBreed](https://sandbox.breedinginsight.net/) platform into R via [BrAPI](https://brapi.org/) calls. It offers basic functions to pull three types of data into a properly formatted data frame:

- Germplasm
- Experiments/Environments
- Observations

### INSTALLATION

To install the latest version, first make sure the `devtools` package is installed and run:

```
devtools::install_github("tyrwh/deltabreedquery")
```

The library can then be loaded per usual with `library(deltabreedquery)`.

### BASIC FUNCTIONS

As of February 2026, all that the library does is allow you to pull the three data types into a neatly formatted data frame:

```
login_deltabreed()
germplasm <- get_germplasm()
experiments <- get_experiments()
observations <- get_observations()
```

You authenticate using a BrAPI Base URL and valid Access Token from the specific program you want to query, and then you can pull the entirety of the data from Germplasm, Experiments/Environments, and Observations.


### LOGGING IN

To "log in", you need to get a valid Access Token from within DeltaBreed and supply it once, after which it's stored in the `deltabreedquery` global env. These access tokens are valid for 

`logout_deltabreed()` does the reverse. All it does is remove the URL and token from your global env.

### SEARCHING

There are multiple ways to get a response from DeltaBreed.

For each data type, there are two functions: one which sends a GET request to fetch all the data of the given type (e.g. `get_germplasm()`) and one which sends a POST request to search for specific terms (e.g. `search_observations(year = 2024)`).

For most purposes, fetching everything via `get_x()` will be the more useful method. For one, searching in BrAPI can only be done via exact string matching as of September 2025. There are no Boolean operators that would allow you to request all experimental data where `Year > 2017`, for example. What's more, for programs without a ton of records, it doesn't take that long to just pull all the data, after which you can filter it however you like. 

Searching is still useful for programs with a large amount of data on DeltaBreed obviously, or if you want to build a more minimal workflow (e.g. writing a field season summary that draws data from specific years).

You can easily allow for people to pass specific search terms into a GET query, but I'm not sure if that's much faster than using a search query. [Search services in BrAPI](https://plant-breeding-api.readthedocs.io/en/latest/docs/best_practices/Search_Services.html) are supposedly pretty optimized as it is.

### NAMING
`deltabreedquery` was just a placeholder name, to be honest. If this ends up being useful to other people besides me we could name it `DeltaBreedR` or just `deltabreed` or something, whatever.

### PRINCIPLES

I used a few basic design principles in writing this:

1. Fetched data should be returned as a tidy data frame that closely resembles how the data appears in DeltaBreed itself.
2. If a naming convention would cause problems in R (spaces, special characters), using unambiguous CamelCase names takes precedent over copying DeltaBreed conventions exactly.
3. All human-readable information associated with a given data type should be included by default, and all non-human readable info should be excluded by default; no `DbId`s in the data frame.

### QUERYING

There are several endpoints that contain the relevant information we need:
- Ontology
- Germplasm
- Trials
- Studies
- ObservationUnits
- Observations

Every get/search should have a checkpoint for 404/401 error and supply a helpful error message if possible.

### PAGINATION

Most BrAPI responses will contain pagination metadata. Details about pagination in BrAPI can be found [here](https://plant-breeding-api.readthedocs.io/en/latest/docs/best_practices/Pagination.html). The exact implementation of pagination varies by the endpoint. Some of them allow you to adjust the page size by appending `?pageSize=x`, while others do not. The default `pageSize` for the `/brapi/v2/germplasm/` endpoint is 50, but for the `/brapi/v2/observations/` endpoint it is 1000.

This library uses a default `pageSize` of 1000 and handles paginated responses automatically. It checks the metadata returned as part of the response JSON, and if `totalPages > 1`, it will continue to request pages as needed.

From some rudimentary testing, I haven't seen any noticeable time difference between a request with `pageSize=500` and one with `pageSize=50`. Thus the overall time for a request scales inversely with the page size, since the larger the pageSize, the more individual calls you have to make.

I assume you could run into timeout issues or something if you set the page size too high. This is probably worth testing or asking the Breeding Insight dev team. For now, I set the default `pageSize` to 1000 since it's a nice round number and hasn't caused any issues thus far.

### TRAITS

The nomenclature for traits gets a bit confusing. The frontend calls it Ontology, but in terms of BrAPI endpoints, the core data is stored on the `/variables` endpoint, not `/ontology` or `/traits`. For simplicity, I've just called it "Traits" throughout.

The Ontology table in DeltaBreed captures most of the core info, so I've designed the output of `get_traits()` to look similar to this. I've also added in a few things (min, max, category levels) from the "Show details" pane as well.

*Note:* I couldn't figure out where the full text descriptions of each trait were stored. Maybe this is in a non-BrAPI endpoint? Either way, I'm skipping this for now.

### GERMPLASM

Nothing super fancy here. The formatting of the data returned by `get_germplasm()` pretty much matches the table view shown in the Germplasm tab of DeltaBreed and the corresponding upload template.

### EXPERIMENTS

In DeltaBreed, the table view for Experiments is minimalist, and the table view of each experiment contains the relevant metadata as columns (location, year, etc.). I thought it would be useful to have a more extensive Experiments table, so I designed this view from scratch. I tried to match naming conventions to the metadata columns used in the observation tables.

Both BrAPI and DeltaBreed use two levels of experimental data, one larger one and one smaller one:

| TYPE  | BRAPI      | DELTABREED |
| ----  | ----       | ---- |
|Large  | Trials		| Experiments |
|Small	| Studies	  | Environments |

BrAPI treats the small entity (Study) as the core one while the larger (Trial) is more just a flexible grouping level. DeltaBreed does the opposite - the larger entity (Experiment) is the core unit, while the smaller one (Environment) is just a subset of this.

The formatting of the data returned by `get_experiments()` combines data from both entity types into a single data frame.

What other info might you want to get, which might require a query to other endpoints?
- Num Unique germplasm entries
- Num ObservationUnits
- Num Traits
- Num Observations

I think this can all be pulled from ObsUnits and Observations? TBD.

### OBSERVATIONS

Observation Units are the physical entities on which you take data: the plots, plants, trees, etc. The `/brapi/v2/observationunits/` endpoint contains metadata about field layout and which entry is in which position.

Observations are the actual phenotype values taken on these entities: plant height, fruit color, yield, etc. These are stored in long format (aka key-value format) in the `/brapi/v2/observations/` endpoint.

Would you ever want to pull Observation Units without pulling the corresponding Observations? It's possible, but I think less likely. I don't plan to work on it for the time being.

The output of `get_observations()` mostly matches the data view used in DeltaBreed and the corresponding upload template, with two major differences:

1. I moved entry information (`GermplasmName`, `GID`, `TestOrCheck`) from the leftmost three columns to the right side, immediately before the phenotype values. This keeps the most general information (the Experiment and Environment metadata) on the left side for ease of reading.
2. I have not included the geospatial columns (`Lat`, `Long`, `RTK`). I haven't seen a program use these so far, so it seems wasteful to include empty columns every time. Long term, it would be good to enable inclusion of these in the output. Might need to learn how spatial coordinate storage works in R.
