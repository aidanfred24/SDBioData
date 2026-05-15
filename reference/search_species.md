# Search for Species by Name

Searches the organism database for species matching a query string.

## Usage

``` r
search_species(query, name_type = "all")
```

## Arguments

- query:

  Character. The species name, partial name, or ID to search for.

- name_type:

  Character. The type of name to search against. Options:

  - `"all"`: Default. Searches both primary and academic names.

  - `"academic"`: Scientific name. (note: not available for all species)

  - `"primary"`: Primary name in database (common name or academic).

  - `"id"`: Exact species ID match.

## Value

A data frame containing information for all matching species. Throws an
error if no species are found.
