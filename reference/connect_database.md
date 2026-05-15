# Query Bioinformatics Database

Retrieves database (.db file) from the SDSU bioinformatics database,
creates a connection via SQLite

## Usage

``` r
connect_database(species_id = NULL)
```

## Arguments

- species_id:

  ID of species selected (Loads organism info data if NULL)

## Value

SQLite connection to the downloaded file
