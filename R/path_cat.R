#' Retrieve Pathway Categories
#'
#' Retrieves pathway category options (e.g. KEGG, GOBP, etc.) for a given
#' species. May take longer for well-documented species (i.e. Human)
#'
#' @param species_id Numeric. The ID of a desired species from database, found
#' using `srch_species()`
#'
#' @returns Data frame of pathway categories for given species
#' @export
#'
path_cat <- function(species_id = NULL){

    # Check for species
    if (is.null(species_id)) {
        stop("species_id is required")
    }

    conn <- connect_database(species_id = species_id)

    # Query unique categories
    cats <- DBI::dbGetQuery(
        conn,
        statement = "select distinct category from pathway;"
        )

    DBI::dbDisconnect(conn)

    if (is.null(cats)){
        stop("No categories found for pathways of given species")
    }

    return(cats)
}
