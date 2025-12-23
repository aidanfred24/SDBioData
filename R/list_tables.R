#' List Table Options
#'
#' Lists all available tables within the database for a specific species.
#'
#' @param species_id Numeric. The selected species ID.
#'   If `NULL`, loads the general organism info database.
#'
#' @returns A character vector of table names available in the database connection.
#' @md
#' @export
list_tables <- function(species_id = NULL) {
    conn <- connect_database(species_id)

    x <- DBI::dbListTables(conn)

    DBI::dbDisconnect(conn)

    return(x)
}
