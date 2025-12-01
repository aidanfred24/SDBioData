#' List Table Options
#'
#' @param species_id Selected species ID (Loads organism info data if NULL)
#'
#' @returns Returns a list of table names
#' @export
#'
list_tables <- function(species_id = NULL) {
    conn <- connect_database(species_id)

    x <- DBI::dbListTables(conn)

    DBI::dbDisconnect(conn)

    return(x)
}
