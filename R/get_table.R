#' Get Table from Selected Database
#'
#' Prints list of tables for selected species and allows user to select
#'  from the list, returning the selected table
#'
#' @param species_id Selected species ID (Loads organism info data if NULL)
#' @param table Selected table, organism info (orgInfo) by default
#'
#' @returns Returns data frame selected from table list
#' @export
#'
get_table <- function(species_id = NULL,
                      table = NULL){
    conn <- connect_database(species_id)

    if(is.null(table) && !is.null(species_id)) {
        table <- "geneInfo"
        message("No table provided, using geneInfo by default")
    } else if (is.null(table) && is.null(species)) {
        table <- "orgInfo"
        message("No table provided, using orgInfo by default")
    }

    # Retrieve table from database file
    x <- DBI::dbGetQuery(
        conn = conn,
        statement = paste0("select * from ", table, ";")
    )

    DBI::dbDisconnect(conn = conn)

    return(x)
}
