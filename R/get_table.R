#' Get Table from Selected Database
#'
#' Retrieves a specific table from the database for a selected species.
#'
#' @param species_id Numeric. The selected species ID.
#'   If `NULL`, the function defaults to loading general organism info.
#' @param table Character. The name of the table to retrieve (e.g., "geneInfo", "pathway").
#'   If `NULL`, defaults to "geneInfo" (if `species_id` is provided) or "orgInfo"
#'   (if `species_id` is `NULL`).
#'
#' @returns A data frame containing the data from the selected table.
#'
#' @seealso \code{\link{list_tables}} to see available tables for a species.
#' @md
#' @export
get_table <- function(species_id = NULL,
                      table = NULL){
    conn <- connect_database(species_id)

    if(is.null(table) && !is.null(species_id)) {
        table <- "geneInfo"
        message("No table provided, using geneInfo by default")
    } else if (is.null(table) && is.null(species_id)) {
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
