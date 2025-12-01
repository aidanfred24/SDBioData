#' Query Bioinformatics Database
#'
#' Retrieves database (.db file) from the SDSU bioinformatics database,
#'  creates a connection via SQLite
#'
#' @param species_id ID of species selected (Loads organism info data if NULL)
#'
#' @returns SQLite connection to the downloaded file
#' @export
#'
connect_database <- function(species_id = NULL){

    # define where database is located
    db_ver <- "data113"
    db_url <- "http://bioinformatics.sdstate.edu/data/"

    # if environmental variable is not set, use relative path
    DATAPATH <- Sys.getenv("IDEP_DATABASE")[1]
    # if not defined in the environment, use too levels above
    if (nchar(DATAPATH) == 0) {
        DATAPATH <- paste0("../../data/")
    }
    #Add version
    DATAPATH <- paste0(DATAPATH, "/", db_ver, "/")
    org_info_file <- paste0(DATAPATH, "demo/orgInfo.db")
    if(!file.exists(org_info_file)) {
        DATAPATH <- paste0("./", db_ver, "/")
        org_info_file <- paste0(DATAPATH, "demo/orgInfo.db")
    }
    # Download file from path
    if (!file.exists(org_info_file)) {
            file_name <- paste0(db_ver, ".tar.gz")
            options(timeout = 3000)
            download.file(
                url = paste0(db_url, db_ver, "/", file_name),
                destfile = file_name,
                mode = "wb",
                quiet = FALSE
            )
            untar(file_name) # untar and unzip the files
            file.remove(file_name) # delete the tar file to save storage
    }

    # Specific species selected
    if (!is.null(species_id)) {
        conn <- DBI::dbConnect(
            drv = RSQLite::dbDriver("SQLite"),
            dbname = org_info_file,
            flags = RSQLite::SQLITE_RO
        )
        # Find species in orgInfo database
        file <- tryCatch(
            DBI::dbGetQuery(
                conn,
                statement = paste0("select file from orgInfo where id = ",
                                   species_id, ";")
            ),
            error = function(e){"Species Not Found"}
        )
        DBI::dbDisconnect(conn)

        # Return error message if species not found
        if (is.null(nrow(file)) || nrow(file) == 0){
            file <- "Species Not Found"
            stop(file)
        }

        org_info_file <- paste0(DATAPATH, "db/", file)
        if(!file.exists(org_info_file)) {
            DATAPATH <- paste0("./", db_ver, "/")
            org_info_file <- paste0(DATAPATH, "db/", file)
        }
        if (!file.exists(org_info_file)) {
            file_name <- paste0(file, ".gz")
            options(timeout = 3000)
            download.file(
                url = paste0(db_url, db_ver, "/db/", file_name),
                destfile = paste0(org_info_file, ".gz"),
                mode = "wb",
                quiet = FALSE
            )
            # Unzip species database file
            R.utils::gunzip(filename = paste0(DATAPATH, "db/", file_name),
                            destname = org_info_file)
        }
    }

    return(DBI::dbConnect(
        drv = RSQLite::dbDriver("SQLite"),
        dbname = org_info_file,
        flags = RSQLite::SQLITE_RO
    ))
}
