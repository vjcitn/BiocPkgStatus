#' verify existence of SQLite resource
#' @import RSQLite
#' @import DBI
#' @import dplyr
#' @export
pkg_status_db_exists = function() {
  if (!file.exists(status_db_path()))
    stop("No file pointed to by environment variable BIOC_PKG_STATUS_SQLITE")
  TRUE
}

#' path to SQLite resource
#' @export
status_db_path = function() {
  Sys.getenv("BIOC_PKG_STATUS_SQLITE")
}

# private
status_db_connection_RO = function(flags=RSQLite::SQLITE_RO)
  RSQLite::dbConnect(RSQLite::SQLite(), status_db_path(), flags=flags)
status_db_disconnect = function(con)
  RSQLite::dbDisconnect(con)

#' list available tables
#' @examples
#' status_db_tables()
#' @export
status_db_tables = function() {
  on.exit(status_db_disconnect(con))  # good way to keep things clean?
  con = status_db_connection_RO()
  con |> DBI::dbListTables() |> sort()
}
  
#' persistent connection
#' @examples
#' \dontrun{
#' # should not do this, it would raise a warning
#' status_db() |> tbl("basic") |> head() # leaves connection open
#' status_db() |> tbl("BcChkERR") |> head()
#' }
#' con = status_db()
#' con |> tbl("basic") |> head()
#' con |> tbl("BcChkERR") |> group_by(type) |> summarise(n=n()) |> arrange(desc(n))
#' RSQLite::dbDisconnect(con)
#' # use on.exit to disconnect in functions
#' @export
status_db = function()
  status_db_connection_RO()
