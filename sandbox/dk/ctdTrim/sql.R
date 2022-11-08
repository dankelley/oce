library(DBI)
library(RSQLite)

getUserName <- function()
{
    res <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")
    if (is.null(res) || 0L == nchar(res))
        res <- "unknown"
    res
}

createDatabase <- function()
{
    dbName <- paste0("analysis_", getUserName(), ".db")
    if (!file.exists(dbName)) {
        message("creating database file '", dbName, "'")
        con <- dbConnect(RSQLite::SQLite(), dbName)
        dbCreateTable(con, "downcast", c("start"="INT", "end"="INT"))
        dbDisconnect(con)
    } else {
        message("database file '", dbName, "' already exists")
    }
}

addToDatabase <- function(file, start, end)
{
    message("DAN 1")
    dbName <- paste0("analysis_", getUserName(), ".db")
    message("DAN 2 (", dbName, ")")
    if (!file.exists(dbName))
        createDatabase()
    message("DAN 3")
    con <- dbConnect(RSQLite::SQLite(), dbName)
    message("DAN 4")
    dbWriteTable(con, "downcast", data.frame(file=file, start=start, end=end), overwrite=TRUE)
    message("DAN 5")
    dbDisconnect(con)
}

getFromDatabase <- function(file)
{
    dbName <- paste0("analysis_", getUserName(), ".db")
    if (!file.exists(dbName))
        return(c(NA, NA))
    con <- dbConnect(RSQLite::SQLite(), dbName)
    analysis <- dbReadTable(con, "downcast")
    message("file=",file)
    print(analysis)
    dbDisconnect(con)
    return(c(1111,2222))
}
