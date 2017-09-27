dbGetQuery <- function(dbconn, statement, ..., .as_tibble = TRUE) {
    if(length(list(...)) > 0) flog.debug("in dbGetQuery, list(...) =\n%s", zzz::sstr(list(...)))
    RV <- DBI::dbGetQuery(dbconn, statement, ...)

    if(is.data.frame(RV) &&
       .as_tibble &&
       requireNamespace("tibble", quietly = TRUE)) {
        RV <- tibble::as_tibble(RV)
    }

    flog.debug(zzz::sstr(RV, .name = "returned table"))
    return(RV)
}


dbGetQueryFile <- function(dbconn, file, ..., locale = readr::default_locale(), .as_tibble = TRUE) {
    flog.debug("reading SQL statement from file %s", file)
    dbGetQuery(dbconn,
               statement = readr::read_file(file, locale),
               ...,
               .as_tibble = .as_tibble)  ## must be named to avoid the value being part of dbGetQuery's ... arg
}
