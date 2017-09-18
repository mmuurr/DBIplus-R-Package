dbconn_summary_string <- function(dbconn) {
    with(DBI::dbGetInfo(dbconn), sprintf("%s@%s:%s/%s", user, host, port, dbname))
}


try_dbDisconnect <- function(dbconn) {
    ## since this function is called as part of a finalizer, there's no guarantee dbconn exists anymore.
    ## thus, only _try_ each individual call.
    try(flog.debug("disconnecting from %s", dbconn_summary_string(dbconn)))
    try(DBI::dbDisconnect(dbconn))
}


dbenv <- function(...) {
    env <- new.env(parent = emptyenv())
    class(env) <- append("dbenv", class(env))

    if(nargs() > 0) {
        env <- dbenv_add_conn(env, ...)
    }

    reg.finalizer(env, function(env) {
        invisible(lapply(unique(as.list(env)), try_dbDisconnect))
    }, onexit = TRUE)

    return(env)
}


str.dbenv <- function(dbenv) {
    cat("dbenv: ")
    NextMethod()
    purrr::iwalk(as.list(dbenv), function(dbconn, name) {
        cat(sprintf("\t(%s)\t%s\n", name, dbconn_summary_string(dbconn)))
    })
    return(invisible(NULL))
}


print.dbenv <- function(dbenv) {
    str(dbenv)
    return(invisible(dbenv))
}


dbenv_add_conn <- function(dbenv, ...) {
    checkmate::assertEnvironment(dbenv)

    l <- list(...)
    checkmate::assertList(l,
                          types = "DBIConnection",
                          any.missing = FALSE,
                          all.missing = FALSE,
                          min.len = 1,
                          names = "unique")
    stopifnot(all(purrr::map_int(l, length) == 1))

    purrr::walk2(names(l), l, assign, pos = dbenv)
    
    return(dbenv)  ## not really needed since dbenv has reference semantics.
}


