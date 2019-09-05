
# needs to test
.to.type.name <- function(basename) {
    paste("gptype_", basename, sep = "")
}

.to.func.name <- function(basename) {
    paste("gprfunc_", basename, sep = "")
}
.simplify.signature <- function(signature) {
    if (is.null(signature) || is.list(signature))
        return (signature)
    if (is.function(signature))
        return (.simplify.signature(signature()))
    if (is.db.data.frame(signature))
        stop("type of signature is db.data.frame, not supported now")

    stop(paste("invalid signature:", signature))
}
.distribute.str <- function(distributeOn) {
    if (is.null(distributeOn)) {
        ""
    } else if (is.character(distributeOn)) {
        dist <- toupper(distributeOn)
        if (dist == "RANDOMLY") "DISTRIBUTED RANDOMLY"
        else if (dist == "REPLICATED") "DISTRIBUTED REPLICATED"
        else stop("invalid distribute value")
    } else if (is.list(distributeOn)) {
        # DISTRIBUTED BY (column)
        paste("DISTRIBUTED BY (", paste(distributeOn, collapse=", "), ")", sep="")
    } else {
        stop("invalid distributed value")
    }
}
.clear.existing.table <- function(output.name, clear.existing) {
    if (is.null(output.name) || !db.existsObject(output.name))
        return ('')
    if (is.logical(clear.existing) && isTRUE(clear.existing))
        return (paste("DROP TABLE IF EXISTS ", output.name, ";", sep=''))
    # NZR truncate talbe if clear.existing is 'truncate',
    # but this doesn't work for greenplum. If we need to support the 'truncate'
    # value for clear.existing, just drop this table.
    #if (is.character(clear.existing) && identical(tolower(clear.existing), "truncate"))
    #    return (paste("DROP TABLE '", output.name, "';"))
    stop("the output table exists, but clear flag is not set")
}
.extract.param.list <- function(param_list) {
    if (is.null(param_list) || length(param_list)==0)
        return ("")
    arg_str_array <- strsplit(deparse(param_list), ", .Names = ")[[1]]
    message(length(arg_str_array))
    if (length(arg_str_array) == 1)
    {
        listStr <- substr(arg_str_array[1], 6, nchar(arg_str_array[1]) - 1)
    }
    else if (length(arg_str_array) == 2)
    {
        listStr <- substr(arg_str_array[1], 16, nchar(arg_str_array[1]) - 1)
    }
    else
    {
        stop("The functon input argument must not inlcude '.Names'")
    }
}

.create.type.sql <- function(typeName, signature_list) {
    if (is.null(signature_list))
      return (NULL)

    #CASE_SENSITIVE:  signature_list
    typelist_str <- sprintf("CREATE TYPE %s AS (\n", typeName)
    typelist <- .simplify.signature(signature_list)

    fieldStr <- paste(names(typelist), typelist, sep=" ", collapse=",\n")
    typelist_str <- paste(typelist_str, fieldStr, "\n);", collapse="")

    return (typelist_str)
}

getRandomNameList <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  a <- paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  a[1]
}

.create.r.wrapper <- function(basename, FUN, col.names, param.list.str, args, runtime.id='', language='plcontainer') {
 #generate output
    local_data_frame_str <- paste(col.names, col.names, sep='=', collapse=', ')
    listStr <- .extract.param.list(args)
    if (nchar(listStr)>0)
        listStr <- paste(', ', listStr, sep='')

    typeName <- .to.type.name(basename)
    funName <- .to.func.name(basename)
    funBody <- paste("# container: ", runtime.id, "\ngplocalf <- ", paste(deparse(FUN), collapse="\n"), sep="")
    localdf <- sprintf("df <- data.frame(%s)", local_data_frame_str)
    localcall <- sprintf("do.call(gplocalf, list(df%s))", listStr);

    #Question: CREATE OR REPLACE FUNCTION?
    createStmt <- sprintf("CREATE FUNCTION %s (%s) RETURNS SETOF %s AS $$\n %s\n %s\nreturn(%s)\n $$ LANGUAGE '%s';",
                          funName, param.list.str, typeName, funBody, localdf, localcall, language);
}

