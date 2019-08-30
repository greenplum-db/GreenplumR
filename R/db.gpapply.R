
# needs to test
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
        paste("DISTRIBUTED BY (", paste(distributeOn, sep=","), ")")
    } else {
        stop("invalid distributed value")
    }
}
.extract.param.list <- function(param_list) {
    if (is.null(param_list) || length(param_list)==0)
        return ("")
    arg_str_array <- strsplit(deparse(param_list), ", .Names = ")[[1]]
    print(length(arg_str_array))
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

db.gpapply <- function(X, MARGIN=NULL, FUN = NULL, output.name=NULL, output.signature=NULL,
        clear.existing=FALSE, case.sensitive=FALSE,output.distributeOn=NULL, language="plcontainer", ...)
{
    if (!is.function(FUN))
        stop("FUN must be a function")
    randomName <- getRandomNameList()[1]

    typeName <- sprintf("gptype_%s", randomName)
    if (!is.null(output.signature))
    {
        typelist_str <- sprintf("CREATE TYPE %s AS (\n", typeName)
        typelist <- .simplify.signature(output.signature)
        fieldStr <- paste(names(typelist), typelist, sep=" ", collapse=",\n")
        typelist_str <- paste(typelist_str, fieldStr, "\n);", sep="")
        print(typelist_str)
        db.q(typelist_str)
    }

    # generate function parameter str
    ar <- attributes(X)
    param_list_str <- ""
    local_data_frame_str <- ""
    relation_name <- ar$.content

    local_data_frame_str <- paste(ar$.col.name, ar$.col.name, sep="=", collapse=", ")
    param_list_str <- paste(ar$.col.name, sep=", ")
    print(param_list_str)

    # extract parameter list from gpapply
    listStr <- .extract.param.list(list(...))
    if (nchar(listStr)>0)
        listStr <- paste(", ", listStr, sep="")
    print(paste('listStr=', listStr))

    funName <- paste("gprfunc_", randomName, sep="")
    funBody <- paste("# container:  plc_r_shared\ngplocalf <- ", paste(deparse(FUN), collapse="\n"))
    localdf <- sprintf("df <- data.frame(%s)\n", local_data_frame_str)
    localcall <- sprintf("do.call(gplocalf, list(df %s))", listStr);

    createStmt <- sprintf("CREATE FUNCTION %s (%s) RETURNS SETOF %s AS $$ %s\n %s\ return(%s)\n $$ LANGUAGE '%s';",
    funName, param_list_str, typeName, funBody, localdf, localcall, language);

    print(createStmt)
    db.q(createStmt)

    if (is.null(output.name))
    {
        query <- sprintf("WITH gpdbtmpa AS (SELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s) tmptbl) SELECT (gpdbtmpb::%s).* FROM gpdbtmpa;",
                funName, param_list_str, param_list_str, relation_name, typeName)
    }
    else
    {
        query <- sprintf("CREATE TABLE %s AS WITH gpdbtmpa AS (SELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s) tmptbl) SELECT (gpdbtmpb::%s).* FROM gpdbtmpa %s;",
                output.name, funName, param_list_str, param_list_str, relation_name, typeName, .distribute.str(output.distributeOn))
    }
    print(query)

    results <- db.q(query, nrows = NULL)

    cleanString <- sprintf("DROP TYPE %s CASCADE;",typeName)

    print(cleanString)
    db.q(cleanString)

    return(results)

}
