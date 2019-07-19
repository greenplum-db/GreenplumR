
# needs to test
.simplify.signature <- function(signature) {
    if (is.null(signature) || is.list(signature))
        return (signature)
    if (is.function(signature))
        return (simplify.signature(signature()))
    if (is.db.data.frame(signature)) {
        map.type <- function(y) {
            switch(typeof(y),
                double  = NZ.DOUBLE,
                integer = NZ.INT32,
                character = list(NZ.VARIABLE, 256),
                logical = NZ.BOOL
            )
        }
        df <- as.db.data.frame(signature)
        res <- lapply(names(df), map.type)
        return(res)
    }
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


db.gpapply <- function(X, MARGIN, FUN = NULL, output.name=NULL, output.signature=NULL,
		clear.existing=FALSE, case.sensitive=FALSE,output.distributeOn=NULL, language="plcontainer", ...)
{	
	args <- list(...)

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
    listStr <- ""
    param_list_str <- ""
	local_data_frame_str <- ""
	relation_name <- ar$.content

    local_data_frame_str <- paste(ar$.col.name, ar$.col.name, sep="=", collapse=", ")
    param_list_str <- paste(ar$.col.name, sep=", ")
	print(param_list_str)

    # extract parameter list from gpapply
    if (length(args)>0) {
        arg_str_array <- strsplit(deparse(args), ", .Names = ")[[1]]
        print(length(arg_str_array))
        if (length(arg_str_array) == 1)
        {
            listStr = substr(arg_str_array[1], 6, nchar(arg_str_array[1]) - 1)
        }
        else if (length(arg_str_array) == 2)
        {
            listStr = substr(arg_str_array[1], 16, nchar(arg_str_array[1]) - 1)
        }
        else
        {
            stop("The functon input argument must not inlcude '.Names'")
        }
        listStr <- paste(", ", listStr, sep="")
    }

	funName <- paste("gprfunc_", randomName, sep="")
	funBody <- paste("# container:  plc_r_shared\ngplocalf <- ", paste(deparse(FUN), collapse="\n"))
	localdf <- sprintf("df <- data.frame(%s)\n", local_data_frame_str)
	localcall <- sprintf("do.call(gplocalf, list(df %s))", listStr);

	createStmt <- sprintf("CREATE FUNCTION %s (%s) RETURNS SETOF %s AS $$ %s\n %s\ return(%s)\n $$ LANGUAGE '%s';",
	funName, param_list_str, typeName, funBody, localdf, localcall, language);

	#print(createStmt)
	db.q(createStmt)

	if (is.null(output.name))
	{
		query <- sprintf("WITH a AS (SELECT (%s(%s)) AS b FROM (SELECT %s FROM %s) tmptbl) SELECT (b::%s).* FROM a;",
				funName, param_list_str, param_list_str, relation_name, typeName)
	}
	else
	{
		query <- sprintf("CREATE TABLE %s AS WITH a AS (SELECT (%s(%s)) AS b FROM (SELECT %s FROM %s) tmptbl) SELECT (b::%s).* FROM a %s;",
				output.name, funName, param_list_str, param_list_str, relation_name, typeName, .distribute.key(distributeOn))
	}
	print(query)

	results <- db.q(query, nrows = NULL)

	cleanString <- sprintf("DROP TYPE %s CASCADE;",typeName)

	print(cleanString)
	db.q(cleanString)

	return(results)

}
