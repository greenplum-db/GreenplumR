# TODO OPTIMIZATION:
#   `X$.cols <- list('col1', 'col2')`
#   If the table has too many columns, and the function only wants few of them, we'd better pass
#   columns as few as possible provided by user.

# needs to test
.generate.gpapply.query <- function(output.name, funName, param_list_str,
                        relation_name, typeName, clear.existing, output.distributeOn){
    if (is.null(output.name))
    {
        query <- sprintf("WITH gpdbtmpa AS (SELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s) tmptbl) SELECT (gpdbtmpb::%s).* FROM gpdbtmpa;",
                funName, param_list_str, param_list_str, relation_name, typeName)
    }
    else
    {
        query <- sprintf("CREATE TABLE %s AS WITH gpdbtmpa AS (SELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s) tmptbl) SELECT (gpdbtmpb::%s).* FROM gpdbtmpa %s;",
                output.name, funName, param_list_str, param_list_str, relation_name, typeName, .distribute.str(output.distributeOn))

        clearStmt <- .clear.existing.table(output.name, clear.existing)
        if (nchar(clearStmt)>0)
            query <- paste(clearStmt, query, sep='\n');
    }
    print(query)
    return (query)
}

# these SQLs may be executed.
# 1. CREATE TYPE gptype_xxx
# 2. CREATE FUNCTION gprfunc_xxx
# 3. Execute the r-wrapper-function
# 4. DROP TYPE gptype_xxx CASCADE
db.gpapply <- function(X, MARGIN=NULL, FUN = NULL, output.name=NULL, output.signature=NULL, clear.existing=FALSE,
                       case.sensitive=FALSE, output.distributeOn=NULL, runtime.id='plc_r_shared', language="plcontainer", ...)
{
    if (is.null(X) || !is.db.data.frame(X))
        stop("X must be a db.data.frame")
    if (!is.function(FUN))
        stop("FUN must be a function")
    .check.output.name(output.name)
    .check.language(language)

    basename <- getRandomNameList()

    #create returned type if output.signature is not null
    typeName <- .to.type.name(basename)
    if (is.null(output.signature)) {
        # TODO: signaturee is null
        stop("NULL signature, not impl")
    } else {
        # signature is not null, create a type
        create_type_sql <- .create.type.sql(typeName, output.signature, case.sensitive = case.sensitive)
        print(create_type_sql)
        db.q(create_type_sql)
    }
    tryCatch({
    # generate function parameter str
    ar <- attributes(X)
    relation_name <- ar$.content
    # "col1 type1, col2 type2"
    # param_list_str_with_type <- paste(ar$.col.name, ar$.col.udt_name, collapse=", ")
    #CASE_SENSITIVE: relation_name, param_list_str
    if (isTRUE(case.sensitive)) {
        if (!is.null(output.name))
            output.name <-  paste("\"", unlist(strsplit(output.name, '\\.')),"\"", sep='', collapse='.')
        # ar$.col.name <- paste("\"", ar$.col.name, "\"", sep='')
        param_list_str_no_type <- paste("\"", ar$.col.name, "\"", sep='', collapse=", ")
    } else {
        # if not case sensitive, all default names created by postgresql are lower case
        if (!is.null(output.name))
            output.name <- tolower(output.name)
        param_list_str_no_type <- paste(ar$.col.name, collapse=", ")
    }

    # Create function
    createStmt <- .create.r.wrapper(basename=basename, FUN=FUN, Xattr=ar, args=list(...),
                                    runtime.id=runtime.id, language=language)
    print(createStmt)
    db.q(createStmt)

    # Run the generated query inside GPDB
    query <- .generate.gpapply.query(output.name, funName = .to.func.name(basename), 
                                    param_list_str = param_list_str_no_type, relation_name = relation_name,
                                    typeName = typeName, clear.existing=clear.existing, output.distributeOn = output.distributeOn)
    print(query)
    results <- db.q(query, nrows = NULL)
    
    #END OF tryCatch
    }, finally = {
    #drop type
    cleanString <- sprintf("DROP TYPE %s CASCADE;",typeName)
    db.q(cleanString)
    })
    
    return (results)

}

