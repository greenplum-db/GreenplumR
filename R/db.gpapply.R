# TODO OPTIMIZATION:
#   `X$.cols <- list('col1', 'col2')`
#   If the table has too many columns, and the function only wants few of them, we'd better pass
#   columns as few as possible provided by user.

# 
.generate.gpapply.query <- function(output.name, funName, func.param.list, select.list.str,
                        relation_name, typeName, clear.existing, distribute.str)
{
    if (is.null(output.name)) {
        query <- sprintf("WITH gpdbtmpa AS (SELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s) tmptbl) SELECT (gpdbtmpb::%s).* FROM gpdbtmpa;",
                funName, func.param.list, select.list.str, relation_name, typeName)
    }
    else {
        query <- sprintf("CREATE TABLE %s AS WITH gpdbtmpa AS (SELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s) tmptbl) SELECT (gpdbtmpb::%s).* FROM gpdbtmpa %s;",
                output.name, funName, func.param.list, select.list.str, relation_name, typeName, distribute.str)

        clearStmt <- .clear.existing.table(output.name, clear.existing)
        if (nchar(clearStmt) > 0)
            query <- paste(clearStmt, query, sep='\n');
    }
    return (query)
}

# these SQLs may be executed.
# 1. CREATE TYPE gptype_xxx
# 2. CREATE FUNCTION gprfunc_xxx
# 3. Execute the r-wrapper-function
# 4. DROP TYPE gptype_xxx CASCADE
db.gpapply <- function(X, MARGIN = NULL, FUN = NULL, output.name = NULL, output.signature = NULL, clear.existing = FALSE,
                       case.sensitive = FALSE, output.distributeOn = NULL, runtime.id = "plc_r_shared", language = "plcontainer", ...)
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
    if (is.null(output.signature))
        # TODO: signaturee is null
        stop("NULL signature, not impl")
    else {
        # signature is not null, create a type
        create_type_sql <- .create.type.sql(typeName, output.signature, case.sensitive = case.sensitive)
        db.q(create_type_sql, verbose = FALSE)
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
                output.name <-  paste('"', unlist(strsplit(output.name, '\\.')),'"', sep='', collapse='.')
        } else {
            # if not case sensitive, all default names created by postgresql are lower case
            if (!is.null(output.name))
                output.name <- tolower(output.name)
        }
        func.param.list <- paste(ar$.col.name, collapse=", ")
        select.list.str <- .select.fields.list(ar$.col.name)

        # Create function
        createStmt <- .create.r.wrapper2(basename=basename, FUN=FUN,
                                selected.type.list = .selected.type.list(ar),
                                selected.equal.list = .selected.equal.list(ar$.col.name),
                                args=list(...), runtime.id=runtime.id,
                                language=language)
        db.q(createStmt, verbose = FALSE)

        # Run the generated query inside GPDB
        distribute.str <- .distribute.str(output.distributeOn,
                                    case.sensitive = case.sensitive)
        query <- .generate.gpapply.query(output.name,
                            funName = .to.func.name(basename),
                            func.param.list = func.param.list,
                            select.list.str = select.list.str,
                            relation_name = relation_name,
                            typeName = typeName,
                            clear.existing = clear.existing,
                            distribute.str = distribute.str)

        results <- db.q(query, nrows = NULL, verbose = FALSE)

    #END OF tryCatch
    }, finally = {
        #drop type
        cleanString <- sprintf("DROP TYPE %s CASCADE;", typeName)
        db.q(cleanString, verbose = FALSE)
    })

    return (results)
}

