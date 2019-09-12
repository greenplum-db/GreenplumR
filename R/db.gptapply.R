# GPTapply
# Must handle if the value of INDEX is case-sensitive
.index.translate <- function(INDEX, ar)
{
    if (is.null(INDEX))
        stop("INDEX value cannot be NULL")
    if (is.integer(INDEX) || (is.double(INDEX) && floor(INDEX) == INDEX)){
        if ((INDEX <= length(ar$.col.name)) && (INDEX >= 1))
            return (ar$.col.name[INDEX])
        else
            stop("INDEX value cannot exceed number of columns in current df")
    }  
    if (!is.character(INDEX))
        stop("INDEX value cannot be determined with value of type ", typeof(INDEX))
    # Take care: the INDEX may be case-sensitive
    index <- tolower(INDEX)
    if (index != INDEX) {
        # not lower case, may case-sensitive
        if (INDEX %in% ar$.col.name)
            return (INDEX)
    }
    #if ar$.col.name has non-lower columns, since index is not case-sensitive, use tolower(col.name)
    if (index %in% tolower(ar$.col.name))
        return (index)
    stop(paste("invalid INDEX value:", INDEX))
}

# these SQLs may be executed.
# 1. CREATE TYPE gptype_xxx
# 2. CREATE FUNCTION gprfunc_xxx
# 3. Execute the r-wrapper-function
# 4. DROP TYPE gptype_xxx CASCADE
db.gptapply <- function(X, INDEX, FUN = NULL, output.name = NULL, output.signature = NULL, clear.existing = FALSE, case.sensitive = FALSE,
        output.distributeOn = NULL, debugger.mode = FALSE, simplify = TRUE, runtime.id = "plc_r_shared", language = "plcontainer", ...)
{
    # handle case when colnames of X are not all lower, and case.sensitive = FALSE
    if (is.null(X) || !is.db.data.frame(X))
        stop("X must be a db.data.frame")
    if (!is.function(FUN))
        stop("FUN must be a function")
    .check.output.name(output.name)
    .check.language(language)

    basename <- getRandomNameList()
    # generate function parameter str

    #create returned type if output.signature is not null
    typeName <- .to.type.name(basename)
    if (is.null(output.signature)) {
        # TODO: signaturee is null
        stop("NULL signature, not impl")
    } else {
        create_type_str <- .create.type.sql(typeName, output.signature, case.sensitive = case.sensitive)
        db.q(create_type_str, verbose = FALSE)
    }

    tryCatch({
        ar <- attributes(X)
        relation_name <- ar$.content
        param.name.list <- paste(ar$.col.name, collapse = ", ")
        if (isTRUE(case.sensitive)) {
            if (!is.null(output.name))
                output.name <-  paste('"', unlist(strsplit(output.name, '\\.')),'"', sep='', collapse='.')
        } else {
            # if not case sensitive, all default names created by postgresql are lower case
            if (!is.null(output.name))
                output.name <- tolower(output.name)
        }
        field.names <- paste('"', ar$.col.name, '"', sep = '')
        INDEX <- .index.translate(INDEX, ar)
        
        .to.type.field <- function(col.name, udt.name, isIndex) {
            return (paste('"', col.name, '" ', udt.name,
                        ifelse(isIndex, '', '[]'), sep = ''))
        }
        .to.group.field <- function(col.name, isIndex) {
            if (!isIndex)
                return (paste('array_agg("', col.name, '") AS ', col.name, sep = ''))
            if (tolower(col.name) == col.name)
                return (col.name)
            return (paste('"', col.name, '" AS ', col.name, sep = ''))
        }
        
        # param.type.list used as the input parameters of the created function
        # parameter names should be double quoted, so they are case-sensitive
        param.type.list <- ""
        param.group.list <- ""
        for (i in 1:length(ar$.col.name)) {
            if (i > 1) {
                .isIndex <- (ar$.col.name[i] == INDEX)
                param.group.list <- paste(param.group.list, ", ", 
                                    .to.group.field(ar$.col.name[i], .isIndex), sep = "")
                param.type.list <- paste(param.type.list, ", ",
                                    .to.type.field(ar$.col.name[i], ar$.col.udt_name[i], .isIndex), sep = "")
            } else {
                .isIndex <- ar$.col.name[i] == INDEX
                param.group.list <- .to.group.field(ar$.col.name[i], .isIndex)
                param.type.list <- .to.type.field(ar$.col.name[i], ar$.col.udt_name[i], .isIndex)
            }
        }
        
        #Create function
        createStmt <- .create.r.wrapper2(basename = basename, FUN = FUN, 
                                    selected.type.list = param.type.list,
                                    # selected column from table X, it may be optimized
                                    selected.equal.list = .selected.equal.list(ar$.col.name),
                                    args = list(...), runtime.id = runtime.id,
                                    language = language)
        db.q(createStmt, verbose = FALSE)

        index <- paste('"', INDEX, '"', sep = '')
        funName <- .to.func.name(basename)
        if (is.null(output.name))
        {
            query <- sprintf("WITH gpdbtmpa AS (\nSELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s GROUP BY %s) tmptbl\n)\nSELECT (gpdbtmpb::%s).* FROM gpdbtmpa;",
                    funName, param.name.list, param.group.list, relation_name, index, typeName)
        }
        else
        {
            query <- sprintf("CREATE TABLE %s AS\nWITH gpdbtmpa AS (\nSELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s GROUP BY %s) tmptbl\n)\nSELECT (gpdbtmpb::%s).* FROM gpdbtmpa %s;",
                    output.name, funName, param.name.list, param.group.list, relation_name, index, typeName, 
                    .distribute.str(output.distributeOn, case.sensitive = case.sensitive))
            clearStmt <- .clear.existing.table(output.name, clear.existing)
            if (nchar(clearStmt) > 0)
                        query <- paste(clearStmt, query)
        }
        results <- db.q(query, nrows = NULL, verbose = FALSE)
    
    #END OF tryCatch
    }, finally = {
        # STEP: Do cleanup
        cleanString <- sprintf("DROP TYPE %s CASCADE;", typeName)
        db.q(cleanString, verbose = FALSE)
    })

    return (results)
}
