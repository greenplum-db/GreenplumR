# GPTapply

.index.translate <- function(INDEX, ar)
{
    if (is.null(INDEX))
        stop("INDEX value cannot be NULL")
    if (is.integer(INDEX) || (is.double(INDEX) && floor(INDEX) == INDEX))
        return (ar$.col.name[INDEX])
    if (is.character(INDEX))
        return (INDEX)
    stop("INDEX value cannot be determined with value of type ", typeof(INDEX))
}

#.generate.gptapply.query <- function(output.name, funName, param.name.list, param.group.list, relation_name, INDEX, typeName, output.distributeOn, clear.existing){
#    if (is.null(output.name))
#    {
#        query <- sprintf("WITH gpdbtmpa AS (\nSELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s GROUP BY %s) tmptbl\n)\nSELECT (gpdbtmpb::%s).* FROM gpdbtmpa;",
#                funName, param.name.list, param.group.list, relation_name, INDEX, typeName)
#    }
#    else
#    {
#        #add distributeOn
#        query <- sprintf("CREATE TABLE %s AS\nWITH gpdbtmpa AS (\nSELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s GROUP BY %s) tmptbl\n)\nSELECT (gpdbtmpb::%s).* FROM gpdbtmpa %s;",
#                output.name, funName, param.name.list, param.group.list, relation_name, INDEX, typeName, .distribute.str(output.distributeOn))
#        clearStmt <- .clear.existing.table(output.name, clear.existing)
#        if (nchar(clearStmt) > 0)
#                    query <- paste(clearStmt, query)
#    }
#
#    return (query)
#}


db.gptapply <- function(X, INDEX, FUN = NULL, output.name = NULL, output.signature = NULL, clear.existing = FALSE, case.sensitive = FALSE,
        output.distributeOn = NULL, debugger.mode = FALSE, simplify = TRUE, runtime.id = "plc_r_shared", language = "plcontainer", ...)
{

    randomName <- getRandomNameList()

    #create type
    typeName <- sprintf("gptype_%s", randomName)
    if (is.null(output.signature)) {
        # signature is null
        stop("output.signature is null")
    } else {
        create_type_str <- .create.type.sql(typeName, output.signature, case.sensitive)
        db.q(create_type_str)
    }

    # generate function parameter str
    ar <- attributes(X)
    param.type.list <- ""
    param.group.list <- ""
    relation_name <- ar$.content

    param.name.list <- paste(ar$.col.name, collapse = ", ")

    if (case.sensitive) {
        if (!is.null(output.name))
            output.name <-  paste('"', output.name, '"', sep = '')
        ar$.col.name <- paste('"', ar$.col.name, '"', sep = '')
    }

    INDEX <- .index.translate(INDEX, ar)
    upperIndex <- toupper(INDEX)
    for (i in 1:length(ar$.col.name)) {
        if (i > 1) {
            if (toupper(ar$.col.name[i]) == upperIndex) {
                param.group.list <- paste(param.group.list, ", ", ar$.col.name[i], sep = "")
                param.type.list <- paste(param.type.list, ", ", ar$.col.name[i], " ", ar$.col.udt_name[i], sep = "")
            }
            else {
                param.group.list <- paste(param.group.list, " , array_agg(", ar$.col.name[i], ") AS ", ar$.col.name[i], sep = "")
                param.type.list <- paste(param.type.list, ", ", ar$.col.name[i], " ", ar$.col.udt_name[i], "[]", sep = "")
            }
        }
        else {
            if (toupper(ar$.col.name[i]) == upperIndex) {
                param.group.list <- paste(param.group.list, ar$.col.name[i], sep = "")
                param.type.list <- paste(param.type.list, ar$.col.name[i], " ", ar$.col.udt_name[i], sep = "")
            }
            else {
                param.group.list <- paste(param.group.list, "array_agg(", ar$.col.name[i], ") AS ", ar$.col.name[i], sep = "")
                param.type.list <- paste(param.type.list, ar$.col.name[i], " ", ar$.col.udt_name[i], "[]", sep = "")
            }
        }
    }
    #print(param.name.list)
    #print(param.group.list)

    createStmt <- .create.r.wrapper(randomName, FUN = FUN, col.names = ar$.col.name, param.list.str = param.type.list,
                    args = list(...), runtime.id = runtime.id, language = language)
    #print(createStmt)
    db.q(createStmt)

    # STEP: Create SQL
    funName <- paste('gprfunc_', randomName, sep = '')
    if (is.null(output.name))
    {
        query <- sprintf("WITH gpdbtmpa AS (\nSELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s GROUP BY %s) tmptbl\n)\nSELECT (gpdbtmpb::%s).* FROM gpdbtmpa;",
                funName, param.name.list, param.group.list, relation_name, INDEX, typeName)
    }
    else
    {
        query <- sprintf("CREATE TABLE %s AS\nWITH gpdbtmpa AS (\nSELECT (%s(%s)) AS gpdbtmpb FROM (SELECT %s FROM %s GROUP BY %s) tmptbl\n)\nSELECT (gpdbtmpb::%s).* FROM gpdbtmpa %s;",
                output.name, funName, param.name.list, param.group.list, relation_name, INDEX, typeName)
        clearStmt <- .clear.existing.table(output.name, clear.existing)
        if (nchar(clearStmt) > 0)
                    query <- paste(clearStmt, query)
    }
    #print(query)
    results <- db.q(query, nrows = NULL)

    # STEP: Do cleanup
    cleanString <- sprintf("DROP TYPE %s CASCADE;",typeName)
    db.q(cleanString)

    return (results)
}
