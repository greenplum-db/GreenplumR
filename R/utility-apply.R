# needs to test
.to.type.name <- function(basename)
{
    paste("gptype_", basename, sep = "")
}

.to.func.name <- function(basename)
{
    paste("gprfunc_", basename, sep = "")
}

.check.output.name <- function(output.name)
{
    if (is.null(output.name))
        return (NULL)
    if (!is.character(output.name))
        stop("output.name must be NULL or a charater")
    res <- regexpr("[a-zA-Z0-9_]+(\\.[a-zA-Z0-9_]+)?", output.name, perl = FALSE)
    match.length <- attr(res, "match.length")
    if (match.length != nchar(output.name))
        stop(paste("invalid output.name:", output.name))
}

.simplify.signature <- function(signature)
{
    if (is.null(signature) || is.list(signature))
        return (signature)
    if (is.function(signature))
        return (.simplify.signature(signature()))
    if (is.db.data.frame(signature))
        stop("type of signature is db.data.frame, not supported now")

    stop(paste("invalid signature:", signature))
}

.distribute.str <- function(distributeOn, case.sensitive)
{
    if (is.null(distributeOn))
        return ("")

    # DISTRIBUTED BY (column)
    if (is.list(distributeOn)) {
        fields <- ifelse(case.sensitive,
                    paste('"', distributeOn, '"', sep = '', collapse = ', '),
                    paste(distributeOn, collapse = ', '))
        return (paste("DISTRIBUTED BY (", fields, ")", sep = ""))
    }

    if (is.character(distributeOn)) {
        dist <- toupper(distributeOn)
        if (dist == "RANDOMLY")
            return ("DISTRIBUTED RANDOMLY")
        if (dist == "REPLICATED")
            return ("DISTRIBUTED REPLICATED")
        stop("invalid distribute value")
    }
    stop("invalid distributed value")
}

.clear.existing.table <- function(output.name, clear.existing)
{
    if (is.null(output.name))
        return ('')
    if (!is.character(output.name))
        stop("output.name must be NULL or type of character")
    if (!db.existsObject(output.name))
        return ('')
    if (is.logical(clear.existing) && isTRUE(clear.existing))
        return (paste("DROP TABLE IF EXISTS ", output.name, ";", sep=""))
    # NZR truncate talbe if clear.existing is 'truncate',
    # but this doesn't work for greenplum. If we need to support the 'truncate'
    # value for clear.existing, just drop this table.
    #if (is.character(clear.existing) && identical(tolower(clear.existing), "truncate"))
    #    return (paste("DROP TABLE '", output.name, "';"))
    stop("the output table exists, but clear flag is not set")
}

.extract.param.list <- function(param_list)
{
    if (is.null(param_list) || length(param_list)==0)
        return ("")
    str <- deparse(substitute(param_list))
    return (substr(str, 6, nchar(str) - 1))
}

.check.language <- function(language)
{
    if (is.null(language) || !is.character(language))
        stop("lanuage must be a character")
    if (language != 'plr' && language != 'plcontainer')
        stop(paste("invalid language:", language))
}

.create.type.sql <- function(typeName, signature_list, case.sensitive=FALSE)
{
    if (is.null(signature_list))
      return (NULL)

    #CASE_SENSITIVE:  signature_list
    typelist_str <- sprintf("CREATE TYPE %s AS (\n", typeName)
    typelist <- .simplify.signature(signature_list)

    if (isTRUE(case.sensitive)){
        if (isTRUE(any(duplicated(names(typelist)))))
            stop(paste("duplicated signature:", paste(names(typelist), collapse=", ")))
        fieldStr <- paste("\"", names(typelist), "\" ", typelist, sep="", collapse=",\n")
    }
    else{
        if (isTRUE(any(duplicated(tolower(names(typelist))))))
            stop(paste("duplicated signature:", paste(names(typelist), collapse=", ")))
        fieldStr <- paste(names(typelist), typelist, sep=" ", collapse=",\n")
    }
    typelist_str <- paste(typelist_str, fieldStr, "\n);", collapse="")

    return (typelist_str)
}

getRandomNameList <- function(n = 1)
{
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    a <- paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
    a[1]
}

# construction of data.frame
.selected.equal.list <- function(col.name)
{
    paste(col.name, col.name, sep = '=', collapse = ', ')
}

# parameter of created function
.selected.type.list <- function(Xattr)
{
    paste(paste('"', Xattr$.col.name, '"', sep = ''), Xattr$.col.udt_name, collapse=", ")
}

.quoted.fields.list <- function(col.name)
{
    paste('"', col.name, '"', sep = '', collapse = ', ')
}

# project of select clause - gptapply
.to.type.field <- function(col.name, udt.name, isIndex) {
    return (paste('"', col.name, '" ', udt.name,
                ifelse(isIndex, '', '[]'), sep = ''))
}

.to.group.field <- function(col.name, isIndex) {
    if (!isIndex)
		return (paste('array_agg("', col.name, '") AS "', col.name, '"', sep = ''))
    return (paste('"', col.name, '"', sep = ''))
}

# selected.type.list should be `"field1" type1 (, "filed.x" typex)*
# double quoted field name make it case sensitive in UDF
.create.r.wrapper2 <- function(basename, FUN, selected.type.list, selected.equal.list, user.args.str, runtime.id, language) {
    typeName <- .to.type.name(basename)
    funName <- .to.func.name(basename)
    funBody <- paste("# container: ", runtime.id, "\ngplocalf <- ", paste(deparse(FUN), collapse="\n"), sep="")
    localdf <- sprintf("df <- data.frame(%s)", selected.equal.list)
    localcall <- sprintf("do.call(gplocalf, list(df%s))", user.args.str)

    createStmt <- sprintf("CREATE FUNCTION %s (%s) RETURNS SETOF %s AS $$\n %s\n %s\nreturn(%s)\n $$ LANGUAGE '%s';",
                          funName, selected.type.list, typeName, funBody, localdf, localcall, language)
    return (createStmt)
}
