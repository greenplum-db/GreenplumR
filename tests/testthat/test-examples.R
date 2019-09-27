context("Unit test of functions used by gpapply/gptapply")

## ----------------------------------------------------------------------
## Test preparations

# Need valid 'pivotalr_port' and 'pivotalr_dbname' values
env <- new.env(parent = globalenv())
#.dbname = get('pivotalr_dbname', envir=env)
#.port = get('pivotalr_port', envir=env)

.host <- 'localhost'
.dbname <- "rtest"
.port <- 15432
.language <- tolower(Sys.getenv('GPRLANGUAGE'))
if (.language != 'plr' && .language != 'plcontainer')
    stop(paste0("invalid GPRLANGUAGE:", .language))
## connection ID
cid <- db.connect(host = .host, port = .port, dbname = .dbname, verbose = FALSE)

# drop-create extension
db.q(paste0('DROP EXTENSION IF EXISTS ', .language, ' CASCADE;'))
db.q(paste0('CREATE EXTENSION ', .language, ';'))

## data in the database
dat <- as.db.data.frame(abalone, conn.id = cid, verbose = FALSE)

random.name <- function() {
    getRandomNameList()
}

test_that("Create Type Test", {
    basename <- random.name()
    typeName <- .to.type.name(basename)
    create_type_str <- .create.type.sql(typeName, signature_list = list("x"="int", "y"="text", "z"="float"))
    db.q(create_type_str, verbose=FALSE)

    res <- db.q(sprintf("select typname from pg_type where typname=lower('%s');", typeName), verbose=FALSE)
    expect_equal(nrow(res), 1)

    drop <- paste('DROP TYPE ', typeName, ';', sep='')
    db.q(drop, verbose=FALSE)

    res <- db.q(sprintf("select typname from pg_type where typname=lower('%s');", typeName), verbose=FALSE)
    expect_equal(nrow(res), 0)
})

test_that(".simplify.signature ", {
    res <- .simplify.signature(list("x"="int"))
    expect_equal(res$x, 'int')

    res <- .simplify.signature(list("x"="int", "y"="text", "z"="float"))
    expect_equal(res$x, 'int')
    expect_equal(res$y, 'text')
    expect_equal(res$z, 'float')

    fs <- function() {
        function() {
            list("x"="int", "y"="text", "z"="float")
        }
    }
    res <- .simplify.signature(fs)
    expect_equal(res$x, 'int')
    expect_equal(res$y, 'text')
    expect_equal(res$z, 'float')

    res <- .simplify.signature(list("x"="int[]", "y"="text[]", "z"="float"))
    expect_equal(res$x, 'int[]')
    expect_equal(res$y, 'text[]')
    expect_equal(res$z, 'float')
})

test_that("Test .clear.existing.table", {
    # 1. Test if output.name is NULL
    .sql <- .clear.existing.table(output.name = NULL, clear.existing = FALSE)
    expect_equal(.sql, "")

    table.name <- sprintf("rand_%s", .unique.string())
    db.q(sprintf("CREATE TABLE %s(id int);", table.name), verbose = FALSE)
    # 2. output.name is not NULL
    .sql <- .clear.existing.table(output.name = table.name, clear.existing = TRUE)
    expect_match(.sql, "^DROP TABLE IF EXISTS rand_", all = FALSE)

    .sql <- tryCatch({
        .clear.existing.table(output.name = table.name, clear.existing = FALSE)
    }, error = function(cond) {
        return (as.character(cond))
    })
    expect_match(.sql, "the output table exists, but clear flag is not set", all = FALSE)

    db.q(sprintf("DROP TABLE %s;", table.name), verbose = FALSE)
})

test_that("Test .distribute.str", {
    .dist <- .distribute.str(NULL)
    expect_equal(.dist, '')
    expect_equal(.distribute.str(NULL), "")
    expect_equal(.distribute.str('randomly'), "DISTRIBUTED RANDOMLY")
    expect_equal(.distribute.str('RANDomly'), "DISTRIBUTED RANDOMLY")
    expect_equal(.distribute.str('replicated'), "DISTRIBUTED REPLICATED")
    expect_equal(.distribute.str('rePLICated'), "DISTRIBUTED REPLICATED")

    # randomName is seemed to be an invalid distributed string
    randomName <- random.name()
    .dist <- tryCatch({
        .distribute.str('invalid character')
    }, error = function(cond) {
        return (randomName)
    })
    expect_equal(.dist, randomName)

    # distributed by (a, b, c)
    .dist <- .distribute.str(list("Apple", "Banana", "Cannon"),
                            case.sensitive = FALSE)
    expect_equal(.dist, "DISTRIBUTED BY (Apple, Banana, Cannon)")
    # case sensitive
    .dist <- .distribute.str(list("Apple", "Banana", "Cannon"),
                            case.sensitive = TRUE)
    expect_equal(.dist, 'DISTRIBUTED BY ("Apple", "Banana", "Cannon")')
})

test_that("Test .create.r.wrapper", {
    basename <- random.name()
    sqrtFUN <- function(x) sqrt(x[[1]])
    .signature <- list("id"="int", "name"="text")
    X <- as.db.data.frame(abalone[c(1:4), c(3,2)], conn.id = cid, verbose = FALSE)
    param_list_str_with_type <- paste(names(.signature), .signature, collapse=", ")

    funName <- .to.func.name(basename)
    typeName <- .to.type.name(basename)
    runtime.id <- 'plc_r_poison'
    language <- .language
    Xattr <- attributes(X)
    .sql <- .create.r.wrapper2(basename = basename, FUN = sqrtFUN,
                                selected.type.list = .selected.type.list(Xattr),
                                selected.equal.list = .selected.equal.list(Xattr$.col.name),
                                args=list('hello'), runtime.id=runtime.id, language=language)

    L <- unlist(strsplit(.sql, split='\n'))

    expect_match(L[1], "^CREATE FUNCTION gprfunc_.* RETURNS SETOF gptype_.*")
    expect_match(L[2], "# container: plc_r_poison")
    expect_match(L[length(L)], paste("LANGUAGE '", language, "';$", sep=""))

    # Create Type and Create Function
    db.q(.create.type.sql(typeName, signature_list=.signature), verbose=FALSE)
    db.q(.sql, verbose=FALSE)

    n.type <- db.q(paste("SELECT typname FROM pg_type WHERE typname=lower('", typeName, "');", sep=""), verbose=FALSE)
    n.func <- db.q(paste("SELECT proname FROM pg_proc WHERE proname=lower('", funName, "');", sep=""), verbose=FALSE)
    expect_equal(nrow(n.type), 1)
    expect_equal(nrow(n.func), 1)

    db.q(paste("DROP TYPE ", typeName, " CASCADE;", sep=""), verbose=FALSE)

    n.type <- db.q(paste("SELECT typname FROM pg_type WHERE typname=lower('", typeName, "');", sep=""), verbose=FALSE)
    n.func <- db.q(paste("SELECT proname FROM pg_proc WHERE proname=lower('", funName, "');", sep=""), verbose=FALSE)
    expect_equal(nrow(n.type), 0)
    expect_equal(nrow(n.func), 0)
})

# ----------------------------------------------------------------------
# To make the computation results available to later test_that
# need to do the calculation on the upper level
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

test_that("Examples of testing string existence", {
    tmp <- dat
    tmp$new.col <- 1
    expect_match(names(tmp), "new.col", all = FALSE)
})

# ----------------------------------------------------------------------
# Same test, different results on different platforms
# ----------------------------------------------------------------------

test_that("Different results on different platforms", {
    res <- as.character(db.q("select version()", conn.id = cid, verbose = FALSE))
    if (.get.dbms.str(cid)$db.str == "PostgreSQL") {
    expect_match(res, "PostgreSQL")
    } else {
    expect_match(res, "Greenplum")
    }
})


# ----------------------------------------------------------------------
# Clean up

db.disconnect(cid, verbose = FALSE)
