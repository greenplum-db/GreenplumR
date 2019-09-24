context("Test matrix of gpapply")

## ----------------------------------------------------------------------
## Test preparations

# Need valid 'pivotalr_port' and 'pivotalr_dbname' values
env <- new.env(parent = globalenv())
#.dbname = get('pivotalr_dbname', envir=env)
#.port = get('pivotalr_port', envir=env)
.verbose <- FALSE

.host <- 'localhost'
.dbname <- "d_apply"
.port <- 15432
.language <- tolower(Sys.getenv('GPRLANGUAGE'))
if (.language != 'plr' && .language != 'plcontainer')
    stop(paste0("invalid GPRLANGUAGE:", .language))
## connection ID
cid <- db.connect(host = .host, port = .port, dbname = .dbname, verbose = .verbose)
.nrow.test <- 10

dat <- abalone[c(1:.nrow.test), ]

tname.1.col <- 'one_Col_Table'
tname.mul.col <- 'mul_Col_Table'
db.q('DROP SCHEMA IF EXISTS test_Schema CASCADE;', verbose = .verbose)
db.q('DROP SCHEMA IF EXISTS "test_Schema" CASCADE;', verbose = .verbose)
db.q(paste('DROP TABLE IF EXISTS "', tname.1.col, '";', sep = ''), verbose = .verbose)
db.q(paste('DROP TABLE IF EXISTS "', tname.mul.col, '";', sep = ''), verbose = .verbose)
db.q('CREATE SCHEMA test_Schema;', verbose = .verbose)
db.q('CREATE SCHEMA "test_Schema";', verbose = .verbose)

# drop-create extension
db.q(paste0('DROP EXTENSION IF EXISTS ', .language, ' CASCADE;'))
db.q(paste0('CREATE EXTENSION ', .language, ';'))
# prepare test table
.dat.1 <- as.data.frame(dat$height)
names(.dat.1) <- c('Height')
dat.name <- names(dat)
dat.name[2] <- toupper(dat.name[2])
names(dat) <- dat.name
dat.1 <- as.db.data.frame(.dat.1, table.name = tname.1.col, verbose = .verbose)
dat.mul <- as.db.data.frame(dat, table.name = tname.mul.col, verbose = .verbose)


# ---------------------------------------------------------------
# prepare data
# ---------------------------------------------------------------
test_that("Test prepare", {
    expect_equal(is.db.data.frame(dat.1), TRUE)
    expect_equal(is.db.data.frame(dat.mul), TRUE)
    expect_equal(nrow(dat.1), .nrow.test)
    expect_equal(ncol(dat.1), 1)
    expect_equal(nrow(dat.mul), .nrow.test)
    expect_equal(ncol(dat.mul), ncol(dat))

    expect_equal(db.existsObject(tname.1.col, conn.id = cid), TRUE)
    expect_equal(db.existsObject(tname.mul.col, conn.id = cid), TRUE)

    res <- db.q("SELECT nspname FROM pg_namespace WHERE nspname = 'test_schema';",
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), 1)

    res <- db.q(paste0("SELECT 1 FROM pg_extension WHERE extname='", .language, "';"))
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)
})

# test table has only one column
dat.test <- dat.1
.signature <- list("Score" = "float")
fn.inc <- function(x)
{
    return (x[1] + 1)
}
# ---------------------------------------------------------------
# ONE COLUMN TABLE
# ---------------------------------------------------------------
# output.name is NULL
test_that("Test output.name is NULL", {
    .output.name <- NULL

    # case sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    expect_equal(ncol(res), ncol(dat.test))

    # case non-sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    expect_equal(ncol(res), ncol(dat.test))

    # clear.existing can be FALSE, or any other values, since output.name is NULL
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = FALSE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    expect_equal(ncol(res), ncol(dat.test))
})

# output.name is not NULL, and it is a single table name
test_that("Test output.name is a table name", {
    .output.name <- 'result_GPapply'

    # case sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"", .output.name,
                "\" WHERE \"Score\" IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

    # case non-sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE,
                    language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Score IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
})

# output.name is not NULL, and it is a single table name
test_that("Test output.name is schema.table", {
    .output.name <- 'test_schema.resultGPapply'

    # case sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)

    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"test_schema\".\"resultGPapply\" WHERE \"Score\" IS NOT NULL;"),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

    # case non-sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Score IS NOT NULL;", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

})

test_that("Test output.name is invalid name", {
    # TODO: this invalid parameter should throw an exception
    .output.names <- list('"ab"', '"b.c"', 'public.ab.cd')
    for(name in .output.names) {
        tryCatch({
            db.gpapply(dat.test, output.name = name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
            stop("can't be here")
        }, error = function(e) {
            expect_match(as.character(e), "invalid output.name:")
        })
    }
})

# -------------------------------------------------------------------------
# output.signature
# -------------------------------------------------------------------------
test_that("Test output.signature", {
    .output.name <- 'aBc'
    # 0. output.signature is a list, skip.
    # 1. output.signature is NULL
    tryCatch({
        res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                    output.signature = NULL)
        stop("shouldn't be here")
    }, error = function(e) {
        expect_match(as.character(e), "NULL signature, not impl")
    })

    # 2. output.signature is a function
    f.sig <- function() return(.signature)
    res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Score IS NOT NULL;", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

    # 3. data type is not supported
    tryCatch({
        f.sig <- function() return(list("Score" = "invalidType"))
        res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'ERROR:  type "invalidtype" does not exist')
    })
    # 4. output.signature is any other invalid value
    tryCatch({
        res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                    output.signature = list(), clear.existing = TRUE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'ERROR:  ')
    })
})

test_that("Test Function applyed to data", {
    # 0. FUN is NULL, or FUN is a simple function, skip
    # 1. FUN is a non-function
    tryCatch({
        res <- db.gpapply(dat.test, output.name = NULL, output.signature = .signature,
                        FUN = 'bad_function')
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), "FUN must be a function")
    })
    # 2. FUN is an anonymous function
    .output.name <- 'test_FUNC'
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = function(x) x[1] + 1, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Score IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    # 3. FUN references outer environment
    tryCatch({
        db.gpapply(dat.test, output.name = .output.name,
                    FUN = function(x) return(fn.inc(x)), output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
        stop("cann't be here")
    }, error = function(e) {
        expect_match(as.character(e), "ERROR:  R interpreter expression evaluation error")
    })
})

test_that("Test clear.existing", {
    .output.name <- 'tab_existing'
    db.q(paste("DROP TABLE IF EXISTS ", .output.name, ";", sep = ""), verbose = .verbose)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname ='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 0, TRUE)

    #0 clear.existing is TRUE, when the table doesn't exist (OK)
    res <- db.gpapply(dat.test, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)
    #1 clear.existing is TRUE, when the table exists        (OK)
    res <- db.gpapply(dat.test, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)

    #2 clear.existing is FALSE, when the table doesn't exist(OK)
    # clear existing table
    db.q(paste("DROP TABLE IF EXISTS ", .output.name, ";", sep = ""), verbose = .verbose)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 0, TRUE)

    res <- db.gpapply(dat.test, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = FALSE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)

    #3 clear.existing is FALSE, when the table exists       (ERROR)
    tryCatch({
        db.gpapply(dat.test, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = FALSE, case.sensitive = FALSE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), "the output table exists, but clear flag is not")
    })
})

# -----------------------------------------------------
# Skip case.sensitive, since it is fully tested
# -----------------------------------------------------

test_that("Test distributedOn", {
    .output.name <- 'testDistribute'
    # randomly
    res <- db.gpapply(dat.test, output.name = .output.name, output.distributeOn = 'randomly',
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Score IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    # replicated
    res <- db.gpapply(dat.test, output.name = .output.name, output.distributeOn = 'replicated',
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Score IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    # columns
    .sql <- "SELECT attname FROM pg_class, gp_distribution_policy gp, pg_attribute pa"
    .sql <- paste(.sql, " WHERE pg_class.oid=gp.localoid and pg_class.relname = '", sep = "")
    .sql <- paste(.sql, tolower(.output.name),
            "' and pa.attrelid=pg_class.oid and pa.attnum=ANY(gp.distkey);", sep = "")
    res <- db.gpapply(dat.test, output.name = .output.name, output.distributeOn = list(names(.signature)[1]),
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(.sql, verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), 1)
})

test_that("Test language", {
    # the language should be 'plr' or 'plcontainer'
    skip("skip.language")
})

test_that("Test additional junk parameters", {
    .output.name <- 'testJunkParameter'
    .func <- function(x, junk1, junk2, junk3) {
        return (x[1] + 1)
    }
    # case sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = .func, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE,
                    language = .language, junk1 = 12, junk2 = "Hello",
                    junk3 = list(id = 1, name = "world"))
    expect_equal(res, NULL)
    res <- db.q(paste('SELECT 1 FROM "', .output.name,
                '" WHERE "Score" IS NOT NULL;', sep = ''),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

})
# --------------------------------------------------------
# consistency of database objects
# --------------------------------------------------------
# the whole/most steps are
# Create Type => Create Function => Drop existing Table
#   => Create Table => Drop Type & Function
#
# When the function gpapply/gptapply returns success or with error,
# we should Run `DROP TYPE IF EXISTS "<GPTYPE>" CASCADE;`
# The created function is always dropped as a dependency of gptype_xxx.
test_that("Test consistency of database objects", {
    q.type <- "SELECT count(1) FROM pg_type WHERE typname like 'gptype_%';"
    q.func <- "SELECT count(1) FROM pg_proc WHERE proname like 'gprfunc_%';"

    q.type_func <- function() {
        res <- db.q(q.type, verbose = .verbose)
        expect_equal(is.data.frame(res), TRUE)
        n.type <- res[1, 1]
        res <- db.q(q.func, verbose = .verbose)
        expect_equal(is.data.frame(res), TRUE)
        n.func <- res[1, 1]
        return (list(type = n.type, func = n.func))
    }
    res <- q.type_func()
    old.type <- res$type
    old.func <- res$func

    tryCatch({
    .output.name <- 'testConsistency'
    .func <- function(x) stop("internal error by myself")
    db.gpapply(dat.test, output.name = .output.name,
                    FUN = .func, output.signature = .signature,
                    clear.existing = TRUE, language = .language)
    stop("can't be here")
    }, error = function(e) {
    }, finally = {
        res <- q.type_func()
        expect_equal(old.type, res$type)
        expect_equal(old.func, res$func)
    })

})

# --------------------------------------------------------------------------
# MULTIPLE COLUMNS TABLE
# --------------------------------------------------------------------------
dat.test <- dat.mul
# columns as the output table
.col.chooser <- c(1, 2, 3, 5, 9)
.signature <- list(id = 'int', 'Sex' = 'text', 'Length' = 'float', height = 'float', shell = 'float')
fn.inc <- function(x)
{
    x$length <- x$length + 1
    x$height <- x$height - 1
    return (x[, c(1, 2, 3, 5, 9)])
}
# output.name is NULL

test_that("MT-Test output.name is NULL", {
    .output.name <- NULL
    # case sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    expect_equal(ncol(res), length(.signature))

    # case non-sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    expect_equal(ncol(res), length(.signature))

    # clear.existing can be FALSE, or any other values, since output.name is NULL
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = FALSE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    expect_equal(ncol(res), length(.signature))
})

# output.name is not NULL, and it is a single table name
test_that("MT-Test output.name is a table name", {
    .output.name <- 'result_GPapply'

    # case sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"", .output.name,
                "\" WHERE \"Length\" IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

    # case non-sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE,
                    language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Length IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
})

# output.name is not NULL, and it is a single table name
test_that("MT-Test output.name is schema.table", {
    .output.name <- 'test_schema.resultGPapply'

    # case sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"test_schema\".\"resultGPapply\" WHERE \"Length\" IS NOT NULL;"),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

    # case non-sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Length IS NOT NULL;", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

})

test_that("MT-Test output.name is invalid name", {
    # TODO: this invalid parameter should throw an exception
    .output.names <- list('"ab"', '"b.c"', 'public.ab.cd')
    for(name in .output.names) {
        tryCatch({
            db.gpapply(dat.test, output.name = name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
            stop("can't be here")
        }, error = function(e) {
            expect_match(as.character(e), "invalid output.name:")
        })
    }
})

# -------------------------------------------------------------------------
# output.signature
# -------------------------------------------------------------------------
test_that("MT-Test output.signature", {
    .output.name <- 'aBc'
    # 0. output.signature is a list, skip.
    # 1. output.signature is NULL
    tryCatch({
        res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                    output.signature = NULL)
        stop("shouldn't be here")
    }, error = function(e) {
        expect_match(as.character(e), "NULL signature, not impl")
    })

    # 2. output.signature is a function
    f.sig <- function() return(.signature)
    res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Length IS NOT NULL;", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

    # 3. data type is not supported
    tryCatch({
        f.sig <- function()
            return(list(id = 'int', sex = 'text', length = 'invalidtype', height = 'float', shell = 'float'))
        res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'ERROR:  type "invalidtype" does not exist')
    })

    # 4. output.signature is any other invalid value
    tryCatch({
        res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                    output.signature = list(), clear.existing = TRUE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'ERROR:  ')
    })

    # 5. output.signature has duplicate elements, case not sensitive
    .sig <- list('A'='int', a='text')
    tryCatch({
        res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                    output.signature = .sig, case.sensitive = FALSE, clear.existing = FALSE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'duplicated signature:')
    })

    # 6. output.signature has duplciate elements, case-sensitive
    .sig <- list(id = 'int', 'ID' = 'text', 'Length' = 'float', height = 'float', shell = 'float')
    res <- db.gpapply(dat.test, output.name = .output.name, FUN = fn.inc,
                output.signature = .sig, case.sensitive = TRUE, clear.existing = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"", .output.name,
                "\" WHERE \"Length\" IS NOT NULL;", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
})

test_that("MT-Test Function applyed to data", {
    # 0. FUN is NULL, or FUN is a simple function, skip
    # 1. FUN is a non-function
    tryCatch({
        res <- db.gpapply(dat.test, output.name = NULL, output.signature = .signature,
                        FUN = 'bad_function')
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), "FUN must be a function")
    })
    # 2. FUN is an anonymous function
    .output.name <- 'test_FUNC'
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = function(x) x[, c(1, 2, 3, 5, 9)], output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Length IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    # 3. FUN references outer environment
    tryCatch({
        db.gpapply(dat.test, output.name = .output.name,
                    FUN = function(x) return(fn.inc(x)), output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
        stop("cann't be here")
    }, error = function(e) {
        expect_match(as.character(e), "ERROR:  R interpreter expression evaluation error")
    })
})

test_that("MT-Test clear.existing", {
    .output.name <- 'tab_existing'
    db.q(paste("DROP TABLE IF EXISTS ", .output.name, ";", sep = ""), verbose = .verbose)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname ='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 0, TRUE)

    #0 clear.existing is TRUE, when the table doesn't exist (OK)
    res <- db.gpapply(dat.test, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)
    #1 clear.existing is TRUE, when the table exists        (OK)
    res <- db.gpapply(dat.test, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)

    #2 clear.existing is FALSE, when the table doesn't exist(OK)
    # clear existing table
    db.q(paste("DROP TABLE IF EXISTS ", .output.name, ";", sep = ""), verbose = .verbose)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 0, TRUE)

    res <- db.gpapply(dat.test, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = FALSE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)

    #3 clear.existing is FALSE, when the table exists       (ERROR)
    tryCatch({
        db.gpapply(dat.test, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = FALSE, case.sensitive = FALSE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), "the output table exists, but clear flag is not")
    })
})

# -----------------------------------------------------
# Skip case.sensitive, since it is fully tested
# -----------------------------------------------------

test_that("MT-Test distributedOn", {
    .output.name <- 'testDistribute'
    # randomly
    res <- db.gpapply(dat.test, output.name = .output.name, output.distributeOn = 'randomly',
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Length IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    # replicated
    res <- db.gpapply(dat.test, output.name = .output.name, output.distributeOn = 'replicated',
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Length IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    # columns
    .case.sensitive <- TRUE
    .sql <- "SELECT attname FROM pg_class, gp_distribution_policy gp, pg_attribute pa"
    .sql <- paste(.sql, " WHERE pg_class.oid=gp.localoid and pg_class.relname = '", sep = "")
    .sql <- paste(.sql, ifelse(.case.sensitive, .output.name, tolower(.output.name)),
            "' and pa.attrelid=pg_class.oid and pa.attnum=ANY(gp.distkey);", sep = "")
    res <- db.gpapply(dat.test, output.name = .output.name, output.distributeOn = as.list(names(.signature)[c(1:3)]),
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = .case.sensitive, language = .language)
    expect_equal(res, NULL)
    res <- db.q(.sql, verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), 3)
})

test_that("MT-Test additional junk parameters", {
    .output.name <- 'testJunkParameter'
    .func <- function(x, junk1, junk2, junk3) {
        x$length <- x$length + 1
        x$height <- x$height - 1
        return (x[, c(1, 2, 3, 5, 9)])
    }
    # case sensitive
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = .func, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE,
                    language = .language, junk1 = 12, junk2 = "Hello",
                    junk3 = list(id = 1, name = "world"))
    expect_equal(res, NULL)
    res <- db.q(paste('SELECT 1 FROM "', .output.name,
                '" WHERE "Length" IS NOT NULL;', sep = ''),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

})
# --------------------------------------------------------
# consistency of database objects
# --------------------------------------------------------
# the whole/most steps are
# Create Type => Create Function => Drop existing Table
#   => Create Table => Drop Type & Function
#
# When the function gpapply/gptapply returns success or with error,
# we should Run `DROP TYPE IF EXISTS "<GPTYPE>" CASCADE;`
# The created function is always dropped as a dependency of gptype_xxx.
test_that("MT-Test consistency of database objects", {
    q.type <- "SELECT count(1) FROM pg_type WHERE typname like 'gptype_%';"
    q.func <- "SELECT count(1) FROM pg_proc WHERE proname like 'gprfunc_%';"

    q.type_func <- function() {
        res <- db.q(q.type, verbose = .verbose)
        expect_equal(is.data.frame(res), TRUE)
        n.type <- res[1, 1]
        res <- db.q(q.func, verbose = .verbose)
        expect_equal(is.data.frame(res), TRUE)
        n.func <- res[1, 1]
        return (list(type = n.type, func = n.func))
    }
    res <- q.type_func()
    old.type <- res$type
    old.func <- res$func

    tryCatch({
    .output.name <- 'testConsistency'
    .func <- function(x) stop("internal error by myself")
    db.gpapply(dat.test, output.name = .output.name,
                    FUN = .func, output.signature = .signature,
                    clear.existing = TRUE, language = .language)
    stop("can't be here")
    }, error = function(e) {
    }, finally = {
        res <- q.type_func()
        expect_equal(old.type, res$type)
        expect_equal(old.func, res$func)
    })

})


# --------------------------------------------------------------------------
# ORIGINAL DATA HAS DUPLICATED COLUMNS (case.sensitive)
# --------------------------------------------------------------------------

dat <- data.frame(iD = c(1,2,2,3,3), ID = c("a","b","ab","c","ad"))
tname <- "dup"
db.q("DROP TABLE IF EXISTS dup;", verbose = .verbose)
dat.test <- as.db.data.frame(dat, table.name = tname, verbose = .verbose)

fn.inc <- function(x)
{
    x$iD <- x$iD + 100
    return (x)
}

test_that("duplicated column table", {
    .output.name <- NULL
    #case sensitive
    .signature <- list("a" = "int", "A" = "text")
    .index <- "ID"
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat))
    expect_equal(ncol(res), ncol(dat))

    #case not sensitive
    .signature <- list("a" = "int", "B" = "text")
    res <- db.gpapply(dat.test, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat))
    expect_equal(ncol(res), ncol(dat))
})
db.disconnect(cid, verbose = .verbose)
