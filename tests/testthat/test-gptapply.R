context("Test matrix of gptapply")

## ----------------------------------------------------------------------
## Test preparations

# Need valid 'pivotalr_port' and 'pivotalr_dbname' values
env <- new.env(parent = globalenv())
#.dbname = get('pivotalr_dbname', envir=env)
#.port = get('pivotalr_port', envir=env)
.verbose <- FALSE

.host <- 'localhost'
.dbname <- "d_tapply"
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
.dat.1 <- as.data.frame(dat$rings)
names(.dat.1) <- c('Rings')
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
.signature <- list("Rings" = "int")
.index <- "Rings"
fn.inc <- function(x)
{
    #should be case sensitive here 
    return (x$Rings + 1)
    #return (x[1] + 1)
}
# -----------------------------------------------------------
# ONE COLUMN TABLE
# -----------------------------------------------------------
test_that("Test output.name is NULL", {
    .output.name <- NULL

    # case sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    res2 <- db.q(paste("SELECT count(1) FROM \"", tname.1.col, "\" GROUP BY \"", .index, "\";", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res) && is.data.frame(res2), TRUE)
    expect_equal(nrow(res), nrow(res2))
    expect_equal(ncol(res), ncol(dat.test))

    # case non-sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(res2))
    expect_equal(ncol(res), ncol(res2))

    # clear.existing can be FALSE, or any other values, since output.name is NULL
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = FALSE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(res2))
    expect_equal(ncol(res), ncol(res2))
})

# output.name is not NULL, and it is a single table name
test_that("Test output.name is a table name", {
    .output.name <- 'result_GPTapply'

    # case sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"", .output.name, "\"",
                " WHERE \"Rings\" IS NOT NULL;", sep = ""),
                verbose = .verbose)
    res2 <- db.q(paste("SELECT count(1) FROM \"", tname.1.col, "\" GROUP BY \"", .index, "\";", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res) && is.data.frame(res2), TRUE)
    expect_equal(nrow(res), nrow(res2))

    # case non-sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE,
                    language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Rings IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(res2))
})

# output.name is not NULL, and it is a single table name
test_that("Test output.name is schema.table", {
    .output.name <- 'test_schema.resultGPTapply'

    # case sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"test_schema\".\"resultGPTapply\" WHERE \"Rings\" IS NOT NULL;"),
                verbose = .verbose)
    res2 <- db.q(paste("SELECT count(1) FROM \"", tname.1.col, "\" GROUP BY \"", .index, "\";", sep = ""))
    expect_equal(is.data.frame(res) && is.data.frame(res2), TRUE)
    expect_equal(nrow(res), nrow(res2))

    # case non-sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Rings IS NOT NULL;", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(res2))
})

test_that("Test output.name is invalid name", {
    # TODO: this invalid parameter should throw an exception
    .output.names <- list('"ab"', '"b.c"', 'public.ab.cd')
    for(name in .output.names) {
        tryCatch({
            db.gptapply(dat.test, INDEX = .index, output.name = name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
            stop("can't be here")
        }, error = function(e) {
            expect_match(as.character(e), "invalid output.name:")
        })
    }
})

# -------------------------------------------------------------------------
# index
# -------------------------------------------------------------------------
test_that("Test INDEX is invalid", {
    # index exceed number of columns
    .test.index <- 3
    .output.name <- 'result_gptapply_temp'
    tryCatch({
        db.gptapply(dat.test, INDEX = .test.index, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), "INDEX value cannot exceed")
    })

    # INDEX column name does not exist in col.name
    .test.index <- "invalidIndex"
    tryCatch({
        db.gptapply(dat.test, INDEX = .test.index, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), "invalid INDEX value:")
    })
})

# -------------------------------------------------------------------------
# output.signature
# -------------------------------------------------------------------------
test_that("Test output.signature", {
    .output.name <- 'aBc'
    # 0. output.signature is a list, skip.
    # 1. output.signature is NULL
    tryCatch({
        res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
                    output.signature = NULL)
        stop("shouldn't be here")
    }, error = function(e) {
        expect_match(as.character(e), "NULL signature, not impl")
    })

    # 2. output.signature is a function
    f.sig <- function() return(.signature)
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Rings IS NOT NULL;", sep = ""), verbose = .verbose)
    res2 <- db.q(paste("SELECT count(1) FROM \"", tname.1.col, "\" GROUP BY \"", .index, "\";", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res) && is.data.frame(res2), TRUE)
    expect_equal(nrow(res), nrow(res2))

    # 3. data type is not supported
    tryCatch({
        f.sig <- function() return(list("Rings" = "invalidType"))
        res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'ERROR:  type "invalidtype" does not exist')
    })
    # 4. output.signature is any other invalid value
    tryCatch({
        res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
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
        res <- db.gptapply(dat.test, INDEX = .index, output.name = NULL, output.signature = .signature,
                        FUN = 'bad_function')
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), "FUN must be a function")
    })
    # 2. FUN is an anonymous function
    .output.name <- 'test_FUNC'
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = function(x) x[1] + 1, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Rings IS NOT NULL;", sep = ""),
                verbose = .verbose)
    res2 <- db.q(paste("SELECT count(1) FROM \"", tname.1.col, "\" GROUP BY \"", .index, "\";", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res) && is.data.frame(res2), TRUE)
    expect_equal(nrow(res), nrow(res2))
    # 3. FUN references outer environment
    tryCatch({
        db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
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
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)
    #1 clear.existing is TRUE, when the table exists        (OK)
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
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

    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = FALSE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)

    #3 clear.existing is FALSE, when the table exists       (ERROR)
    tryCatch({
        db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
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
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, output.distributeOn = 'randomly',
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Rings IS NOT NULL;", sep = ""),
                verbose = .verbose)
    res2 <- db.q(paste("SELECT count(1) FROM \"", tname.1.col, "\" GROUP BY \"", .index, "\";", sep = ""))
    expect_equal(is.data.frame(res) && is.data.frame(res2), TRUE)
    expect_equal(nrow(res), nrow(res2))
    # replicated
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, output.distributeOn = 'replicated',
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Rings IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(res2))
    # columns
    .sql <- "SELECT attname FROM pg_class, gp_distribution_policy gp, pg_attribute pa"
    .sql <- paste(.sql, " WHERE pg_class.oid=gp.localoid and pg_class.relname = '", sep = "")
    .sql <- paste(.sql, tolower(.output.name),
            "' and pa.attrelid=pg_class.oid and pa.attnum=ANY(gp.distkey);", sep = "")
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, output.distributeOn = list(names(.signature)[1]),
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
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = .func, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE,
                    language = .language, junk1 = 12, junk2 = "Hello",
                    junk3 = list(id = 1, name = "world"))
    expect_equal(res, NULL)
    res <- db.q(paste('SELECT 1 FROM "', .output.name,
                '" WHERE "Rings" IS NOT NULL;', sep = ''),
                verbose = .verbose)
    res2 <- db.q(paste("SELECT count(1) FROM \"", tname.1.col, "\" GROUP BY \"", .index, "\";", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res) && is.data.frame(res2), TRUE)
    expect_equal(nrow(res), nrow(res2))
})

# --------------------------------------------------------------------------
# MULTIPLE COLUMNS TABLE
# --------------------------------------------------------------------------
dat.test <- dat.mul
# columns as the output table
.col.chooser <- c(1,2,3,10)
#.signature <- list(id = 'int', 'Sex' = 'text', 'Rings' = 'int', height = 'float', shell = 'float')
.signature <- list(id = 'int', sex = 'text', 'Length' = 'float', 'Rings' = 'int')

fn.inc <- function(x)
{
    # x$length <- x$length + 1
    # x$height <- x$height - 1
    x$id <- x$rings
    x$length <- mean(x$length)
    return (x[, c(1,2,3,10)])
}

# output.name is not NULL, and it is a single table name
test_that("MT-Test output.name is a table name", {
    .output.name <- NULL

    # case sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), .nrow.test)
    expect_equal(ncol(res), length(.signature))

    # case non-sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE,
                    language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), .nrow.test)
    expect_equal(ncol(res), length(.signature))
})

# output.name is not NULL, and it is a single table name
test_that("MT-Test output.name is a table name", {
    .output.name <- 'result_GPTapply'

    # case sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"", .output.name,
                "\" WHERE \"Rings\" IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

    # case non-sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE,
                    language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Rings IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
})

test_that("MT-Test output.name is invalid name", {
    # TODO: this invalid parameter should throw an exception
    .output.names <- list('"ab"', '"b.c"', 'public.ab.cd')
    for(name in .output.names) {
        tryCatch({
            db.gptapply(dat.test, INDEX = .index, output.name = name,
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
        res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
                    output.signature = NULL)
        stop("shouldn't be here")
    }, error = function(e) {
        expect_match(as.character(e), "NULL signature, not impl")
    })

    # 2. output.signature is a function
    f.sig <- function() return(.signature)
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name,
                " WHERE Rings IS NOT NULL;", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))

    # 3. data type is not supported
    tryCatch({
        f.sig <- function()
            return(list(id = 'int', sex = 'text', length = 'invalidtype', height = 'float', shell = 'float'))
        res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'ERROR:  type "invalidtype" does not exist')
    })

    # 4. output.signature is any other invalid value
    tryCatch({
        res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
                    output.signature = list(), clear.existing = TRUE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'ERROR:  ')
    })

    # 5. output.signature has duplicate elements, case not sensitive
    .sig <- list('A'='int', a='text')
    tryCatch({
        res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
                    output.signature = .sig, case.sensitive = FALSE, clear.existing = FALSE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'duplicated signature:')
    })

    # 6. output.signature has duplciate elements, case-sensitive
    .sig <- list(id = 'int', 'ID' = 'text', 'Length' = 'float', 'Rings' = 'int')
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, FUN = fn.inc,
                output.signature = .sig, case.sensitive = TRUE, clear.existing = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT * FROM \"", .output.name,
                "\" WHERE \"Length\" IS NOT NULL;", sep = ""), verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
})

test_that("MT-Test Function applyed to data", {
    # 0. FUN is NULL, or FUN is a simple function, skip
    # 1. FUN is a non-function
    tryCatch({
        res <- db.gptapply(dat.test, INDEX = .index, output.name = NULL, output.signature = .signature,
                        FUN = 'bad_function')
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), "FUN must be a function")
    })
    # 2. FUN is an anonymous function
    .output.name <- 'test_FUNC'
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = function(x) x[, c(1, 2, 3, 10)], output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Rings IS NOT NULL;", sep = ""),
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
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)
    #1 clear.existing is TRUE, when the table exists        (OK)
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
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

    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                FUN = fn.inc, output.signature = .signature,
                clear.existing = FALSE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)

    #3 clear.existing is FALSE, when the table exists       (ERROR)
    tryCatch({
        db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
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
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, output.distributeOn = 'randomly',
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Rings IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    # replicated
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, output.distributeOn = 'replicated',
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Rings IS NOT NULL;", sep = ""),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
    # columns
    .case.sensitive <- TRUE
    .sql <- "SELECT attname FROM pg_class, gp_distribution_policy gp, pg_attribute pa"
    .sql <- paste(.sql, " WHERE pg_class.oid=gp.localoid and pg_class.relname = '", sep = "")
    .sql <- paste(.sql, ifelse(.case.sensitive, .output.name, tolower(.output.name)),
            "' and pa.attrelid=pg_class.oid and pa.attnum=ANY(gp.distkey);", sep = "")
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name, output.distributeOn = as.list(names(.signature)[c(1:3)]),
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
        return (x[, c(1, 2, 3, 10)])
    }
    # case sensitive
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = .func, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE,
                    language = .language, junk1 = 12, junk2 = "Hello",
                    junk3 = list(id = 1, name = "world"))
    expect_equal(res, NULL)
    res <- db.q(paste('SELECT 1 FROM "', .output.name,
                '" WHERE "Rings" IS NOT NULL;', sep = ''),
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.test))
})

# --------------------------------------------------------------------------
# ORIGINAL DATA HAS DUPLICATED COLUMNS (case.sensitive)
# --------------------------------------------------------------------------

dat <- data.frame(iD = c(1,2,2,3,3), ID = c("a","b","ab","c","ad"))
tname <- "dup"
db.q("DROP TABLE IF EXISTS dup;")
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
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat))
    expect_equal(ncol(res), ncol(dat))

    #case not sensitive
    .signature <- list("a" = "int", "B" = "text")
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                    FUN = fn.inc, output.signature = .signature,
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat))
    expect_equal(ncol(res), ncol(dat))
})

db.disconnect(cid)
