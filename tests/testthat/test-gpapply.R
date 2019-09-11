context("Test matrix of gpapply")

## ----------------------------------------------------------------------
## Test preparations

# Need valid 'pivotalr_port' and 'pivotalr_dbname' values
env <- new.env(parent = globalenv())
#.dbname = get('pivotalr_dbname', envir=env)
#.port = get('pivotalr_port', envir=env)

.dbname <- "d_apply"
.port <- 15432
.language <- 'plr'
## connection ID
cid <- db.connect(port = .port, dbname = .dbname, verbose = FALSE)
.nrow.test <- 10
dat <- abalone[c(1:.nrow.test),]

tname.1.col <- 'one_Col_Table'
tname.mul.col <- 'mul_Col_Table'
db.q(paste("DROP SCHEMA IF EXISTS ", "test_Schema", " CASCADE;", sep=''))
db.q(paste("DROP TABLE IF EXISTS \"", tname.1.col, "\";", sep = ''), verbose = FALSE)
db.q(paste("DROP TABLE IF EXISTS \"", tname.mul.col, "\";", sep = ''), verbose = FALSE)
.dat.1 <- as.data.frame(dat$height)
names(.dat.1) <- c('height')
.dat.mul <- dat
dat.1.col <- as.db.data.frame(.dat.1, table.name = tname.1.col, verbose = FALSE)
dat.mul.col <- as.db.data.frame(.dat.mul, table.name = tname.mul.col, verbose = FALSE)

fn.inc <- function(x) {
    return (x[1]+1)
}

# ---------------------------------------------------------------
# prepare data
# ---------------------------------------------------------------
test_that("Test prepare", {
    testthat::skip_on_cran()
    expect_equal(is.db.data.frame(dat.1.col), TRUE)
    expect_equal(is.db.data.frame(dat.mul.col), TRUE)
    expect_equal(nrow(dat.1.col), .nrow.test)
    expect_equal(ncol(dat.1.col), 1)
    expect_equal(nrow(dat.mul.col), .nrow.test)
    expect_equal(ncol(dat.mul.col), ncol(dat))

    expect_equal(db.existsObject(tname.1.col, conn.id = cid), TRUE)
    expect_equal(db.existsObject(tname.mul.col, conn.id = cid), TRUE)

    res <- db.q("CREATE SCHEMA test_Schema")
    expect_equal(res, NULL)
    res <- db.q("SELECT nspname FROM pg_namespace WHERE nspname='test_schema';")
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), 1)
})

# ---------------------------------------------------------------
# ONE COLUMN TABLE
# ---------------------------------------------------------------
# output.name is NULL
test_that("Test output.name is NULL", {
    .output.name <- NULL

    # case sensitive
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = fn.inc, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))
    expect_equal(ncol(res), ncol(dat.1.col))

    # case non-sensitive
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = fn.inc, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))
    expect_equal(ncol(res), ncol(dat.1.col))

    # clear.existing can be FALSE, or any other values, since output.name is NULL
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = fn.inc, output.signature = list("Score" = "float"),
                    clear.existing = FALSE, case.sensitive = TRUE, language = .language)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))
    expect_equal(ncol(res), ncol(dat.1.col))
})

# output.name is not NULL, and it is a single table name
test_that("Test output.name is a table name", {
    .output.name <- 'result_GPapply'

    # case sensitive
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = fn.inc, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"", .output.name, "\" WHERE \"Score\" IS NOT NULL;", sep=''))
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))

    # case non-sensitive
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = fn.inc, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Score IS NOT NULL;", sep=''))
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))
})

# output.name is not NULL, and it is a single table name
test_that("Test output.name is schema.table", {
    .output.name <- 'test_schema.resultGPapply'
    
    # case sensitive
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = fn.inc, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"test_schema\".\"resultGPapply\" WHERE \"Score\" IS NOT NULL;"))
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))

    # case non-sensitive
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = fn.inc, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Score IS NOT NULL;", sep=''))
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))

})

test_that("Test output.name is invalid name", {
    # TODO: this invalid parameter should throw an exception
    .output.names <- list('"ab"', "\"b.c\"", "public.ab.cd")
    for(name in .output.names) {
        tryCatch({
            db.gpapply(dat.1.col, output.name = name,
                    FUN = fn.inc, output.signature = list("Score" = "float"),
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
        res <- db.gpapply(dat.1.col, output.name = .output.name, FUN = fn.inc,
                    output.signature = NULL)
        stop("shouldn't be here")
    }, error = function(e) {
        expect_match(as.character(e), "NULL signature, not impl")
    })

    # 2. output.signature is a function
    f.sig <- function() return(list("Score" = "float"))
    res <- db.gpapply(dat.1.col, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Score IS NOT NULL;", sep=''))
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))

    # 3. data type is not supported
    tryCatch({
        f.sig <- function() return(list("Score" = "invalidType"))
        res <- db.gpapply(dat.1.col, output.name = .output.name, FUN = fn.inc,
                    output.signature = f.sig, clear.existing = TRUE, language = .language)
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), 'ERROR:  type "invalidtype" does not exist')
    })
    # 4. output.signature is any other invalid value
    tryCatch({
        res <- db.gpapply(dat.1.col, output.name = .output.name, FUN = fn.inc,
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
        res <- db.gpapply(dat.1.col, output.name = NULL, output.signature = list("Score"="float"),
                        FUN='bad')
        stop("can't be here")
    }, error = function(e) {
        expect_match(as.character(e), "FUN must be a function")
    })
    # 2. FUN is an anonymous function
    .output.name <- 'test_FUNC'
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = function(x)x[1]+1, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Score IS NOT NULL;", sep=''))
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))
    # 3. FUN references outer environment
    tryCatch({
        db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = function(x) return(fn.inc(x)), output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
        stop("cann't be here")
    }, error = function(e) {
        expect_match(as.character(e), "ERROR:  R interpreter expression evaluation error")
    })
})

test_that("Test clear.existing", {
    .output.name <- 'tab_existing'
    db.q(paste("DROP TABLE IF EXISTS ", .output.name, ";", sep=''))
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep=''))
    expect_equal(is.data.frame(res) && nrow(res) == 0, TRUE)

    #0 clear.existing is TRUE, when the table doesn't exist (OK)
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                FUN = function(x)x[1]+1, output.signature = list("Score" = "float"),
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep=''))
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)
    #1 clear.existing is TRUE, when the table exists        (OK)
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                FUN = function(x)x[1]+1, output.signature = list("Score" = "float"),
                clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep=''))
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)

    #2 clear.existing is FALSE, when the table doesn't exist(OK)
    # clear existing table
    db.q(paste("DROP TABLE IF EXISTS ", .output.name, ";", sep=''))
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep=''))
    expect_equal(is.data.frame(res) && nrow(res) == 0, TRUE)

    res <- db.gpapply(dat.1.col, output.name = .output.name,
                FUN = function(x)x[1]+1, output.signature = list("Score" = "float"),
                clear.existing = FALSE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM pg_class WHERE relname='", .output.name, "';", sep=''))
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)

    #3 clear.existing is FALSE, when the table exists       (ERROR)
    tryCatch({
        db.gpapply(dat.1.col, output.name = .output.name,
                FUN = function(x)x[1]+1, output.signature = list("Score" = "float"),
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
    res <- db.gpapply(dat.1.col, output.name = .output.name, output.distributeOn = 'randomly',
                    FUN = function(x)x[1]+1, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Score IS NOT NULL;", sep=''))
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))
    # replicated
    res <- db.gpapply(dat.1.col, output.name = .output.name, output.distributeOn = 'replicated',
                    FUN = function(x)x[1]+1, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = FALSE, language = .language)
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM ", .output.name, " WHERE Score IS NOT NULL;", sep=''))
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))
    # columns
    .sql <- "SELECT attname FROM pg_class pc, gp_distribution_policy gp, pg_attribute pa"
    .sql <- paste(.sql, " WHERE pc.oid=gp.localoid and pc.relname = '", sep='')
    .sql <- paste(.sql, .output.name, "' and pa.attrelid=pc.oid and pa.attnum=ANY(gp.distkey);")
})

test_that("Test language", {
    # the language should be 'plr' or 'plcontainer'
    skip("skip.language")
})

test_that("Test additional junk parameters", {
    .output.name <- 'testJunkParameter'
    .func <- function(x, junk1=NULL, junk2=NULL, junk3=NULL) {
        return (x[1]+1)
    }
    # case sensitive
    res <- db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = .func, output.signature = list("Score" = "float"),
                    clear.existing = TRUE, case.sensitive = TRUE, language = .language,
                    junk1 = 12, junk2 = "Hello", junk3 = list(id=1, name='world'))
    expect_equal(res, NULL)
    res <- db.q(paste("SELECT 1 FROM \"", .output.name, "\" WHERE \"Score\" IS NOT NULL;", sep=''))
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), nrow(dat.1.col))

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
        res <- db.q(q.type, verbose = FALSE)
        expect_equal(is.data.frame(res) && nrow(res)==ncol(res), TRUE)
        n.type <- nrow(res)
        res <- db.q(q.func, verbose = FALSE)
        expect_equal(is.data.frame(res) && nrow(res)==ncol(res), TRUE)
        n.func <- nrow(res)
        return (list(type=n.type, func=n.func))
    }
    res <- q.type_func()
    old.type <- res$type
    old.func <- res$func

    tryCatch({
    .output.name <- 'testConsistency'
    .func <- function(x) stop("internal error by myself")
    db.gpapply(dat.1.col, output.name = .output.name,
                    FUN = .func, output.signature = list("Score" = "float"),
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



db.disconnect(cid, verbose = FALSE)
