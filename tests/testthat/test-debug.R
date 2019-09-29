context("Test matrix of gpapply")

## ----------------------------------------------------------------------
## Test preparations

# Need valid 'pivotalr_port' and 'pivotalr_dbname' values
env <- new.env(parent = globalenv())
#.dbname = get('pivotalr_dbname', envir=env)
#.port = get('pivotalr_port', envir=env)
.verbose <- TRUE

.host <- Sys.getenv('PGHOST', 'localhost')
.dbname <- "debug_apply"
.port <- strtoi(Sys.getenv('PGPORT'))
if (is.na(.port))
    stop("PGPORT not set")
.language <- tolower(Sys.getenv('GPRLANGUAGE'))
if (.language != 'plr' && .language != 'plcontainer')
    stop(paste0("invalid GPRLANGUAGE:", .language))
## connection ID
cid <- db.connect(host = .host, port = .port, dbname = .dbname, verbose = .verbose)
.nrow.test <- 10

dat <- abalone[c(1:.nrow.test), ]

tname.mul.col <- 'mul_Col_Table'
db.q('DROP SCHEMA IF EXISTS test_Schema CASCADE;', verbose = .verbose)
db.q('DROP SCHEMA IF EXISTS "test_Schema" CASCADE;', verbose = .verbose)
db.q('CREATE SCHEMA test_Schema;', verbose = .verbose)
db.q('CREATE SCHEMA "test_Schema";', verbose = .verbose)
db.q(paste('DROP TABLE IF EXISTS "', tname.mul.col, '";', sep = ''), verbose = .verbose)

# drop-create extension
db.q(paste0('DROP EXTENSION IF EXISTS ', .language, ' CASCADE;'))
db.q(paste0('CREATE EXTENSION ', .language, ';'))
# prepare test table
print(names(dat))
# [1] "id"       "sex"      "length"   "diameter" "height"   "whole"
# [7] "shucked"  "viscera"  "shell"    "rings"
names(dat) <- c('Id', 'Sex', 'Length', 'length', 'Height', 'height', 'shucked', 'Shucked', 'shell', 'Rings')
dat.mul <- as.db.data.frame(dat, table.name = tname.mul.col, verbose = .verbose)


# ---------------------------------------------------------------
# prepare data
# ---------------------------------------------------------------
test_that("Test prepare", {
    expect_equal(is.db.data.frame(dat.mul), TRUE)
    expect_equal(nrow(dat.mul), .nrow.test)
    expect_equal(ncol(dat.mul), ncol(dat))

    expect_equal(db.existsObject(tname.mul.col, conn.id = cid), TRUE)

    res <- db.q("SELECT nspname FROM pg_namespace WHERE nspname = 'test_schema';",
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), 1)

    res <- db.q("SELECT nspname FROM pg_namespace WHERE nspname = 'test_Schema';",
                verbose = .verbose)
    expect_equal(is.data.frame(res), TRUE)
    expect_equal(nrow(res), 1)

    res <- db.q(paste0("SELECT 1 FROM pg_extension WHERE extname='", .language, "';"))
    expect_equal(is.data.frame(res) && nrow(res) == 1, TRUE)
})

dat.test <- dat.mul

test_that("Debug MT gpapply", {
    .output.name <- 'debugNormal'
    .signature <- list('Id'='int', 'Sex'='text', 'Length'='float', 'sLength'='float')
    .func <- function(X) {
        X$length <- X$length + 1
        X$Length <- X$Length + 100
        return (X[, c(1:4)])
    }
    res <- db.gpapply(dat.test, output.name = .output.name,
                      FUN = .func, output.signature = .signature,
                      clear.existing = TRUE, case.sensitive = TRUE,
                      debugger.mode = TRUE, language = .language)
    expect_equal(res, NULL)
})

test_that("Debug MT-junk gpapply", {
    .output.name <- 'debugJunk'
    .signature <- list('Id'='int', 'Sex'='text', 'Length'='float', 'sLength'='float')
    .func <- function(X, junk1, junk2) {
        print(junk1)
        print(junk2)
        X$length <- X$length + 1
        X$Length <- X$Length + 100
        return (X[, c(1:4)])
    }
    res <- db.gpapply(dat.test, output.name = .output.name,
                      FUN = .func, output.signature = .signature,
                      clear.existing = TRUE, case.sensitive = TRUE,
                      debugger.mode = TRUE, language = .language,
                      junk1 = 12, junk2 = 'Helo')
    expect_equal(res, NULL)
})

.index <- 'Rings'
test_that("Debug MT gptapply", {
    .output.name <- 'debugNormalT'
    .signature <- list('Id'='int', 'Sex'='text', 'Length'='float', 'sLength'='float')
    .func <- function(X) {
        X$length <- X$length + 1
        X$Length <- X$Length + 100
        return (X[, c(1:4)])
    }
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                      FUN = .func, output.signature = .signature,
                      clear.existing = TRUE, case.sensitive = TRUE,
                      debugger.mode = TRUE, language = .language)
    expect_equal(res, NULL)
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                      FUN = .func, output.signature = .signature,
                      clear.existing = TRUE, case.sensitive = FALSE,
                      debugger.mode = TRUE, language = .language)
    expect_equal(res, NULL)
})

test_that("Debug MT-junk gptapply", {
    .output.name <- 'debugJunkT'
    .signature <- list('Id'='int', 'Sex'='text', 'Length'='float', 'sLength'='float')
    .func <- function(X, junk1, junk2) {
        print(junk1)
        print(junk2)
        X$length <- X$length + 1
        X$Length <- X$Length + 100
        return (X[, c(1:4)])
    }
    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                      FUN = .func, output.signature = .signature,
                      clear.existing = TRUE, case.sensitive = TRUE,
                      debugger.mode = TRUE, language = .language,
                      junk1 = 12, junk2 = 'Helo')
    expect_equal(res, NULL)

    res <- db.gptapply(dat.test, INDEX = .index, output.name = .output.name,
                      FUN = .func, output.signature = .signature,
                      clear.existing = TRUE, case.sensitive = FALSE,
                      debugger.mode = TRUE, language = .language,
                      junk1 = 12, junk2 = 'Helo')
    expect_equal(res, NULL)
})

db.disconnect(cid)
