# `db.gpapply`: 
 Apply a R function to every row of data of a data frame object.


## Description


 gpapply allows you to run a R function with input data frame in GPDB through PL/container or PL/R language.
 The R function will be parsed, then an UDF will be created in GPDB schema for execution.
 The calculation will be done in parallel with computing resources of GPDB segment hosts.


## Usage

```r
db.gpapply(X, FUN = NULL, output.name = NULL, output.signature = NULL,
            clear.existing = FALSE, case.sensitive = FALSE, output.distributeOn = NULL,
            debugger.mode = FALSE, runtime.id = "plc_r_shared", language = "plcontainer", input.signature = NULL, ...)
```


## Arguments

Argument      |Description
------------- |----------------
```X```     |      type must be db.data.frame, the first argument of FUN
```FUN```     |      The function to apply each row of db.data.frame X
```MARGIN```     |      padding argument, can be ignored
```output.name```     |      The name of output table 
```clear.existing```     |      whether clear existing output table stored in GPD before executing the query
```output.signature```     |      The signature of output table, can be a list or a function that takes no parameters. e.g. output.signature <- list(id = 'int', 'Sex' = 'text', 'Length' = 'float', height = 'float', shell = 'float') or output.signature <- function() list(id = 'int') 
```case.sensitive```     |      Bool. Whether output.name and the colume names of input tables are case sensitive or not
```output.distributeOn```     |      Specify how output table is stored in database 
```debugger.mode```     |      TRUE to print the executed SQL internally. 
```runtime.id```     |      Used by "plcontainer" language only. The runtime id is set by plcontainer to specify a runtime cnofiguration. See plcontainer for more information. The argument type is string, e.g. "plc_r_shared" 
```language```     |      value should be "plcontainer" or "plr" 
```input.signature```     |      A list contains the name matching pattern between input table column name (i.e. parameter X) and apply function arugments if the type of apply function first parameter is not a data.frame. e.g. input.signature <- list('c1' = 'arg1')  

Other parameters: if the function FUN has extra argument other than X, you can append them after all of the required arguments list.

## Return Value


 A `data.frame` that contains the result if the result is not empty. Otherwise, it returns a logical value, which indicates whether the SQL query has been sent to the database successfully.




## Examples

```r 
db.gptapply(X = dbDF,
        FUN = funcToRun,
        output.signature = list(id = 'int', 'Sex' = 'text'),
        clear.existing = FALSE,
        case.sensitive = FALSE,
        output.distributeOn = 'id',
        runtime.id = "plc_r_shared",
        language = "plcontainer", arg1OfFUN, arg2OfFUN)
 ``` 

