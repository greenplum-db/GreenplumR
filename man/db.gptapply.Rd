\name{db.gptapply}
\alias{db.gptapply}

\title{
  Data inside GPDB is group by a selected index. 
  A R function is applied to every row of grouped data.
}

\description{
  GPDB will use the PL/Containe or PL/R to run the input R function. 
  An R-wrapper of function will be created as an UDF inside GPDB. 
  The calculation can be done on parallel. 
}

\usage{
db.gptapply(X, INDEX, FUN = NULL, output.name = NULL, output.signature = NULL,
            clear.existing = FALSE, case.sensitive = FALSE,
            output.distributeOn = NULL, debugger.mode = FALSE,
            runtime.id = "plc_r_shared", language = "plcontainer", 
            input.signature = NULL, ...)
}

\arguments{
  \item{\dots}{
    The parameter of input function
  }

  \item{X}{
    db.data.frame
  }

  \item{INDEX}{
    The index
  }

  \item{FUN}{
    The input function
  }

  \item{output.name}{
    The name of output table
  }

  \item{output.signature}{
    The parameter of output table
    e.g. output.signature <- list(id = 'int', 'Sex' = 'text', 'Length' = 'float', height = 'float', shell = 'float')
  }

  \item{input.signature}{
      The parameter to match the applying function arguments and table columns' name
      The pair is function arguments name = table column name
      e.g. input.signature <- list('arg_f_1' = 'arg_t_1', 'arg_f_2' = 'arg_t_2')
      NOTICE: The order of both arguments must not change
  }

  \item{case.sensitive}{
    Whether output.name, colnames of input tables, etc. are case sensitive
  }

  \item{clear.existing}{
    whether clear existing table stored in db before executing the query
  }

  \item{output.distributeOn}{
    Specify how output table is stored in database
  }

  \item{debugger.mode}{
    Set to TRUE if you want to print the executed SQL internally.
  }

  \item{runtime.id}{
    Used by plcontainer only. The runtime id is set by plcontainer to specify a runtime cnofiguration.
    See plcontainer for more information.
    e.g. plc_r_shared
  }

  \item{language}{
    language used in database
    e.g. plcontainer
  }
}

\value{
  A \code{data.frame} that contains the result if the result is not empty. Otherwise, it returns a logical value, which indicates whether the SQL query has been sent to the database successfully.
}

\author{
  Author: Pivotal Inc.
}

\examples{
\dontrun{
    db.gptapply(X = dbDF, 
      "id", 
      FUN = function, 
      output.signature = list(id = 'int', 'Sex' = 'text'),
      clear.existing = FALSE, 
      case.sensitive = FALSE,
      output.distributeOn = id,
      ...)
}
}

\keyword{utility}
\keyword{database}
