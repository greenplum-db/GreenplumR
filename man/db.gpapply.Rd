\name{db.gpapply}
\alias{db.gpapply}

\title{
  Execute a SQL query
}
\description{
  This function sends SQL queries into the connected database to execute, and then extracts the result if there is any.
}
\usage{
db.gpapply(X, MARGIN = NULL, FUN = NULL, output.name = NULL, output.signature = NULL, clear.existing = FALSE,
                       case.sensitive = FALSE, output.distributeOn = NULL, runtime.id = "plc_r_shared", language = "plcontainer", ...)
}

\arguments{
  \item{\dots}{
    The parameter of input function
  }

  \item{X}{
    db.data.frame
  }

  \item{MARGIN}{
      margin, not used by db.gpapply
  }

  \item{FUN}{
    The function to apply each row of db.data.frame
  }

  \item{output.name}{
    The name of output table
  }

  \item{clear.existing}{
    whether clear existing table stored in db before executing the query
  }
  
  \item{output.signature}{
    The signature of output table, can be a list or a function that takes no parameters.
    e.g. output.signature <- list(id = 'int', 'Sex' = 'text', 'Length' = 'float', height = 'float', shell = 'float')
    or output.signature <- function() list(id = 'int')
  }

  \item{case.sensitive}{
    Whether output.name, colnames of input tables are case sensitive
  }

  \item{output.distributeOn}{
    Specify how output table is stored in database
  }

  \item{runtime.id}{
    runtime.id
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
