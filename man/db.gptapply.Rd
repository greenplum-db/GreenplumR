\name{db.gptapply}
\alias{db.gptapply}

\title{
  Execute a SQL query
}
\description{
  This function sends SQL queries into the connected database to execute, and then extracts the result if there is any.
}
\usage{
db.gptapply(X, INDEX, FUN = NULL, output.name=NULL, output.signature=NULL,
		clear.existing=FALSE, case.sensitive=FALSE,output.distributeOn=NULL,debugger.mode = FALSE, simplify = TRUE, ...)
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

\value{
  A \code{data.frame} that contains the result if the result is not empty. Otherwise, it returns a logical value, which indicates whether the SQL query has been sent to the database successfully.
}

\author{
  Author: Pivotal Inc.
}

\examples{
\dontrun{
    gptapply(X=dbDF, "ID", function, output.signature=output_signature, ...)
}
}

\keyword{utility}
\keyword{database}
