% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_methods.R
\name{apply_methods}
\alias{apply_methods}
\alias{apply_methods.list}
\alias{apply_methods.benchmark_tbl}
\alias{apply_methods.tbl_df}
\alias{apply_metrics}
\alias{begin_benchmark}
\title{Apply methods}
\usage{
apply_methods(x, fn_list, name = NULL, suppress.messages = TRUE)

\method{apply_methods}{list}(x, fn_list, name = NULL, suppress.messages = TRUE)

\method{apply_methods}{benchmark_tbl}(x, fn_list, name = NULL, suppress.messages = TRUE)

\method{apply_methods}{tbl_df}(x, fn_list, name = NULL, suppress.messages = TRUE)

apply_metrics(x, fn_list, name = NULL, suppress.messages = TRUE)

begin_benchmark(x, fn_list, name = NULL, suppress.messages = TRUE)
}
\arguments{
\item{x}{the list of data or benchmark tibble to apply methods to}

\item{fn_list}{the list of methods to be applied}

\item{name}{(optional) the name of the column for methods applied}

\item{suppress.messages}{TRUE if messages from running methods should be
suppressed}
}
\value{
benchmark_tbl object containing results from methods applied, the
  first column is the name of the dataset as factors, middle columns contain
  method names as factors and the final column is a list of results of
  applying the methods.
}
\description{
apply_methods() and its aliases apply_metrics and begin_benchmark take either
lists of datasets or benchmark_tbl objects and applies a list of functions.
The output is a benchmark_tbl where each method has been applied to each
dataset or preceeding result.
}
\examples{
# list of data
datasets <- list(
    set1 = rnorm(500, mean = 2, sd = 1),
    set2 = rnorm(500, mean = 1, sd = 2)
)

# list of functions
add_noise <- list(
    none = identity,
    add_bias = function(x) { x + 1 }
)

res <- apply_methods(datasets, add_noise)

}
\seealso{
\code{\link{time_methods}}
}
