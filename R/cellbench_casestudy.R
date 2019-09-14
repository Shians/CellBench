#' Open vignetted containing a case study using CellBench
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cellbench_case_study()
#' }
cellbench_case_study <- function() {
    browseURL(system.file("doc", "CellBenchCaseStudy.html", package = "CellBench"))
}
