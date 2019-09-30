#' Open vignetted containing a case study using CellBench
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cellbench_case_study()
#' }
cellbench_case_study <- function() {
    browseURL(system.file("case-study", "CellBenchCaseStudy.html", package = "CellBench"))
}
