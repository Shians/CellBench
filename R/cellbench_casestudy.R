#' Open vignetted containing a case study using CellBench
#'
#' @return opens a vignette containing a case study
#'
#' @examples
#' \dontrun{
#' cellbench_case_study()
#' }
#'
#' @importFrom utils browseURL
#'
#' @export
cellbench_case_study <- function() {
    browseURL(system.file("case-study", "CellBenchCaseStudy.html", package = "CellBench"))
}
