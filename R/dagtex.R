#' Create a new DAG
#'
#' @return
#' @export
#'
#' @examples
dagtex <- function(...) {
  structure(
    list(
      nodes = list(),
      edges = list(),
      latex = list(),
      texPreview_options = list(...)
    ),
    class = "dagtex"
  )
}


