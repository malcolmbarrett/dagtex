dagtex <- function() {
  structure(
    list(
      nodes = list(),
      edges = list(),
      latex = list()
    ),
    class = "dagtex"
  )
}

print.dagtex <- function(x, ...) {
  is_empty_dag <- all(purrr::map_lgl(x, purrr::is_empty))
  if (is_empty_dag) cat("An empty DAG") else print.default(x, ...)
  invisible(x)
}
