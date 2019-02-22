#' Add edges
#'
#' @param .dag
#' @param .from
#' @param .to
#' @param start_position
#' @param end_position
#' @param .options
#' @param is_curved
#' @param curve
#' @param is_double_arrow
#' @param linetype
#' @param annotate
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' @rdname add_edges
add_edge <- function(.dag, .from, .to, start_position = NULL, end_position = NULL,
                     .options = NULL, is_curved = FALSE, curve = "up",
                     is_double_arrow = FALSE, linetype = "solid",
                     annotate = NULL, ...) {

  id <- count_edges(.dag) + 1

  if (is.character(.from)) .from <- get_id(.dag, .from)
  if (is.character(.to)) .to <- get_id(.dag, .to)

  add_edge_to_dag(
    .dag = .dag,
    .id = id,
    .from = .from,
    .to = .to,
    start_position = start_position,
    end_position = end_position,
    .options = .options,
    is_curved = is_curved,
    curve =  curve,
    is_double_arrow = is_double_arrow,
    linetype = linetype,
    annotate = annotate
  )
}

#' @export
#' @rdname add_nodes
add_curved_edge <- function(.dag, .from, .to, start_position = NULL, end_position = NULL,
                     .options = NULL, curve = "up",
                     is_double_arrow = FALSE, linetype = "solid",
                     annotate = NULL, ...) {
  add_edge(.dag = .dag, .from = .from, .to = .to, start_position = start_position,
           end_position = end_position, .options = .options, is_curved = TRUE,
           curve = curve, is_double_arrow = is_double_arrow, linetype = linetype,
           annotate = annotate, ...)
}

add_edge_to_dag <- function(.dag, .id, .from, .to, start_position = NULL,
                            end_position = NULL, .options = NULL, is_curved = FALSE,
                            curve = "up", is_double_arrow = FALSE, linetype = "solid",
                            annotate = NULL) {

  .from <- process_position(.from, start_position)
  .to <- process_position(.to, end_position)


  edge <- structure(
    list(
      id = .id,
      from = .from,
      to = .to,
      is_curved = is_curved,
      curve =  curve,
      options = .options,
      linetype = linetype,
      is_double_arrow = is_double_arrow,
      annotate = annotate
    ),
    class = "dagtex_edge"
  )


  .dag$edges[[.id]] <- edge

  .dag
}


#' @export
#' @rdname add_nodes
annotate_edge <- function(text, placement = "midway", position = "above") {
  paste0("node[", placement, ", ", position, "]{", text, "}")
}

count_edges <- function(.dag) length(.dag$edges)

process_position <- function(target, position) {
  ifelse(
    !is.null(position),
    paste0(target, ".", position),
    target
  )
}

get_id <- function(.dag, .var) {
   node_names <- purrr::map_chr(.dag$nodes, "name")
   node_ids <- purrr::map_dbl(.dag$nodes, "id")
   node_index <- which(node_names == .var)
   node_ids[node_index]
}
