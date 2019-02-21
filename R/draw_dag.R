plot_dagtex <- function(.dag, density = 320, ...) {

  latex_code <- get_latex_code(.dag, add_header = FALSE)

  if (knitr::is_latex_output()) return(knitr::asis_output(latex_code))

  tikz_opts <- '\\usetikzlibrary{positioning, calc, shapes.geometric,
  shapes.multipart, shapes, arrows.meta, arrows, decorations.markings,
  external, trees}'

  pkg_opts <- texPreview::build_usepackage(pkg = 'tikz', uselibrary = tikz_opts)

  if (knitr::is_html_output()) {
    return(
      texPreview::tex_preview(
        latex_code,
        usrPackages = pkg_opts,
        density = density,
        resizebox = FALSE,
        returnType = "html5",
        ...)
    )
  }

  texPreview::tex_preview(
    latex_code,
    usrPackages = pkg_opts,
    density = density,
    ...)
}

plot.dagtex <- function(...) {
  plot_dagtex(...)
}

insert_latex <- function(.dag, ...) {
  .dag$latex <- c(.dag$latex, ...)

  .dag
}

tikz_picture <- function(..., scale = NULL, scale_x = NULL, scale_y = NULL) {
  # TODO: should just be options?
  dag_scale <- paste0(
    "[",
    ifelse(is.null(scale), "", paste0("scale=", scale)),
    ifelse(is.null(scale_x), "", paste0("xscale=", scale_x)),
    ifelse(is.null(scale_y), "", paste0("yscale=", scale_y)),
    "]"
  )

  if (dag_scale == "[]") dag_scale <- NULL

  begin_tikzpicture <- paste0("\\begin{tikzpicture}", dag_scale)


  paste(
    c(
      begin_tikzpicture,
      "\\tikzset{>=latex}",
      "\\tikzstyle{Arrow} = [->, thick, preaction = {decorate}]",
      "\\tikzstyle{DoubleArrow} = [<->, thick, preaction = {decorate}]",
      ...,
      "\\end{tikzpicture}"
    ),
    collapse = "\n"
  )
}

get_latex_code <- function(.dag, add_header = TRUE) {

  latex_code <- tikz_picture(latexify_dag(.dag))

  if (add_header) {
    tikz_opts <- '\\usetikzlibrary{positioning, calc, shapes.geometric,
    shapes.multipart, shapes, arrows.meta, arrows, decorations.markings,
    external, trees}'

    pkg_opts <- texPreview::build_usepackage(pkg = 'tikz', uselibrary = tikz_opts)
    latex_code <- paste(c(pkg_opts, latex_code), collapse = "\n")
  }

  structure(latex_code, class = "latex_code")
}

print.latex_code <- function(x, ...) {
  cat(x, ...)
}

latexify_dag <- function(.dag) {
  if (any_swig_nodes(.dag)) {}
  nodes_latex <- purrr::map_chr(.dag$nodes, latexify_node)
  edges_latex <- purrr::map_chr(.dag$edges, latexify_edge)
  nodes_and_edges <- c(nodes_latex, edges_latex)

  paste(nodes_and_edges, collapse = "\n")
}

latexify_node <- function(.node) {
  if (.node$is_swig) return(latexify_swig(.node))

  node_options <- compile_node_options(.node)
  node_id <- paste0("(", .node$id, ") ")
  node_text <- paste0("{", .node$name, "}")


  paste0(
    "\\node",
    node_options,
    node_id,
    node_text,
    ";"
  )
}

compile_node_options <- function(.node) {
  shape <- ifelse(
    .node$shape != "",
    paste0(.node$shape, ", draw"),
    ""
  )

  compiled_options <- paste0(shape, .node$position, .node$options, collapse = ", ")

  node_options <- paste0("[", compiled_options, "] ")
  if (node_options == "[] ") node_options <- NULL
  node_options
}

latexify_swig <- function(.node) {

  node_options <- paste0("[", .node$id, "]")
  if (node_options == "[]") node_options <- ""
  node_id <- paste0("(", .node$id, ")")
  node_text <- paste0("{", .node$name, "}")


  paste(
    "\\node",
    node_options,
    node_id,
    node_text,
    ";"
  )
}

latexify_edge <- function(.edge) {

  edge_from <- paste0(" (", .edge$from, ") ")
  edge_to <- paste0("(", .edge$to, ")")
  edge_options <- compile_edge_options(.edge)

  if (.edge$is_curved) {
    line_code <- "to "
    line_curve <- ifelse(
      .edge$curve == "up",
      "[out=25, in=160] ",
      "[out=-25, in=-160] "
    )
  } else {
    line_code <- "-- "
    line_curve <- NULL
  }


  paste0(
    "\\draw",
    edge_options,
    edge_from,
    line_code,
    line_curve %||% "",
    .edge$annotate %||% "",
    edge_to,
    ";"
  )
}

compile_edge_options <- function(.edge) {
  linetype <- ifelse(
    .edge$linetype != "solid",
    .edge$linetype,
    ""
  )

  arrow_type <- ifelse(.edge$is_double_arrow, "DoubleArrow", "Arrow")

  compiled_options <- paste0(arrow_type, linetype, .edge$options, collapse = ", ")

  edge_options <- paste0("[", compiled_options, "]")
  edge_options
}
