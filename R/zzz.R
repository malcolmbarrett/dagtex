.onLoad <- function(libname = find.package("dagtex"), pkgname = "dagtex") {
  load_packages <- getOption("dagtex.load_tikz", default = TRUE)
  if (load_packages) {
    knitr::knit_meta_add(
      list(
        rmarkdown::latex_dependency_tikz(
          c("positioning",
            "calc",
            "shapes.geometric",
            "shapes.multipart",
            "shapes",
            "arrows.meta",
            "arrows",
            "decorations.markings",
            "external",
            "trees")
        )
      )
    )
  }
}
