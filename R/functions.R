
#' @title Extract loaded packages from a text file
#' @description
#' Reads and searchs a specified text file for occurrences of "library()"
#' and "p_load()" calls, returns package names.
#'
#' @details
#' Note: the following packages are excluded from the result:
#' - pacman
#' @param scope The file to be read.
#' @return character(n)
find_used_packages <- function(scope) {
  pkg_lines <- grep("^(library\\(|p_load\\()", readLines(scope), value = TRUE)
  pkg_names <- vapply(pkg_lines, FUN.VALUE = character(1), FUN = function(x) {
    regmatches(x, gregexpr("(?<=\\().*(?=\\))", x, perl = TRUE))[[1]]
  })
  pkg_names <- unique(sort(pkg_names))
  pkg_names <- setdiff(pkg_names, c("pacman"))
  return(pkg_names)
}

#' @title Helper function: print used packages\
#'
#' @description
#' Function to print a character vector of packages names in a nice way
#' (max 3 packages in a row). Assumes that 'pacman' is used for loading / installing.
#' @param packages character(n)
print_used_packages <- function(packages) {
  packages <- sprintf("\"%s\"", packages)
  wrapped <- character()
  for (i in seq_len(ceiling(length(packages) / 3)))
    wrapped[i] <- toString(packages[(i * 3 - 2):min((i * 3), length(packages))])

  wrapped <- paste(wrapped, collapse = ",\n\t")
  wrapped <- paste0("\t", wrapped)
  final <- sprintf("# install.packages(\"pacman\")\npacman::p_load(\n%s\n)", wrapped)
  cat(final)
}
