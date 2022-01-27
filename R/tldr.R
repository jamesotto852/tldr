#' Short form, tl;dr style documentation in the console
#'
#' Similar to `?` and `help()`
#'
#' @param topic A name or character string specifying documentation goal
#'
#' @return
#' @export
#'
#' @examples
#' tldr(tldr)
#' tldr("tldr")
#' tldr(palmerpenguins::penguins)
#'
tldr <- function(topic) {
  # Right now, doing everything with substitute + eval
  # Would rather do w/ newer tools
  topicExpr <- substitute(topic)

  if (is.call(topicExpr) && (topicExpr[[1L]] == "::" || topicExpr[[1L]] == ":::")) {
    package <- as.character(topicExpr[[2L]])
    topicExpr <- topicExpr[[3L]]
  }

  # Accept either name or string specifying topic
  topic <- if (is.name(topicExpr)) as.character(topicExpr) else topic
  package <- NULL

  invisible(eval(substitute(tldr_help(TOPIC, package = PACKAGE),
                         list(TOPIC = topic, PACKAGE = package))))
}

tldr_path <- function(path, topic) {
  # looking in PKG/man_tldr/ for files w/ the name topic.Rd
  # Before package installation, they hang out in /inst/man_tldr/
  file.path(path, "man_tldr", paste0(topic, ".Rd"))
}

tldr_exists <- function(path, topic) file.exists(tldr_path(path, topic))

tldr_help <- function(topic, package) {
  dirs <- if (is.null(package)) find.package(loadedNamespaces()) else find.package(package)
  hits <- vapply(dirs, tldr_exists, logical(1), topic)

  if (all(!hits)) stop("Topic not found")

  Rd <- tldr_path(dirs[min(which(hits))], topic)
  Rd <- tools::parse_Rd(Rd)
  Rd2tldr(Rd)
}


