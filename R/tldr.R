#' Short form, tl;dr style documentation in the console
#'
#' Similar to `?` and `help()`
#'
#' @param topic A name or character string specifying documentation topic
#' @paramtldr topic A name or character string specifying documentation topic
#'
#' @exampletldr Several ways to bring up documentation on a topic:
#' \code{tldr(tldr)}
#' \code{tldr('tldr')}
#' \code{tldr(tldr::tldr)}
#'
#' @export
#'
#' @examples
#' tldr(tldr)
#' tldr("tldr")
tldr <- function(topic) {
  topicExpr <- substitute(topic)
  package <- NULL

  if (is.call(topicExpr)) {
    if (topicExpr[[1L]] == "::" || topicExpr[[1L]] == ":::") {
      package <- as.character(topicExpr[[2L]])
      topicExpr <- topicExpr[[3L]]
    } else {
      # an attempt at parsing input like tldr(tldr())
      topicExpr <- topicExpr[[1L]]

      # REALLY abusing short circuit here:
      if (!is.symbol(topicExpr) && (topicExpr[[1L]] == "::" || topicExpr[[1L]] == ":::")) {
        package <- as.character(topicExpr[[2L]])
        topicExpr <- topicExpr[[3L]]
      }
    }
  }

  # Accept either name or string specifying topic
  topic <- if (is.name(topicExpr)) as.character(topicExpr) else topic

  package <- if (is.null(package)) tldr_package(topic)

  invisible(eval(substitute(tldr_help(TOPIC, package = PACKAGE),
                         list(TOPIC = topic, PACKAGE = package))))
}

tldr_path <- function(path, topic) {
  # looking in PKG/tldr/ for files w/ the name topic.Rd
  # Before package installation, they hang out in /inst/tldr/
  file.path(path, "tldr", paste0(topic, ".Rd"))
}


tldr_help <- function(topic, package) {
  package <- if (is.null(package)) tldr_package(topic) else package
  dir <- find.package(package)

  Rd_path <- tldr_path(dir, topic)
  if (!file.exists(Rd_path)) stop("Topic not found", call. = FALSE)

  Rd <- tools::parse_Rd(Rd_path)
  Rd2tldr(Rd, package)
}

tldr_package <- function(topic) {
  dirs <- find.package(loadedNamespaces())
  hits <- vapply(dirs, tldr_exists, logical(1), topic)

  # Look in tldrDocs as last resort
  if (all(!hits)) {
    return("tldrDocs")
  }

  # Need a better way to pick the directory if multiple hits
  dir <- dirs[min(which(hits))]

  regmatches(dir, regexpr("[^(/|\\)]*$", dir))
}

tldr_exists <- function(path, topic) file.exists(tldr_path(path, topic))
