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

  package <- if (is.null(package)) tldr_package(topic) else package

  invisible(eval(substitute(tldr_help(TOPIC, package = PACKAGE),
                         list(TOPIC = topic, PACKAGE = package))))
}

tldr_path <- function(path, topic) {
  # looking in PKG/tldr/ for files w/ the name topic.Rd
  # Before package installation, they hang out in /inst/tldr/

  # Hash topic if there are special characters
  topic <- tldr_encode(topic)

  file.path(path, "tldr", paste0(topic, ".Rd"))
}


tldr_help <- function(topic, package) {

  # Need original value of package for error message
  package_parsed <- if (is.null(package)) tldr_package(topic) else package

  # If package is a base package, need to instead look at tldrDocs:
  if (package_parsed %in% c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")) {
    package_parsed <- "tldrDocs"
  }

  dir <- find.package(package_parsed)

  Rd_path <- tldr_path(dir, topic)

  if (file.exists(Rd_path)) {

    Rd <- tools::parse_Rd(Rd_path)
    Rd2tldr(Rd, package)

  } else {

    # Format input for cli
    if (is.null(package) || package == "tldrDocs") {
      input <- paste0("{.code ", topic, "}")
    } else {
      input <- paste0("{.fun ", package, "::", topic, "}")
    }

    cli_alert_danger(paste0("Topic not found (", input, ")"))
    invisible(NULL)

  }

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


# If there are special characters topic needs to be hashed
tldr_encode <- function(topic) {
  # Remove escape characters from .Rd code:
  topic <- gsub("\\\\", "", topic)

  special <- grepl("[^a-zA-Z0-9\\._]", topic)
  topic[special] <- vapply(topic[special], digest::digest, character(1), algo = "md5")
  topic[special] <- paste0("tldr_", topic[special])

  topic
}



