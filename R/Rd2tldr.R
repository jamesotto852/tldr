#' @import cli
#' @import crayon
NULL

# this script contains unexported functions which deal with
# the object returned from tools::parse_Rd()

RdTags <- get("RdTags", asNamespace("tools"))


Rd2tldr <- function(Rd, package) {
  top_level_tags <- RdTags(Rd)

  type <- if ("\\docType" %in% top_level_tags) Rd[[which(top_level_tags == "\\docType")]] else "function"

  name <- Rd[[which(top_level_tags == "\\name")]]
  title <- Rd[[which(top_level_tags == "\\title")]]

  aliases <- if ("\\alias" %in% top_level_tags) Rd[which(top_level_tags == "\\alias")] else NULL
  arguments <- if ("\\arguments" %in% top_level_tags) Rd[[which(top_level_tags == "\\arguments")]] else NULL
  details <- if ("\\details" %in% top_level_tags) Rd[[which(top_level_tags == "\\details")]] else NULL

  Rd2tldr_name(name)
  Rd2tldr_aliases(aliases, type, package)
  Rd2tldr_title(title)

  if (!is.null(arguments)) Rd2tldr_arguments(arguments)
  if (!is.null(details)) Rd2tldr_details(details)

  invisible(NULL)
}

### helper functions to work on elements of parsed Rd:


## deal with "header"

Rd2tldr_name <- function(Rd) {
  cli_h1(Rd)
}

Rd2tldr_aliases <- function(Rd, type, package) {
  aliases <- unlist(Rd)
  if (type == "function") aliases <- paste0(aliases, "()")
  aliases <- paste(package, aliases, sep = "::", collapse = ", ")

  cli_text(aliases)
}

Rd2tldr_title <- function(Rd) {
  cli_h2(Rd)
}


## deal with argument tag

# set up nested list to populate with args
Rd2tldr_arguments <- function(Rd) {

  # Only want nested lists (non-nested are just "\\n")
  keep <- vapply(Rd, is.list, logical(1))
  Rd <- Rd[keep]

  cli_li("Useful Arguments:")
  ul <- cli_ul()
  lapply(Rd, Rd2tldr_arguments_item)
  cli_end(ul)

  cli_text()
}

# deal with individual items in the arguments tag
Rd2tldr_arguments_item <- function(Rd) {
  arg <- Rd[[1]]
  desc <- Rd[[2]]

  cli_li("{blue(arg)}: {desc}")
}


## deal with details tag
Rd2tldr_details <- function(Rd) {

  # Only want non-newline elements
  keep <- vapply(Rd, function(x) x != "\n", logical(1))
  Rd <- Rd[keep]

  cli_li("Common Tasks:")
  ul <- cli_ul()
  lapply(Rd, Rd2tldr_details_item)
  cli_end(ul)

  cli_text()
}

# Rd2tldr_details(Rd[[13]])

# Deal with individual items in the details tag
Rd2tldr_details_item <- function(Rd) {

  # Lots of cases, depends on Rd_tag:
  switch(attr(Rd, "Rd_tag"),
         "TEXT" = Rd2tldr_details_item_text(Rd),
         "\\code" = Rd2tldr_details_item_code(Rd),
  )

}

# Deal with different types of tags for detail items:
Rd2tldr_details_item_text <- function(Rd) {
  cli_li(Rd)
}

Rd2tldr_details_item_code <- function(Rd) {
  Rd <- paste0("    ", Rd[[1]])
  cli_code(Rd)
}
