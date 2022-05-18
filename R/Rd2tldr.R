#' @import cli
#' @importFrom crayon blue
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
  format <- if ("\\format" %in% top_level_tags) Rd[[which(top_level_tags == "\\format")]] else NULL

  # Rd2tldr_name(name)
  Rd2tldr_title(title)
  Rd2tldr_aliases(aliases, type, package)

  if (!is.null(arguments)) Rd2tldr_arguments(arguments)
  if (!is.null(details)) Rd2tldr_details(details)
  if (!is.null(format)) Rd2tldr_format(format)

  invisible(NULL)
}

### helper functions to work on elements of parsed Rd:


## deal with "header"

Rd2tldr_name <- function(Rd) {
  cli_h1(Rd)
}

Rd2tldr_aliases <- function(Rd, type, package) {
  aliases <- unlist(Rd)

  # Special handling for base objects w/ docs
  if ("tldrDocs" %in% package)  {

    # Want first element of find()
    package <- vapply(aliases, function(x) rev(find(x))[1], character(1))

    # First 8 characters are "package:"
    package <- substring(package, 9)

  }

  if (type == "function" & package[1] != "datasets") aliases <- paste0(aliases, "()")


  aliases <- paste(package, aliases, sep = "::", collapse = ", ")

  cli_text(aliases)
  cli_text()
}

Rd2tldr_title <- function(Rd) {
  # cli_h2(Rd)
  cli_h1(Rd)
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
  # Only want \\code + non-newline elements
  # Mild abuse of short-circuit
  keep <- vapply(Rd, function(x) attr(x, "Rd_tag") == "\\code" ||x != "\n", logical(1))
  Rd <- Rd[keep]

  cli_li("Common Tasks:")
  ul <- cli_ul()
  lapply(Rd, Rd2tldr_details_item)
  cli_end(ul)

  cli_text()
}

# Deal with individual items in the details tag
Rd2tldr_details_item <- function(Rd) {
  # Lots of cases, depends on Rd_tag:
  # Could add support for markup style `code`
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
  # Recursion for multi-line contents
  if (length(Rd) > 1) {
    len <- length(Rd)

    # Trim surrounding newlines
    if (Rd[[len]] == "\n") Rd[[len]] <- NULL
    if (Rd[[1]] == "\n") Rd[[1]] <- NULL

    return(lapply(Rd, Rd2tldr_details_item_code))
  }

  Rd <- paste0("    ", Rd[[1]])
  cli_code(Rd)
}



## deal with format tag
Rd2tldr_format <- function(Rd) {

  # First, get text description
  chars <- vapply(Rd, is.character, logical(1))
  desc <- paste0(Rd[chars], collapse = "")
  desc <- gsub("\\n", "", desc)

  # Getting first instance of \describe{} (if it exists)
  describe_indeces <- which(vapply(Rd, function(x) attr(x, "Rd_tag"), "character") == "\\describe")

  if (any(describe_indeces)) {

    Rd <- Rd[[min(describe_indeces)]]

    # Getting \item elements
    item_indeces <- which(vapply(Rd, function(x) attr(x, "Rd_tag"), "character") == "\\item")
    Rd <- Rd[item_indeces]

    if (nchar(desc) == 0) {
      cli_li("Data Format:")
    } else {
      cli_li(desc)
    }

    # If there are \\item tags, deal w/ them
    if (length(Rd) > 0) {
      ul <- cli_ul()
      lapply(Rd, Rd2tldr_format_item)
      cli_end(ul)
    }

  } else {
    cli_li(desc)
  }

  cli_text()
}

Rd2tldr_format_item <- function(Rd) {
  item <- Rd[[1]]
  desc <- Rd[[2]]

  cli_li("{blue(item)}: {desc}")
}



