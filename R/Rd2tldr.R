#' @import cli
NULL

# this script contains unexported functions which deal with
# the object returned from tools::parse_Rd()

RdTags <- get("RdTags", asNamespace("tools"))


Rd2tldr <- function(Rd, package) {

  # TODO - allow user to define custom themes
  cli_div(theme = list(span.name = list(color = "cyan", "font-weight" = "bold")))

  top_level_tags <- RdTags(Rd)

  type <- if ("\\docType" %in% top_level_tags) Rd[[which(top_level_tags == "\\docType")]] else "function"

  name <- Rd[[which(top_level_tags == "\\name")]]
  title <- Rd[[which(top_level_tags == "\\title")]]

  aliases <- if ("\\alias" %in% top_level_tags) Rd[which(top_level_tags == "\\alias")] else NULL
  arguments <- if ("\\arguments" %in% top_level_tags) Rd[[which(top_level_tags == "\\arguments")]] else NULL
  format <- if ("\\format" %in% top_level_tags) Rd[[which(top_level_tags == "\\format")]] else NULL
  details <- if ("\\details" %in% top_level_tags) Rd[[which(top_level_tags == "\\details")]] else NULL

  # Rd2tldr_name(name)
  Rd2tldr_title(title)
  Rd2tldr_aliases(aliases, type, package)

  if (!is.null(arguments)) Rd2tldr_arguments(arguments)
  if (!is.null(format)) Rd2tldr_format(format)
  if (!is.null(details)) Rd2tldr_details(details)

  invisible(NULL)
}

### helper functions to work on elements of parsed Rd:


## deal with "header"

Rd2tldr_name <- function(Rd) {
  cli_h1(Rd)
}

Rd2tldr_title <- function(Rd) {
  cli_h1(Rd)
}

Rd2tldr_aliases <- function(Rd, type, package) {
  aliases <- unlist(Rd)

  # Special handling for base objects w/ docs
  if ("tldrDocs" %in% package)  {

    # Want first element of find()
    package <- vapply(aliases, function(x) rev(utils::find(x))[1], character(1))

    # First 8 characters are "package:"
    package <- substring(package, 9)

  }

  # TODO: add global option to disable hyperlinks
  if (type == "function" & package[1] != "datasets") {

    # Only want links for functions w/ non-special names:
    special <- grepl("[^a-zA-Z0-9\\._]", aliases)
    aliases[special] <- paste0(package, "::`", aliases[special], "`()")
    aliases[!special] <- paste0("{.fun ", package, "::", aliases[!special], "}")

    aliases <- paste0(aliases, collapse = ", ")

  } else {
    aliases <- paste(package, aliases, sep = "::", collapse = ", ")
  }

  # TODO: Below code is broken -- {.topic XXXX} doesn't always work for datasets
  if (FALSE) {
    class <- if (type == "function" & package[1] != "datasets") "{.fun " else "{.topic "
    aliases <- paste0(class, package, "::", aliases, "}", collapse = ", ")
  }

  cli_text(aliases)
  cli_text()
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

  # Add spaces b/w commas in arg
  arg <- gsub(",", ", ", arg)

  # Allow for missing desc
  if (length(desc) == 0) {
    cli_li("{.name {arg}}")
  } else {
    # Markup for descriptions
    desc <- Rd2tldr_TEXT_markup(desc)
    desc <- combine_run(desc)

    cli_li(paste0("{.name ", arg, "}: ", desc))
  }

}


## deal with details tag
Rd2tldr_details <- function(Rd) {

  # Only want \\code + non-newline elements (suffices to check first element)
  # Mild abuse of short-circuit
  Rd <- Filter(function(x) attr(x[1], "Rd_tag") == "\\code" || x[1] != "\n", Rd)

  # Format + combine links and text:
  Rd <- Rd2tldr_TEXT_markup(Rd)
  Rd <- combine_run(Rd)


  cli_li("Common Tasks:")
  ul <- cli_ul()

  # Special handling for first element, no extra linebreak
  Rd2tldr_details_item(Rd[[1]], first = TRUE)

  lapply(Rd[-1], Rd2tldr_details_item)
  cli_end(ul)

  cli_text()
}

# Deal with individual items in the details tag
Rd2tldr_details_item <- function(Rd, first = FALSE) {

  # Sometimes this is missing:
  if (is.null(attr(Rd, "Rd_tag"))) attr(Rd, "Rd_tag") <- "TEXT"

  switch(attr(Rd, "Rd_tag"),
    "TEXT" = Rd2tldr_details_item_text(Rd, first = first),
    "RCODE" = Rd2tldr_details_item_code(Rd),
    "\\code" = Rd2tldr_details_item_code(Rd)
  )

}

Rd2tldr_details_item_text <- function(Rd, first = FALSE) {
  # Spaces out examples
  # Could look at global option (TLDR_SPACING)
  if (!first) cli_text()

  cli_li(Rd)
}

Rd2tldr_details_item_code <- function(Rd) {

  # collapse multi-line contents
  if (length(Rd) > 1) {

    len <- length(Rd)

    # Trim surrounding newlines
    if (Rd[[len]] == "\n") Rd[[len]] <- NULL
    if (Rd[[1]] == "\n") Rd[[1]] <- NULL

    Rd <- paste0(unlist(Rd), collapse = "    ")

  }

  Rd <- paste0("    ", Rd[[1]])

  cli_code(Rd)

}


# deal with LaTeX style markup
Rd2tldr_TEXT_markup <- function(Rd) {

  lapply(Rd, Rd2tldr_TEXT_markup_item)

}

Rd2tldr_TEXT_markup_item <- function(Rd) {

  if (attr(Rd, "Rd_tag") %in% c("\\code", "TEXT")) return(Rd)

  res <- switch(attr(Rd, "Rd_tag"),
    "\\link" = Rd2tldr_TEXT_markup_item_link(Rd),
    "\\dfn" = Rd2tldr_TEXT_markup_item_dfn(Rd), # Direct link to FuNction (no derefence)
    "\\file" = Rd2tldr_TEXT_markup_item_file(Rd), # vignettes
    "\\href" = Rd2tldr_TEXT_markup_item_href(Rd),
    "\\url" = Rd2tldr_TEXT_markup_item_url(Rd),
    "\\emph" = Rd2tldr_TEXT_markup_item_emph(Rd),
    "\\bold" = Rd2tldr_TEXT_markup_item_bold(Rd)
  )

  attr(res, "Rd_tag") <- "TEXT"

  res

}


Rd2tldr_TEXT_markup_item_link <- function(Rd) {
  paste0("{.fun ", Rd, "}")
}

Rd2tldr_TEXT_markup_item_dfn <- function(Rd) {
  pkg <- strsplit(Rd[[1]], "::")[[1]][1]
  fn <- strsplit(Rd[[1]], "::")[[1]][2]

  paste0("{.help [{.fun ", fn, "}](", pkg, "::", fn, ")}")
}

Rd2tldr_TEXT_markup_item_file <- function(Rd) {
  # TODO .vignette from cli seems to fail with hyphenated vignette titles
  paste0("{.vignette ", Rd, "}")
}

Rd2tldr_TEXT_markup_item_href <- function(Rd) {
  paste0("{.href [", Rd[[2]], "](", Rd[[1]], ")}")
}

Rd2tldr_TEXT_markup_item_url <- function(Rd) {
  paste0("{.url ", Rd, "}")
}

Rd2tldr_TEXT_markup_item_emph <- function(Rd) {
  paste0("{.emph ", Rd, "}")
}

Rd2tldr_TEXT_markup_item_bold <- function(Rd) {
  paste0("{.strong ", Rd, "}")
}


combine_run <- function(Rd) {

  if (length(Rd) <= 1) return(Rd)

  # initialize out list; p = length of out
  out <- Rd[1]; p <- 1L

  # iterate over elements of Rd, growing out
  for (i in seq_along(Rd)[-1]) {

    ce <- Rd[[i]]   # current element
    le <- Rd[[i-1]] # last element

    # update depending on whether current elt and last were characters,
    # if not, grow the list
    if (attr(ce, "Rd_tag") %in% c("TEXT", "\\link") && attr(le, "Rd_tag") %in% c("TEXT", "\\link")) {
      out[[p]] <- paste0(out[[p]], ce)
      attr(out[[p]], "Rd_tag") <- "TEXT"
    } else {
      out <- c(out, ce)
      p <- p + 1L
    }
  }

  out
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

  cli_li("{.name {item}}: {desc}")
}



