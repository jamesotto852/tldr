#' TEMP TITLE
#'
#' roxygen2 extension facilitating documenting via roxygen2::roxygenize(roclets = c("rd", "tldr_roclet"))
#'
#' @name tldr_roclet
#' @rdname tldr_roclet
NULL
# Right now, all files exported under same topic: "tldr_roclet"
# probably not the right thing to do



# Helper fun
"%||%" <- function(a, b) {
  if (length(a) > 0) a else b
}



# Special handling of tldr tags:
# (just hand off non-tldr tags to roxygen2::roxy_tag_rd)
#' @rdname tldr_roclet
#' @export
roxy_tag_rd_tldr <- function(x, base_path, env) {
  if (grepl("tldr", x$tag)) {
    UseMethod("roxy_tag_rd_tldr")
  }  else {
    roxygen2::roxy_tag_rd(x, base_path, env)
  }
}



# Everything but roclet_tldr should ignore tldr tags
# Need a pair of these methods for each tldr tag

# parameters
#' @rdname tldr_roclet
#' @importFrom roxygen2 roxy_tag_rd
#' @method roxy_tag_rd roxy_tag_paramtldr
#' @export
roxy_tag_rd.roxy_tag_paramtldr <- function(x, base_path, env) {
  NULL
}
#' @rdname tldr_roclet
#' @method roxy_tag_rd_tldr roxy_tag_paramtldr
#' @export
roxy_tag_rd_tldr.roxy_tag_paramtldr <- function(x, base_path, env) {
  roxygen2::rd_section("paramtldr", x$val)
}
# details
#' @rdname tldr_roclet
#' @method roxy_tag_rd roxy_tag_exampletldr
#' @export
roxy_tag_rd.roxy_tag_exampletldr <- function(x, base_path, env) {
  NULL
}
#' @rdname tldr_roclet
#' @method roxy_tag_rd_tldr roxy_tag_exampletldr
#' @export
roxy_tag_rd_tldr.roxy_tag_exampletldr <- function(x, base_path, env) {
  roxygen2::rd_section("exampletldr", x$val)
}



# tldr version of roxygen2:::block_to_rd.roxy_block
#' @rdname tldr_roclet
#' @export
block_to_rd_tldr <- function (block, base_path, env) {
  UseMethod("block_to_rd_tldr")
}


#' @rdname tldr_roclet
#' @method block_to_rd_tldr roxy_block
#' @export
block_to_rd_tldr.roxy_block <- function (block, base_path, env) {
  block <- roxygen2:::process_templates(block, base_path)
  if (!roxygen2:::needs_doc(block)) {
    return()
  }
  name <- roxygen2::block_get_tag(block, "name")$val %||% block$object$topic
  if (is.null(name)) {
    roxygen2::roxy_tag_warning(block$tags[[1]], "Missing name")
    return()
  }
  rd <- roxygen2:::RoxyTopic$new()
  roxygen2:::topic_add_name_aliases(rd, block, name)
  for (tag in block$tags) {
    rd$add(roxy_tag_rd_tldr(tag, env = env, base_path = base_path))
  }
  describe_rdname <- roxygen2:::topic_add_describe_in(rd, block, env)
  filename <- describe_rdname %||% roxygen2::block_get_tag(block, "rdname")$val %||%
    roxygen2:::nice_name(name)
  rd$filename <- paste0(filename, ".Rd")
  rd
}



# Set up tldr parameter tag:
#' @rdname tldr_roclet
#' @importFrom roxygen2 roxy_tag_parse
#' @method roxy_tag_parse roxy_tag_paramtldr
#' @export
roxy_tag_parse.roxy_tag_paramtldr <- function(x) {
  name <- regmatches(x$raw, regexpr("^\\S*", x$raw))
  desc <- substr(x$raw, nchar(name) + 2, nchar(x$raw))

  x$val <- list(
    name = name,
    desc = desc
  )

  x
}

#' @rdname tldr_roclet
#' @method format rd_section_paramtldr
#' @export
format.rd_section_paramtldr <- function(x, ...) {
  # Need to deal w/ multiple tags
  # x is a list w/ possibly duplicated names
  names <- x$value[names(x$value) == "name"]
  names <- unlist(names)

  descs <- x$value[names(x$value) == "desc"]
  descs <- unlist(descs)

  paste0(
    "\\arguments{\n",
    paste0("  \\item{", names, "}{", descs, "}\n", collapse = ""),
    "}\n"
  )
}

# Set up tldr details tag:
#' @rdname tldr_roclet
#' @method roxy_tag_parse roxy_tag_exampletldr
#' @export
roxy_tag_parse.roxy_tag_exampletldr <- function(x) {
  # text <- regmatches(test, regexpr("^.*?(?=\\\\code)", test, perl = TRUE))
  # code <- regmatches(test, regexpr("\\\\code.*", test))
  #
  # x$val <- list(
  #   name = name,
  #   desc = desc
  # )

  x$val <- x$raw

  x
}

#' @rdname tldr_roclet
#' @method format rd_section_exampletldr
#' @export
format.rd_section_exampletldr <- function(x, ...) {
  paste0(
    "\\details{\n",
    paste0(x$value, collapse = "\n"),
    "}\n"
  )
}



# Set up tldr_roclet
#' @rdname tldr_roclet
#' @export
tldr_roclet <- function() {
  roxygen2::roclet("tldr")
}


# Very similar to roxygen2:::roclet_process.roclet_rd
#' @rdname tldr_roclet
#' @importFrom roxygen2 roclet_process
#' @method roclet_process roclet_tldr
#' @export
roclet_process.roclet_tldr <- function(x, blocks, env, base_path) {
  results <- list()
  topics <- roxygen2:::RoxyTopics$new()

  for (block in blocks) {

    # Find all tags relevant to tldr:
    tldr_tags <- c("title", "alias", "docType", "exampletldr", "paramtldr")
    relevant_tags <- vapply(block$tags, function(x) x$tag %in% tldr_tags, logical(1))

    block$tags <- block$tags[relevant_tags]

    # rd <- roxygen2:::block_to_rd(block, base_path, env)
    rd <- block_to_rd_tldr(block, base_path, env)

    topics$add(rd)

  }

  topics$topics
}

#' @rdname tldr_roclet
#' @importFrom roxygen2 roclet_clean
#' @method roclet_clean roclet_tldr
#' @export
roclet_clean.roclet_tldr <- function(x, base_path) {
  rd <- dir(file.path(base_path, "inst", "tldr"), full.names = TRUE)
  rd <- rd[!file.info(rd)$isdir]
  unlink(purrr::keep(rd, roxygen2:::made_by_roxygen))
}

# Very similar to roxygen2:::roclet_output.roclet_rd
#' @rdname tldr_roclet
#' @importFrom roxygen2 roclet_output
#' @method roclet_output roclet_tldr
#' @export
roclet_output.roclet_tldr <- function(x, results, base_path, ..., is_first = FALSE) {
  man <- normalizePath(file.path(base_path, "inst", "tldr"))
  contents <- vapply(results, format, character(1))
  # TODO
  # Want to write a file for each alias, need to alter this slightly
  paths <- file.path(man, names(results))
  mapply(roxygen2:::write_if_different, paths, contents, MoreArgs = list(check = TRUE))

  if (!is_first) {
    old_paths <- setdiff(dir(man, full.names = TRUE), paths)
    old_paths <- old_paths[!file.info(old_paths)$isdir]
    old_roxygen <- Filter(roxygen2:::made_by_roxygen, old_paths)
    if (length(old_roxygen) > 0) {
      message(paste0("Deleting ", basename(old_roxygen),
                     collapse = "\n"))
      unlink(old_roxygen)
    }
  }
  paths
}

