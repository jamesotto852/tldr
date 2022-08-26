#' Roxygen2 extensions
#'
#' roxygen2 extension code facilitating documenting via roxygen2::roxygenize(roclets = c("tldr_roclet"))
#'
#' @name tldr_roclet
#' @rdname tldr_roclet
#' @ignoretldr
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

# format
#' @rdname tldr_roclet
#' @method roxy_tag_rd roxy_tag_formattldr
#' @export
roxy_tag_rd.roxy_tag_formattldr <- function(x, base_path, env) {
  NULL
}
#' @rdname tldr_roclet
#' @method roxy_tag_rd_tldr roxy_tag_formattldr
#' @export
roxy_tag_rd_tldr.roxy_tag_formattldr <- function(x, base_path, env) {
  roxygen2::rd_section("formattldr", x$val)
}

# ignore
#' @rdname tldr_roclet
#' @method roxy_tag_rd roxy_tag_ignoretldr
#' @export
roxy_tag_rd.roxy_tag_ignoretldr <- function(x, base_path, env) {
  NULL
}
#' @rdname tldr_roclet
#' @method roxy_tag_rd_tldr roxy_tag_ignoretldr
#' @export
roxy_tag_rd_tldr.roxy_tag_ignoretldr <- function(x, base_path, env) {
  roxygen2::rd_section("ignoretldr", x$val)
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
  block <- process_templates(block, base_path)

  # Check for cases in which no docs are required
  if (!needs_doc(block)) {
    return()
  }
  # if (any(vapply(block$tags, function(x) x$tag == "ignoretldr", logical(1)))) {
  #   return()
  # }

  name <- roxygen2::block_get_tag(block, "name")$val %||% block$object$topic
  if (is.null(name)) {
    roxygen2::roxy_tag_warning(block$tags[[1]], "Missing name")
    return()
  }
  rd <- RoxyTopic$new()
  topic_add_name_aliases(rd, block, name)
  for (tag in block$tags) {
    if (!(tag$tag %in% c("name", "rdname", "aliases"))) rd$add(roxy_tag_rd_tldr(tag, env = env, base_path = base_path))
  }
  # Don't think the following 4 lines are necessary:
  describe_rdname <- topic_add_describe_in(rd, block, env)
  filename <- describe_rdname %||% roxygen2::block_get_tag(block, "rdname")$val %||%
    nice_name(name)
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

# Set up tldr format tag:
#' @rdname tldr_roclet
#' @method roxy_tag_parse roxy_tag_formattldr
#' @export
roxy_tag_parse.roxy_tag_formattldr <- function(x) {
  x$val <- x$raw

  x
}
#' @rdname tldr_roclet
#' @method format rd_section_formattldr
#' @export
format.rd_section_formattldr <- function(x, ...) {
  paste0(
    "\\format{\n",
    paste0(x$value, collapse = "\n"),
    "}\n"
  )
}

# Set up tldr ignore tag:
#' @rdname tldr_roclet
#' @method roxy_tag_parse roxy_tag_ignoretldr
#' @export
roxy_tag_parse.roxy_tag_ignoretldr <- function(x) {
  # This is "parsing"
  x$val <- x$raw

  x
}
#' @rdname tldr_roclet
#' @method format rd_section_ignoretldr
#' @export
format.rd_section_ignoretldr <- function(x, ...) {
  # roclet_output() avoids writing files w/ this tag in their contents
  "\\ignoretldr\n"
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
  topics <- RoxyTopics$new()

  for (block in blocks) {

    # Find all tags relevant to tldr:
    # tldr_tags <- c("title", "alias", "docType", "exampletldr", "paramtldr")
    tldr_tags <- c("name", "rdname", "title", "aliases", "docType", "exampletldr",  "paramtldr", "formattldr", "ignoretldr")
    relevant_tags <- vapply(block$tags, function(x) x$tag %in% tldr_tags, logical(1))

    block$tags <- block$tags[relevant_tags]

    # rd <- roxygen2:::block_to_rd(block, base_path, env)
    rd <- block_to_rd_tldr(block, base_path, env)

    topics$add(rd)

  }

  # Not using any of this from roxygen2:
  # topics_process_family(topics, env)
  # topics_process_inherit(topics, env)
  # topics$drop_invalid()
  # topics_fix_params_order(topics)
  # topics_add_default_description(topics)

  topics$topics
}

#' @rdname tldr_roclet
#' @importFrom roxygen2 roclet_clean
#' @method roclet_clean roclet_tldr
#' @export
roclet_clean.roclet_tldr <- function(x, base_path) {
  rd <- dir(file.path(base_path, "inst", "tldr"), full.names = TRUE)
  rd <- rd[!file.info(rd)$isdir]
  unlink(purrr::keep(rd, made_by_roxygen))
}

# Very similar to roxygen2:::roclet_output.roclet_rd
#' @rdname tldr_roclet
#' @importFrom roxygen2 roclet_output
#' @method roclet_output roclet_tldr
#' @export
roclet_output.roclet_tldr <- function(x, results, base_path, ..., is_first = FALSE) {

  if (!dir.exists(file.path(base_path, "inst", "tldr"))) dir.create(file.path(base_path, "inst", "tldr"), recursive = TRUE)

  # Why normalizePath? For write_if_different()?
  man <- normalizePath(file.path(base_path, "inst", "tldr"))
  normalizePath(file.path(here::here(), "inst", "tldr"))
  contents <- vapply(results, format, character(1))

  # Throw out entries w/ @ignoretldr tag
  contents <- grep("\\ignoretldr", contents, value = TRUE, invert = TRUE)

  # Write a file for each alias, repeating contents when necessary
  aliases <- get_aliases(contents)
  aliases_count <- vapply(aliases, length, numeric(1))
  contents <- rep(contents, aliases_count)
  aliases <- unlist(aliases)

  paths <- file.path(man, paste0(aliases, ".Rd", recycle0 = TRUE))
  mapply(write_if_different, paths, contents, MoreArgs = list(check = TRUE))

  if (!is_first) {
    old_paths <- setdiff(dir(man, full.names = TRUE), paths)
    old_paths <- old_paths[!file.info(old_paths)$isdir]
    old_roxygen <- Filter(made_by_roxygen, old_paths)
    if (length(old_roxygen) > 0) {
      message(paste0("Deleting ", basename(old_roxygen),
                     collapse = "\n"))
      unlink(old_roxygen)
    }
  }
  paths
}

# Helper fun to find aliases via regexp
# potential to fail if aliases contain `}`
get_aliases <- function(vec) {
  if (length(vec) == 0) return(character(0))
  if (length(vec) > 1) return(lapply(vec, get_aliases))
  regmatches(vec, gregexpr("(?<=\\\\alias\\{).*(?=\\})", vec, perl = TRUE))[[1]]
}

