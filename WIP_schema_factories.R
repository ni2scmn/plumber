# TODO numeric `format` attribute
# TODO distinct btwn numeric and integer?
# TODO arrays of objects / scalar types

# TODO string -> distinct btwn format (date, ..) and pattern (regex)
# TODO find out what formats besides date are valid

ps_string <- function(
    min_length = NULL,
    max_length = NULL,
    regex = NULL,
    default = NULL,
    required = FALSE) {

  stopifnot(
    "`min_length` must be non negative integer or NULL" =
      rlang::is_scalar_integerish(min_length) || is.null(min_length),
    "`max_length` must be non negative integer or NULL" =
      rlang::is_scalar_integerish(max_length) || is.null(max_length),
    "`regex` must be scalar string or NULL" =
      rlang::is_scalar_character(regex) || is.null(regex),
    "`default` must be scalar string or NULL" =
      rlang::is_scalar_character(default) || is.null(default),
    "`required` must be scalar logical" =
      rlang::is_scalar_logical(required)
  )

  stopifnot(
    "`max_length` must not be smaller than `min_length`" =
      is.null(min_length) || is.null(max_length) || min_length <= max_length,
    "`default` must be NULL when `required` is TRUE" =
      is.null(default) || !required
  )

  structure(
    list(
      min_length = min_length,
      max_length = max_length,
      regex = regex,
      default = default,
      required = required
    ),
    class = c("plumber_req_string", "plumber_req_schema") # TODO list?
  )
}

ps_numeric <- function(
    min = NULL,
    max = NULL,
    default = NULL,
    required = FALSE) {

  stopifnot(
    "`min` must be scalar numeric or NULL" =
      rlang::is_scalar_double(min) || rlang::is_scalar_integer(min) || is.null(min),
    "`max` must be scalar numeric or NULL" =
      rlang::is_scalar_double(max) || rlang::is_scalar_integer(max) || is.null(max),
    "`default` must be scalar string or NULL" =
      rlang::is_scalar_character(default) || is.null(default),
    "`required` must be scalar logical" =
      rlang::is_scalar_logical(required)
  )

  structure(
    list(
      min = min,
      max = max,
      default = default,
      required = required
    ),
    class = c("plumber_req_numeric", "plumber_req_schema") # TODO list?
  )
}

# TODO are NA's allowed?
ps_enum <- function(
    values = NULL,
    default = NULL,
    required = FALSE) {

  stopifnot(
    "`values` must be a character vector" = rlang::is_character(values),
    "`default` must be scalar string or NULL" =
      rlang::is_scalar_character(default) || is.null(default),
    "`required` must be scalar logical" =
      rlang::is_scalar_logical(required)
  )

  structure(
    list(
      values = values,
      default = default,
      required = required
    ),
    class = c("plumber_req_enum", "plumber_req_schema")
  )


}

ps_object <- function(..., required = FALSE) {
  structure(
    list(..., required = required),
    class = c("plumber_req_object", "plumber_req_schema")
  )
}

ps <- function(...) {
  structure(
    list(...),
    class = c("plumber_req_structure", "plumber_req_structure")
  )
}






