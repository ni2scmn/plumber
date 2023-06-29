validate <- function(schema, request_string) {
  request <- jsonlite::fromJSON(
    txt = request_string,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE,
    flatten = FALSE
  )

  validate_int(schema, request)
}

validate_int <- function(schema, request) {
  UseMethod("validate_int")
}

validate_int.default <- function(schema, request) {
  rlang::abort("Unimplemented")
}

validate_int.plumber_req_structure <- function(schema, request) {
  all(purrr::map2_lgl(schema, request, validate_int))
}

validate_int.plumber_req_object <- function(schema, request) {
  print("validate_int.plumber_req_object")
  required_attributes <- schema %>%
    purrr::keep( ~ inherits(.x, "plumber_req_schema") && .x$required)

  if(!all(required_attributes %in% names(request))) {
    rlang::abort("Missing mandatory!")
  }

  all(
    purrr::map2_lgl(
      .x = schema[names(request)],
      .y = request,
      .f = validate_int
    )
  )
}

validate_int.plumber_req_enum <- function(schema, request) {
  print("validate_int.plumber_req_enum")
  stopifnot(inherits(schema, "plumber_req_enum"))
  request %in% schema$values
}

validate_int.plumber_req_numeric <- function(schema, request) {
  print("validate_int.plumber_req_numeric")
  stopifnot(inherits(schema, "plumber_req_numeric"))
  if(!is.null(schema$min) && request < schema$min) {
    rlang::abort("too low")
  }
  if(!is.null(schema$max) && request > schema$max) {
    rlang::abort("too high")
  }
  TRUE
}

min_length = NULL,
max_length = NULL,
regex = NULL,
default = NULL,
required = FALSE

validate_int.plumber_req_string <- function(schema, request) {
  print("validate_int.plumber_req_string")
  TRUE
}
