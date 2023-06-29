library(magrittr)

demo_schema <- ps(
  name = ps_object(
    forename = ps_string(min_length = 3),
    surname = ps_string(max_length = 10),
    required = TRUE
  ),
  age = ps_numeric(min = 0, max = 100),
  type = ps_enum(
    values = c("NEW", "EXISTING", "TBD")
  )
)

demo_object <- list(
  name = list(
    forename = "Hallo",
    surname = "World"
  ),
  age = 30,
  type = "NEW"
)

demo_object_json <- jsonlite::toJSON(demo_object)


validate(
  schema = demo_schema,
  request = demo_object_json
)
