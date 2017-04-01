TESTS_PATH = system.file("tests", package = "data.cube")
if (TESTS_PATH == '')
  TESTS_PATH = dirname(sys.frame(1)$ofile)

create_provider <- function(name) {
  # TODO: this should be rather:
  # provider = FileModelProvider(path)
  path = file.path(TESTS_PATH, 'models', name)
  metadata = read_model_metadata(path)

  StaticModelProvider$new(metadata=metadata)
}


read_model_metadata <- function(source) {
  jsonlite::fromJSON(readChar(source, file.info(source)$size), simplifyVector = F)
}
