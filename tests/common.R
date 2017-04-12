TESTS_PATH = system.file("tests", package = "data.cube")
if (TESTS_PATH == '')
  TESTS_PATH = dirname(sys.frame(1)$ofile)

create_provider <- function(name) {
  # TODO: this should be rather:
  # provider = FileModelProvider(path)
  path = self.model_path(name)
  metadata = read_model_metadata(path)

  StaticModelProvider$new(metadata=metadata)
}

self.model_path <- function(name) {
  file.path(TESTS_PATH, 'models', name)
}

self.model_metadata <- function(model) {
  read_model_metadata(self.model_path(model))
}

self.assertListEqual = function(a, b, ...) {
  testthat::expect_true(compareListOfClasses(a, b), ...)
}
