assert_instance <- function(obj, class_, label) {
  # Raises ArgumentError when `obj` is not instance of `cls`

  if (!(class_$className %in% class(obj))) {
    stop(sprintf("ModelInconsistencyError: %s should be sublcass of %s, provided: %s",
                 label,
                 class_$className,
                 typeof(obj)))
  }
}

assert_all_instances <- function(list_, class_, label="object") {
  # Raises ArgumentError when objects in `list_` are not instances of `cls`

  for (obj in nvl(list_, c()))
    assert_instance(obj, class_, label="object")
}
