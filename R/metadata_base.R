# Base classs for all model objects.
ModelObject = setRefClass(
  'ModelObject',
  fields = c('name', 'label', 'description', 'info'),
  methods = list(

    initialize = function(name=NULL, label=NULL, description=NULL, info=list(), ...) {
      # Initializes model object basics. Assures that the `info` is a dictionary.

      callSuper(...)

      name <<- name
      label <<- label
      description <<- description
      info <<- info


    }

  )
)
