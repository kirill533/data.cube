source('tests/common.R')

DIM_DATE_DESC = list(
  "name"= "date",
  "levels"= list(
    list("name"= "year"),
    list("name"= "month", "attributes"= list("month", "month_name")),
    list("name"= "day")
  ),
  "hierarchies"= list(
    list("name"= "ymd", "levels"= list("year", "month", "day")),
    list("name"= "ym", "levels"= list("year", "month"))
  )
)

DIM_FLAG_DESC = list("name"= "flag")

DIM_PRODUCT_DESC = list(
  "name"= "product",
  "levels"= list(
    list("name"= "category", "attributes"= list("key", "name")),
    list("name"= "subcategory", "attributes"= list("key", "name")),
    list("name"= "product", "attributes"= list("key", "name", "description"))
  )
)


context('AttributeTestCase')


test_that('test_basics', {
  # """Attribute creation and attribute references"""
  attr = Attribute$new("foo")
  testthat::expect_equal("foo", attr$name)
  testthat::expect_equal("foo", as.character(attr))
  testthat::expect_equal("foo", attr$ref)
})

test_that('test_simplify', {
  # """Simplification of attribute reference (with and without details)"""

  level = Level$new("name", attributes=list(Attribute$new("name")))
  dim = Dimension("group", levels=list(level))
  attr = dim$attribute("name")
  testthat::expect_equal("name", attr$name)

  # Simplified -> dimension name
  testthat::expect_equal("group", as.character(attr))
  testthat::expect_equal("group", attr$ref)

  level = Level("name", attributes=list(Attribute$new("key"), Attribute$new("name")))
  dim = Dimension("group", levels=list(level))
  attr = dim$attribute("name")
  testthat::expect_equal("name", attr$name)
  testthat::expect_equal("group.name", as.character(attr))
  testthat::expect_equal("group.name", attr$ref)
})

test_that('test_create_attribute', {

  # """Coalesce attribute object (string or Attribute instance)"""

  level = Level("name", attributes=list(Attribute("key"), Attribute("name")))
  dim = Dimension("group", levels=list(level))

  obj = Attribute.from_metadata("name")
  testthat::expect_s4_class(obj, Attribute$className)
  testthat::expect_equal("name", obj$name)

  obj = Attribute.from_metadata(list("name" = "key"))
  obj$setDimension(dim)
  testthat::expect_s4_class(obj, Attribute$className)
  testthat::expect_equal("key", obj$name)
  testthat::expect_true(dim$equal_to(obj$getDimension()))

  attr = dim$attribute("key")
  obj = Attribute.from_metadata(attr)
  obj$setDimension(dim)
  testthat::expect_s4_class(obj, Attribute$className)
  testthat::expect_equal("key", obj$name)
  testthat::expect_true(obj$equal_to(attr))

})


context('MeasuresTestsCase')

self.setUp = function() {

  source('tests/common.R')

  self.metadata <<- self.model_metadata("measures.json")

  self.cubes_md <<- list()

  for (cube in self.metadata$cubes)
    self.cubes_md[[cube$name]] <<- cube
}

self.cube = function(name) {
  # """Create a cube object `name` from measures test model."""
  Cube.from_metadata(self.cubes_md[[name]])
}

self.setUp()

test_that('test_basic', {
  md = list()
  testthat::expect_error({
    measure = Measure.from_metadata(md)
  }, "ModelError")

  measure = Measure.from_metadata("amount")
  testthat::expect_s4_class(measure, Measure$className)
  testthat::expect_equal("amount", measure$name)

  md = list("name" = "amount")
  measure = Measure.from_metadata(md)
  testthat::expect_equal("amount", measure$name)
})

test_that('test_copy', {
  md = list("name"= "amount")
  measure = Measure.from_metadata(md)
  measure2 = measure$copy()
  testthat::expect_true(measure$equal_to(measure2))
})

test_that('test_aggregate', {
  md = list()
  testthat::expect_error({
    measure = MeasureAggregate.from_metadata(md)
  }, "ModelError")

  measure = MeasureAggregate.from_metadata("amount_sum")
  testthat::expect_s4_class(measure, MeasureAggregate$className)
  testthat::expect_equal("amount_sum", measure$name)
})

test_that('test_create_default_aggregates', {
  measure = Measure.from_metadata("amount")
  aggs = measure$default_aggregates()
  testthat::expect_equal(1, length(aggs))
  agg = aggs[[1]]
  testthat::expect_equal("amount_sum", agg$name)
  testthat::expect_equal("amount", agg$measure)
  testthat::expect_equal("sum", agg$FUN)
  testthat::expect_null(agg$formula)

  md = list("name"= "amount", "aggregates"= list("sum", "min"))
  measure = Measure.from_metadata(md)
  aggs = measure$default_aggregates()
  testthat::expect_equal(2, length(aggs))
  testthat::expect_equal("amount_sum", aggs[[1]]$name)
  testthat::expect_equal("amount", aggs[[1]]$measure)
  testthat::expect_equal("sum", aggs[[1]]$FUN)
  testthat::expect_null(aggs[[1]]$formula)

  testthat::expect_equal("amount_min", aggs[[2]]$name)
  testthat::expect_equal("amount", aggs[[2]]$measure)
  testthat::expect_equal("min", aggs[[2]]$FUN)
  testthat::expect_null(aggs[[2]]$formula)
})

test_that('test_fact_count', {
  md = list("name"= "count", "FUN"= "count")
  agg = MeasureAggregate.from_metadata(md)

  testthat::expect_equal("count", agg$name)
  testthat::expect_null(agg$measure)
  testthat::expect_equal("count", agg$FUN)
  testthat::expect_null(agg$formula)
})

test_that('test_empty2', {
  # """No measures in metadata should yield count measure with record
  #       count"""
  cube = self.cube("empty")
  testthat::expect_s4_class(cube, Cube$className)
  testthat::expect_equal(0, length(cube$measures()))
  testthat::expect_equal(1, length(cube$aggregates() ))

  aggregate = cube$aggregates()[[1]]
  testthat::expect_equal("fact_count", aggregate$name)
  testthat::expect_equal("count", aggregate$FUN)
  testthat::expect_null(aggregate$measure)
})

test_that('test_amount_default', {
  # """Plain measure definition should yield measure_sum aggregate"""
  cube = self.cube("amount_default")
  measures = cube$measures()
  testthat::expect_equal(1, length(measures))
  testthat::expect_equal("amount", measures[[1]]$name)
  testthat::expect_null(measures[[1]]$expression)

  aggregates = cube$aggregates()
  testthat::expect_equal(1, length(aggregates))
  testthat::expect_equal("amount_sum", aggregates[[1]]$name)
  testthat::expect_equal("amount", aggregates[[1]]$measure)
  testthat::expect_null(aggregates[[1]]$expression)
})

test_that('test_fact_count2', {
  cube = self.cube("fact_count")
  measures = cube$measures()
  testthat::expect_equal(0, length(measures))

  aggregates = cube$aggregates()
  testthat::expect_equal(1, length(aggregates))
  testthat::expect_equal("total_events", aggregates[[1]]$name)
  testthat::expect_null(aggregates[[1]]$measure)
  testthat::expect_null(aggregates[[1]]$expression)
})

test_that('test_amount_sum', {
  cube = self.cube("amount_sum")
  measures = cube$measures()
  testthat::expect_equal(1, length(measures))
  testthat::expect_equal("amount", measures[[1]]$name)
  testthat::expect_null(measures[[1]]$expression)

  aggregates = cube$aggregates()
  testthat::expect_equal(1, length(aggregates))
  testthat::expect_equal("amount_sum", aggregates[[1]]$name)
  testthat::expect_equal("sum", aggregates[[1]]$FUN)
  testthat::expect_equal("amount", aggregates[[1]]$measure)
  testthat::expect_null(aggregates[[1]]$expression)
})

test_that('test_explicit_implicit_combined', {
  # Test explicit aggregates
  #
  cube = self.cube("amount_sum_explicit")
  measures = cube$measures()
  testthat::expect_equal(1, length(measures))
  testthat::expect_equal("amount", measures[[1]]$name)
  testthat::expect_null(measures[[1]]$expression)

  aggregates = cube$aggregates()
  testthat::expect_equal(1, length(aggregates))
  testthat::expect_equal("total", aggregates[[1]]$name)
  testthat::expect_equal("amount", aggregates[[1]]$measure)
  testthat::expect_null(aggregates[[1]]$expression)

  cube = self.cube("amount_sum_combined")
  measures = cube$measures()
  testthat::expect_equal(1, length(measures))
  testthat::expect_equal("amount", measures[[1]]$name)
  testthat::expect_null(measures[[1]]$expression)

  aggregates = cube$aggregates()
  testthat::expect_equal(3, length(aggregates))
  names = sapply(aggregates, function(a)a$name)

  testthat::expect_equal(c("total",
                            "amount_min",
                            "amount_max"), as.character(names))
})

test_that('test_backend_provided', {
  cube = self.cube("backend_provided_aggregate")
  measures = cube$measures()
  testthat::expect_equal(0, length(measures))

  aggregates = cube$aggregates()
  testthat::expect_equal(1, length(aggregates))
  testthat::expect_equal("total", aggregates[[1]]$name)
  testthat::expect_null(aggregates[[1]]$measure)
  testthat::expect_null(aggregates[[1]]$expression)
})

test_that('measure_expression', {
  cube = self.cube("measure_expression")
  measures = cube$measures()
  testthat::expect_equal(3, length(measures))

  testthat::expect_equal("price", measures[[1]]$name)
  testthat::expect_null(measures[[1]]$expression)
  testthat::expect_equal("costs", measures[[2]]$name)
  testthat::expect_null(measures[[2]]$expression)

  testthat::expect_equal("revenue", measures[[3]]$name)
  testthat::expect_equal("price - costs", measures[[3]]$expression)

  # No aggregates. Test was disabled in Cubes, so probably it was a mistake in the test
  aggregates = cube$aggregates()
  testthat::expect_equal(3, length(aggregates))
  testthat::expect_equal("price_sum", aggregates[[1]]$name)
  testthat::expect_equal("price", aggregates[[1]]$measure)
  testthat::expect_equal("costs_sum", aggregates[[2]]$name)
  testthat::expect_equal("costs", aggregates[[2]]$measure)
  testthat::expect_equal("revenue_sum", aggregates[[3]]$name)
  testthat::expect_equal("revenue", aggregates[[3]]$measure)
})

test_that('test_implicit', {
  # TODO: this should be in model.py tests
  cube = self.cube("default_aggregates")
  aggregates = as.character(sapply(cube$aggregates(), function(a)a$name))
  testthat::expect_equal(c("amount_sum",
                            "amount_min",
                            "amount_max"),
                           aggregates)
})

test_that('test_explicit', {
  cube = self.cube("explicit_aggregates")
  aggregates = as.character(sapply(cube$aggregates(), function(a)a$name))
  testthat::expect_equal(c("amount_sum",
                            "amount_wma",
                            "count"),
                           aggregates)
})

test_that('test_explicit_conflict', {
  testthat::expect_error({
    cube = self.cube("explicit_aggregates_conflict")
  }, 'ModelError.*function mismatch')
})

context('LevelTestCase')

test_that('test_initialization', {
  # """Empty attribute list for new level should raise an exception """
  testthat::expect_error(Level$new("month"), 'ModelError')
})

test_that('test_has_details', {
  # """Level "has_details" flag"""
  attrs = list(Attribute("year"))
  level = Level("year", attrs)
  testthat::expect_false(level$has_details())

  attrs = list(Attribute("month"), Attribute("month_name"))
  level = Level("month", attrs)
  testthat::expect_true(level$has_details())
})

test_that('test_operators', {
  # """Level to string conversion"""
  attrs = list(Attribute("foo"))
  testthat::expect_equal("date", as.character(Level("date", attrs)))
})

test_that('test_create', {
  # """Create level from a dictionary"""
  desc = "year"
  level = Level.from_metadata(desc)
  testthat::expect_s4_class(level, Level$className)
  testthat::expect_equal("year", level$name)
  testthat::expect_equal("year", sapply(level$attributes, as.character))

  # Test default: Attributes
  desc = list("name" = "year")
  level = Level.from_metadata(desc)
  testthat::expect_s4_class(level, Level$className)
  testthat::expect_equal("year", level$name)
  testthat::expect_equal("year", sapply(level$attributes, as.character))

  # Test default: Attributes
  desc = list("name"= "year", "attributes"= list("key"))
  level = Level.from_metadata(desc)
  testthat::expect_s4_class(level, Level$className)
  testthat::expect_equal("year", level$name)
  testthat::expect_equal("key", sapply(level$attributes, as.character))
  testthat::expect_false(level$has_details())

  desc = list("name"= "year", "attributes"= list("key", "label"))
  level = Level.from_metadata(desc)
  testthat::expect_true(level$has_details())
  testthat::expect_equal(c("key", "label"), sapply(level$attributes, as.character))

  # Level from description with full details
  desc = list(
    "name"= "month",
    "attributes"= list(
      list("name"= "month"),
      list("name"= "month_name", "locales"= c("en", "sk")),
      list("name"= "month_sname", "locales"= c("en", "sk"))
    )
  )

  level = Level.from_metadata(desc)
  testthat::expect_true(level$has_details())
  testthat::expect_equal(3, length(level$attributes))
  testthat::expect_equal(c("month", "month_name", "month_sname"), sapply(level$attributes, as.character))
})

test_that('test_key_label_attributes', {
  attrs = list(Attribute("code"))
  level = Level("product", attrs)
  testthat::expect_s4_class(level$key, Attribute$className)
  testthat::expect_equal("code", as.character(level$key))
  testthat::expect_s4_class(level$label_attribute, Attribute$className)
  testthat::expect_equal("code", as.character(level$label_attribute))

  attrs = list(Attribute("code"), Attribute("name"))
  level = Level("product", attrs)
  testthat::expect_s4_class(level$key, Attribute$className)
  testthat::expect_equal("code", as.character(level$key))
  testthat::expect_s4_class(level$label_attribute, Attribute$className)
  testthat::expect_equal("name", as.character(level$label_attribute))

  attrs = list(Attribute("info"), Attribute("code"), Attribute("name"))
  level = Level("product", attrs, key="code",
                label_attribute="name")
  testthat::expect_s4_class(level$key, Attribute$className)
  testthat::expect_equal("code", as.character(level$key))
  testthat::expect_s4_class(level$label_attribute, Attribute$className)
  testthat::expect_equal("name", as.character(level$label_attribute))

  # Test key/label in full desc
  desc = list(
    "name"= "product",
    "attributes"= c("info", "code", "name"),
    "label_attribute"= "name",
    "key"= "code"
  )

  level = Level.from_metadata(desc)
  testthat::expect_s4_class(level$key, Attribute$className)
  testthat::expect_equal("code", as.character(level$key))
  testthat::expect_s4_class(level$label_attribute, Attribute$className)
  testthat::expect_equal("name", as.character(level$label_attribute))
})

test_that('test_level_inherit', {
  desc = list(
    "name" = "product_type",
    "label" = "Product Type"
  )

  level = Level.from_metadata(desc)
  testthat::expect_equal(1, length(level$attributes))

  attr = level$attributes[[1]]
  testthat::expect_equal("product_type", attr$name)
  testthat::expect_equal("Product Type", attr$label)
})

test_that('test_comparison', {
  # """Comparison of level instances"""

  attrs = list(Attribute("info"), Attribute("code"), Attribute("name"))
  level1 = Level("product", attrs, key="code",
                 label_attribute="name")
  level2 = Level("product", attrs, key="code",
                 label_attribute="name")
  level3 = Level("product", attrs)
  attrs = list(Attribute("month"), Attribute("month_name"))
  level4 = Level("product", attrs)

  testthat::expect_true(level1$equal_to(level2))
  testthat::expect_false(level2$equal_to(level3))
  testthat::expect_false(level2$equal_to(level4))
})


context('HierarchyTestCase')

self.setUp = function() {
  source('tests/common.R')
  self.levels <<- list(
    Level$new("year", attributes=list(Attribute("year"))),
    Level$new("month",
          attributes=list(
            Attribute("month"),
            Attribute("month_name"),
            Attribute("month_sname")
          )),
    Level$new("day", attributes=list(Attribute("day"))),
    Level$new("week", attributes=list(Attribute("week")))
  )
  self.level_names <<- sapply(self.levels, function(level)level$name)
  self.dimension <<- Dimension$new("date", levels=self.levels)
  levels = list(self.levels[[1]], self.levels[[2]], self.levels[[3]])
  self.hierarchy <<- Hierarchy$new("default",
                             levels)
}

self.setUp()

# No dimension on initialization should raise an exception
test_that('test_initialization', {
  # """No dimension on initialization should raise an exception."""
  testthat::expect_error(Hierarchy$new("default", c()), "ModelInconsistencyError.*not be empty")
  testthat::expect_error(Hierarchy$new("default", list()), "ModelInconsistencyError.*not be empty")

  testthat::expect_error(Hierarchy$new("default", c('iamastring')), "ModelInconsistencyError.*as strings")
  testthat::expect_error(Hierarchy$new("default", list('iamastring')), "ModelInconsistencyError.*as strings")

})

# Hierarchy operators length(), hier[] and level in hier
test_that('test_operators', {
  self.setUp()
  # """Hierarchy operators length(), hier[] and level in hier"""
  # __len__
  testthat::expect_equal(3, self.hierarchy$len())

  # __getitem__ by name
  testthat::expect_true(self.levels[[1]]$equal_to(self.hierarchy$getitem(1)))

  # __contains__ by name or level
  testthat::expect_true(self.hierarchy$contains(self.levels[[1]]))
  testthat::expect_true(self.hierarchy$contains("year"))
  testthat::expect_false(self.hierarchy$contains("flower"))
})

test_that('test_levels_for', {
  self.setUp()
  # """Levels for depth"""
  levels = self.hierarchy$levels_for_depth(0)
  testthat::expect_equal(list(), levels)

  levels = self.hierarchy$levels_for_depth(1)
  testthat::expect_true(self.levels[[1]]$equal_to(levels[[1]]))

  testthat::expect_error(self.hierarchy$levels_for_depth(4), 'HierarchyError')
})

test_that('test_level_ordering', {
  # """Ordering of levels (next, previous)"""
  testthat::expect_true(self.levels[[1]]$equal_to(self.hierarchy$next_level(NULL)))
  testthat::expect_true(self.levels[[2]]$equal_to(self.hierarchy$next_level(self.levels[[1]])))
  testthat::expect_true(self.levels[[3]]$equal_to(self.hierarchy$next_level(self.levels[[2]])))
  testthat::expect_null(self.hierarchy$next_level(self.levels[[3]]))

  testthat::expect_null(self.hierarchy$previous_level(NULL))
  testthat::expect_null(self.hierarchy$previous_level(self.levels[[1]]))
  testthat::expect_true(self.levels[[1]]$equal_to(self.hierarchy$previous_level(self.levels[[2]])))
  testthat::expect_true(self.levels[[2]]$equal_to(self.hierarchy$previous_level(self.levels[[3]])))

  testthat::expect_equal(1, self.hierarchy$level_index(self.levels[[1]]))
  testthat::expect_equal(2, self.hierarchy$level_index(self.levels[[2]]))
  testthat::expect_equal(3, self.hierarchy$level_index(self.levels[[3]]))

  testthat::expect_error(self.hierarchy$level_index(self.levels[[4]]), 'HierarchyError')
})

# Path roll-up for hierarchy
test_that('test_rollup', {
  self.setUp()
  # """Path roll-up for hierarchy"""
  path = c(2010, 1, 5)

  testthat::expect_equal(c(2010, 1), self.hierarchy$rollup(path))
  testthat::expect_equal(c(2010, 1), self.hierarchy$rollup(path, "month"))
  testthat::expect_equal(c(2010), self.hierarchy$rollup(path, "year"))
  testthat::expect_error(self.hierarchy$rollup(c(2010), "month"), 'HierarchyError')
  testthat::expect_error(self.hierarchy$rollup(c(2010), "unknown"), 'HierarchyError')

})

# Test base paths
test_that('test_base_path', {
  # """Test base paths"""
  testthat::expect_true(self.hierarchy$path_is_base(c(2012, 1, 5)))
  testthat::expect_false(self.hierarchy$path_is_base(c(2012, 1)))
  testthat::expect_false(self.hierarchy$path_is_base(c(2012)))
  testthat::expect_false(self.hierarchy$path_is_base(c()))
})

# Collecting attributes and keys
test_that('test_attributes', {
  # """Collecting attributes and keys"""
  keys = sapply(self.hierarchy$key_attributes(), function(a)a$name )
  testthat::expect_equal(c("year", "month", "day"), as.character(keys))

  attrs = sapply(self.hierarchy$all_attributes(), function(a)a$name )
  testthat::expect_equal(c("year", "month", "month_name", "month_sname", "day"), as.character(attrs))
})

test_that('test_copy', {
  self.setUp()
  DummyDimension = setRefClass(
    'DummyDimension',
    fields = c('name'),
    methods = list(
      initialize = function() {
        name <<- "dummy"
      },

      is_flat = function() {
        FALSE
      }
    )
  )

  left = self.hierarchy$getLevels()[[1]]$attributes[[1]]
  left$setDimension(DummyDimension$new())

  clone = self.hierarchy$copy()

  left = self.hierarchy$getLevels()[[1]]$attributes[[1]]
  right = clone$getLevels()[[1]]$attributes[[1]]
  # Make sure that the dimension is not copied
  testthat::expect_null(right$dimension)

  testthat::expect_true(self.hierarchy$getLevels()[[1]]$equal_to(clone$getLevels()[[1]]))
  testthat::expect_true(self.hierarchy$getLevels()[[2]]$equal_to(clone$getLevels()[[2]]))
  testthat::expect_true(self.hierarchy$equal_to(clone))

})

context('DimensionTestCase')

self.setUp = function() {
  self.levels <<- list(
    Level("year", attributes=create_list_of(Attribute, list("year"))),
    Level("month", attributes=create_list_of(Attribute, list("month", "month_name",
                                                         "month_sname"))),
    Level("day", attributes=create_list_of(Attribute, list("day"))),
    Level("week", attributes=create_list_of(Attribute, list("week")))
  )
  self.level_names <<- sapply(self.levels, function(level){level$name})
  self.dimension <<- Dimension("date", levels=self.levels)

  levels = list(self.levels[[1]], self.levels[[2]], self.levels[[3]])
  self.hierarchy <<- Hierarchy("default", levels)
}

self.setUp()

test_that('test_create', {
  self.setUp()
  # """Dimension from a dictionary"""
  dim = Dimension.from_metadata("year")
  testthat::expect_s4_class(dim, Dimension$className)
  testthat::expect_equal("year", dim$name)
  testthat::expect_equal("year", as.character(sapply(dim$attributes(), as.character)))

  # Test default: explicit level attributes
  desc = list("name" = "date", "levels" = list("year"))
  dim = Dimension.from_metadata(desc)
  testthat::expect_true(dim$is_flat())
  testthat::expect_false(dim$has_details())
  testthat::expect_s4_class(dim, Dimension$className)
  testthat::expect_equal("date", dim$name)

  # Dimension string is a reference and is equal to dimension name if
  # dimension is flat
  testthat::expect_equal("date", as.character(sapply(dim$attributes(), as.character)))

  desc = list("name"= "date", "levels"= list("year", "month", "day"))
  dim = Dimension.from_metadata(desc)
  testthat::expect_s4_class(dim, Dimension$className)
  testthat::expect_equal("date", dim$name)
  refs = as.character(sapply(dim$attributes(), as.character))
  testthat::expect_equal(c("date.year", "date.month", "date.day"), refs)
  testthat::expect_false(dim$is_flat())
  testthat::expect_false(dim$has_details())
  testthat::expect_equal(3, length(dim$getLevels()))
  for (level in dim$getLevels())
    testthat::expect_s4_class(level, Level$className)

  testthat::expect_equal(1, length(dim$hierarchies()))
  # testthat::expect_equal(3, length(dim$hierarchy())) # disabled for R, why 3???

  # Test default: implicit single level attributes
  desc = list("name"= "product", "attributes"= c("code", "name"))
  dim = Dimension.from_metadata(desc)
  refs = sapply(dim$attributes(), as.character)
  testthat::expect_equal(c("product.code", "product.name"), as.character(refs))
  testthat::expect_equal(1, length(dim$getLevels()))
  testthat::expect_equal(1, length(dim$hierarchies()))
})

# Flat dimension and 'has details' flags
test_that('test_flat_dimension', {
  # """Flat dimension and 'has details' flags"""
  dim = Dimension.from_metadata("foo")
  testthat::expect_true(dim$is_flat())
  testthat::expect_false(dim$has_details())
  testthat::expect_equal(1, length(dim$getLevels()))

  level = dim$level("foo")
  testthat::expect_s4_class(level, Level$className)
  testthat::expect_equal("foo", level$name)
  testthat::expect_equal(1, length(level$attributes))
  testthat::expect_equal("foo", as.character(level$key))

  attr = level$attributes[[1]]
  testthat::expect_s4_class(attr, Attribute$className)
  testthat::expect_equal("foo", attr$name)
})

# Comparison of dimension instances
test_that('test_comparisons', {
  # """Comparison of dimension instances"""

  dim1 = Dimension.from_metadata(DIM_DATE_DESC)
  dim2 = Dimension.from_metadata(DIM_DATE_DESC)

  testthat::expect_true(dim1$getLevels()[[1]]$equal_to(dim2$getLevels()[[1]]))
  testthat::expect_true(dim1$hierarchies()[[1]]$equal_to(dim2$hierarchies()[[1]]))
  testthat::expect_equal(length(dim1$getLevels()), length(dim2$getLevels()))
  testthat::expect_equal(length(dim1$hierarchies()), length(dim2$hierarchies()))

  testthat::expect_true(dim1$equal_to(dim2))
})

test_that('test_to_dict', {
  self.setUp()
  desc = self.dimension$to_dict()
  dim = Dimension.from_metadata(desc)

  testthat::expect_true(compareListOfClasses(self.dimension$hierarchies(), dim$hierarchies()))
  testthat::expect_true(compareListOfClasses(self.dimension$getLevels(), dim$getLevels()))
  testthat::expect_true(self.dimension$equal_to(dim))
})

test_that('test_template', {
  self.setUp()
  dims = list("date"= self.dimension)
  desc = list("template"= "date", "name"= "date")

  dim = Dimension.from_metadata(desc, dims)
  testthat::expect_true(self.dimension$equal_to(dim))
  hier = dim$hierarchy()
  testthat::expect_equal(4, length(hier$getLevels()))

  desc$hierarchy = list("year", "month")
  dim = Dimension.from_metadata(desc, dims)
  testthat::expect_equal(1, length(dim$hierarchies()))
  hier = dim$hierarchy()
  testthat::expect_equal(2, length(hier$getLevels()))

  template = self.dimension$to_dict()
  template$hierarchies = list(
    list("name"= "ym", "levels"= c("year", "month")),
    list("name"= "ymd", "levels"= c("year", "month", "day"))
  )

  template$default_hierarchy_name = "ym"
  template = Dimension.from_metadata(template)
  dims = list("date"= template)
  desc = list("template"= "date", "name"="another_date")
  dim = Dimension.from_metadata(desc, dims)
  testthat::expect_equal(2, length(dim$hierarchies()))
  testthat::expect_equal(c("ym", "ymd"), sapply(dim$hierarchies(), function(hier)hier$name))
})

test_that('test_template_hierarchies', {
  self.setUp()
  md = list(
    "name"= "time",
    "levels"= c("year", "month", "day", "hour"),
    "hierarchies"= list(
      list("name"= "full", "levels"= c("year", "month", "day", "hour")),
      list("name"= "ymd", "levels"= c("year", "month", "day")),
      list("name"= "ym", "levels"= c("year", "month")),
      list("name"= "y", "levels"= c("year"))
    )
  )
  dim_time = Dimension.from_metadata(md)
  templates = list("time"= dim_time)
  md = list(
    "name"= "date",
    "template"= "time",
    "hierarchies"= list(
      "ymd", "ym", "y"
    )
  )

  dim_date = Dimension.from_metadata(md, templates)

  testthat::expect_equal(dim_date$name, "date")
  testthat::expect_equal(length(dim_date$hierarchies()), 3)
  names = sapply(dim_date$hierarchies(), function(h)h$name)
  testthat::expect_equal(c("ymd", "ym", "y"), names)
})

test_that('test_template_info', {

  md = list(
    "name"= "template",
    "levels"= list(
      list( "name"= "one", "info"= list("units"="$", "format"= "foo"))
    )
  )
  tempdim = Dimension.from_metadata(md)

  md = list(
    "name"= "dim",
    "levels"= list(
      list( "name"= "one", "info"= list("units"="USD"))
    ),
    "template"= "template"
  )

  templates = list("template"= tempdim)
  dim = Dimension.from_metadata(md, templates)

  level = dim$level("one")
  testthat::expect_true("units" %in% names(level$info))
  testthat::expect_true("format" %in% names(level$info))

  testthat::expect_equal(level$info[["units"]], "USD")
  testthat::expect_equal(level$info[["format"]], "foo")
})

context('CubeTestCase')

self.setUp = function() {
  a = list(DIM_DATE_DESC, DIM_PRODUCT_DESC, DIM_FLAG_DESC)
  self.measures <<- create_list_of(Measure, list("amount", "discount"))
  self.details <<- create_list_of(Attribute, list("detail"))
  self.dimensions <<- lapply(a, Dimension.from_metadata)
  self.cube <<- Cube("contracts",
                   dimensions=self.dimensions,
                   measures=self.measures,
                   details=self.details)
}

self.setUp()

test_that('test_create_cube', {
  cube = list(
    "name"= "cube",
    "dimensions"= list("date"),
    "aggregates"= list("record_count"),
    "details"= list("some_detail", "another_detail")
  )
  cube = Cube.from_metadata(cube)

  testthat::expect_equal(cube$name, "cube")
  testthat::expect_equal(length(cube$aggregates()), 1)
  testthat::expect_equal(length(cube$getDetails()), 2)
})

test_that('test_get_dimension', {
  self.setUp()
  testthat::expect_true(compareListOfClasses(self.dimensions, self.cube$getDimensions()))

  testthat::expect_equal("date", self.cube$dimension("date")$name)
  testthat::expect_equal("product", self.cube$dimension("product")$name)
  testthat::expect_equal("flag", self.cube$dimension("flag")$name)
  testthat::expect_error(self.cube$dimension("xxx"), 'NoSuchDimensionError')
})

test_that('test_get_measure', {
  self.setUp()
  self.assertListEqual(self.measures, self.cube$getMeasures())

  testthat::expect_equal("amount", self.cube$measure("amount")$name)
  testthat::expect_equal("discount", self.cube$measure("discount")$name)
  testthat::expect_error(self.cube$measure("xxx"), 'NoSuchAttributeError')
})

test_that('test_attributes', {
  self.setUp()
  all_attributes = self.cube$all_attributes()

  refs = sapply(all_attributes, function(a)a$ref)

  expected = c(
    'date.year',
    'date.month',
    'date.month_name',
    'date.day',
    'product.key',
    'product.name',
    'product.description',
    'flag',
    'detail',
    'amount',
    'discount')

  testthat::expect_equal(expected, refs)

  attributes = self.cube$get_attributes(c("date.year", "product.name"))
  refs = sapply(attributes, function(a)a$ref)
  expected = c('date.year', 'product.name')
  testthat::expect_equal(expected, refs)

  attributes = self.cube$get_attributes(c("amount"))
  refs = sapply(attributes, function(a)a$ref)
  testthat::expect_equal(c("amount"), refs)

  testthat::expect_error(self.cube$get_attributes(c("UNKNOWN"), 'NoSuchAttributeError'))
})

# @unittest.skip("deferred (needs workspace)")
test_that('test_to_dict', {
  self.setUp()
  desc = self.cube$to_dict()
  # dims = dict((dim.name, dim) for dim in self.dimensions)
  # cube = Cube.from_metadata(desc, dims)
  # testthat::expect_equal(self.cube$dimensions, cube.dimensions)
  # testthat::expect_equal(self.cube.measures, cube.measures)
  # testthat::expect_equal(self.cube, cube)
})

# @unittest.skip("requires revision")
test_that('test_links', {
  # TODO= test link alias!
  dims = object_dict(self.dimensions)
  #
  links = list(list("name"= "date"))
  cube = Cube("contracts",
               dimension_links=links,
               measures=self.measures)

  cube$link_dimension(self.dimensions[[1]])
  cube$link_dimension(self.dimensions[[2]])
  cube$link_dimension(self.dimensions[[3]])

  # testthat::expect_equal(length(cube$getDimensions()), 1)
  # dim = cube.dimension("date")
  # testthat::expect_equal(length(dim.hierarchies), 2)
  #
  # links = [{"name"= "date"}, "product", "flag"]
  # cube = Cube("contracts",
  #             dimension_links=links,
  #             measures=self.measures)
  # cube.link_dimensions(dims)
  # testthat::expect_equal(length(cube.dimensions), 3)
  # testthat::expect_s4_class(cube.dimension("flag"), Dimension)
})

# @unittest.skip("requires revision")
test_that('test_link_hierarchies', {
  # dims = dict((d.name, d) for d in self.dimensions)
  #
  # links = [{"name"= "date"}]
  # cube = Cube("contracts",
  #             dimension_links=links,
  #             measures=self.measures)
  # cube.link_dimensions(dims)
  # dim = cube.dimension("date")
  # testthat::expect_equal(length(dim.hierarchies), 2)
  # testthat::expect_equal(dim.hierarchy()$name, "ymd")
  #
  # links = [{"name"= "date", "nonadditive"=None}]
  # cube = Cube("contracts",
  #             dimension_links=links,
  #             measures=self.measures)
  # cube.link_dimensions(dims)
  # dim = cube.dimension("date")
  # testthat::expect_equal(length(dim.hierarchies), 2)
  # testthat::expect_equal(dim.hierarchy()$name, "ymd")
  #
  # links = [{"name"= "date", "hierarchies"= ["ym"]}]
  # cube = Cube("contracts",
  #             dimension_links=links,
  #             measures=self.measures)
  # cube.link_dimensions(dims)
  # dim = cube.dimension("date")
  # testthat::expect_equal(length(dim.hierarchies), 1)
  # testthat::expect_equal(dim.hierarchy()$name, "ym")
})

test_that('test_inherit_nonadditive', {
  self.setUp()
  dims = list(DIM_DATE_DESC, DIM_PRODUCT_DESC, DIM_FLAG_DESC)

  cube = list(
    "name"= "contracts",
    "dimensions"= list("date", "product"),
    "nonadditive"= "time",
    "measures"= list("amount", "discount")
  )

  dims = lapply(dims, Dimension.from_metadata)
  #dims = dict((dim.name, dim) for dim in dims)

  cube = Cube.from_metadata(cube)

  measures = cube$getMeasures()
  testthat::expect_equal(measures[[1]]$nonadditive, "time")
})

context('ReadModelDescriptionTestCase')

self.setUp = function() {

}

test_that('test_from_file', {
  path = self.model_path("model.json")
  desc = read_model_metadata(path)

  testthat::expect_true(is.list(desc))
  testthat::expect_true("cubes" %in% names(desc))
  testthat::expect_true("dimensions" %in% names(desc))
  testthat::expect_equal(1, length(desc$cubes))
  testthat::expect_equal(6, length(desc$dimensions))

})

test_that('test_from_bundle', {

  path = self.model_path("test.cubesmodel")
  desc = read_model_metadata(path)

  testthat::expect_true(is.list(desc))
  testthat::expect_true("cubes" %in% names(desc))
  testthat::expect_true("dimensions" %in% names(desc))
  testthat::expect_equal(1, length(desc$cubes))
  testthat::expect_equal(6, length(desc$dimensions))

  path = self.model_path("model.json")

  testthat::expect_error(read_model_metadata_bundle(path), 'ArgumentError')

})
