
context('SQLQueryContextTestCase')


# Test basic SQL statement generation in the browser.
context('SQLStatementsTestCase')

# Test proper selection of attribute column.
test_that('test_attribute_column', {

})

test_that('test_condition_for_point', {

})


# Test multi-level point
#
# Note:
# This test requires that there is only one item for 2015-01-01
# See data in DW demo
test_that('test_condition_for_hierarchy_point', {

})


# Test Browser.range_condition
test_that('test_range_condition', {

})


context('SQLAggregateTestCase')

# Aggregate all aggregates without any cell and no drilldown
test_that('test_aggregate_base', {

})

# Aggregate with point cut
test_that('test_aggregate_point', {

})

# Aggregate with set cut
test_that('test_aggregate_set', {

})

# Aggregate with range cut
test_that('test_aggregate_range', {

})

# Aggregate with multiple cuts
test_that('test_aggregate_multiple', {

})

# Aggregate with negative cut (point, set, range)
test_that('test_aggregate_negative', {

})

# Test basic drilldown
test_that('test_drilldown', {

})

# Test implicit level from drilldown and cell
test_that('test_drilldown_implicit', {

})

# Test drilldown with explicit hierarchy level
test_that('test_drilldown_explicit', {

})


