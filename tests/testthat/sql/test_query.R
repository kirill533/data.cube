library(data.table)
library(DBI)
library(RSQLite)


test_query.setUp <- function() {



}



context('SchemaBasicsTestCase')



test_that('test_physical_table', {



})

test_that('test_collected_tables_fact_only', {

})

test_that('test_fact_columns', {

})

# Test fetching fact columns.
test_that('test_unknown_column', {

})

# Test that mapping.extract works
test_that('test_mapping_extract', {

})

test_that('test_required_tables_with_no_joins', {

})

# Test selection from the very basic star – no joins, just one table
test_that('test_star_basic', {

})

test_that('test_no_table_in_mapping', {

})

# Test independent utility functions and structures.
context('SchemaUtilitiesTestCase')

# Test basic structure conversions.
test_that('test_to_join_key', {

})


test_that('test_to_join', {

})

context('SchemaJoinsTestCase')

# Test master-detail-detail snowflake chain joins
test_that('test_required_tables', {

})

# Test exception when detail is specified twice (loop in graph)
test_that('test_detail_twice', {

})

test_that('test_no_join_detail_table', {

})


# Test single join, two joins
test_that('test_join', {

})

# Test compound (multi-column) join key
test_that('test_compound_join_key', {

})

# Test compound (multi-column) join key
test_that('test_compound_join_different_length', {

})

# Test single aliased join, test two joins on same table, one aliased
test_that('test_join_alias', {

})

# Test whether the fact will be included in the star schema
test_that('test_fact_is_included', {

})

# Test master-detail-detail snowflake chain joins
test_that('test_snowflake_joins', {

})

# Test master-detail-detail snowflake chain joins
test_that('test_snowflake_aliased_joins', {

})

# Test 'detail' join method
test_that('test_join_method_detail', {

})

# Test 'detail' join master
test_that('test_join_method_master', {

})

# Test that mapping.unary works
test_that('test_unary', {

})

# Test using a statement as a table
test_that('test_statement_table', {

})

context('QueryTestCase')


test_that('test_basic', {

})
