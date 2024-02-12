library(testthat)

# Exploratory Data Analysis Tests

# Test 3.1
test_that("Data set loaded successfully - mathDF", {
  expect_is(mathDF, "data.frame")
  expect_gt(nrow(mathDF), 0)
  expect_gt(ncol(mathDF), 0)
})

# Test 3.2
test_that("Data set loaded successfully - portugueseDF", {
  expect_is(portugueseDF, "data.frame")
  expect_gt(nrow(portugueseDF), 0)
  expect_gt(ncol(portugueseDF), 0)
})

# Test 3.3
test_that("Column counts of mathDF and portugueseDF are correct", {
  expect_equal(ncol(mathDF), ncol(portugueseDF))
})

# Dataset Preprocessingw Tests

# Test 4.1: Were 'mathDF' and 'portugueseDF' tibbles created?
test_that("Were Math and Portuguese tibbles created?", {
  expect_is(tibblemathDF, "tbl_df")
  expect_is(tibbleportugueseDF, "tbl_df")
})

# Test 4.2: Are 'address_type' columns factors?
test_that("Are 'address_type' columns factors?", {
  expect_is(tibblemathDF$address_type, "factor")
  expect_is(tibbleportugueseDF$address_type, "factor")
})

# Test 4.3: Are 'travel_time' columns numeric?
test_that("Are 'travel_time' columns numeric?", {
  expect_is(tibblemathDF$travel_time, "numeric")
  expect_is(tibbleportugueseDF$travel_time, "numeric")
})

# Test 4.4: Are 'study_time' columns numeric?
test_that("Are 'study_time' columns numeric?", {
  expect_is(tibblemathDF$study_time, "numeric")
  expect_is(tibbleportugueseDF$study_time, "numeric")
})

# Test 4.5: Were missing values handled correctly?
test_that("Were missing values handled correctly?", {
  expect_true(all(!is.na(tibblemathDF$grade_1)))
  expect_true(all(!is.na(tibblemathDF$grade_2)))
  expect_true(all(!is.na(tibblemathDF$final_grade)))
  
  expect_true(all(!is.na(tibbleportugueseDF$grade_1)))
  expect_true(all(!is.na(tibbleportugueseDF$grade_2)))
  expect_true(all(!is.na(tibbleportugueseDF$final_grade)))
})

# Data Engineering Tests

# Test 5.1: Does the merged_data data frame contain the expected columns?
test_that("Merged Data Columns Test", {
  expect_true("student_id" %in% colnames(merged_data))
  expect_true("grade_1.x" %in% colnames(merged_data))
  expect_true("grade_2.x" %in% colnames(merged_data))
})

# Test 5.2: Was the weighted_average column calculated correctly?
test_that("Weighted Average Test", {
  expect_true("weighted_average_grade" %in% colnames(merged_data))
})

# Test 5.3: Were avg_travel_time, avg_internet_access, avg_study_time columns calculated correctly?
test_that("Address Type Summary Test", {
  expect_true("avg_travel_time" %in% colnames(address_type_summary))
  expect_true("avg_internet_access" %in% colnames(address_type_summary))
  expect_true("avg_study_time" %in% colnames(address_type_summary))
})

# Test 5.4: Do rural_students and urban_students data frames contain the expected columns?
test_that("Rural and Urban Students Test", {
  expect_true("student_id" %in% colnames(rural_students))
  expect_true("weighted_average_grade" %in% colnames(rural_students))
  expect_true("travel_time.x" %in% colnames(rural_students))
})

# Data Analysis Tests

# Test 6.1: Checking for missing values in the merged_data data frame.
test_that("6.1 Missing Values Test", {
  expect_false(any(is.na(merged_data)))
})

# Test 6.2: Does the similar_study_time_groups data frame contain the expected columns?
test_that("6.2 Similar Study Time Groups Test", {
  expect_true("address_type.x" %in% colnames(similar_study_time_groups))
  expect_true("internet_access.x" %in% colnames(similar_study_time_groups))
  expect_true("weighted_average_grade" %in% colnames(similar_study_time_groups))
  expect_true("avg_travel_time" %in% colnames(similar_study_time_groups))
})

# Test 6.3: Checking for errors during the creation of the plots.
test_that("6.3 Plots Creation Test", {
  expect_true(exists("plot_travel_time_difference"))
  expect_true(exists("plot_internet_access_difference"))
})
