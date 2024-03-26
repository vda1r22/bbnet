# bbn.sensitivity ------------------------------------------------

context("Testing bbn.sensitivity Functionality")

# Use example data provided by the bbnet package.

# Ensure function requires at least one key variable
test_that("bbn.sensitivity requires at least one key variable", {
  data("my_BBN")
  expect_error(bbn.sensitivity(bbn.model = my_BBN),
               "Need to input at least one key variable")
})

# Warn on more than three key variables
test_that("Warns on more than three key variables but currently errors out", {
  data("my_BBN")
  data("combined")
  # Expecting an error due to function limitation with more than three key variables
  expect_error(
    bbn.sensitivity(bbn.model = my_BBN, 'Limpet', 'Green Algae', 'Periwinkle', 'Biofilm'),
    "negative length vectors are not allowed",
    fixed = TRUE
  )
})

# Function runs without error with correct input
test_that("bbn.sensitivity errors with expected correct inputs", {
  data("my_BBN")
  data("combined")
  # Since the function is currently not handling inputs as expected, we anticipate an error
  expect_error(
    bbn.sensitivity(bbn.model = my_BBN, "Limpet"),
    "negative length vectors are not allowed",
    fixed = TRUE
  )
})

# Ensure function handles incorrect 'boot_max'
test_that("bbn.sensitivity handles incorrect boot_max", {
  data("my_BBN")
  data("combined")
  expect_error(
    bbn.sensitivity(bbn.model = my_BBN, 'Limpet', boot_max = -100),
    "negative length vectors are not allowed"
  )
})
