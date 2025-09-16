test_that(
  desc = "Validating file exists.", 
  code = {
    expect_no_error(
      object = file_validation(
        filename = example_script("generic_script")
      )
    )
  }
)

test_that(
  desc = "Ensuring error message exists.", 
  code = {
    expect_error(
      object = file_validation(
        filename = "this_file_does_not_exist.R"
      )
    )
  }
)

test_that(
  desc = "Verifying warning.", 
  code = {
    expect_warning(
      object = file_validation(
        filename = paste0(
          system.file(package = "autogradeR"), 
          "/../README.Rmd"
        )
      )
    )
  }
)



