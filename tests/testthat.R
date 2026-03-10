# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(reticulate)

python_path <- Sys.getenv("RETICULATE_PYTHON", unset = "")
if (nchar(python_path) > 0) {
  reticulate::use_python(python_path, required = FALSE)
}

test_check("marea")
