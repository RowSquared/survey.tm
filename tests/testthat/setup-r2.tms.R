test_that("Environmental Variables set", {

  if (Sys.getenv("suso_designer_pw")=="" | Sys.getenv("suso_designer_user")=="") {
    stop(paste0(
      "\nCannot run tests. SurSol Designer credentials not stored in environmental variables",
      "\n'suso_designer_pw' and 'suso_designer_user'"))
  }
})
