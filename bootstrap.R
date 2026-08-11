args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) {
  stop("Usage: Rscript bootstrap.R [expected-release-tag]")
}

archiveDescribe <- "$Format:%(describe:tags,match=v[0-9]*)$"
describe <- tryCatch(
  suppressWarnings(
    system2(
      "git",
      c("describe", "--tags", "--long", "--dirty", "--match", shQuote("v[0-9]*")),
      stdout = TRUE,
      stderr = TRUE
    )
  ),
  error = function(error) structure(conditionMessage(error), status = 1L)
)
status <- attr(describe, "status")
if (!is.null(status) && status != 0L) {
  if (startsWith(archiveDescribe, "$Format:")) {
    stop("Unable to derive the package version from Git tags: ", paste(describe, collapse = "\n"))
  }
  describe <- archiveDescribe
}
if (length(describe) != 1L) {
  stop("Expected one version description")
}

exactMatches <- regmatches(
  describe,
  regexec("^v([0-9]+\\.[0-9]+\\.[0-9]+)$", describe)
)[[1]]
if (length(exactMatches) != 0L) {
  baseVersion <- exactMatches[[2]]
  distance <- 0L
  dirty <- FALSE
} else {
  matches <- regmatches(
    describe,
    regexec(
      "^v([0-9]+\\.[0-9]+\\.[0-9]+)-([0-9]+)-g[0-9a-f]+(-dirty)?$",
      describe
    )
  )[[1]]
  if (length(matches) == 0L) {
    stop("Git tag must use the form vX.Y.Z: ", describe)
  }

  baseVersion <- matches[[2]]
  distance <- as.integer(matches[[3]])
  dirty <- length(matches) == 4L && identical(matches[[4]], "-dirty")
}

if (distance == 0L && !dirty) {
  packageVersion <- baseVersion
} else {
  packageVersion <- paste(baseVersion, "9000", distance, sep = ".")
  if (dirty) {
    packageVersion <- paste(packageVersion, "1", sep = ".")
  }
}

if (length(args) == 1L) {
  expectedTag <- args[[1]]
  if (!grepl("^v[0-9]+\\.[0-9]+\\.[0-9]+$", expectedTag)) {
    stop("Expected release tag must use the form vX.Y.Z: ", expectedTag)
  }
  if (distance != 0L || dirty || !identical(packageVersion, sub("^v", "", expectedTag))) {
    stop(
      "Release tag does not match the checked-out commit: expected ",
      expectedTag,
      ", derived ",
      packageVersion
    )
  }
}

description <- readLines("DESCRIPTION", warn = FALSE)
versionLine <- grep("^Version:", description)
if (length(versionLine) != 1L) {
  stop("DESCRIPTION must contain exactly one Version field")
}
description[[versionLine]] <- paste("Version:", packageVersion)
writeLines(description, "DESCRIPTION", useBytes = TRUE)

cat(packageVersion, "\n", sep = "")
