test_that("the listening sheet lists every species and its clips", {
  dir <- withr::local_tempdir()
  cfg <- structure(
    list(name = "Test deployment", location = list(place = "Nowhere")),
    class = "birdnet_deployment"
  )

  written <- dplyr::tibble(
    species = c("Zebra Finch", "Zebra Finch", "Emu"),
    rank = c(1L, 2L, 1L),
    confidence = c(0.98, 0.91, 0.72),
    site = "A",
    source = "x.wav",
    offset_s = c(0, 3, 6),
    clip = file.path(dir, c("zebra_finch/01.wav", "zebra_finch/02.wav",
                            "emu/01.wav"))
  )
  data <- dplyr::tibble(
    `Common Name` = c(rep("Zebra Finch", 5), rep("Emu", 2)),
    threshold_source = c(rep("calibrated", 5), rep("unvalidated", 2))
  )

  path <- write_verification_sheet(written, data, cfg, dir)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_true(file.exists(path))
  expect_match(html, "Zebra Finch", fixed = TRUE)
  expect_match(html, "Emu", fixed = TRUE)
  # Relative paths only, or the sheet breaks the moment the folder is moved.
  expect_match(html, 'src="zebra_finch/01.wav"', fixed = TRUE)
  expect_false(grepl(dir, html, fixed = TRUE))
  # Every placeholder filled.
  expect_false(grepl("{{", html, fixed = TRUE))
})

test_that("unvalidated species are flagged and sorted first", {
  dir <- withr::local_tempdir()
  cfg <- structure(list(name = "T", location = list(place = "P")),
                   class = "birdnet_deployment")

  written <- dplyr::tibble(
    species = c("Zebra Finch", "Emu"), rank = 1L, confidence = c(0.9, 0.7),
    site = "A", source = "x.wav", offset_s = 0,
    clip = file.path(dir, c("zebra_finch/01.wav", "emu/01.wav"))
  )
  # Zebra Finch has more detections, so only the flag can put Emu first.
  data <- dplyr::tibble(
    `Common Name` = c(rep("Zebra Finch", 50), rep("Emu", 2)),
    threshold_source = c(rep("calibrated", 50), rep("unvalidated", 2))
  )

  html <- paste(readLines(write_verification_sheet(written, data, cfg, dir,
                                                   flag_species = "Emu"),
                          warn = FALSE), collapse = "\n")

  expect_lt(regexpr('data-species="Emu"', html, fixed = TRUE),
            regexpr('data-species="Zebra Finch"', html, fixed = TRUE))
  expect_match(html, "not validated", fixed = TRUE)
})

test_that("HTML special characters in a species name are escaped", {
  expect_equal(escape_html('Fox & "Cat" <b>'), "Fox &amp; &quot;Cat&quot; &lt;b&gt;")
})

test_that("clip folders for species no longer reported are removed", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "zebra_finch"))
  dir.create(file.path(dir, "thick_billed_grasswren"))
  file.create(file.path(dir, "thick_billed_grasswren", "01.wav"))
  file.create(file.path(dir, "clips.csv"))

  suppressMessages(prune_stale_clips(dir, "Zebra Finch"))

  expect_true(dir.exists(file.path(dir, "zebra_finch")))
  expect_false(dir.exists(file.path(dir, "thick_billed_grasswren")))
  expect_true(file.exists(file.path(dir, "clips.csv")))
})

test_that("n_species keeps the most-detected species only", {
  skip_if_not(nzchar(Sys.which("ffmpeg")), "ffmpeg not available")
  dir <- withr::local_tempdir()
  rec <- file.path(dir, "recordings")
  dir.create(rec)
  # A one-second silent wav is enough: the test is about which species survive.
  system2("ffmpeg", c("-v", "error", "-y", "-f", "lavfi", "-i",
                      "anullsrc=r=8000:cl=mono", "-t", "5",
                      shQuote(file.path(rec, "a.wav"))))

  cfg <- structure(
    list(name = "T", location = list(place = "P"),
         sites = list(list(id = "A", recordings = rec))),
    class = "birdnet_deployment"
  )
  data <- dplyr::tibble(
    `Common Name` = c(rep("Common", 10), rep("Middling", 5), rep("Rare", 1)),
    Confidence = 0.9,
    `Begin Path` = file.path(rec, "a.wav"),
    `File Offset (s)` = 0,
    Site = "A",
    threshold_source = "default"
  )

  out <- suppressMessages(
    export_top_clips(data, cfg, dir, n_per_species = 1, n_species = 2)
  )

  expect_setequal(out$species, c("Common", "Middling"))
  expect_equal(attr(out, "n_species_total"), 3L)

  # The sheet has to say it is a subset, or "all correct" reads as a verdict
  # on species nobody was played.
  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE),
                collapse = "\n")
  expect_match(html, "2 most-detected of the 3 species", fixed = TRUE)
})
