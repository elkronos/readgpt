# test-review5-records.R -- the fifth pass over the record set: regressions an
# independent check found in the fourth pass (3422d81), and the parts of earlier
# fixes that were incomplete. Each test failed on 3422d81. Where the code before
# any fix (eeff205) got a case right, the test checks it still comes out right;
# where the only safe answer is "neither record", it says so.

r5_ris <- function(path, recs) {
  lines <- unlist(lapply(recs, function(r) c(
    "TY  - JOUR", paste0("AU  - ", strsplit(r$au, "; ", fixed = TRUE)[[1]]),
    paste0("TI  - ", r$ti), paste0("PY  - ", r$py),
    if (!is.null(r$jo)) paste0("JO  - ", r$jo), if (!is.null(r$do)) paste0("DO  - ", r$do),
    "ER  - ", "")))
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  path
}

r5_files <- function(names, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  for (f in names) writeLines(sprintf("Full text of %s.", f), file.path(d, f))
  d
}

r5_rec <- function(recs, files = NULL, env = parent.frame()) {
  f <- r5_ris(withr::local_tempfile(fileext = ".ris", .local_envir = env), recs)
  gr_records(f, files = if (is.null(files)) NULL else r5_files(files, env = env))
}

r5_matched <- function(r) ifelse(is.na(r$records$file), NA_character_, basename(r$records$file))
r5_dup <- function(recs) r5_rec(recs, env = parent.frame())$records$duplicate_of
r5_count <- function(r, stage) r$counts$n[r$counts$stage == stage]
R5 <- function(au, ti, py, jo = NULL, do = NULL) list(au = au, ti = ti, py = py, jo = jo, do = do)

# ---------------------------------------------------------------------------
# records-1: an all-capitals surname with a particle lost its first word.
# ---------------------------------------------------------------------------

test_that("an all-capitals surname keeps its particle, with or without a comma", {
  fa <- readgpt:::record_first_author(c("VAN DAM, P", "DE JONG, P", "DE LA ROSA, J", "ST. JOHN, M",
                                        "LE ROUX J", "VAN DER BERG, A", "VAN DAM P", "LI WS", "KIM S",
                                        "LI W S", "J LI", "LE T", "LI WEI"))
  expect_identical(fa$surname, c("VAN DAM", "DE JONG", "DE LA ROSA", "ST. JOHN", "LE ROUX",
                                 "VAN DER BERG", "VAN DAM", "LI", "KIM", "LI", "LI", "LE", "LI"))
  expect_identical(fa$initial, c("P", "P", "J", "M", "J", "A", "P", "W", "S", "W", "J", "T", "W"))
})

test_that("an all-capitals particle surname takes no other paper's file", {
  ti1 <- "Balance training in older adults"; ti2 <- "Gait speed and falls in the community"
  # VAN DAM was read as "VAN" and given van Dijk's paper.
  r <- r5_rec(list(R5("VAN DAM, P", ti1, 2019)), "van Dijk 2019.pdf")
  expect_identical(r$records$retrieved, FALSE)
  expect_identical(basename(r$unmatched_files), "van Dijk 2019.pdf")
  # ...and with van Dijk's record there too, the file was nobody's.
  r <- r5_rec(list(R5("VAN DAM, P", ti1, 2019), R5("van Dijk, K.", ti2, 2019)), "van Dijk 2019.pdf")
  expect_identical(r5_matched(r), c(NA, "van Dijk 2019.pdf"))
  # "DE LA ROSA" was "ROSA", and its key blocked Rosa's own file.
  r <- r5_rec(list(R5("DE LA ROSA, J", ti1, 2019), R5("Rosa, K.", ti2, 2019)), "Rosa 2019.pdf")
  expect_identical(r5_matched(r), c(NA, "Rosa 2019.pdf"))
  # Each still finds its own.
  r <- r5_rec(list(R5("VAN DAM, P", ti1, 2019), R5("LE ROUX J", ti2, 2019)),
              c("van Dam 2019.pdf", "Le Roux 2019.pdf"))
  expect_identical(r5_matched(r), c("van Dam 2019.pdf", "Le Roux 2019.pdf"))
})

test_that("a Web of Science capitals name merges with its mixed-case copy that has a DOI", {
  ti <- "Sleep duration and academic performance in adolescents"
  for (p in list(c("DE JONG, P", "de Jong, P."), c("LE ROUX, J", "Le Roux J"),
                 c("VAN DER BERG, A", "van der Berg, A."), c("ST. JOHN, M", "St. John, M."),
                 c("DI LUCA, M", "Di Luca, M."))) {
    expect_identical(r5_dup(list(R5(p[1], ti, 2019, jo = "SLEEP"),
                                 R5(p[2], ti, 2019, jo = "Sleep", do = "10.1093/sleep/zsz001"))),
                     c(NA, 1L), info = p[1])
  }
})

# ---------------------------------------------------------------------------
# records-5: BibTeX's "King, Jr., Robert" gave the initial J.
# ---------------------------------------------------------------------------

test_that("a suffix in the middle of a BibTeX name is not the initial", {
  fa <- readgpt:::record_first_author(c("King, Jr., Robert", "Smith, Jr., Robert", "King, Jr.",
                                        "Wallace RB 3rd", "KING JR, R", "Smith JR"))
  expect_identical(fa$surname, c("King", "Smith", "King", "Wallace", "KING", "Smith"))
  expect_identical(fa$initial, c("R", "R", NA, "R", "R", "J"))

  d <- withr::local_tempdir()
  writeLines(c("@article{king2019, author = {King, Jr., Robert and Okafor, Ade},",
               "title = {Effects of exercise on sleep quality in older adults}, journal = {Sleep},",
               "year = {2019}}"), file.path(d, "a_scholar.bib"))
  r5_ris(file.path(d, "b_scopus.ris"), list(R5("King, R.; Okafor, A.",
    "Effects of exercise on sleep quality in older adults", 2019, jo = "Sleep", do = "10.1093/sleep/zsz099")))
  expect_identical(gr_records(d)$records$duplicate_of, c(NA, 1L))
})

# ---------------------------------------------------------------------------
# records-6: "\l{} " ate the space after it; stacked accents half decoded.
# ---------------------------------------------------------------------------

test_that("a LaTeX letter ended by {} keeps the space after it", {
  un <- readgpt:::bib_unlatex
  expect_identical(un("Pawe\\l{} Nowak and Jan Kowalski"), "Pawe\u0142 Nowak and Jan Kowalski")
  expect_identical(un("Anna Wei\\ss{} and John Smith"), "Anna Wei\u00df and John Smith")
  expect_identical(un("\\O{}stergaard"), "\u00d8stergaard")
  # The space that ends a bare command still goes.
  expect_identical(un("Stra\\ss e"), "Stra\u00dfe")

  d <- withr::local_tempdir()
  writeLines(c("@article{a, author = {Pawe\\l{} Nowak and Jan Kowalski},",
               "title = {A cohort study of night shift workers}, year = {2019}}",
               "@article{b, author = {Kowalski, Micha\\l{} and Nowak, Jan},",
               "title = {Sleep and appetite in shift workers}, year = {2018}}"), file.path(d, "a.bib"))
  r5_ris(file.path(d, "b.ris"), list(R5("Nowak, P.; Kowalski, J.", "A cohort study of night shift workers", 2019)))
  r <- gr_records(d, files = r5_files(c("Nowak 2019.pdf", "Kowalski_2018.pdf")))
  expect_identical(r$records$authors[1:2], c("Pawe\u0142 Nowak; Jan Kowalski", "Kowalski, Micha\u0142; Nowak, Jan"))
  expect_identical(r$records$duplicate_of, c(NA, NA, 1L))
  expect_identical(r5_matched(r), c("Nowak 2019.pdf", "Kowalski_2018.pdf", NA))
})

test_that("stacked LaTeX accents compose to the Vietnamese letter", {
  un <- readgpt:::bib_unlatex
  expect_identical(un("Nguy{\\~{\\^{e}}}n, V"), "Nguy{\u1ec5}n, V")
  expect_identical(un("Nguy\\~{\\^e}n, V"), "Nguy\u1ec5n, V")
  expect_identical(un("Tr{\\`{\\^a}}n"), "Tr{\u1ea7}n")
  expect_identical(un("\\'{\\^e}"), "\u1ebf")
  expect_identical(un("{\\d{\\^o}}"), "{\u1ed9}")
  expect_identical(un("L\\\"{u}\\'{\\\"u}"), "L\u00fc\u01d8")
  # No table entry: the mark follows the letter, and the key still folds.
  expect_identical(un("\\^{\\'e}"), "\u00e9\u0302")
  expect_identical(readgpt:::title_key(un("\\^{\\'e}")), "e")
  # Commands that are not accents are left alone.
  expect_identical(un("$\\hat{x}$ \\href{y} \\vspace{1}"), "$\\hat{x}$ \\href{y} \\vspace{1}")

  d <- withr::local_tempdir()
  writeLines(c("@article{a, author = {Nguy{\\~{\\^{e}}}n, Van and Tr{\\`{\\^a}}n, Binh},",
               "title = {Dengue in the Mekong delta}, year = {2020}}"), file.path(d, "a.bib"))
  expect_identical(gr_records(d)$records$authors, "Nguy\u1ec5n, Van; Tr\u1ea7n, Binh")
})

# ---------------------------------------------------------------------------
# records-7: a quoted value ending in a backslash never closed.
# ---------------------------------------------------------------------------

test_that("a quoted BibTeX value ending in a backslash closes at its quote", {
  f <- withr::local_tempfile(fileext = ".bib")
  writeLines(c('@article{a11, author = "Back, I.", title = "Ends with a line break\\\\", year = 2016, journal = {K}}',
               '@article{a12, author="M\\"uller, J. and Lee, K.", title = "An \\"{U}ber title", year = {2021}}'), f)
  r <- gr_records(f)$records
  expect_identical(r$year, c("2016", "2021"))
  expect_identical(r$venue[1], "K")
  # The umlaut written \" is still text, not a quote.
  expect_identical(r$authors[2], "M\u00fcller, J.; Lee, K.")
  expect_identical(r$title[2], "An \u00dcber title")
})

# ---------------------------------------------------------------------------
# records-9: compatibility characters the fold table left out.
# ---------------------------------------------------------------------------

test_that("Roman numerals, circled digits, fractions and ordinals fold as they used to", {
  tk <- readgpt:::title_key
  expect_identical(tk("Phase \u2161 trial of drug X in type \u2161 diabetes"),
                   tk("Phase II trial of drug X in type II diabetes"))
  expect_identical(tk("Stage \u2173 and \u216b"), tk("Stage iv and XII"))
  expect_identical(tk("Trial \u2460 and \u2473"), tk("Trial 1 and 20"))
  expect_identical(tk("\u215c inch"), tk("3/8 inch"))
  expect_identical(tk("5 m\u2113 dose"), tk("5 ml dose"))
  expect_identical(tk("1\u00ba ano"), tk("1o ano"))
  expect_identical(tk("x\u2099 and \u24b6"), tk("xn and A"))

  r <- r5_rec(list(R5("Jones, Ann", "Phase \u2161 trial of drug X in type \u2161 diabetes", 2020),
                   R5("Jones, A.", "Phase II trial of drug X in type II diabetes", 2020)))
  expect_identical(r$records$duplicate_of, c(NA, 1L))
  r <- r5_rec(list(R5("Smith, John", "Phase \u2162 trial of drug X in adults", 2019)),
              "Phase III trial of drug X in adults.txt")
  expect_identical(r$records$retrieved, TRUE)
})

# ---------------------------------------------------------------------------
# records-4: a journal written with its expansion was a different journal.
# ---------------------------------------------------------------------------

test_that("an acronym written with its expansion is the acronym's journal", {
  ti <- "Effects of exercise on sleep quality in older adults"
  pair <- function(a, b) r5_dup(list(R5("Hulley, S.", ti, 1998, jo = a),
                                     R5("Hulley S", ti, 1998, jo = b, do = "10.1001/jama.280.19.1690")))
  expect_identical(pair("JAMA", "JAMA - Journal of the American Medical Association"), c(NA, 1L))
  expect_identical(pair("Jama", "JAMA-JOURNAL OF THE AMERICAN MEDICAL ASSOCIATION"), c(NA, 1L))
  expect_identical(pair("BMJ", "BMJ-BRITISH MEDICAL JOURNAL"), c(NA, 1L))
  expect_identical(pair("Journal of the American Medical Association",
                        "JAMA - Journal of the American Medical Association"), c(NA, 1L))
  expect_identical(pair("Proceedings of the National Academy of Sciences",
                        "PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA"),
                   c(NA, 1L))
  # Different journals stay different.
  expect_identical(pair("BMJ", "BMJ Open"), c(NA_integer_, NA_integer_))
  expect_identical(pair("BMJ-BRITISH MEDICAL JOURNAL", "BMJ Open"), c(NA_integer_, NA_integer_))
  expect_identical(pair("JAMA", "JAMA Internal Medicine"), c(NA_integer_, NA_integer_))
  expect_identical(pair("JAMA - Journal of the American Medical Association", "JAMA Internal Medicine"),
                   c(NA_integer_, NA_integer_))

  # The PubMed copy without a DOI and the Scopus copy with it are one report.
  r <- r5_rec(list(R5("Hulley, S.", ti, 1998, jo = "JAMA"),
                   R5("Hulley S", ti, 1998, jo = "JAMA - Journal of the American Medical Association",
                      do = "10.1001/jama.280.19.1690")), "10.1001_jama.280.19.1690.pdf")
  expect_equal(r5_count(r, "records screened"), 1L)
  expect_equal(r5_count(r, "reports not retrieved"), 0L)
})

# ---------------------------------------------------------------------------
# synthesis-1: an organisation's acronym and its name were two authors.
# ---------------------------------------------------------------------------

test_that("an organisation and its acronym are one author", {
  ti <- "Guidelines on physical activity and sedentary behaviour"
  for (p in list(c("WHO", "World Health Organization"),
                 c("CDC", "Centers for Disease Control and Prevention"),
                 c("NICE", "National Institute for Health and Care Excellence"),
                 c("World Health Organisation", "World Health Organization"))) {
    expect_identical(r5_dup(list(R5(p[1], ti, 2020), R5(p[2], ti, 2020))), c(NA, 1L), info = p[1])
  }
  # Another organisation, or a person, is not the acronym.
  for (p in list(c("OECD", "World Health Organization"), c("WHO", "Okafor, A."))) {
    expect_identical(r5_dup(list(R5(p[1], ti, 2020), R5(p[2], ti, 2020))),
                     c(NA_integer_, NA_integer_), info = p[1])
  }
})

# ---------------------------------------------------------------------------
# records-5 and records-6: a surname matching the other record's given name.
# ---------------------------------------------------------------------------

test_that("a surname that is another author's given name does not join a DOI record", {
  ti <- "Authors reply to the correspondence"
  doi <- "10.1016/S0140-6736(20)31234-5"
  for (p in list(c("Li, W.", "Zhang, Li; Chen, Y."), c("Kim, H.", "Smith, Kim"), c("Wei, L.", "Wang, Wei"),
                 c("Lin, M.", "Chen, Lin"), c("Li, W.", "Li Wei"))) {
    for (o in list(1:2, 2:1)) {
      recs <- list(R5(p[1], ti, 2020, jo = "Lancet"), R5(p[2], ti, 2020, jo = "The Lancet", do = doi))[o]
      r <- r5_rec(recs, "10.1016_S0140-6736(20)31234-5.txt")
      # Li's record took Zhang's DOI and Zhang's file.
      expect_identical(r$records$duplicate_of, c(NA_integer_, NA_integer_), info = p[1])
      expect_identical(r5_matched(r), c(NA, "10.1016_S0140-6736(20)31234-5.txt")[o], info = p[1])
    }
  }
})

test_that("name order counts between records without DOIs only when the initial bears it out", {
  ti <- "Effects of exercise on sleep quality in older adults"
  # The help says the surnames must agree; these are different people.
  for (p in list(c("Thomas, P.", "Thomas Smith"), c("Li, X.", "Li Wang"), c("Lee, K.", "Lee Anderson"),
                 c("Thomas, P.", "Smith, Thomas"), c("Li, W.", "Zhang, Li"))) {
    expect_identical(r5_dup(list(R5(p[1], ti, 2020), R5(p[2], ti, 2020))),
                     c(NA_integer_, NA_integer_), info = paste(p, collapse = " / "))
  }
  # "Li Wei" and "Li, W." are one person written two ways...
  expect_identical(r5_dup(list(R5("Li, W.", ti, 2020, jo = "Sleep"), R5("Li Wei", ti, 2020, jo = "Sleep"))),
                   c(NA, 1L))
  expect_identical(r5_dup(list(R5("Li, W.", ti, 2020), R5("Li Wei", ti, 2020))), c(NA, 1L))
  # ...but not in two journals.
  expect_identical(r5_dup(list(R5("Li, W.", ti, 2020, jo = "Lancet"), R5("Li Wei", ti, 2020, jo = "Sleep Medicine"))),
                   c(NA_integer_, NA_integer_))
})

# ---------------------------------------------------------------------------
# records-audit-03: one surname, two initials, one generic title.
# ---------------------------------------------------------------------------

test_that("two initials of one surname are two authors without DOIs too", {
  expect_identical(r5_dup(list(R5("Wang, L.", "Letter to the editor", 2020, jo = "Int J Cardiol"),
                               R5("Wang, H.", "Letter to the editor", 2020, jo = "Clin Nutr"))),
                   c(NA_integer_, NA_integer_))
  expect_identical(r5_dup(list(R5("Kim, S.", "Author's reply", 2020, jo = "Gut"),
                               R5("Kim, J.", "Author's reply", 2020, jo = "Hepatology"))),
                   c(NA_integer_, NA_integer_))
  ti <- "Nursing care of patients after laparoscopic cholecystectomy"
  expect_identical(r5_dup(list(R5("Wang, L.", ti, 2021), R5("Wang, H.", ti, 2021))), c(NA_integer_, NA_integer_))
  # One author written two ways is still one work.
  expect_identical(r5_dup(list(R5("Wang, Li", ti, 2021), R5("Wang L", ti, 2021))), c(NA, 1L))
})

# ---------------------------------------------------------------------------
# records-2: a compound surname and its first part.
# ---------------------------------------------------------------------------

test_that("a hyphenated surname's file is not its first part's, nor its second's", {
  ga <- list(R5("Garc\u00eda-L\u00f3pez, M.", "Balance training in older adults", 2019),
             R5("Garc\u00eda, R.", "Gait speed in the community", 2019))
  files <- c("Garcia-Lopez - 2019 - Balance.pdf", "Garcia - 2019 - Gait.pdf")
  for (o in list(1:2, 2:1)) {
    r <- r5_rec(ga[o], files)
    expect_identical(r5_matched(r), files[o])
    expect_equal(r5_count(r, "reports not retrieved"), 0L)
  }
  sj <- list(R5("Smith, J.", "Balance training in older adults", 2019),
             R5("Smith-Jones, K.", "Gait speed in the community", 2019))
  for (o in list(1:2, 2:1)) {
    expect_identical(r5_matched(r5_rec(sj[o], c("Smith 2019.pdf", "Smith-Jones 2019.pdf"))),
                     c("Smith 2019.pdf", "Smith-Jones 2019.pdf")[o])
  }
  # Alone, Smith or Jones is not given Smith-Jones's paper.
  expect_identical(r5_rec(sj[1], "Smith-Jones 2019.pdf")$records$retrieved, FALSE)
  expect_identical(r5_rec(list(R5("Jones, K.", "Gait speed", 2019)), "Smith-Jones 2019.pdf")$records$retrieved,
                   FALSE)
  # A hyphen that separates fields is not part of a name.
  expect_identical(r5_rec(sj[1], "Smith-EtAl-2019.pdf")$records$retrieved, TRUE)
  expect_identical(r5_rec(sj[1], "smith-et-al-2019-balance.pdf")$records$retrieved, TRUE)
  expect_identical(r5_rec(sj[1], "Smith-2019-Balance training.pdf")$records$retrieved, TRUE)
})

# ---------------------------------------------------------------------------
# records-audit-01: a surname named first is not always the first author.
# ---------------------------------------------------------------------------

test_that("a surname earlier in a filename does not take another work's paper", {
  wiley <- "Early Intervention in Psychiatry - 2020 - Smith - Screening for the psychosis prodrome.txt"
  recs <- list(R5("Smith, A.", "Screening for the psychosis prodrome in primary care", 2020),
               R5("Early, K.", "Mentoring for newly qualified nurses", 2020))
  for (o in list(1:2, 2:1)) {
    # Early was retrieved with Smith's paper, in either order.
    expect_identical(r5_matched(r5_rec(recs[o], wiley)), c(wiley, NA)[o])
  }
  # The title in the name decides it even when Smith is also Early's co-author.
  recs[[2]]$au <- "Early, K.; Smith, A."
  expect_identical(r5_matched(r5_rec(recs, wiley)), c(wiley, NA))
  # Nothing in the name says whose it is: neither, and the file is listed.
  jama <- list(R5("Smith, A.", "Statins and dementia", 2019), R5("Jama, A.", "Malaria in Somalia", 2019))
  for (o in list(1:2, 2:1)) {
    r <- r5_rec(jama[o], "jama_smith_2019_oi_190012.txt")
    expect_identical(r$records$retrieved, c(FALSE, FALSE))
    expect_identical(basename(r$unmatched_files), "jama_smith_2019_oi_190012.txt")
  }
  young <- list(R5("Smith, A.", "Exercise in young adults: a trial", 2019), R5("Young, K.", "Hip fracture outcomes", 2019))
  for (o in list(1:2, 2:1)) {
    expect_identical(r5_rec(young[o], "Young adults exercise - Smith 2019.txt")$records$retrieved, c(FALSE, FALSE))
  }
  # A name that lists the first work's authors is still the first work's.
  kl <- list(R5("Kim, S.; Lee, K.", "Sleep and mood in nurses", 2020), R5("Lee, J.", "Shift work and appetite", 2020))
  for (o in list(1:2, 2:1)) {
    expect_identical(r5_matched(r5_rec(kl[o], c("Kim and Lee 2020.txt", "Lee 2020.txt"))),
                     c("Kim and Lee 2020.txt", "Lee 2020.txt")[o])
  }
})

test_that("the end of a longer surname is not a surname, and a record's capitals mark its parts", {
  alone <- function(au, py, file) r5_rec(list(R5(au, "Balance training in older adults", py)), file,
                                         env = parent.frame())$records$retrieved
  # Connor was given O'Connor's paper, Souza De Souza's, Berg van der Berg's.
  expect_identical(alone("Connor, J.", 2019, "O'Connor 2019.txt"), FALSE)
  expect_identical(alone("Souza, M.", 2021, "De Souza 2021.txt"), FALSE)
  expect_identical(alone("Berg, T.", 2020, "van der Berg 2020.txt"), FALSE)
  expect_identical(alone("Kay, T.", 2020, "Mc Kay 2020.txt"), FALSE)
  expect_identical(alone("John, K.", 2019, "St. John 2019.pdf"), FALSE)
  # Each longer name still finds its own, however the file spells it.
  expect_identical(alone("O'Connor, P.", 2019, "O'Connor 2019.txt"), TRUE)
  expect_identical(alone("de Souza, M.", 2021, "De Souza 2021.txt"), TRUE)
  expect_identical(alone("DeWitt, A.", 2018, "De Witt 2018.txt"), TRUE)
  expect_identical(alone("OBrien, K.", 2018, "O'Brien_2018.txt"), TRUE)
  expect_identical(alone("McKay, R.", 2019, "McKay2019.txt"), TRUE)
  # A particle in a given-name-first BibTeX name is part of the surname.
  expect_identical(readgpt:::record_first_author(c("Jan van Leeuwen", "Bin Li"))$surname, c("van Leeuwen", "Li"))
  d <- withr::local_tempdir()
  writeLines(c("@inproceedings{k, author = {Jan van Leeuwen and Min Kim},",
               "title = {Retrieval practice in interface learning}, booktitle = {CHI}, year = {2021}}"),
             file.path(d, "dblp.bib"))
  for (f in c("van Leeuwen 2021.pdf", "vanLeeuwen2021.pdf")) {
    expect_identical(gr_records(file.path(d, "dblp.bib"), files = r5_files(f))$records$retrieved, TRUE, info = f)
  }
})
