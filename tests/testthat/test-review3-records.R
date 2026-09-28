# test-review3-records.R -- the third review of the record set: regressions the
# second pass of fixes introduced, and the parts of earlier fixes that were
# incomplete. Each test failed on the code it was written against (41fe931),
# and each checks the case the code before any of these fixes (eeff205) got
# right still comes out right.

r3_ris <- function(path, recs) {
  lines <- unlist(lapply(recs, function(r) c(
    "TY  - JOUR", paste0("AU  - ", strsplit(r$au, "; ", fixed = TRUE)[[1]]),
    paste0("TI  - ", r$ti), paste0("PY  - ", r$py),
    if (!is.null(r$jo)) paste0("JO  - ", r$jo), if (!is.null(r$do)) paste0("DO  - ", r$do),
    if (!is.null(r$l1)) paste0("L1  - ", r$l1), "ER  - ", "")))
  # Bytes, as a reference manager writes them.
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  path
}

r3_files <- function(names, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  for (f in names) writeLines(sprintf("Full text of %s. A trial with n = 120.", f), file.path(d, f))
  d
}

r3_matched <- function(r) ifelse(is.na(r$records$file), NA_character_, basename(r$records$file))
r3_count <- function(r, stage) r$counts$n[r$counts$stage == stage]

# ---------------------------------------------------------------------------
# records-1: the kept copy lacked the DOI and journal its duplicate carried.
# ---------------------------------------------------------------------------

test_that("the kept record takes the DOI and venue its duplicate carried", {
  d <- withr::local_tempdir()
  writeLines(c("@article{smith2019, author = {Smith, J.},",
               "title = {Spaced practice improves long-term retention}, year = {2019}}"),
             file.path(d, "a_scholar.bib"))
  writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - Spaced practice improves long-term retention",
               "PY  - 2019", "DO  - 10.1000/spaced.2019", "JO  - Journal of Educational Psychology",
               "VL  - 111", "DB  - Scopus", "ER  -"), file.path(d, "b_scopus.ris"))
  files <- r3_files("10.1000_spaced.2019.txt")
  r <- gr_records(d, files = files)
  expect_identical(r$records$duplicate_of, c(NA, 1L))
  expect_identical(r$records$doi[1], "10.1000/spaced.2019")
  expect_identical(r$records$venue[1], "Journal of Educational Psychology")
  expect_identical(r$records$volume[1], "111")
  # Where each row came from stays its own.
  expect_identical(r$records$source_file, c("a_scholar.bib", "b_scopus.ris"))
  expect_identical(r3_matched(r), c("10.1000_spaced.2019.txt", NA))

  cl <- gr_mock_client(function(messages, params) '{"n": 120, "n__quote": "A trial with n = 120"}')
  ext <- quiet(gr_extract(r, gr_fields(n = gr_field("participants", type = "integer")), client = cl))
  # The extraction table said DOI NA and venue NA for a document it had found
  # through that very DOI.
  expect_identical(ext$table$doi, "10.1000/spaced.2019")
  expect_identical(ext$table$venue, "Journal of Educational Psychology")
})

# ---------------------------------------------------------------------------
# records-2: two copies of one work, each with its own attached file.
# ---------------------------------------------------------------------------

test_that("a record and its duplicate each carrying a PDF retrieve the work", {
  d <- withr::local_tempdir()
  pdf <- file.path(d, "PDF"); dir.create(pdf)
  file.create(file.path(pdf, c("Okafor-2021-Retrieval practice.pdf",
                               "Okafor-2021-Retrieval practice-1.pdf")))
  lib <- r3_ris(file.path(d, "lib.ris"), list(
    list(au = "Okafor, A.", ti = "Retrieval practice in the classroom", py = 2021,
         do = "10.1000/rp.2021.77", l1 = "internal-pdf://1234567890/Okafor-2021-Retrieval practice.pdf"),
    list(au = "Okafor, A.", ti = "Retrieval practice in the classroom", py = 2021,
         do = "10.1000/rp.2021.77", l1 = "internal-pdf://0987654321/Okafor-2021-Retrieval practice-1.pdf")))
  r <- gr_records(lib, files = pdf)
  # Pooled, the two paths were "two candidates" and neither was claimed: not
  # retrieved, both files unmatched, the full text gone from the corpus.
  expect_identical(r$records$retrieved, c(TRUE, NA))
  expect_identical(r3_matched(r), c("Okafor-2021-Retrieval practice.pdf",
                                    "Okafor-2021-Retrieval practice-1.pdf"))
  expect_equal(r3_count(r, "reports not retrieved"), 0L)
  expect_length(r$unmatched_files, 0L)
  expect_length(corpus_sources(r), 1L)

  # Zotero storage: the same file name in two folders, full paths recorded.
  st <- file.path(d, "storage")
  dir.create(file.path(st, "AAAA1111"), recursive = TRUE); dir.create(file.path(st, "BBBB2222"))
  p1 <- file.path(st, "AAAA1111", "Okafor - 2021 - Retrieval practice.txt")
  p2 <- file.path(st, "BBBB2222", "Okafor - 2021 - Retrieval practice.txt")
  writeLines("first copy", p1); writeLines("second copy", p2)
  zot <- r3_ris(file.path(d, "zot.ris"), list(
    list(au = "Okafor, C.", ti = "Retrieval practice in the classroom", py = 2021, l1 = p1),
    list(au = "Okafor, C.", ti = "Retrieval practice in the classroom", py = 2021, l1 = p2)))
  rz <- gr_records(zot, files = st)
  expect_identical(rz$records$retrieved, c(TRUE, NA))
  expect_identical(normalizePath(rz$records$file[1]), normalizePath(p1))
  expect_length(rz$unmatched_files, 0L)

  # Three copies, the kept one with no path: the first duplicate's file is the
  # work's, and the second duplicate keeps its own.
  ab <- r3_files(c("a.pdf", "b.pdf"))
  three <- r3_ris(file.path(d, "three.ris"), list(
    list(au = "Okafor, C.", ti = "Retrieval practice in the classroom", py = 2021, do = "10.1000/rp.2021.77"),
    list(au = "Okafor, C.", ti = "Retrieval practice in the classroom", py = 2021, do = "10.1000/rp.2021.77",
         l1 = "a.pdf"),
    list(au = "Okafor, C.", ti = "Retrieval practice in the classroom", py = 2021, do = "10.1000/rp.2021.77",
         l1 = "b.pdf")))
  r3 <- gr_records(three, files = ab)
  expect_identical(r3$records$retrieved, c(TRUE, NA, NA))
  expect_identical(r3_matched(r3), c("a.pdf", NA, "b.pdf"))
  expect_length(corpus_sources(r3), 1L)

  # The case the pooling was for still works: only the duplicate's file exists.
  r4 <- gr_records(three, files = r3_files("b.pdf"))
  expect_identical(r3_matched(r4), c("b.pdf", NA, NA))
})

# ---------------------------------------------------------------------------
# records-3: a duplicate's e-pub year knocked out another work's own key.
# ---------------------------------------------------------------------------

test_that("a duplicate's e-pub year does not take another paper's author-year file", {
  d <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "AU  - Jones, K.", "TI  - Retrieval practice in secondary science classrooms",
               "PY  - 2020", "DO  - 10.1000/rp.2020.1", "ER  - ", "",
               "TY  - JOUR", "AU  - Jones, A.", "TI  - Sleep and memory consolidation in adolescents",
               "PY  - 2019", "ER  - ", ""), file.path(d, "a_scopus.ris"))
  # PubMed gives the same Jones K paper its e-pub year.
  writeLines(c("TY  - JOUR", "AU  - Jones K", "TI  - Retrieval practice in secondary science classrooms.",
               "PY  - 2019", "DO  - 10.1000/rp.2020.1", "ER  - ", ""), file.path(d, "b_pubmed.ris"))
  files <- r3_files(c("Jones 2020.pdf", "Jones 2019.pdf"))
  r <- gr_records(d, files = files)
  expect_identical(r$records$duplicate_of, c(NA, NA, 1L))
  expect_identical(r3_matched(r), c("Jones 2020.pdf", "Jones 2019.pdf", NA))
  expect_equal(r3_count(r, "reports not retrieved"), 0L)
  expect_length(r$unmatched_files, 0L)

  # And when Jones A is in both exports, the usual overlap.
  writeLines(c("TY  - JOUR", "AU  - Jones K", "TI  - Retrieval practice in secondary science classrooms.",
               "PY  - 2019", "DO  - 10.1000/rp.2020.1", "ER  - ", "",
               "TY  - JOUR", "AU  - Jones A", "TI  - Sleep and memory consolidation in adolescents.",
               "PY  - 2019", "ER  - ", ""), file.path(d, "b_pubmed.ris"))
  r2 <- gr_records(d, files = files)
  expect_identical(r2$records$duplicate_of, c(NA, NA, 1L, 2L))
  expect_identical(r3_matched(r2)[1:2], c("Jones 2020.pdf", "Jones 2019.pdf"))
  expect_equal(r3_count(r2, "reports retrieved"), 2L)
})

# ---------------------------------------------------------------------------
# records-4 and synthesis-1: a group or acronym author voided the first author.
# ---------------------------------------------------------------------------

test_that("a group author later in the list does not hide the first author", {
  d <- withr::local_tempdir()
  files <- r3_files(c("Horby 2021.pdf", "Smith2019.pdf", "WHO_2020.pdf"))
  f <- r3_ris(file.path(d, "a.ris"), list(
    list(au = "Horby P; Lim WS; RECOVERY Collaborative Group",
         ti = "Dexamethasone in hospitalized patients with covid", py = 2021),
    list(au = "Smith, J.; Okafor, A.; COVIDSurg Collaborative",
         ti = "Timing of surgery after covid infection", py = 2019),
    list(au = "WHO", ti = "Guidance on mask use in community settings", py = 2020)))
  r <- gr_records(f, files = files)
  expect_identical(r3_matched(r), c("Horby 2021.pdf", "Smith2019.pdf", "WHO_2020.pdf"))

  # The same paper indexed with and without the group, venue spelled two ways.
  g <- r3_ris(file.path(d, "g.ris"), list(
    list(au = "Smith, J.; RECOVERY Collaborative Group",
         ti = "Tocilizumab in severe covid pneumonia outcomes", py = 2021, jo = "Eur Respir J"),
    list(au = "Smith, J.", ti = "Tocilizumab in severe covid pneumonia outcomes", py = 2021,
         jo = "European Respiratory Journal")))
  expect_identical(gr_records(g)$records$duplicate_of, c(NA, 1L))

  # Different first authors are different letters, group or no group.
  h <- r3_ris(file.path(d, "h.ris"), list(
    list(au = "Horby, P.; RECOVERY Collaborative Group",
         ti = "Response to the commentary on dexamethasone trial", py = 2021),
    list(au = "Jones, K.", ti = "Response to the commentary on dexamethasone trial", py = 2021, jo = "BMJ"),
    list(au = "Smith, J.; EPIC Study Group", ti = "Response to the commentary on exercise and sleep",
         py = 2021),
    list(au = "Garcia, R.", ti = "Response to the commentary on exercise and sleep", py = 2021),
    list(au = "WHO", ti = "Correspondence on community mask guidance", py = 2020),
    list(au = "Okafor, A.", ti = "Correspondence on community mask guidance", py = 2020)))
  expect_true(all(is.na(gr_records(h)$records$duplicate_of)))

  # ...so Garcia's file is no longer handed to the Smith record it was merged into.
  v <- r3_ris(file.path(d, "v.ris"), list(
    list(au = "Smith, J; Okafor, A; ADAPT Study Group",
         ti = "Response to the commentary on exercise and sleep outcomes", py = 2021, jo = "BMJ"),
    list(au = "Garcia, R.", ti = "Response to the commentary on exercise and sleep outcomes",
         py = 2021, jo = "BMJ")))
  rv <- gr_records(v, files = r3_files("Garcia2021.pdf"))
  expect_identical(r3_matched(rv), c(NA, "Garcia2021.pdf"))
})

test_that("the record set does not depend on how citations read author lists", {
  # Contract C3: the citation code may refuse an author list it cannot render
  # safely; which records are one work, and which file is whose, must not
  # change with it.
  local_mocked_bindings(bib_surnames = function(x) stop("records.R must not read citations"))
  d <- withr::local_tempdir()
  f <- r3_ris(file.path(d, "a.ris"), list(
    list(au = "Smith, J.", ti = "Spacing and long-term retention in adults", py = 2019),
    list(au = "Smith JA", ti = "Spacing and long-term retention in adults.", py = 2019)))
  r <- gr_records(f, files = r3_files("Smith 2019.pdf"))
  expect_identical(r$records$duplicate_of, c(NA, 1L))
  expect_identical(r3_matched(r), c("Smith 2019.pdf", NA))
})

# ---------------------------------------------------------------------------
# records-5 and records-audit-03: a surname matched inside a longer name.
# ---------------------------------------------------------------------------

test_that("a surname must match a whole surname, not part of another name", {
  d <- withr::local_tempdir()
  f <- r3_ris(file.path(d, "a.ris"), list(
    list(au = "Li, X.", ti = "Reply to the letter about remdesivir", py = 2020, jo = "Lancet"),
    list(au = "Lin, Y.", ti = "Reply to the letter about remdesivir", py = 2020, jo = "JAMA",
         do = "10.1001/jama.2020.9876")))
  r <- gr_records(f, files = r3_files("10.1001_jama.2020.9876.txt"))
  # Lin's JAMA reply was filed under Li's Lancet record.
  expect_identical(r$records$duplicate_of, c(NA_integer_, NA_integer_))
  expect_identical(r3_matched(r), c(NA, "10.1001_jama.2020.9876.txt"))

  pairs <- list(c("Chen, L.", "Cheng, W."), c("Park, J.", "Parker, S."), c("Li, X.", "Williams, R."),
                c("Kim, H.", "Smith, Kimberly"), c("Ma, Y.", "Thomas, P."), c("He, Q.", "Chen, H."),
                c("Li, X.", "Liu, Y."))
  for (p in pairs) {
    for (doi in list(NULL, "10.1136/bmj.x")) {
      g <- r3_ris(withr::local_tempfile(fileext = ".ris"), list(
        list(au = p[1], ti = "Effects of exercise on sleep quality in older adults", py = 2020),
        list(au = p[2], ti = "Effects of exercise on sleep quality in older adults", py = 2020, do = doi)))
      expect_identical(gr_records(g)$records$duplicate_of, c(NA_integer_, NA_integer_),
                       info = paste(p, collapse = " / "))
    }
  }
  # One person written two ways is still one work.
  for (p in list(c("Smith, J.", "Smith JA"), c("Garc\u00eda, R.", "Garcia R"),
                 c("van der Berg, P.", "Berg, P. van der"), c("O'Brien, K.", "OBrien K"),
                 c("Nguy\u1ec5n, T. H.", "Nguyen, T. H."), c("M\u00fcller, K.", "Mueller K"),
                 c("S\u00f8rensen, L.", "Soerensen L"))) {
    g <- r3_ris(withr::local_tempfile(fileext = ".ris"), list(
      list(au = p[1], ti = "Effects of exercise on sleep quality in older adults", py = 2020),
      list(au = p[2], ti = "Effects of exercise on sleep quality in older adults", py = 2020)))
    expect_identical(gr_records(g)$records$duplicate_of, c(NA, 1L), info = paste(p, collapse = " / "))
  }
})

test_that("a DOI-less record joins a DOI record only when nothing contradicts it", {
  base <- list(au = "Smith, J.", ti = "Effects of exercise on sleep quality in older adults", py = 2020)
  merge_of <- function(a, b) {
    f <- r3_ris(withr::local_tempfile(fileext = ".ris", .local_envir = parent.frame()),
                list(modifyList(base, a), modifyList(base, b)))
    gr_records(f)$records$duplicate_of
  }
  # Same author, one venue missing or the same journal abbreviated: one work.
  expect_identical(merge_of(list(), list(do = "10.1000/x1", jo = "Sleep")), c(NA, 1L))
  expect_identical(merge_of(list(jo = "J Sleep Res"), list(do = "10.1000/x1", jo = "Journal of Sleep Research")),
                   c(NA, 1L))
  # Two journals, or two initials: two works, as they were before DOI-less
  # records could join DOI ones at all.
  expect_identical(merge_of(list(jo = "Lancet"), list(do = "10.1000/x1", jo = "BMJ")),
                   c(NA_integer_, NA_integer_))
  expect_identical(merge_of(list(jo = "BMJ"), list(do = "10.1000/x1", jo = "BMJ Open")),
                   c(NA_integer_, NA_integer_))
  expect_identical(merge_of(list(au = "Smith, K."), list(do = "10.1000/x1")), c(NA_integer_, NA_integer_))
})

test_that("identical non-Latin titles by different authors or in different journals stay apart", {
  ti <- "\u8179\u8154\u955c\u80c6\u56ca\u5207\u9664\u672f\u60a3\u8005\u7684\u62a4\u7406\u4f53\u4f1a"
  au <- c("Li, Q.", "Liu, M.", "Chen, H.", "He, Y.", "Wang, L.", "Wang, H.")
  f <- r3_ris(withr::local_tempfile(fileext = ".ris"), lapply(seq_along(au), function(i)
    list(au = au[i], ti = ti, py = 2021, jo = paste("Journal", LETTERS[i]))))
  expect_true(all(is.na(gr_records(f)$records$duplicate_of)))
  g <- r3_ris(withr::local_tempfile(fileext = ".ris"), lapply(seq_along(au), function(i)
    list(au = au[i], ti = ti, py = 2021)))
  expect_true(all(is.na(gr_records(g)$records$duplicate_of)))
  # One author of one name in two journals is two papers too; in one journal, one.
  h <- r3_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Wang, L.", ti = ti, py = 2021, jo = "Journal A"),
    list(au = "Wang, L.", ti = ti, py = 2021, jo = "Journal B"),
    list(au = "Wang, L.", ti = ti, py = 2021, jo = "Journal A")))
  expect_identical(gr_records(h)$records$duplicate_of, c(NA, NA, 1L))

  # And in a C locale the title keys raise no translation warnings.
  if (isTRUE(l10n_info()[["UTF-8"]])) {
    withr::local_locale(c(LC_CTYPE = "C"))
    expect_no_warning(r <- gr_records(f))
    expect_true(all(is.na(r$records$duplicate_of)))
  }
})

# ---------------------------------------------------------------------------
# records-6: BibTeX accent commands left a stray letter in the name.
# ---------------------------------------------------------------------------

test_that("BibTeX accent commands become the letters they spell", {
  bib <- c(Dvorak = "Dvo{\\v{r}}{\\'a}k, Jan", Erdos = "Erd{\\H{o}}s, P{\\'a}l",
           Francois = "Fran{\\c{c}}ois, Marie", Gunes = "G{\\\"u}ne{\\c{s}}, Ali",
           Kovacevic = "Kova{\\v{c}}evi{\\'c}, Ivan", Was = "W{\\k{a}}s, Jan",
           Zotero = "Dvo{\\v r}{\\'a}k, Jan", Bare = "Dvo\\v{r}\\'ak, Jan", Sorensen = "S{\\o}rensen, Lars")
  uni <- c(Dvorak = "Dvo\u0159\u00e1k", Erdos = "Erd\u0151s", Francois = "Fran\u00e7ois",
           Gunes = "G\u00fcne\u015f", Kovacevic = "Kova\u010devi\u0107", Was = "W\u0105s",
           Zotero = "Dvo\u0159\u00e1k", Bare = "Dvo\u0159\u00e1k", Sorensen = "S\u00f8rensen")
  # Each database record carries the BibTeX author's own initial: two initials
  # that differ are two people, whatever the title (records-audit-03).
  ini <- c(Dvorak = "J", Erdos = "P", Francois = "M", Gunes = "A", Kovacevic = "I", Was = "J",
           Zotero = "J", Bare = "J", Sorensen = "L")
  for (nm in names(bib)) {
    d <- withr::local_tempdir()
    writeLines(c("@inproceedings{k1,", sprintf("  author = {%s and Smith, Anna},", bib[[nm]]),
                 "  title = {A study of long enough titles for matching},", "  year = {2019},", "}"),
               file.path(d, "scholar.bib"), useBytes = TRUE)
    r3_ris(file.path(d, "conf.ris"), list(list(au = paste0(uni[[nm]], ", ", ini[[nm]], ".; Smith, A."),
                                               ti = "A study of long enough titles for matching", py = 2019)))
    r <- gr_records(d)
    expect_identical(sub(",.*$", "", r$records$authors[2]), uni[[nm]], info = nm)
    expect_identical(r$records$duplicate_of, c(NA, 1L), info = nm)
  }
  # Math, commands and paths are left alone.
  expect_identical(readgpt:::bib_unlatex("$\\alpha$ \\url{x} \\vspace{1}"), "$\\alpha$ \\url{x} \\vspace{1}")
  expect_identical(readgpt:::bib_unlatex("{\\'\\i}ber Stra\\ss e"), "{\u00ed}ber Stra\u00dfe")
})

# ---------------------------------------------------------------------------
# records-7: a bare value with spaces, and a quote written \".
# ---------------------------------------------------------------------------

test_that("bare BibTeX values keep their words, and an escaped quote does not end a value", {
  f <- withr::local_tempfile(fileext = ".bib")
  writeLines(c(
    '@article{a2, author="M\\"uller, J. and Lee, K.", title = {Plain title here}, year = {2020}}',
    '@article{a3, title = {Part one}, journal = Nature Medicine, year = 2021, author = {Doe, A.}}',
    '@article{a4, author = {Roe, B.}, title = {Month test}, year = 2022 month = jan}',
    '@article{a5, author = {Poe, C.}, title = {Newline test}, journal = Nature Medicine',
    '  year = 2023}'), f)
  r <- gr_records(f)$records
  # The quote parity flipped and the title and year were lost.
  expect_identical(r$authors[1], "M\u00fcller, J.; Lee, K.")
  expect_identical(r$title[1], "Plain title here")
  expect_identical(r$year[1], "2020")
  # "Nature Medicine" was read as "Nature".
  expect_identical(r$venue[2], "Nature Medicine")
  expect_identical(r$year[2], "2021")
  # A missing comma before the next field is still recovered.
  expect_identical(r$year[3], "2022")
  expect_identical(r$venue[4], "Nature Medicine")
  expect_identical(r$year[4], "2023")
})

# ---------------------------------------------------------------------------
# records-9: compatibility characters no longer folded.
# ---------------------------------------------------------------------------

test_that("superscripts, ligatures, fractions and full-width letters fold as they used to", {
  tk <- readgpt:::title_key
  expect_identical(tk("Area in m\u00b2 of green space"), tk("Area in m2 of green space"))
  expect_identical(tk("E\ufb03cacy of the new drug"), tk("Efficacy of the new drug"))
  expect_identical(tk("\u00bd dose vs full dose trial"), tk("1/2 dose vs full dose trial"))
  expect_identical(tk("\ufb01sh \ufb02ow"), "fishflow")
  expect_identical(tk("\uff23\uff2f\uff36\uff29\uff24\uff0d\uff11\uff19"), "covid19")
  expect_identical(tk("CO\u2082 emissions"), tk("CO2 emissions"))

  d <- withr::local_tempdir()
  r3_ris(file.path(d, "a.ris"), list(list(au = "Jones, Ann", ti = "Area in m\u00b2 of green space and wellbeing", py = 2020)))
  r3_ris(file.path(d, "b.ris"), list(list(au = "Jones, A.", ti = "Area in m2 of green space and wellbeing", py = 2020)))
  expect_identical(gr_records(d)$records$duplicate_of, c(NA, 1L))
  f <- r3_ris(withr::local_tempfile(fileext = ".ris"),
              list(list(au = "Smith, John", ti = "E\ufb03cacy of the new drug in adults", py = 2019)))
  r <- gr_records(f, files = r3_files("Efficacy of the new drug in adults.txt"))
  expect_identical(r$records$retrieved, TRUE)
})

# ---------------------------------------------------------------------------
# records-audit-01: the rest of the surname-as-substring class, and lost matches.
# ---------------------------------------------------------------------------

test_that("a capital inside a surname does not start another surname", {
  alone <- function(au, py, file) {
    f <- r3_ris(withr::local_tempfile(fileext = ".ris", .local_envir = parent.frame()),
                list(list(au = au, ti = "Balance training in older adults", py = py)))
    gr_records(f, files = r3_files(file, env = parent.frame()))
  }
  # Kay was given McKay's paper, Witt DeWitt's.
  expect_identical(alone("Kay, R.", 2019, "McKay2019.txt")$records$retrieved, FALSE)
  expect_identical(alone("Witt, R.", 2018, "DeWitt_2018.txt")$records$retrieved, FALSE)
  expect_identical(alone("Souza, R.", 2021, "DeSouza2021.txt")$records$retrieved, FALSE)
  # ...and McKay's own file still matches.
  expect_identical(alone("McKay, R.", 2019, "McKay2019.txt")$records$retrieved, TRUE)

  # Both works in the export, both files on disk: each gets its own, in either order.
  files <- r3_files(c("McKay2019.txt", "Kay2019.txt"))
  recs <- list(list(au = "McKay, R.", ti = "Balance training in older adults", py = 2019),
               list(au = "Kay, S.", ti = "Gait speed and falls", py = 2019))
  for (o in list(1:2, 2:1)) {
    r <- gr_records(r3_ris(withr::local_tempfile(fileext = ".ris"), recs[o]), files = files)
    expect_identical(r3_matched(r), c("McKay2019.txt", "Kay2019.txt")[o])
  }
})

test_that("a file naming two surnames is the first one's, and frees the other's own", {
  files <- r3_files(c("Kim and Lee 2020.txt", "Lee 2020.txt"))
  recs <- list(list(au = "Kim, S.; Lee, K.", ti = "Sleep and mood in nurses", py = 2020),
               list(au = "Lee, J.", ti = "Shift work and appetite", py = 2020))
  for (o in list(1:2, 2:1)) {
    r <- gr_records(r3_ris(withr::local_tempfile(fileext = ".ris"), recs[o]), files = files)
    expect_identical(r3_matched(r), c("Kim and Lee 2020.txt", "Lee 2020.txt")[o])
  }
  files2 <- r3_files(c("van der Berg - 2020.txt", "Berg 2020.txt"))
  r <- gr_records(r3_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Berg, K.", ti = "Gait speed and falls", py = 2020),
    list(au = "van der Berg, P.", ti = "Retrieval in class", py = 2020))), files = files2)
  expect_identical(r3_matched(r), c("Berg 2020.txt", "van der Berg - 2020.txt"))
  # "et al" run into the surname is still the surname.
  r2 <- gr_records(r3_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Smith, J.", ti = "Cognitive load in class", py = 2019))), files = r3_files("smithetal2019.txt"))
  expect_identical(r2$records$retrieved, TRUE)
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-02: order under a C character type.
# ---------------------------------------------------------------------------

test_that("exports and inventories sort the same way when the character type is C", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "this file system cannot hold the non-ASCII file name here")
  d <- withr::local_tempdir()
  inv <- file.path(d, "inv"); dir.create(inv)
  for (nm in c("adams", "Baker", "\u00c9vora")) {
    writeLines(enc2utf8(c("TY  - JOUR", paste0("AU  - ", nm, ", A."), "TI  - A paper",
                          "PY  - 2020", "ER  - ")), file.path(d, paste0(nm, ".ris")), useBytes = TRUE)
  }
  for (nm in c("adams", "Baker", "zhou", "\u00c9vora")) writeLines("text", file.path(inv, paste0(nm, ".txt")))
  want_ex <- c("Baker.ris", "adams.ris", "\u00c9vora.ris")
  want_inv <- c("Baker.txt", "adams.txt", "zhou.txt", "\u00c9vora.txt")
  expect_identical(enc2utf8(basename(readgpt:::export_paths(d))), want_ex)
  expect_identical(enc2utf8(gr_inventory(inv)$files$file), want_inv)
  withr::local_locale(c(LC_CTYPE = "C"))
  # enc2utf8() in a C locale turned the name into "<c3><89>vora" text, which
  # sorts before "Baker": [study N] numbering and replays followed the locale.
  ex <- readgpt:::export_paths(d)
  expect_identical(readgpt:::mark_utf8(basename(ex)), want_ex)
  expect_identical(readgpt:::mark_utf8(gr_inventory(inv)$files$file), want_inv)
  # And the record set reads at all: basename() of a UTF-8 name aborted here.
  expect_identical(nrow(gr_records(d)$records), 3L)
})
