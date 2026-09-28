# test-review5-extract.R -- the fifth pass on the extraction value check: an
# English sentence with a stray foreign word, a Greek letter or an accent
# took the other-language fallback and verified any invented number; count
# words anywhere ("no difference", "the first visit", "one of") verified an
# invented 0, 1 or 2; a faithful quote of separate passages was refused when a
# later one started mid-sentence; honest numbers in other languages and in a
# few English forms were refused; and a quoted table of counts took minutes.

# UTF-8 bytes as they are, in any locale.
r5_file <- function(...) {
  f <- tempfile(fileext = ".txt")
  writeBin(charToRaw(enc2utf8(paste0(paste(c(...), collapse = "\n\n"), "\n"))), f)
  f
}

# One document holding `text`, one field of `type`, and a model that fills it
# with `value` and quotes `quote`. Returns the lax and the strict extraction.
r5_extract <- function(text, value, type = "integer", quote = text) {
  f <- r5_file("Background text about the trial design.", text)
  fl <- gr_fields(x = gr_field("The value", type = type))
  js <- as.character(jsonlite::toJSON(list(x = value, x__quote = quote),
                                      auto_unbox = TRUE, digits = NA))
  cl <- gr_mock_client(function(messages, params) js)
  list(lax = quiet(gr_extract(f, fl, client = cl, recipe = "fast", keep_answers = TRUE)),
       strict = quiet(gr_extract(f, fl, client = cl, recipe = "fast", require_quote = TRUE)))
}

r5_verified <- function(cs) {
  type <- if (length(cs) > 2L) cs[[3]] else "integer"
  x <- r5_extract(cs[[1]], cs[[2]], type)
  expect_true(isTRUE(x$lax$evidence$verified), info = cs[[1]])
  expect_identical(x$lax$table$n_unverified, 0L, info = cs[[1]])
  expect_equal(x$strict$table$x, cs[[2]], info = cs[[1]])
}

r5_refused <- function(cs) {
  type <- if (length(cs) > 2L) cs[[3]] else "integer"
  x <- r5_extract(cs[[1]], cs[[2]], type)
  expect_false(isTRUE(x$lax$evidence$verified), info = cs[[1]])
  expect_identical(x$lax$evidence$match, 1, info = cs[[1]])
  expect_identical(x$lax$table$n_unverified, 1L, info = cs[[1]])
  expect_true(is.na(x$strict$table$x), info = cs[[1]])
}

r5_bq <- function(value, quote, source = quote, type = "integer") {
  readgpt:::quote_backs_value(value, quote, source, gr_field("x", type = type))
}

# ---------------------------------------------------------------------------
# extract-1: an English sentence is not in another language
# ---------------------------------------------------------------------------

test_that("an English sentence with a foreign word, a Greek letter or an accent states no number", {
  # Each states no number. Each took the fallback for quotes in another
  # language and verified the invented value, which require_quote kept.
  cases <- list(
    list("Mean (SE) scores improved in the intervention group.", 42),
    list("Patients with de novo metastatic disease were eligible.", 42),
    list("TNF-\u03b1 levels fell in the treatment group.", 250),
    list("Treatment-na\u00efve patients had lower mortality than controls.", 250),
    list("Patients with von Willebrand disease were excluded.", 250),
    list("Tumours were resected en bloc in the surgical arm.", 250),
    list("Patients with Sj\u00f6gren syndrome were excluded from the trial.", 250),
    list("Concentrations were measured in \u00b5g per litre at baseline.", 250),
    list("The protocol was approved by the ethics board of the H\u00f4pital Necker.", 5000),
    list("Only patients with de novo lesions were eligible for enrolment.", 5000)
  )
  for (cs in cases) r5_refused(cs)

  ql <- readgpt:::quote_language
  for (s in c("Mean (SE) scores improved", "resected en bloc", "de novo lesions",
              "von Willebrand disease", "para-aminosalicylic acid",
              "TNF-\u03b1 levels fell", "Sj\u00f6gren syndrome", "ethics board of the H\u00f4pital")) {
    expect_identical(ql(s), "en", info = s)
    expect_false(r5_bq(250L, s), info = s)
  }
})

test_that("a quotation in another language is read in that language, not waved through", {
  # A Latin-script language is read with its own number words, so an honest
  # value verifies and an invented one does not; the old fallback verified
  # both.
  r5_verified(list("Se incluyeron veinticuatro pacientes en el estudio.", 24))
  r5_refused(list("Se incluyeron veinticuatro pacientes en el estudio.", 999))
  expect_identical(readgpt:::quote_language("Se incluyeron veinticuatro pacientes en el estudio."),
                   "es")
  # A script whose number words nothing here reads keeps the span check when
  # no number can be read from the quote, as before.
  ru <- paste("\u0412 \u0438\u0441\u0441\u043b\u0435\u0434\u043e\u0432\u0430\u043d\u0438\u0435",
              "\u0432\u043a\u043b\u044e\u0447\u0438\u043b\u0438",
              "\u0434\u0432\u0430\u0434\u0446\u0430\u0442\u044c",
              "\u0447\u0435\u0442\u044b\u0440\u0435 \u043f\u0430\u0446\u0438\u0435\u043d\u0442\u0430.")
  expect_identical(readgpt:::quote_language(ru), "script")
  expect_true(r5_bq(24L, ru))
  # But not when a number it can read says otherwise.
  expect_false(r5_bq(24L, paste(ru, "120")))
})

# ---------------------------------------------------------------------------
# extract-2: a count word is a number only where it counts something
# ---------------------------------------------------------------------------

test_that("an invented 0, 1, 2 or 0.5 is not verified by a word that counts nothing", {
  cases <- list(
    list("There was no difference in mortality between the arms.", 0),
    list("There was no significant effect on mortality.", 0),
    list("There was no difference in mortality; five patients died in each arm.", 0),
    list("One of the secondary outcomes was readmission.", 1),
    list("No one was lost to follow-up in the first year.", 1),
    list("Deaths were recorded at the first visit.", 1),
    list("The first patient was enrolled in March and the last in June.", 1),
    list("A single investigator screened the records.", 1),
    list("The primary outcome was assessed in the first and second years of follow-up.", 2),
    list("The second author extracted the data.", 2),
    list("Data were held by a third party.", 0.333, "number"),
    list("Patients walked for a half-hour each day.", 0.5, "number")
  )
  for (cs in cases) r5_refused(cs)
})

test_that("the words that do count still verify the count they state", {
  cases <- list(
    list("No participants died during follow-up.", 0),
    list("There were no deaths in either arm.", 0),
    list("No serious adverse events were reported.", 0),
    list("None of the participants died during follow-up.", 0),
    list("No one was lost to follow-up in the first year.", 0),
    list("Both groups received the same dose.", 2),
    list("Half of the patients received the placebo.", 0.5, "number"),
    list("Twenty-four patients were enrolled at two sites.", 24),
    list("Participants were randomised to one of three arms.", 3),
    list("One patient died during follow-up.", 1)
  )
  for (cs in cases) r5_verified(cs)
  qn <- function(s) sort(readgpt:::quote_numbers(readgpt:::normalise_for_match(s)))
  # A compound ordinal is still read, as before; a lone one is a position.
  expect_identical(qn("twenty-first"), 21)
  expect_identical(qn("the first and second years"), numeric(0))
})

# ---------------------------------------------------------------------------
# extract-3: separate passages are separate quotations
# ---------------------------------------------------------------------------

test_that("a quote of separate passages verifies when a later one starts mid-sentence", {
  text <- paste("The trial enrolled adults with moderate asthma.",
                "In total, 240 patients were randomised to two arms.")
  quotes <- c(
    bullets = "- The trial enrolled adults with moderate asthma.\n- 240 patients were randomised to two arms.",
    paragraphs = "The trial enrolled adults with moderate asthma.\n\n240 patients were randomised to two arms.",
    lines = "The trial enrolled adults with moderate asthma.\n240 patients were randomised to two arms.",
    quoted = "\"The trial enrolled adults with moderate asthma.\" \"240 patients were randomised to two arms.\""
  )
  for (nm in names(quotes)) {
    x <- r5_extract(text, 240, quote = quotes[[nm]])
    expect_true(isTRUE(x$lax$evidence$verified), info = nm)
    expect_identical(x$lax$evidence$match, 1, info = nm)
    expect_identical(x$lax$table$n_unverified, 0L, info = nm)
    expect_identical(x$strict$table$x, 240L, info = nm)
    expect_true(isTRUE(gr_verify_evidence(x$lax$answers[[1]])$verified), info = nm)
  }
  # The audit no longer says a quote that states 240 does not state it.
  x <- r5_extract(text, 240, quote = quotes[["bullets"]])
  p <- tempfile(fileext = ".html")
  suppressMessages(gr_audit_report(p, extraction = x$lax, open = FALSE))
  h <- paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = " ")
  expect_false(grepl("not stating the value", h, fixed = TRUE))
  # An invented value is still refused, whatever the form.
  r5_refused(list(text, 250, "integer"))
  xb <- r5_extract(text, 250, quote = quotes[["bullets"]])
  expect_false(isTRUE(xb$lax$evidence$verified))
  expect_true(is.na(xb$strict$table$x))
})

test_that("two passages within one sentence may not leave out a negation between them", {
  src <- "Patients who did not consent were excluded, and 240 patients were randomised."
  expect_false(r5_bq(240L, "- Patients who did\n- 240 patients were randomised.", src))
  expect_true(r5_bq(240L, "- Patients who did not consent were excluded\n- 240 patients were randomised.",
                    src))
  # A capitalised word at a line break starts a passage, and the dropped
  # "not" or "without" before it is still caught.
  hisp <- "Patients who were not\nHispanic were excluded from the analysis."
  expect_false(r5_bq("non-Hispanic patients excluded",
                     "Patients who were\nHispanic were excluded from the analysis.", hisp,
                     type = "string"))
  expect_false(r5_bq("HIV-infected", "Patients\nHIV infection were enrolled.",
                     "Patients without\nHIV infection were enrolled.", type = "string"))
  expect_false(r5_bq("reduced mortality", "- the drug did\n- reduce mortality at 12 months.",
                     "In the trial, the drug did not reduce mortality at 12 months.", type = "string"))
  # An elision inside one passage is held to the rule as before.
  expect_false(r5_bq(30, "Revenue ... rose 30%.", "Revenue fell 12%. Costs rose 30%.",
                     type = "number"))
  expect_true(r5_bq(120L, "All 120 completed it.\nWe enrolled 120 adults.",
                    "We enrolled 120 adults. It lasted a year. All 120 completed it."))
})

# ---------------------------------------------------------------------------
# extract-4, model-output-04, corpus-3: honest numbers in other languages
# ---------------------------------------------------------------------------

test_that("a number word in another language verifies beside a digit or an English look-alike", {
  cases <- list(
    list("Foram inclu\u00eddos vinte e quatro pacientes no estudo.", 24),
    list("Se incluyeron veinticuatro pacientes que no hab\u00edan recibido tratamiento.", 24),
    list("Se incluyeron once pacientes en el estudio.", 11),
    list("Vingt-six patients ont \u00e9t\u00e9 inclus dans l'\u00e9tude.", 26),
    list("Ten opzichte van de controlegroep werden vierentwintig pati\u00ebnten ingesloten.", 24),
    list("Se incluyeron veinticuatro pacientes en 2019.", 24),
    list("Se incluyeron veinticuatro pacientes; el 50 % eran mujeres.", 24),
    list("Die Studie umfasste drei Studienarme mit je 40 Patienten.", 3),
    list("Se incluyeron veinticuatro pacientes entre 2018 y 2019.", 24),
    list("Es wurden vierundzwanzig Patienten in 3 Zentren eingeschlossen.", 24),
    list("Au total, vingt-quatre patients ont \u00e9t\u00e9 inclus dans 3 centres.", 24),
    list("Il campione comprendeva centoventi pazienti.", 120),
    list("La cohorte comprenait 1,2 millions de personnes.", 1200000),
    list("Die Kohorte umfasste 1,2 Millionen Personen.", 1200000),
    list("La cohorte incluy\u00f3 1,2 millones de personas.", 1200000),
    list("Die Kohorte umfasste 1,2 Mio. Personen.", 1200000)
  )
  for (cs in cases) r5_verified(cs)
  # A number the quote does not state is refused in every language.
  refused <- list(
    list("Foram inclu\u00eddos vinte e quatro pacientes no estudo.", 0),
    list("Se incluyeron once pacientes en el estudio.", 1),
    list("Ten opzichte van de controlegroep werden vierentwintig pati\u00ebnten ingesloten.", 10),
    list("Se incluyeron veinticuatro pacientes en 2019.", 99),
    list("Se incluyeron 120 pacientes en el estudio.", 24),
    list("Die Studie umfasste drei Studienarme mit je 40 Patienten.", 4)
  )
  for (cs in refused) r5_refused(cs)

  qn <- function(s) sort(readgpt:::quote_numbers(readgpt:::normalise_for_match(s)))
  expect_identical(qn("quatre-vingt-dix-sept patients ont \u00e9t\u00e9 inclus"), 97)
  expect_identical(qn("dos mil trescientos cuarenta y cinco pacientes fueron incluidos"), 2345)
  expect_identical(qn("zweihundertvierundf\u00fcnfzig Patienten wurden eingeschlossen"), 254)
  expect_identical(qn("centottanta pazienti sono stati inclusi"), 180)
  expect_identical(qn("cincuenta por ciento de los pacientes fueron mujeres"), c(0.5, 50))
  # A number word in an idiom is not a number.
  expect_identical(qn("Diese Faktoren wurden bei der Analyse au\u00dfer Acht gelassen."),
                   numeric(0))
  expect_identical(qn("Die Patienten wurden in acht Zentren behandelt."), 8)
  # A word for one alone is an article; inside a number it is one.
  expect_identical(qn("un estudio con una cohorte de los pacientes"), numeric(0))
  expect_identical(qn("treinta y un pacientes fueron incluidos"), 31)
})

# ---------------------------------------------------------------------------
# extract-6: honest English forms
# ---------------------------------------------------------------------------

test_that("words with a percent sign, halves, fractions and a spaced bn are read", {
  cases <- list(
    list("Fifty-four percent of participants were women.", 0.54, "number"),
    list("Median follow-up was two and a half years.", 2.5, "number"),
    list("Annual revenue reached $1.2 bn in 2020.", 1.2e9, "number"),
    list("Sales reached 3 mn units in the year.", 3e6, "number"),
    list("A quarter of patients relapsed within a year.", 0.25, "number"),
    list("One in five patients responded to treatment.", 0.2, "number"),
    list("Two thirds of participants were women.", 0.67, "number"),
    list("Median follow-up was 2\u00bd years in both arms.", 2.5, "number"),
    # A minus sign on the line before a superscript exponent.
    list("The association was significant (p < 1.0 \u00d7 10\u2212\u00b9\u2070).", 1e-10,
         "number")
  )
  for (cs in cases) r5_verified(cs)
  refused <- list(
    list("Fifty-four percent of participants were women.", 0.45, "number"),
    list("Median follow-up was two and a half years.", 3.5, "number"),
    list("A quarter of patients relapsed within a year.", 0.4, "number")
  )
  for (cs in refused) r5_refused(cs)
})

# ---------------------------------------------------------------------------
# extract-5: a quoted table of counts is read in linear time
# ---------------------------------------------------------------------------

test_that("a long run of grouped digits is read without enumerating every stretch", {
  qn <- function(s) readgpt:::quote_numbers(readgpt:::normalise_for_match(s))
  expect_true(all(c(120, 118, 120118) %in% qn("A table row 120 118 follows.")))
  six <- qn("1 204 305 406 507 608")
  expect_true(all(c(1204, 1204305406507, 204305406507608) %in% six))
  expect_false(1204305406507608 %in% six)
  set.seed(1)
  row <- paste(sample(100:999, 600, TRUE), collapse = " ")
  t <- system.time(n <- qn(row))[["elapsed"]]
  expect_lt(t, 2)
  expect_true(length(n) > 600)
})
