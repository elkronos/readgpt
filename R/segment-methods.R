# segment-methods.R -- AXIS 2: the segmentation strategies themselves.
#
# Each segmenter is a genuinely different *hypothesis about where meaning
# breaks*:
#
# PROVENANCE: `paragraph`, `sentence`, `structural`, `page`, `semantic` and
# `contextual` carry each chunk's page/section/block through to `gr_chunks`, so
# citations can point back at the source. `fixed`, `recursive` and `proposition`
# do not: the first two work on the concatenated document text by design, and
# `proposition` REWRITES the text, so no chunk corresponds to any source block.
# That is the cost of ignoring (or rewriting) structure, and it is reported as
# NA rather than guessed at. What `proposition` and `contextual` do keep is the
# document text behind each chunk, in `source_text`, because their chunk text
# holds words that are not the document's -- a model's rewrite, a context line
# -- and a quote has to be checked against the document.
#
#   fixed       -- meaning is uniform; cut on a ruler. The control condition.
#   paragraph   -- the author's paragraph breaks are the real boundaries.
#   sentence    -- sentences are atomic; pack them tightly.
#   recursive   -- respect the strongest separator that still fits (the
#                  LangChain-style cascade).
#   structural  -- headings are the boundaries; never merge across sections.
#   page        -- the page is the unit (forms, invoices, scanned records).
#   semantic    -- boundaries are where the embedding of the text *shifts*.
#   contextual  -- like paragraph, but each chunk is prefixed with where it sits,
#                  so a chunk read in isolation still knows what it is about.
#   proposition -- an LLM rewrites the text into standalone factual statements.
#                  Because the text is rewritten, these chunks carry no
#                  provenance back to a source block.
#
# They differ in cost too, which is stated in `cost` so the UI can warn:
# `fixed`/`paragraph`/`sentence`/`recursive`/`structural`/`page`/`contextual`
# are free; `semantic` costs one embedding pass; `proposition` costs one model
# call per `proposition_batch_tokens`-sized batch of blocks (default 900), not
# one per block.

#' Register a segmentation strategy
#'
#' Axis 2 is a registry, so "where does meaning break in this document?" is a
#' question you can answer for your own material rather than choosing from a
#' fixed list. A registered segmenter is a first-class one: it appears in
#' [gr_segmenters()], can be named in a [gr_recipe()], and goes through the same
#' token-cap enforcement and reporting as the built-ins.
#'
#' @param name Segmenter name, used in specs and recipes. Re-registering an
#'   existing name replaces it.
#' @param fn Function of `(doc, spec, client, trace)` returning a `gr_chunks`.
#'   Build the return value with [new_chunks()]; [gr_segment()] rejects
#'   anything else. `doc` is a [gr_document]; `spec` carries `max_tokens`,
#'   `overlap_tokens` and `min_tokens`, which [pack_units-style][new_chunks]
#'   helpers respect for you.
#' @param description One-line description, shown by [gr_segmenters()].
#' @param cost `"free"`, `"embedding"` or `"llm"`: what one run spends, so a
#'   UI can warn before it is spent.
#' @param needs_client Whether the segmenter requires a client. [gr_segmenters()]
#'   reports it, so a UI can check before offering the strategy. When `TRUE` and
#'   no client is supplied, [gr_segment()] calls `fn` and then warns with class
#'   `"gr_segment_fallback"`, unless `fn` raised a warning of that class itself,
#'   so one fallback gives one warning. Raising your own is better, because it
#'   can name what `fn` fell back *to*:
#'   `warning(warningCondition("No client; using 'paragraph'.", class =
#'   "gr_segment_fallback"))`. Record the downgrade in the returned `method`
#'   (`"mine->paragraph"`) so it survives into `gr_chunk_stats()`.
#' @return Invisibly, `name`.
#' @seealso [new_chunks()] to build the return value, [gr_segmenters()],
#'   [gr_segment()], [gr_segment_spec()], [gr_recipe()]
#' @family segmentation functions
#' @export
#' @examples
#' # One chunk per bullet list. Build the result with the same helper the
#' # built-ins use, so the token cap and reporting still apply.
#' gr_register_segmenter("by_bullet", description = "one chunk per bullet",
#'   fn = function(doc, spec, client, trace) {
#'     units <- unlist(strsplit(doc$text, "\n(?=[-*])", perl = TRUE))
#'     new_chunks(units, "by_bullet", spec)
#'   })
#' subset(gr_segmenters(), name == "by_bullet")
gr_register_segmenter <- function(name, fn, description = "",
                                  cost = c("free", "embedding", "llm"),
                                  needs_client = FALSE) {
  cost <- match.arg(cost)
  if (!is.function(fn)) gr_abort("`fn` must be a function of (doc, spec, client, trace).")
  registry_set("segmenters", name, list(name = name, fn = fn, description = description,
                                        cost = cost, needs_client = needs_client))
}

#' List registered segmentation strategies
#'
#' The catalogue for axis 2. Use it to see which strategies are free, which
#' spend an embedding pass or a model call, and which need a client at all.
#' The last is checkable here rather than only in prose, because a segmenter
#' that needs a client and does not get one falls back to a different strategy.
#'
#' @return A data frame with one row per registered segmenter: `name`, `cost`
#'   (`"free"`, `"embedding"` or `"llm"`), `needs_client` (`"TRUE"`/`"FALSE"`;
#'   when `TRUE` and no client is supplied, [gr_segment()] warns and the
#'   segmenter falls back) and `description`.
#' @seealso [gr_segment()], [gr_segment_spec()], [gr_register_segmenter()],
#'   [gr_chunk_stats()] to compare what they produce
#' @family segmentation functions
#' @export
#' @examples
#' gr_segmenters()
#'
#' # The ones that cost money, and the ones that need a client to work at all.
#' subset(gr_segmenters(), cost != "free" | needs_client == "TRUE")[, 1:3]
gr_segmenters <- function() {
  registry_table("segmenters", c("cost", "needs_client", "description"))
}

# ---------------------------------------------------------------------------

#' @noRd
seg_fixed <- function(doc, spec, client, trace) {
  # Deliberately structure-blind: a sliding window over the token stream.
  w <- words_of(doc$text)
  if (!length(w)) return(new_chunks(character(0), "fixed", spec))
  # Convert the token cap into a word count via the live tokenizer instead of
  # assuming 1 word == 1 token (the assumption that made every old limit wrong).
  # Build each window by measuring, not by assuming a constant token/word ratio.
  # The ratio estimate produced windows over the cap, which the enforcement pass
  # then re-cut, destroying the overlap.
  txt <- character(0); starts <- integer(0)
  i <- 1L
  while (i <= length(w)) {
    j <- i; last <- i - 1L
    while (j <= length(w)) {
      if (gr_count_tokens(paste(w[i:j], collapse = " ")) > spec$max_tokens) break
      last <- j; j <- j + 1L
    }
    if (last < i) last <- i                       # always consume at least one word
    txt <- c(txt, paste(w[i:last], collapse = " "))
    starts <- c(starts, i)
    if (last >= length(w)) break
    # Step forward by the window minus the requested overlap, measured in words.
    back <- 0L
    while (back < (last - i) &&
           gr_count_tokens(paste(w[(last - back):last], collapse = " ")) < spec$overlap_tokens) {
      back <- back + 1L
    }
    nxt <- max(last - back + 1L, i + 1L)
    i <- nxt
  }
  # A final window wholly contained in its predecessor is a duplicate API call.
  if (length(txt) > 1L && length(starts) > 1L &&
      grepl(txt[length(txt)], txt[length(txt) - 1L], fixed = TRUE)) {
    txt <- txt[-length(txt)]
  }
  new_chunks(txt, "fixed", spec)
}

#' @noRd
seg_paragraph <- function(doc, spec, client, trace) {
  b <- doc$blocks
  packed <- pack_units(b$text, spec$max_tokens, spec$overlap_tokens, spec$min_tokens,
                       meta = b[, c("page", "section", "block_id")])
  new_chunks(packed$text, "paragraph", spec,
             page = packed$meta$page %||% NA_integer_,
             section = packed$meta$section %||% NA_character_,
             block_id = packed$meta$block_id %||% NA_integer_)
}

#' @noRd
seg_sentence <- function(doc, spec, client, trace) {
  b <- doc$blocks
  sents <- list(); meta <- list()
  for (i in seq_len(nrow(b))) {
    s <- sentences_of(b$text[i])
    if (!length(s)) next
    sents[[length(sents) + 1L]] <- s
    meta[[length(meta) + 1L]] <- b[rep(i, length(s)), c("page", "section", "block_id")]
  }
  if (!length(sents)) return(new_chunks(character(0), "sentence", spec))
  packed <- pack_units(unlist(sents, use.names = FALSE), spec$max_tokens,
                       spec$overlap_tokens, spec$min_tokens, joiner = " ",
                       meta = do.call(rbind, meta))
  new_chunks(packed$text, "sentence", spec,
             page = packed$meta$page %||% NA_integer_,
             section = packed$meta$section %||% NA_character_,
             block_id = packed$meta$block_id %||% NA_integer_)
}

#' @noRd
seg_recursive <- function(doc, spec, client, trace) {
  seps <- spec$separators %||% c("\n\n\n", "\n\n", "\n", ". ", "; ", ", ", " ")
  split_rec <- function(txt, depth) {
    if (gr_count_tokens(txt) <= spec$max_tokens) return(txt)
    if (depth > length(seps)) return(hard_split(txt, spec$max_tokens))
    sep <- seps[depth]
    parts <- strsplit(txt, sep, fixed = TRUE)[[1]]
    if (length(parts) <= 1L) return(split_rec(txt, depth + 1L))
    # Re-attach the separator so the text is not silently altered.
    parts <- paste0(parts, c(rep(sep, length(parts) - 1L), ""))
    parts <- parts[has_content(parts)]
    unlist(lapply(parts, function(p) {
      if (gr_count_tokens(p) <= spec$max_tokens) p else split_rec(p, depth + 1L)
    }), use.names = FALSE)
  }
  pieces <- split_rec(doc$text, 1L)
  # The separator is already re-attached to each piece, so pieces join with "".
  # But an overlap tail is rebuilt from words and carries no trailing separator,
  # so joining it with "" welded it onto the next word ("delta" + "alpha" ->
  # "deltaalpha"), destroying both real words. Use a space when overlap is on.
  joiner <- if (spec$overlap_tokens > 0L) " " else ""
  packed <- pack_units(pieces, spec$max_tokens, spec$overlap_tokens, spec$min_tokens,
                       joiner = joiner)
  new_chunks(packed$text, "recursive", spec)
}

#' What `structural` writes where a chunk has no heading above it.
#'
#' A literal, not NA, because the column is also a display label. That made it
#' invisible to every downstream test of the form `all(is.na(section))` --
#' `preview` read it as "this document has structure" and planned one unit for
#' the whole thing. Named here so the producer and the consumers cannot disagree
#' about the spelling.
#' @noRd
.gr_no_section <- "[no section]"

#' @noRd
seg_structural <- function(doc, spec, client, trace) {
  b <- doc$blocks
  sec <- b$section
  # `is_head[i]` records that block i is ITSELF the heading line, not merely that
  # it belongs to that section. The prefix below re-states the heading, so the
  # heading block has to be dropped from the body or every section chunk begins
  # with the title twice.
  is_head <- rep(FALSE, nrow(b))
  # Fall back to detecting headings inline when the extractor found none.
  if (all(is.na(sec))) {
    is_head <- grepl("^(#{1,6}[ \t]|\\d+(\\.\\d+)*[ \t.)]+[A-Z]|[A-Z][A-Z0-9 ,'\u2019&/-]{6,}$)",
                     b$text, perl = TRUE) & gr_count_tokens(b$text) < 30L
    # The label is the heading line as written, less any markdown hashes. The
    # heading block is dropped from the body below and this label is prefixed
    # in its place, so it has to carry the whole line: the normalised form
    # stripped leading numbers, and "2022 Results" and "2023 Results" both
    # became "## Results", leaving neither year in any chunk -- and a
    # year-led sentence taken for a heading lost its year the same way.
    cur <- NA_character_
    for (i in seq_len(nrow(b))) {
      if (is_head[i]) cur <- trimws(sub("^#{1,6}[ \t]*", "", trimws(b$text[i]), perl = TRUE))
      sec[i] <- cur
    }
  } else {
    # The extractor supplied sections. A block whose entire text is the section
    # label is the heading itself. Normalise BOTH sides: the extractor's label
    # for "## 3.1 Methods" is "3.1 Methods" while the block text still carries
    # the hashes, so comparing a normalised label against a raw one never
    # matched and the title was emitted twice.
    is_head <- !is.na(sec) & heading_label(b$text) == heading_label(sec)
  }
  sec[is.na(sec)] <- .gr_no_section
  # One group per unbroken run of a section, not per label. Grouping by label
  # gathered every section of the same name into the first one: a paper's two
  # studies each with a "Methods" gave one chunk holding both studies' methods,
  # placed before the second study's heading, and a document's preamble was
  # joined to trailing endnotes because both had no section. Order is the
  # document's, and a run never crosses a heading.
  run <- cumsum(c(TRUE, sec[-1L] != sec[-length(sec)]))[seq_along(sec)]
  groups <- split(seq_len(nrow(b)), run)
  out <- character(0); pg <- integer(0); sc <- character(0); bid <- integer(0)
  for (idx in groups) {
    nm <- sec[idx[1]]
    labelled <- isTRUE(spec$prefix_section) && !identical(nm, .gr_no_section)
    # Drop the heading block only when the prefix will carry it. Without the
    # prefix, dropping it would lose the heading text from the document.
    body_idx <- if (labelled) idx[!is_head[idx]] else idx
    if (!length(body_idx)) {
      # The section is nothing but its heading -- a document title, a part
      # divider. Skipping it dropped that text from the chunk set entirely, so
      # the document's own title reached no reader. Emit the heading as its own
      # chunk instead, unprefixed (the prefix would only repeat it).
      body_idx <- idx
      labelled <- FALSE
    }
    # Section title is prepended so a chunk retains its heading even when the
    # section is long enough to need several chunks.
    prefix <- if (labelled) paste0("## ", nm, "\n\n") else ""
    packed <- pack_units(b$text[body_idx],
                         max(spec$max_tokens - gr_count_tokens(prefix), 32L),
                         spec$overlap_tokens, spec$min_tokens,
                         meta = b[body_idx, c("page", "section", "block_id"), drop = FALSE])
    if (!length(packed$text)) next
    m <- packed$meta
    out <- c(out, paste0(prefix, packed$text))
    # Each chunk reports the block it STARTS at, not the section's first block:
    # a five-chunk section used to claim every chunk came from block 1.
    pg  <- c(pg,  if (is.null(m)) rep(b$page[body_idx[1]], length(packed$text)) else m$page)
    sc  <- c(sc,  rep(nm, length(packed$text)))
    bid <- c(bid, if (is.null(m)) rep(b$block_id[body_idx[1]], length(packed$text)) else m$block_id)
  }
  new_chunks(out, "structural", spec, page = pg, section = sc, block_id = bid)
}

#' Normalise a heading line into a bare label.
#'
#' Strips markdown hashes, section numbering and trailing colons. Used to
#' compare a block's text against its section label, which reach `seg_structural`
#' in different shapes: the markdown extractor stores "3.1 Methods" while the
#' block itself still reads "## 3.1 Methods". The label the segmenter PREFIXES
#' is the extractor's section string, or for a heading found inline the heading
#' line itself, never this normalised form.
#' @noRd
heading_label <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^#{1,6}[ \t]*", "", x, perl = TRUE)
  x <- sub("^\\d+(?:\\.\\d+)*[ \t.)]+", "", x, perl = TRUE)
  x <- sub("[ \t:.\u2014-]+$", "", x, perl = TRUE)
  trimws(x)
}

#' @noRd
seg_page <- function(doc, spec, client, trace) {
  b <- doc$blocks
  if (all(is.na(b$page))) {
    gr_warn(paste0("The 'page' segmenter needs page provenance, which this source does not ",
                   "carry (only PDFs currently do). Falling back to 'paragraph'."),
            class = "gr_segment_fallback")
    out <- seg_paragraph(doc, spec, client, trace)
    out$method <- "page->paragraph"
    return(out)
  }
  pages <- split(seq_len(nrow(b)), b$page)
  out <- character(0); pg <- integer(0); sc <- character(0); bid <- integer(0)
  for (nm in names(pages)) {
    idx <- pages[[nm]]
    joined <- paste(b$text[idx], collapse = "\n\n")
    # A page over the cap is split rather than sent oversized, but the split is
    # reported so the user knows the page/chunk correspondence broke.
    parts <- if (gr_count_tokens(joined) > spec$max_tokens) {
      gr_msg(sprintf("Page %s exceeds the %d-token cap; splitting it into parts.",
                     nm, spec$max_tokens))
      hard_split(joined, spec$max_tokens)
    } else joined
    out <- c(out, parts)
    pg <- c(pg, rep(as.integer(nm), length(parts)))
    sc <- c(sc, rep(as_chr1(b$section[idx[1]], NA_character_), length(parts)))
    # block_id was never passed, so every `page` chunk reported NA and citations
    # could not point below page granularity. A page chunk joins all of that
    # page's blocks, so it starts at the first of them.
    bid <- c(bid, rep(b$block_id[idx[1]], length(parts)))
  }
  new_chunks(out, "page", spec, page = pg, section = sc, block_id = bid)
}

#' @noRd
seg_semantic <- function(doc, spec, client, trace) {
  if (is.null(client)) {
    gr_warn("Semantic segmentation needs a client for embeddings; falling back to 'paragraph'.",
            class = "gr_segment_fallback")
    out <- seg_paragraph(doc, spec, client, trace); out$method <- "semantic->paragraph"
    return(out)
  }
  # Work at sentence-group granularity: embed small windows, then cut where
  # consecutive windows are most dissimilar.
  unit_list <- lapply(seq_len(nrow(doc$blocks)), function(i) {
    s <- sentences_of(doc$blocks$text[i]); if (length(s)) s else doc$blocks$text[i]
  })
  units <- unlist(unit_list, use.names = FALSE)
  # Keep each unit's source block so page/section survive into the chunks.
  umeta <- doc$blocks[rep(seq_len(nrow(doc$blocks)), vapply(unit_list, length, integer(1))),
                      c("page", "section", "block_id"), drop = FALSE]
  keep_u <- has_content(units)
  units <- units[keep_u]; umeta <- umeta[keep_u, , drop = FALSE]
  if (length(units) < 3L) {
    out <- seg_paragraph(doc, spec, client, trace); out$method <- "semantic->paragraph"
    return(out)
  }
  win <- as.integer(spec$semantic_window %||% 2L)
  ctx <- vapply(seq_along(units), function(i) {
    paste(units[max(1L, i - win + 1L):i], collapse = " ")
  }, character(1))
  emb <- gr_embed(client, ctx, trace = trace)
  src <- attr(emb, "embedding_source") %||% "api"
  if (identical(src, "lexical")) {
    gr_msg("Semantic segmentation is running on lexical fallback vectors, so boundaries reflect word overlap, not meaning.")
  }
  d <- vapply(seq_len(nrow(emb) - 1L), function(i)
    1 - cosine_similarity(emb[i, ], emb[i + 1L, ]), numeric(1))
  pct <- spec$semantic_percentile %||% 90
  thresh <- stats::quantile(d, pct / 100, names = FALSE)
  cut_after <- which(d >= thresh)

  starts <- c(1L, cut_after + 1L)
  ends <- c(cut_after, length(units))
  pieces <- vapply(seq_along(starts), function(i)
    paste(units[starts[i]:ends[i]], collapse = " "), character(1))
  pmeta <- umeta[starts, , drop = FALSE]
  # Respect the hard cap and the minimum: semantic boundaries decide *where*,
  # the packer decides *how much*.
  packed <- pack_units(pieces, spec$max_tokens, spec$overlap_tokens, spec$min_tokens,
                       joiner = " ", meta = pmeta)
  trace_note(trace, "segment.semantic",
             list(units = length(units), boundaries = length(cut_after),
                  threshold = round(thresh, 4), embedding_source = src))
  out <- new_chunks(packed$text, "semantic", spec,
                    page = packed$meta$page %||% NA_integer_,
                    section = packed$meta$section %||% NA_character_,
                    block_id = packed$meta$block_id %||% NA_integer_)
  out$extra <- list(boundaries = length(cut_after), embedding_source = src,
                    distance_threshold = thresh)
  out
}

#' The most one segmentation request can cost: `input_tokens` of prompt and a
#' reply at `max_output`, or the model's own ceiling if lower, at the client
#' model's price. NA when the model has no price, which the trace cannot count
#' either.
#' @noRd
seg_call_usd <- function(client, input_tokens, max_output) {
  model <- as_chr1(client$model, "unknown")
  # The segmenter's own call warns about an unknown model; once is enough.
  quiet <- function(expr) tryCatch(suppressWarnings(expr, classes = "gr_unknown_model"),
                                   error = function(e) NULL)
  info <- quiet(gr_model_info(model))
  if (is.null(info)) return(NA_real_)
  out <- min(as_num1(max_output, 0), as_num1(info$max_output, Inf))
  as_num1(quiet(gr_estimate_cost(model, input_tokens, out)), NA_real_)
}

#' @noRd
seg_contextual <- function(doc, spec, client, trace) {
  # Contextual retrieval: each chunk keeps a short pointer to
  # where it sits, so a chunk retrieved alone is still interpretable. Free
  # variant uses document + section metadata; `context_source = "llm"` spends
  # one call per chunk to write the blurb.
  base <- seg_paragraph(doc, spec, client, trace)
  d <- base$chunks
  if (!nrow(d)) { base$method <- "contextual"; return(base) }
  title <- basename(doc$source)
  src <- as_chr1(spec$context_source %||% "metadata")
  if (identical(src, "llm") && is.null(client)) {
    gr_warn(paste0("context_source = 'llm' needs a client; using the free metadata blurb ",
                   "instead. The chunk set records context_source = 'metadata'."),
            class = "gr_segment_fallback")
    src <- "metadata"
  }

  headers <- if (identical(src, "llm")) {
    doc_summary <- gr_truncate_tokens(doc$text, 1500, "")
    sys_prompt <- "You situate an excerpt within its source document. Reply with one sentence, no preamble."
    ask <- function(excerpt) paste0(
      "<document>\n", doc_summary, "\n</document>\n\n<excerpt>\n", excerpt,
      "\n</excerpt>\n\nIn one sentence, say what this excerpt is about and where it fits in the document.")
    # The longest excerpt with its reply at the cap: what gr_lapply() holds a
    # parallel batch to, since the workers cannot see what the run spends.
    worst <- seg_call_usd(client, sum(gr_count_tokens(c(sys_prompt, ask("")))) + max(d$tokens), 90L)
    unlist(gr_lapply(seq_len(nrow(d)), function(i, trace) {
      if (!trace_can_call(trace)) return("")
      res <- gr_call(client, list(
        list(role = "system", content = sys_prompt),
        list(role = "user", content = ask(d$text[i]))
      ), max_output = 90L, trace = trace, label = "segment.context")
      if (res$ok) res$text else ""
    }, parallel = spec$parallel, label = "context blurb", trace = trace,
       client = client, item_usd = worst),
      use.names = FALSE)
  } else {
    vapply(seq_len(nrow(d)), function(i) {
      bits <- c(sprintf("Source: %s", title),
                if (!is.na(d$section[i])) sprintf("Section: %s", d$section[i]),
                if (!is.na(d$page[i])) sprintf("Page: %d", d$page[i]),
                sprintf("Part %d of %d", i, nrow(d)))
      paste(bits, collapse = " | ")
    }, character(1))
  }
  # The header is not the document's text, and under context_source = "llm" a
  # model wrote it. Kept as the chunk's text it became part of what quotes were
  # checked against, so a quote of a figure the context-writing call invented
  # ("revenue rose to 52 million" of a document that says 45.2) verified as a
  # verbatim quotation. `source_text` is the body alone; see new_chunks().
  body <- d$text
  d$text <- ifelse(nzchar(headers), paste0("[", headers, "]\n\n", body), body)
  out <- new_chunks(d$text, "contextual", spec, page = d$page, section = d$section,
                    block_id = d$block_id,
                    source_text = ifelse(nzchar(headers), body, NA_character_))
  out$extra <- list(context_source = src)
  out
}

#' A list of propositions as a flat character vector, whatever shape it arrived in.
#'
#' An array of strings simplifies to a character vector; an array of ARRAYS to a
#' matrix, which `as.character()` flattens column-major and so transposes; an
#' array of objects to a data frame, which `as.character()` deparses, putting
#' the literal text `c("A.", "B.")` into the document. [text_leaves()] reads
#' each shape in the order it was written. An object is read through its text
#' key when it has one -- `{"statement": ..., "confidence": "high"}` is one
#' proposition, not two -- and through every string when it has none, so a
#' proposition under a key nobody anticipated is kept. A number beside the text
#' is a label, not a proposition.
#' @noRd
prop_strings <- function(x) {
  text_leaves(x, prefer = c("text", "proposition", "statement", "sentence", "claim",
                            "assertion", "content"),
              object_numbers = FALSE)
}

#' @noRd
seg_proposition <- function(doc, spec, client, trace) {
  # Dense X-retrieval style: rewrite the text into standalone assertions, each
  # of which is self-contained (no dangling pronouns). Expensive, but it is the
  # segmentation that most changes what retrieval can find.
  if (is.null(client)) {
    gr_warn("Proposition segmentation needs a client; falling back to 'sentence'.",
            class = "gr_segment_fallback")
    out <- seg_sentence(doc, spec, client, trace); out$method <- "proposition->sentence"
    return(out)
  }
  batch_tokens <- as.integer(spec$proposition_batch_tokens %||% 900)
  batches <- pack_units(doc$blocks$text, batch_tokens, 0L, 0L,
                        meta = doc$blocks[, c("page", "section", "block_id")])
  schema <- list(type = "object", additionalProperties = FALSE,
                 required = list("propositions"),
                 properties = list(propositions = list(
                   type = "array", items = list(type = "string"))))
  # A batch that cannot be decomposed -- the call failed, the run hit its
  # ceiling, or the reply held nothing usable -- is KEPT AS WRITTEN rather than
  # dropped. It used to return character(0), so one bad batch in ten silently
  # removed a tenth of the document from everything downstream; only a run in
  # which EVERY batch failed noticed, by falling back to sentences. A segmenter
  # must never lose text.
  keep <- function(i) list(p = batches$text[i], kept = TRUE)
  sys_prompt <- paste0(
    "Decompose text into standalone propositions. Each proposition must be a single ",
    "self-contained factual statement: resolve every pronoun and abbreviation to the ",
    "entity it refers to, keep all numbers, dates and units verbatim, and add nothing ",
    "that is not stated in the text.")
  # The largest batch with its reply at the cap: what gr_lapply() holds a
  # parallel batch to, since the workers cannot see what the run spends.
  worst <- seg_call_usd(client, gr_count_tokens(sys_prompt) +
                          max(c(0L, gr_count_tokens(batches$text))), 2000L)
  res <- gr_lapply(seq_along(batches$text), function(i, trace) {
    if (!trace_can_call(trace)) return(keep(i))
    out <- gr_call_json(client, list(
      list(role = "system", content = sys_prompt),
      list(role = "user", content = batches$text[i])
    ), schema = schema, schema_name = "propositions", trace = trace,
       label = "segment.proposition", max_output = 2000L)
    if (!out$ok) return(keep(i))
    # as.character() on whatever simplifyVector made of it flattened a matrix
    # column-major (transposing the propositions) and deparsed a data frame
    # (putting the literal R expression c("A.", "B.") into the document text).
    p <- prop_strings(json_field(out$value, "propositions", scalar = FALSE))
    p <- p[has_content(p)]
    if (!length(p)) keep(i) else list(p = p, kept = FALSE)
  }, parallel = spec$parallel, label = "proposition batch", trace = trace,
     client = client, item_usd = worst)
  kept <- vapply(res, function(r) isTRUE(r$kept), logical(1))
  if (all(kept)) {
    gr_warn("Proposition extraction returned nothing; falling back to 'sentence'.",
            class = "gr_segment_fallback")
    out <- seg_sentence(doc, spec, client, trace); out$method <- "proposition->sentence"
    return(out)
  }
  if (any(kept)) {
    gr_warn(sprintf(paste0("%d of %d proposition batch(es) could not be decomposed and are kept ",
                           "as written, so no text is lost; those chunks are paragraphs, not ",
                           "propositions."), sum(kept), length(kept)),
            class = "gr_segment_fallback")
  }
  props <- unlist(lapply(res, function(r) r$p), use.names = FALSE)
  # Which batch each proposition came from, so each chunk can name the
  # document text it was written from.
  from <- rep(seq_along(res), vapply(res, function(r) length(r$p), integer(1)))
  real <- has_content(props)
  props <- props[real]
  from <- from[real]
  packed <- pack_units(props, spec$max_tokens, 0L, spec$min_tokens, joiner = "\n")
  # The propositions are the model's words, not the document's: a fact the
  # decomposing call added was checked against itself and verified, match 1.
  # `source_text` holds the batches each chunk's propositions came from, which
  # is what a quote from it has to be found in. A batch kept as written is its
  # own source.
  source <- vapply(seq_len(nrow(packed$span)), function(k) {
    b <- seq(from[packed$span$first[k]], from[packed$span$last[k]])
    paste(batches$text[b], collapse = "\n\n")
  }, character(1))
  out <- new_chunks(packed$text, "proposition", spec, source_text = source)
  # `propositions` counts propositions: a batch kept as written is a paragraph,
  # and counting it here reported a partly decomposed document as fully done.
  n_props <- sum(vapply(res[!kept], function(r) length(r$p), integer(1)))
  out$extra <- list(propositions = n_props, batches = length(batches$text),
                    batches_kept_as_written = sum(kept))
  out
}

#' @noRd
register_builtin_segmenters <- function() {
  gr_register_segmenter("fixed", seg_fixed, cost = "free",
    description = "Uniform token windows, ignoring structure. The control condition.")
  gr_register_segmenter("paragraph", seg_paragraph, cost = "free",
    description = "Greedy packing of the author's paragraphs up to the cap.")
  gr_register_segmenter("sentence", seg_sentence, cost = "free",
    description = "Sentence-boundary packing; tighter, more chunks, cleaner edges.")
  gr_register_segmenter("recursive", seg_recursive, cost = "free",
    description = "Cascade through separators, using the strongest one that fits.")
  gr_register_segmenter("structural", seg_structural, cost = "free",
    description = "Never merge across headings; each chunk keeps its section title.")
  gr_register_segmenter("page", seg_page, cost = "free",
    description = "One chunk per page. For forms, invoices and scanned records.")
  # needs_client = TRUE because seg_semantic falls back to `paragraph` without
  # one. Declaring FALSE made `gr_segmenters()` disagree with the behaviour, so
  # the one column a UI would check before offering the strategy was wrong.
  gr_register_segmenter("semantic", seg_semantic, cost = "embedding", needs_client = TRUE,
    description = "Cut where consecutive embeddings diverge most. One embedding pass.")
  gr_register_segmenter("contextual", seg_contextual, cost = "free", needs_client = FALSE,
    description = "Paragraph chunks prefixed with where they sit; context_source='llm' writes it per chunk.")
  gr_register_segmenter("proposition", seg_proposition, cost = "llm", needs_client = TRUE,
    description = "Rewrite into standalone factual statements. Expensive; best for dense factual recall.")
  invisible(NULL)
}
