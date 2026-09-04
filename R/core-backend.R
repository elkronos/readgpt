# core-backend.R -- talk to a model through someone else's client.
#
# WHY THIS FILE EXISTS
# `gr_client()` speaks one HTTP dialect: an OpenAI-compatible /responses or
# /chat/completions endpoint. That was the right first move and it is the wrong
# only move. The three axes this package exists for -- ingest, segment, read --
# have nothing to do with which vendor answers the call, and wiring them to one
# request shape means every new provider is a change to this package rather than
# a change to the caller's code.
#
# It also puts the package in the wrong fight. R already has a good transport
# layer: `ellmer` covers twenty-odd providers, local Ollama and Hugging Face
# among them, and `ragnar` covers retrieval and embedding stores. Neither has
# reading strategies -- no map-reduce, no refine, no iterative, no reranking, no
# traversal signatures, no per-run cost accounting. That is what this package
# is. So the useful thing is not to reimplement their transport, it is to accept
# it.
#
# `gr_backend_client()` is the seam. A backend is any function of
# `(messages, params)` that returns text, and everything the package does around
# the call -- token caps, cost rails, provenance, traces, caching, replay,
# comparison -- keeps working unchanged. `gr_ellmer_client()` is then sixty
# lines on top of it rather than a second HTTP stack.

#' Use any function as the model transport
#'
#' Wraps an arbitrary R function as a client. `handler` receives
#' `(messages, params)` and returns a string or a [gr_result]; everything else
#' the package does -- context budgeting, cost and call caps, provenance, the
#' run trace, [gr_cache()], [gr_replay_client()], [gr_compare()] -- works
#' exactly as it does for the built-in HTTP client.
#'
#' Use it to reach a provider this package does not speak to, a company proxy, a
#' local model, or another R package's client. [gr_ellmer_client()] is this
#' function with an `ellmer` chat object plugged in.
#'
#' @param handler Function of `(messages, params)`. `messages` is a list of
#'   `list(role=, content=)` with roles `"system"`, `"developer"`, `"user"` or
#'   `"assistant"`. `params` carries `model`, `max_output`, `temperature`,
#'   `schema`, `schema_name` and `prompt_tokens`. Return a single string, or a
#'   [gr_result] for full control over token usage and failure reporting.
#' @param embed Optional function of `(texts, params)` returning a numeric
#'   matrix with one row per input. Without it, readers that embed
#'   (`retrieve`, and the `semantic` segmenter) fall back to hashed lexical
#'   vectors and warn.
#' @param model,embedding_model Model ids reported to the rest of the package.
#'   If `model` is not in the model registry you will get the usual unknown-model
#'   warning and a conservative context window; [gr_register_model()] fixes that,
#'   and getting it right matters because it is what sizes your chunks.
#' @param max_retries,timeout Recorded on the client for completeness. Retrying
#'   is the backend's business -- this package does not retry a handler, because
#'   it cannot know whether the failure was transient.
#' @return An object of class `gr_backend_client`, usable anywhere a
#'   [gr_client()] is, with `$calls()` recording every request made through it.
#'
#' @section Failure is data, not an exception:
#' A handler that raises is caught and reported as a failed [gr_result], the
#' same as an HTTP error, so one bad chunk does not abort a run. A handler that
#' returns `""` is treated as an empty completion, which this package reports as
#' `ok = FALSE` rather than passing `""` downstream as evidence.
#'
#' @seealso [gr_ellmer_client()], [gr_client()], [gr_mock_client()],
#'   [gr_register_model()], [gr_cache_client()]
#' @export
#' @examples
#' # Any function will do. This one is deterministic so the example is too.
#' cl <- gr_backend_client(function(messages, params) {
#'   user <- Filter(function(m) m$role == "user", messages)
#'   sprintf("I was asked %d thing(s) with a %d-token cap.",
#'           length(user), params$max_output)
#' }, model = "my-backend")
#'
#' gr_call(cl, "What was revenue?", max_output = 128L)$text
#'
#' # It composes with everything else: caching, tracing, cost accounting.
#' tr <- gr_trace()
#' invisible(gr_call(cl, "again", trace = tr))
#' gr_trace_summary(tr)[c("calls", "tokens_in", "tokens_out")]
gr_backend_client <- function(handler, embed = NULL, model = "backend-model",
                              embedding_model = "backend-embed",
                              max_retries = 0L, timeout = NULL) {
  if (!is.function(handler)) {
    gr_abort("`handler` must be a function of (messages, params).")
  }
  if (!is.null(embed) && !is.function(embed)) {
    gr_abort("`embed` must be a function of (texts, params), or NULL.")
  }
  log <- new.env(parent = emptyenv())
  log$calls <- list()
  log$embeds <- list()
  structure(list(
    model = as_chr1(model, "backend-model"),
    api = "backend", base_url = "backend://",
    embedding_model = as_chr1(embedding_model, "backend-embed"),
    max_retries = as.integer(clamp(max_retries, 0, 10)),
    retry_pause_base = 0,
    timeout = clamp(timeout %||% gr_options("request_timeout"), 1, 3600),
    extra_body = list(),
    # A per-instance identity, and the cache key depends on it.
    #
    # Named `.client_id` and NOT `.cache_id`: `$` on a list partial-matches, so
    # with a field called `.cache_id` the expression `client$.cache` -- which is
    # how every caller asks whether a cache is attached -- would silently return
    # this id string for any client that has no cache. That is the same
    # partial-matching trap that made `parsed$output` return `output_text` in the
    # previous release, and the only reliable defence is not to create a name
    # that is a prefix of another.
    #
    # For gr_client() the key can be made of api, base_url and model, because
    # those fully describe what will answer: two clients with the same three
    # give the same answers, in this session or next week, which is what makes a
    # durable cache safe. For a backend they describe nothing -- every backend
    # client is api="backend", base_url="backend://", model="backend-model" --
    # and the thing that actually answers is an R closure. Two different
    # handlers therefore shared cache entries and silently traded answers.
    #
    # Hashing the closure is not the fix: two handlers can share a body and
    # differ in what they captured. Identity is, and identity is per-session by
    # construction -- which is correct, because nothing about an R closure is
    # reproducible across sessions anyway.
    .client_id = gr_new_id("backend"),
    handler = handler, embed_handler = embed, .log = log,
    calls  = function() log$calls,
    embeds = function() log$embeds,
    reset  = function() { log$calls <- list(); log$embeds <- list(); invisible(NULL) }
  ), class = c("gr_backend_client", "gr_client"))
}

#' @export
print.gr_backend_client <- function(x, ...) {
  cat(sprintf("<gr_backend_client> model=%s embeddings=%s, %d call(s) made\n",
              x$model, if (is.function(x$embed_handler)) "supplied" else "none (lexical fallback)",
              length(x$.log$calls)))
  invisible(x)
}

# ---------------------------------------------------------------------------
# ellmer
# ---------------------------------------------------------------------------

#' Read documents through an ellmer chat
#'
#' Uses an `ellmer` `Chat` object as the transport, so every provider ellmer
#' supports -- Anthropic, Google, Bedrock, Azure, Ollama, Hugging Face, and the
#' rest -- becomes available to every reading strategy here, with this package's
#' context budgeting, cost rails, traces, caching and replay unchanged around it.
#'
#' ellmer is a suggested dependency: this function is the only thing in the
#' package that needs it.
#'
#' @param chat An ellmer `Chat`, e.g. from `ellmer::chat_anthropic()` or
#'   `ellmer::chat_ollama()`.
#' @param embed Optional function of `(texts, params)` returning one row per
#'   input -- for example a thin wrapper around `ragnar::embed_ollama()`. Without
#'   it, `retrieve` and the `semantic` segmenter fall back to lexical vectors and
#'   warn.
#' @param model Model id reported to this package. Defaults to the chat's own
#'   model. Register it with [gr_register_model()] if it is not already known --
#'   the context window is what sizes your chunks, so a wrong one is not cosmetic.
#' @return A [gr_backend_client()].
#'
#' @section Requirements on the chat:
#' The adapter calls `$chat()`, `$clone()`, `$set_turns()` and
#' `$set_system_prompt()`, and refuses a chat missing any of them
#' (`gr_bad_backend`). The last two are not conveniences: this package puts its
#' instructions in the system prompt, and it clears turns so that one chunk's
#' call cannot leak into the next. A chat that silently dropped either would
#' produce unconstrained answers with nothing to show for it.
#'
#' @section What does not carry over:
#' Two things, both worth knowing before you rely on them.
#'
#' `temperature` belongs to the chat object, not to the call. ellmer fixes
#' sampling parameters when the chat is constructed, so a `temperature` in a
#' [gr_read_spec()] cannot be honoured per-call; it is ignored and warned about
#' once (`gr_ellmer_temperature`). Build a second chat if you need a second
#' temperature.
#'
#' Each call is independent. An ellmer chat accumulates turns, and this package
#' issues many unrelated calls per run, so every call runs against a fresh deep
#' clone with its turns cleared. Your chat object is never mutated, and no
#' conversation history leaks from one chunk's call into the next.
#'
#' @seealso [gr_backend_client()], [gr_client()], [gr_register_model()]
#' @export
#' @examples
#' \dontrun{
#' library(ellmer)
#'
#' # Any provider ellmer speaks to, with any reading strategy here.
#' cl <- gr_ellmer_client(chat_anthropic(model = "claude-sonnet-4-5"))
#' ans <- answer_document("report.pdf", "What was revenue?", "thorough", client = cl)
#'
#' # Locally, for nothing:
#' local <- gr_ellmer_client(chat_ollama(model = "llama3.1"))
#' gr_compare("report.pdf", "What was revenue?",
#'            c("fast", "thorough"), client = local)$summary
#' }
gr_ellmer_client <- function(chat, embed = NULL, model = NULL) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    gr_abort(paste0("gr_ellmer_client() needs the 'ellmer' package. ",
                    "Install it with install.packages('ellmer')."),
             class = "gr_missing_dep")
  }
  # Duck-typed, not class-checked. ellmer's Chat is an R6 object whose class
  # names are its business; what this adapter requires is the methods it calls.
  check_chat_methods(chat)

  model <- as_chr1(model %||% tryCatch(as_chr1(chat$get_model()), error = function(e) NULL) %||%
                     "ellmer-model")
  warned <- new.env(parent = emptyenv())

  handler <- function(messages, params) {
    if (!identical(as_chr1(params$model, model), model) && is.null(warned$model)) {
      warned$model <- TRUE
      gr_warn(sprintf(paste0("This run asked for model '%s', but an ellmer chat answers with ",
                             "the model it was built with ('%s'). The call is going to '%s'. ",
                             "Recipes carry a model, so pass model = '%s' to override it and ",
                             "keep the context window and cost estimates honest."),
                      as_chr1(params$model, "?"), model, model, model),
              class = "gr_ellmer_model")
    }
    if (!is.null(params$temperature) && is.null(warned$temp)) {
      warned$temp <- TRUE
      gr_warn(paste0("temperature is set on the ellmer chat object, not per call, so the ",
                     "requested temperature of ", format(params$temperature),
                     " is being ignored. Construct the chat with the temperature you want."),
              class = "gr_ellmer_temperature")
    }
    parts <- ellmer_split_messages(messages)
    sys <- parts$system
    user <- parts$user

    one <- chat$clone(deep = TRUE)
    # Turns first, then the system prompt: a fresh call must not inherit the
    # history of the previous chunk's call, and must not append to the caller's.
    #
    # These were `try(..., silent = TRUE)`. Both failures are silent and both are
    # severe: unclearable turns leak one chunk's conversation into the next (and
    # inflate the token counts read back from get_tokens()), and a dropped system
    # prompt removes every instruction the answer depends on. A chat that cannot
    # do either cannot honour this adapter's contract, so it fails loudly.
    hard <- function(expr, what) tryCatch(expr, error = function(e)
      gr_abort(sprintf(paste0("The ellmer chat could not %s: %s. This adapter cannot keep calls ",
                              "independent or instructed without it."), what, conditionMessage(e)),
               class = "gr_bad_backend"))
    hard(one$set_turns(list()), "have its turns cleared")
    if (nzchar(sys)) hard(one$set_system_prompt(sys), "accept a system prompt")

    txt <- if (is.null(params$schema)) {
      one$chat(user, echo = "none")
    } else {
      type <- ellmer_type(params$schema)
      if (is.null(type)) {
        # Not convertible: take the documented degraded path rather than sending
        # a schema ellmer cannot express. Readers that need JSON already handle
        # unparseable output.
        one$chat(user, echo = "none")
      } else {
        as.character(jsonlite::toJSON(one$chat_structured(user, type = type, echo = "none"),
                                      auto_unbox = TRUE, null = "null"))
      }
    }
    ellmer_result(txt, model, ellmer_usage(one, params, txt))
  }

  gr_backend_client(handler, embed = embed, model = model,
                    embedding_model = paste0(model, "-embed"))
}

#' The methods this adapter actually calls, and the check that they are there.
#'
#' Separated out so the requirement can be tested without ellmer installed --
#' which matters, because it is a requirement about ellmer.
#'
#' `set_system_prompt` is required, not optional. In this package the system
#' prompt IS the contract: "answer only from the excerpt", "reply exactly
#' NOT_IN_DOCUMENT", "copy passages verbatim", "rate 0 to 10". A chat that
#' cannot take one would send the user turn alone and report `ok = TRUE` -- an
#' unconstrained answer with nothing anywhere to say it was unconstrained.
#' @noRd
.gr_ellmer_methods <- c("chat", "clone", "set_turns", "set_system_prompt")

#' @noRd
check_chat_methods <- function(chat) {
  missing <- .gr_ellmer_methods[
    !vapply(.gr_ellmer_methods, function(m) is.function(chat[[m]]), logical(1))]
  if (length(missing)) {
    gr_abort(sprintf(paste0("`chat` does not look like an ellmer Chat: it has no %s method(s). ",
                            "Pass the result of ellmer::chat_openai(), chat_anthropic(), ",
                            "chat_ollama() or similar."),
                     paste(sprintf("`%s()`", missing), collapse = ", ")),
             class = "gr_bad_backend")
  }
  invisible(TRUE)
}

#' Turn an ellmer reply into a `gr_result`, honouring the empty-completion rule.
#'
#' The same invariant `handler_result()` enforces for every other handler: an
#' empty reply is a refusal, a content filter or a tool call, not an answer.
#' Building the `gr_result` inside the handler bypassed that check, so a run in
#' which every call was refused reported zero errors and billed them as paid.
#' @noRd
ellmer_result <- function(txt, model, usage) {
  txt <- as_chr1(txt)
  if (!nzchar(trimws(txt))) {
    return(gr_result(FALSE, text = "", error = "empty completion", model = model,
                     finish_reason = "empty", usage = usage))
  }
  gr_result(TRUE, text = txt, model = model, usage = usage)
}

#' Split readgpt messages into ellmer's two slots.
#'
#' ellmer has a system prompt and a turn; this package has a list of roles. The
#' mapping is separated out because it is the part most likely to be wrong and
#' the only part that can be tested without ellmer installed. Note the last
#' clause: a prompt made only of system messages must still send *something* as
#' the user turn, or the provider receives an empty request.
#' @noRd
ellmer_split_messages <- function(messages) {
  is_sys <- vapply(messages, function(m) as_chr1(m$role, "user") %in% c("system", "developer"),
                   logical(1))
  txt <- function(ms) paste(vapply(ms, function(m) as_chr1(m$content), character(1)),
                            collapse = "\n\n")
  sys <- txt(messages[is_sys])
  user <- txt(messages[!is_sys])
  if (!nzchar(trimws(user))) {
    # Everything was a system message. Send it as the user turn and leave the
    # system prompt empty rather than asking a model to answer nothing.
    return(list(system = "", user = sys))
  }
  list(system = sys, user = user)
}

#' Token usage from an ellmer chat, with a local fallback.
#'
#' `$get_tokens()` returns a data frame whose exact columns are ellmer's to
#' change. Rather than depend on a shape, read whatever looks like input and
#' output counts and otherwise count locally -- a wrong usage figure would
#' silently corrupt every cost estimate and budget decision downstream, and this
#' package's own tokenizer is deliberately biased to over-count.
#' @noRd
ellmer_usage <- function(chat, params, txt) {
  local <- list(input = as_int1(params$prompt_tokens, 0L),
                output = sum(gr_count_tokens(as_chr1(txt))))
  tk <- tryCatch(chat$get_tokens(), error = function(e) NULL)
  if (!is.data.frame(tk) || !nrow(tk)) return(local)
  pick <- function(pattern) {
    col <- grep(pattern, names(tk), ignore.case = TRUE, value = TRUE)[1]
    if (is.na(col)) return(NA_integer_)
    v <- suppressWarnings(sum(as.numeric(tk[[col]]), na.rm = TRUE))
    if (is.finite(v)) as.integer(v) else NA_integer_
  }
  inp <- pick("^input|prompt")
  out <- pick("^output|completion")
  list(input = if (is.na(inp)) local$input else inp,
       output = if (is.na(out)) local$output else out)
}

#' Convert a flat JSON Schema object to an ellmer type.
#'
#' Returns NULL for anything it cannot express, so the caller can degrade rather
#' than send something wrong. Only flat objects of scalars, enums and scalar
#' arrays are handled -- which is every schema this package sends, and the
#' common case for a user-written reader.
#' @noRd
ellmer_type <- function(schema) {
  if (!is.list(schema) || !identical(as_chr1(schema$type, ""), "object")) return(NULL)
  props <- schema$properties
  if (!is.list(props) || !length(props) || is.null(names(props))) return(NULL)
  required <- as.character(unlist(schema$required %||% names(props), use.names = FALSE))

  scalar <- function(p) {
    switch(as_chr1(p$type, ""),
           string  = if (length(p$enum)) ellmer::type_enum(
                       values = as.character(unlist(p$enum, use.names = FALSE)))
                     else ellmer::type_string(),
           integer = ellmer::type_integer(),
           number  = ellmer::type_number(),
           boolean = ellmer::type_boolean(),
           NULL)
  }
  fields <- list()
  for (nm in names(props)) {
    p <- props[[nm]]
    if (!is.list(p)) return(NULL)
    ty <- if (identical(as_chr1(p$type, ""), "array")) {
      inner <- scalar(p$items %||% list())
      if (is.null(inner)) return(NULL)
      ellmer::type_array(items = inner)
    } else {
      scalar(p)
    }
    if (is.null(ty)) return(NULL)
    fields[[nm]] <- ty
  }
  # `required` is per-field in ellmer and per-object in JSON Schema.
  out <- tryCatch(
    do.call(ellmer::type_object, c(fields, list(.required = all(names(props) %in% required)))),
    error = function(e) tryCatch(do.call(ellmer::type_object, fields), error = function(e2) NULL))
  out
}
