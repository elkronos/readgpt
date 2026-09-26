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
#' the package does (context budgeting, cost and call caps, provenance, the
#' run trace, [gr_cache()], [gr_replay_client()], [gr_compare()]) works
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
#' @param id Stable identity for this backend, used by [gr_cache()] and by
#'   [gr_read_many()]'s `store`. **Read this before setting up a durable cache.**
#'
#'   For [gr_client()] a cache key can be built from the API shape, base URL and
#'   model, because those completely describe what will answer: two clients with
#'   the same three give the same answers today and next month, which is what
#'   makes a cache safe to keep on disk. A backend has none of that (every one
#'   of them is `api = "backend"`, `base_url = "backend://"`), and the thing
#'   that actually answers is an R closure, whose behaviour this package cannot
#'   inspect.
#'
#'   So the default is a fresh id per client object, which is *safe*: two
#'   different handlers can never trade answers. It is also *session-scoped*, so
#'   a cache or store will not be reused by a later session. Pass a stable `id`
#'   to get cross-session reuse. Pass one **only** when the handler does
#'   answer the same way every time, because that is the assertion you are
#'   making. Hashing the closure would not do: two handlers can share a body and
#'   differ in what they captured.
#' @param model,embedding_model Model ids reported to the rest of the package.
#'   If `model` is not in the model registry you will get the usual unknown-model
#'   warning and a conservative context window; [gr_register_model()] fixes that,
#'   and getting it right matters because it is what sizes your chunks.
#' @param max_retries,timeout Recorded on the client for completeness. Retrying
#'   is the backend's business: this package does not retry a handler, because
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
                              embedding_model = "backend-embed", id = NULL,
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
    # The identity the cache and corpus-store keys depend on. See `id` above.
    #
    # Named `.client_id` and NOT `.cache_id`: `$` on a list partial-matches, so
    # with a field called `.cache_id` the expression `client$.cache` -- which is
    # how every caller asks whether a cache is attached -- would silently return
    # this id string for any client that has no cache. That is the same
    # partial-matching trap that made `parsed$output` return `output_text` in the
    # previous release, and the only reliable defence is not to create a name
    # that is a prefix of another.
    .client_id = as_chr1(id %||% gr_new_id("backend")),
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
#' supports (Anthropic, Google, Bedrock, Azure, Ollama, Hugging Face, and the
#' rest) becomes available to every reading strategy here, with this package's
#' context budgeting, cost rails, traces, caching and replay unchanged around it.
#'
#' ellmer is a suggested dependency: this function is the only thing in the
#' package that needs it.
#'
#' @param chat An ellmer `Chat`, e.g. from `ellmer::chat_anthropic()` or
#'   `ellmer::chat_ollama()`.
#' @param embed Optional function of `(texts, params)` returning one row per
#'   input, for example a thin wrapper around `ragnar::embed_ollama()`. Without
#'   it, `retrieve` and the `semantic` segmenter fall back to lexical vectors and
#'   warn.
#' @param model Model id reported to this package. Defaults to the chat's own
#'   model. Register it with [gr_register_model()] if it is not already known.
#'   The context window is what sizes your chunks, so a wrong one is not cosmetic.
#' @return A [gr_backend_client()].
#'
#' @section Requirements on the chat:
#' The adapter calls `$chat()`, `$chat_structured()`, `$clone()`, `$set_turns()`
#' and `$set_system_prompt()`, and refuses a chat missing any of them
#' (`gr_bad_backend`). `$chat_structured()` is used by every schema-bearing call
#' (each `rerank` score and each `iterative` round), so a chat without it
#' would have failed mid-run rather than at construction.
#' The last two are not conveniences: this package puts its
#' instructions in the system prompt, and it clears turns so that one chunk's
#' call cannot leak into the next. A chat that silently dropped either would
#' produce unconstrained answers with nothing to show for it.
#'
#' @section What does not carry over:
#' A few things, all worth knowing before you rely on them.
#'
#' `temperature` belongs to the chat object, not to the call. ellmer fixes
#' sampling parameters when the chat is constructed, so a `temperature` in a
#' [gr_read_spec()] cannot be honoured per-call; it is ignored and warned about
#' once (`gr_ellmer_temperature`). Build a second chat if you need a second
#' temperature.
#'
#' The output cap does carry over, with ellmer 0.5.0 or later: each call runs on
#' a chat rebuilt from yours (same provider, settings, system prompt and tools)
#' with `max_tokens` set to that call's cap, as the built-in client sends it.
#' Callbacks registered with `$on_request_start()` and the like are not carried
#' onto that copy. With an older ellmer, or a chat that is not ellmer's, the
#' chat's own limit applies and this is warned about once
#' (`gr_ellmer_max_output`). Either way the provider's stop reason is reported,
#' so a reply cut off at the limit comes back with `finish_reason = "length"`.
#'
#' A JSON schema ellmer cannot express is sent as an instruction in the prompt
#' instead of as structured output, and warned about once (`gr_ellmer_schema`).
#' Every schema this package sends converts.
#'
#' Each call is independent. An ellmer chat accumulates turns, and this package
#' issues many unrelated calls per run, so every call runs against a fresh deep
#' clone with its turns cleared. Your chat object is never mutated, and no
#' conversation history leaks from one chunk's call into the next.
#'
#' For [gr_cache()] and [gr_read_many()]'s `store`, the client's identity covers
#' the provider, endpoint, model, the chat's `params()` and `api_args`, and its
#' system prompt, so two chats that differ in any of them never share answers.
#' A chat whose settings cannot be read gets an identity that lasts only for the
#' session.
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
      # Only a model the caller named can differ: the built-in recipes name
      # none, so a read with no `model` goes out as the client's, which is this
      # chat's. The old advice ("recipes carry a model, so pass model = ...")
      # told the caller to name one, which is what causes the mismatch.
      gr_warn(sprintf(paste0("This run named model '%s', but an ellmer chat answers, and is ",
                             "billed, as the model it was built with ('%s'). Leave `model` ",
                             "unset to read with the chat's model."),
                      as_chr1(params$model, "?"), model),
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
    # The per-call output cap. Every cap this package sets -- a map step's 700
    # tokens, a revision's room for the whole draft, the context-headroom clamp
    # in gr_call() -- was dropped here, and the chat's own max_tokens went out
    # instead: a default chat_anthropic() cut a 7000-token revision at 4096, and
    # a chat built with a large limit wrote replies far longer than the caps the
    # cost preflight priced.
    capped <- ellmer_capped(one, params$max_output)
    if (!is.null(capped)) {
      one <- capped
    } else if (is.null(warned$max_output)) {
      warned$max_output <- TRUE
      own <- as_int1(ellmer_model_config(one)$params$max_tokens)
      gr_warn(sprintf(paste0("This run caps each reply (this call at %d tokens), but this chat sends ",
                             "the output limit it was built with (%s) and gives no way to change ",
                             "it per call, so the run's caps are not applied. Replies can run ",
                             "longer than cost estimates assume; one cut off at the chat's own ",
                             "limit is reported with finish_reason \"length\". ellmer 0.5.0 or ",
                             "later lets readgpt set the cap on each call."),
                      as_int1(params$max_output, 0L),
                      if (is.na(own)) "the provider's default" else sprintf("max_tokens = %d", own)),
              class = "gr_ellmer_max_output")
    }
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
        # unparseable output. It used to be taken in silence, with nothing in the
        # prompt asking for JSON, so the free-text reply failed to parse and
        # every document was screened "unclear" with status "ok". Now it says
        # so, and the schema goes in the prompt instead.
        if (is.null(warned$schema)) {
          warned$schema <- TRUE
          gr_warn(sprintf(paste0("The '%s' schema could not be expressed as an ellmer type, so ",
                                 "those calls ask for JSON in the prompt instead of through ",
                                 "structured output. Replies that do not parse take the reader's ",
                                 "degraded path."), as_chr1(params$schema_name, "result")),
                  class = "gr_ellmer_schema")
        }
        one$chat(paste0(user, "\n\nReply with only a JSON object matching this JSON Schema, ",
                        "and nothing else:\n",
                        jsonlite::toJSON(params$schema, auto_unbox = TRUE, null = "null")),
                 echo = "none")
      } else {
        as.character(jsonlite::toJSON(one$chat_structured(user, type = type, echo = "none"),
                                      auto_unbox = TRUE, null = "null"))
      }
    }
    ellmer_result(txt, model, ellmer_usage(one, params, txt), ellmer_finish_reason(one))
  }

  # Unlike a bare backend, an ellmer chat wraps something addressable and
  # stable: a provider, a model, an endpoint. So this one CAN say what it is,
  # and a durable cache or a resumable store works across sessions -- which for
  # the real transport is the whole point of having them.
  cl <- gr_backend_client(handler, embed = embed, model = model,
                          embedding_model = paste0(model, "-embed"),
                          id = ellmer_identity(chat, model))
  # The chat bills as its own model whatever a recipe names, so pricing has to
  # be able to tell an ellmer client from a bare backend (client_billed_model()).
  class(cl) <- c("gr_ellmer_client", class(cl))
  cl
}

#' A stable identity for an ellmer chat: what will answer, not which object asked.
#'
#' What answers is more than a provider, a model and an endpoint. The sampling
#' parameters (temperature, max_tokens, reasoning effort), the provider-specific
#' `api_args`, and the chat's own system prompt -- which the adapter keeps
#' whenever this package sends none -- all live on the chat, and none of them
#' were in the id. Two chats that differed only there shared a cache: the one
#' at temperature 1.5 replayed the temperature-0 chat's answers, including the
#' replies its 200-token cap had cut off. The per-call cap does not need to be
#' here; cache_key() already carries it.
#'
#' A chat whose settings cannot be read (not ellmer's, or an ellmer that keeps
#' them somewhere else) gets a session-scoped id, the same safe default as a
#' bare [gr_backend_client()]: a cache that cannot be reused next session rather
#' than one that returns another chat's answers.
#' @noRd
ellmer_identity <- function(chat, model) {
  cfg <- ellmer_model_config(chat)
  if (is.null(cfg)) return(gr_new_id("ellmer"))
  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  paste0("ellmer-", gr_hash(list(
    "readgpt-ellmer-v2", as_chr1(model, "?"),
    as_chr1(class(provider)[1], "?"),
    as_chr1(tryCatch(as_chr1(provider@base_url), error = function(e) NULL), "?"),
    params = cfg$params, extra_args = cfg$extra_args,
    system_prompt = as_chr1(tryCatch(chat$get_system_prompt(), error = function(e) NULL), ""))))
}

#' The settings an ellmer chat was built with: `params()` and `api_args`.
#'
#' ellmer 0.5.0 moved them from the provider to a Model object
#' (`$get_model_object()`) and deprecated the provider's copies, so the Model is
#' asked first and the provider only when there is no Model. `NULL` when neither
#' holds a readable `params` list.
#' @noRd
ellmer_model_config <- function(chat) {
  obj <- tryCatch(chat$get_model_object(), error = function(e) NULL)
  if (is.null(obj)) obj <- tryCatch(chat$get_provider(), error = function(e) NULL)
  params <- tryCatch(suppressWarnings(obj@params), error = function(e) NULL)
  if (!is.list(params)) return(NULL)
  extra <- tryCatch(suppressWarnings(obj@extra_args), error = function(e) NULL)
  list(params = params, extra_args = if (is.list(extra)) extra else list())
}

#' A copy of one call's chat that sends this call's output cap.
#'
#' ellmer fixes `max_tokens` when the chat is built and has no setter for it.
#' From 0.5.0 its public interface can still build the same chat with a
#' different limit: a new `Chat` from the clone's provider, its model object
#' with `max_tokens` replaced, its system prompt and its tools. Callbacks
#' registered with `$on_request_start()` and the like have no getter and are not
#' carried over. Returns `one` itself when it already sends that limit, and
#' `NULL` when this chat or this ellmer cannot be given one, so the caller can
#' say so.
#' @noRd
ellmer_capped <- function(one, max_output) {
  cap <- as_int1(max_output)
  model_obj <- tryCatch(one$get_model_object(), error = function(e) NULL)
  params <- tryCatch(model_obj@params, error = function(e) NULL)
  if (is.na(cap) || !is.list(params)) return(NULL)
  if (identical(as_int1(params$max_tokens), cap)) return(one)
  tryCatch({
    params$max_tokens <- cap
    model_obj@params <- params
    fresh <- ellmer::Chat$new(provider = one$get_provider(), model = model_obj,
                              system_prompt = one$get_system_prompt())
    tools <- one$get_tools()
    if (length(tools)) fresh$set_tools(tools)
    fresh
  }, error = function(e) NULL)
}

#' Why an ellmer reply stopped, in the provider's words or ellmer's.
#'
#' From ellmer 0.5.0 the assistant turn carries a normalised `finish_reason`
#' (`"max_tokens"` when the reply hit the output limit). Before that only the
#' provider's own reply is there, so that is read instead. Either way
#' gr_result() maps the truncation spellings to `"length"`. It used to be left
#' NA, so nothing downstream could tell a cut-off reply from a complete one.
#' @noRd
ellmer_finish_reason <- function(chat) {
  turn <- tryCatch(chat$last_turn(), error = function(e) NULL)
  if (is.null(turn)) return(NA_character_)
  fr <- tryCatch(as_chr1(turn@finish_reason, NA_character_), error = function(e) NA_character_)
  if (!is.na(fr) && nzchar(fr)) return(fr)
  js <- tryCatch(turn@json, error = function(e) NULL)
  fld <- function(x, nm) if (is.list(x)) x[[nm, exact = TRUE]] else NULL
  first <- function(x) if (is.list(x) && length(x)) x[[1]] else NULL
  status <- fld(js, "status")
  if (identical(as_chr1(status), "incomplete")) {
    status <- fld(fld(js, "incomplete_details"), "reason") %||% status
  }
  as_chr1(fld(js, "stop_reason") %||%                          # Anthropic
            fld(first(fld(js, "choices")), "finish_reason") %||% # Chat Completions
            fld(first(fld(js, "candidates")), "finishReason") %||% # Gemini
            status,                                             # Responses API
          NA_character_)
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
.gr_ellmer_methods <- c("chat", "chat_structured", "clone", "set_turns",
                        "set_system_prompt")

#' @noRd
check_chat_methods <- function(chat) {
  # A closure is not subsettable, and passing the wrong thing here is the most
  # likely mistake. Say so in this function's own language rather than letting
  # `[[` raise "object of type 'closure' is not subsettable".
  if (!is.list(chat) && !is.environment(chat)) {
    gr_abort(paste0("`chat` must be an ellmer Chat object, not a ", class(chat)[1],
                    ". Pass the result of ellmer::chat_openai(), chat_anthropic(), ",
                    "chat_ollama() or similar."),
             class = "gr_bad_backend")
  }
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
#' `finish_reason` is the stop reason read from the chat's last turn.
#' @noRd
ellmer_result <- function(txt, model, usage, finish_reason = NA_character_) {
  txt <- as_chr1(txt)
  if (!nzchar(trimws(txt))) {
    return(gr_result(FALSE, text = "", error = "empty completion", model = model,
                     finish_reason = "empty", usage = usage))
  }
  gr_result(TRUE, text = txt, model = model, usage = usage, finish_reason = finish_reason)
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
    v <- suppressWarnings(as.numeric(tk[[col]]))
    # A column that is there but holds nothing usable -- all NA, or text, which
    # is what an unfamiliar provider's get_tokens() may well give -- is UNKNOWN,
    # not zero. `sum(na.rm = TRUE)` returned 0, which is finite, so the NA below
    # never fired and the local estimate was never used: a real call recorded as
    # having spent no tokens at all, and costed at nothing.
    if (!length(v) || all(is.na(v))) return(NA_integer_)
    v <- sum(v, na.rm = TRUE)
    if (is.finite(v)) as.integer(v) else NA_integer_
  }
  inp <- pick("^input|prompt")
  out <- pick("^output|completion")
  list(input = if (is.na(inp)) local$input else inp,
       output = if (is.na(out)) local$output else out)
}

#' Convert a JSON Schema object to an ellmer type.
#'
#' Returns NULL for anything it cannot express, so the caller can degrade rather
#' than send something wrong. Handles objects, arrays, scalars and enums, nested
#' to any depth, and the nullable form `type = c("string", "null")` -- which is
#' what every schema this package sends is made of.
#'
#' The type is read as a VECTOR. It was read with as_chr1(), which joins
#' `c("string", "null")` into "string\nnull"; no branch matched that, so the
#' screening, extraction and claims schemas -- all of which have nullable fields
#' -- converted to NULL and went out as a bare chat with no schema. Every
#' document was then screened "unclear" with status "ok". Arrays of objects
#' (claims, outline, preview plans) and arrays of arrays (claim reconciliation)
#' were refused the same way.
#'
#' A field that is nullable, or that the object does not list as required,
#' becomes `required = FALSE` in ellmer, which sends it as nullable to
#' providers that demand every field and as optional to the rest. Descriptions
#' are carried, because in this package they are instructions ("copied
#' verbatim, or null if none does"). A null among an enum's values is the
#' nullable marker, not a value.
#' @noRd
ellmer_type <- function(schema) {
  convert <- function(p, required = TRUE) {
    if (!is.list(p)) return(NULL)
    types <- as.character(unlist(p$type, use.names = FALSE))
    kind <- setdiff(types, "null")
    if (length(kind) != 1L) return(NULL)
    req <- required && !("null" %in% types)
    desc <- if (is.character(p$description) && length(p$description) == 1L) p$description
    switch(kind,
      string = {
        if (length(p$enum)) {
          vals <- as.character(unlist(p$enum, use.names = FALSE))
          vals <- vals[!is.na(vals)]
          if (!length(vals)) return(NULL)
          ellmer::type_enum(values = vals, description = desc, required = req)
        } else {
          ellmer::type_string(description = desc, required = req)
        }
      },
      integer = ellmer::type_integer(description = desc, required = req),
      number  = ellmer::type_number(description = desc, required = req),
      boolean = ellmer::type_boolean(description = desc, required = req),
      array = {
        items <- convert(p$items %||% list())
        if (is.null(items)) return(NULL)
        ellmer::type_array(items = items, description = desc, required = req)
      },
      object = {
        props <- p$properties
        if (!is.list(props) || !length(props) || is.null(names(props))) return(NULL)
        need <- as.character(unlist(p$required %||% names(props), use.names = FALSE))
        fields <- list()
        for (nm in names(props)) {
          ty <- convert(props[[nm]], nm %in% need)
          if (is.null(ty)) return(NULL)
          fields[[nm]] <- ty
        }
        do.call(ellmer::type_object, c(list(.description = desc), fields, list(.required = req)))
      },
      NULL)
  }
  if (!is.list(schema) ||
      !identical(setdiff(as.character(unlist(schema$type, use.names = FALSE)), "null"), "object")) {
    return(NULL)
  }
  tryCatch(convert(schema), error = function(e) NULL)
}
