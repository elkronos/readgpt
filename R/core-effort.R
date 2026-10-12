# core-effort.R -- how long a reasoning model thinks, stage by stage.
#
# WHY THIS FILE EXISTS
# The default models are reasoning models, and nothing in a read, a corpus run
# or a write-up said how much they should reason. Every call went out at the
# provider's default effort: the four hundred per-chunk extraction calls of a
# corpus reasoned as long as the one call that drew its claims, and every one
# of them billed that reasoning as output, inside the same cap as the reply
# (reasoning_output_cap()). The only lever was a client's `extra_body`, which
# sets one effort for every request the client makes and which no recipe,
# store key, comparison or trace could see: two runs at different efforts
# through one client looked like the same run.
#
# `effort` is that lever, per stage. It is a value carried on the read spec
# (and given to gr_claims(), gr_outline() and gr_synthesise(), which make their
# calls outside one), resolved against each call's trace label in model_call(),
# and sent only to a model that can take it. Resolving it there rather than at
# each call site means the label an effort was chosen by is the label the trace
# records, including the ones a merge tree builds as it goes
# ("synthesise.merge.level2"), and it needs nothing a parallel worker cannot
# see: the map travels in the spec, and the registry it consults is one the
# workers are sent.
#
# Left NULL, nothing in this file touches a request: no body field, no entry in
# the handler's params, no part of any key. Every cache entry, recording and
# corpus store made before it still answers.

#' The reasoning efforts readgpt knows. Others are sent as given, with a warning.
#' @noRd
.gr_effort_levels <- c("none", "minimal", "low", "medium", "high", "xhigh")

#' Check an `effort` setting and say what is wrong with it.
#'
#' NULL, one string, or a character vector named by call-label prefix, with at
#' most one element unnamed or named "default". The shape is refused outright:
#' a map that cannot be read would send the wrong effort to every call it
#' covers, quietly. A value is not: providers add levels faster than this
#' package ships, so one it does not know is warned about and sent anyway. A
#' near miss is named, because a typo is sent too and the provider refuses
#' every request that carries it.
#' @noRd
check_effort <- function(effort, arg = "effort") {
  if (is.null(effort)) return(NULL)
  if (is.factor(effort)) effort <- stats::setNames(as.character(effort), names(effort))
  shape <- function(why) {
    gr_abort(sprintf(paste0("`%s` %s. Give one reasoning effort (\"low\"), or a character vector ",
                            "of them named by the stages they are for, such as ",
                            "c(extract = \"low\", \"synthesise.section\" = \"high\")."), arg, why),
             class = "gr_bad_setting")
  }
  if (!is.character(effort) || !length(effort)) shape("must be a character vector")
  if (anyNA(effort) || !all(has_visible(effort))) shape("has a missing or blank value")
  keys <- effort_keys(effort)
  if (sum(!nzchar(keys)) > 1L) {
    shape(paste0("has more than one value for the calls no name matches (unnamed, or named ",
                 "\"default\")"))
  }
  dup <- unique(keys[nzchar(keys) & duplicated(keys)])
  if (length(dup)) {
    shape(sprintf("names %s more than once", paste(sprintf("'%s'", dup), collapse = ", ")))
  }
  # The same letters in another order counts as near: "hihg" is two edits
  # from "high" and the most ordinary typo there is.
  letters_of <- function(x) vapply(strsplit(tolower(trimws(x)), ""), function(ch)
    paste(sort(ch), collapse = ""), character(1))
  for (v in setdiff(unique(unname(effort)), .gr_effort_levels)) {
    near <- .gr_effort_levels[tolower(trimws(v)) == .gr_effort_levels |
                                as.integer(utils::adist(tolower(v), .gr_effort_levels)) <= 1L |
                                letters_of(v) == letters_of(.gr_effort_levels)]
    gr_warn(sprintf(paste0("Reasoning effort '%s' is not one of %s%s. It is sent as given, since a ",
                           "provider can add levels, and a provider that does not know it will ",
                           "refuse every request it is sent with."),
                    v, paste(.gr_effort_levels, collapse = ", "),
                    if (length(near)) sprintf(", but '%s' is", near[1]) else ""),
            class = "gr_effort_unknown")
  }
  effort
}

#' The label prefix each element of an effort map is for; "" for the default.
#' @noRd
effort_keys <- function(effort) {
  keys <- names(effort) %||% rep("", length(effort))
  keys[is.na(keys)] <- ""
  keys <- trimws(keys)
  keys[keys == "default"] <- ""
  keys
}

#' The effort one call is sent with, or NULL for none.
#'
#' A name covers a label equal to it or beginning with it and a dot, so
#' "synthesise" covers "synthesise.section" and "synthesise.merge.level2" but
#' not a stage that merely starts with the same letters. The longest name that
#' covers the label wins, so `c(synthesise = "low", "synthesise.section" =
#' "high")` writes the sections at high and does the rest at low. A label no
#' name covers takes the default, or, with none, is sent no effort at all.
#' @noRd
effort_for <- function(effort, label) {
  if (!length(effort)) return(NULL)
  vals <- as.character(effort)
  keys <- effort_keys(effort)
  label <- as_chr1(label, "")
  hit <- nzchar(keys) & (keys == label | startsWith(label, paste0(keys, ".")))
  pick <- if (any(hit)) which(hit)[which.max(nchar(keys[hit]))]
          else if (any(!nzchar(keys))) which(!nzchar(keys))[1]
          else return(NULL)
  v <- vals[[pick]]
  if (is.na(v) || !has_visible(v)) NULL else v
}

#' The effort a call actually goes out with: resolved for its label, and only
#' for a model whose registry entry says it reasons.
#'
#' A model that does not reason refuses the field with a 400, so it is left
#' out, as `temperature` is left out for a model that refuses that. Left out
#' without a warning: a recipe that reads with a cheap non-reasoning
#' `skim_model` and answers with a reasoning model is an ordinary setup, and
#' the effort reaches every call that can take it. Each step of the trace
#' records the effort its request carried, so what was and was not sent is on
#' the record either way.
#' @noRd
effort_to_send <- function(effort, label, info) {
  if (is.null(effort) || !isTRUE(info$reasoning)) return(NULL)
  effort_for(effort, label)
}

#' Put an effort into a request body, in the field the API reads it from.
#'
#' Applied after the client's `extra_body` and before the call's own extra
#' fields. A client built with `extra_body = list(reasoning = list(effort =
#' "high"))` sets a default for everything it sends, and the setting for a
#' stage is the more specific of the two, so it wins for the calls it covers;
#' the rest of that `reasoning` object (a `summary` setting, say) is kept. A
#' field written by hand into one gr_call() is more specific still.
#' @noRd
effort_body <- function(body, effort, api) {
  if (is.null(effort)) return(body)
  if (identical(api, "responses")) {
    r <- body[["reasoning", exact = TRUE]]
    body$reasoning <- if (is.list(r)) utils::modifyList(r, list(effort = effort))
                      else list(effort = effort)
  } else {
    body$reasoning_effort <- effort
  }
  body
}

#' A segment spec carrying the run's effort, for the segmenters that call a
#' model (the contextual and proposition segmenters).
#'
#' The effort belongs to the read spec, one setting for the whole run, so that
#' it is one entry in a recipe, a corpus store's key and a comparison's
#' settings, and one override through `...`. Segmentation happens before the
#' read, with the segment spec, so the pipelines hand it across here. Returned
#' unchanged when there is no effort, so a segment spec, and anything keyed on
#' one, is what it always was.
#' @noRd
segment_effort <- function(segment, read) {
  effort <- read[["effort", exact = TRUE]]
  if (is.null(effort) || is.null(segment)) return(segment)
  segment$effort <- effort
  segment
}

#' What reasoning effort the requests of a trace carried, as one line:
#' "low on 12, high on 3 of 16 request(s)". NA when none carried one, which
#' is every trace recorded without `effort`.
#' @noRd
trace_effort_text <- function(trace) {
  if (!inherits(trace, "gr_trace")) return(NA_character_)
  req <- Filter(function(st) !identical(st$kind, "local") && !identical(st$kind, "embedding") &&
                  !is.null(st$tokens), trace$steps)
  eff <- vapply(req, function(st) {
    p <- if (is.list(st$params)) st$params else list()
    as_chr1(p[["effort", exact = TRUE]], NA_character_)
  }, character(1))
  if (!length(eff) || all(is.na(eff))) return(NA_character_)
  n <- table(eff[!is.na(eff)])
  n <- n[order(-n, names(n))]
  sprintf("%s of %d request(s)",
          paste(sprintf("%s on %d", names(n), as.integer(n)), collapse = ", "), length(eff))
}
