# ingest-url.R: a document given as a web address.
#
# An address used to be read as text. "https://example.org/report.pdf" ends in
# an extension an extractor claims, so it was a file that did not exist, and
# any other address became a one-line document about the address, answered with
# partial = FALSE. It is now downloaded and read with the extractor for what
# came back, and the document's source is the address.

#' Content types, and the extension their download is read as.
#' @noRd
.gr_url_types <- c(
  "application/pdf" = "pdf",
  "text/html" = "html",
  "application/xhtml+xml" = "xhtml",
  "text/plain" = "txt",
  "text/csv" = "csv",
  "text/tab-separated-values" = "tsv",
  "text/markdown" = "md",
  "text/x-markdown" = "md",
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "docx",
  "image/png" = "png",
  "image/jpeg" = "jpg",
  "image/tiff" = "tiff",
  "image/gif" = "gif",
  "image/bmp" = "bmp"
)

#' Is `x` one web address? Space around it, as a spreadsheet column or a file
#' read line by line leaves, does not stop it being one; space inside it is
#' encoded when it is fetched.
#' @noRd
is_url <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(FALSE)
  x <- trimws(x)
  !grepl("[\r\n]", x) && grepl("^https?://[^[:space:]/]", x, ignore.case = TRUE)
}

#' A web address as readgpt shows and keeps it: in messages and errors, as a
#' document's `source`, in the trace and in a corpus's labels.
#'
#' The request needs the address in full, but a presigned or tokenised link
#' (S3, GCS and Azure signatures, download tokens) carries its credentials in
#' the query, and a user name and password can sit ahead of the host. Both
#' were copied into the "Fetching" line, the fetch errors, the document's
#' source, the trace, and a corpus's summary, answers and store rows. So the
#' user name and password are dropped and the query (and a fragment) replaced
#' by a short fingerprint, as report_url() shows an address in the audit
#' report and with the same fingerprint, so two addresses that differ only in
#' the query stay apart.
#'
#' The fingerprint follows a space where report_url() writes a "?": that
#' function hides whatever follows a "?", so an address shown here and shown
#' again by the report got a second fingerprint (of the first one), and in
#' error text, where it takes the address to end at the space, a garbled one.
#' Written this way, report_url(), report_url_text() and this function leave
#' it as it is. The cache key and the store key are made from the address
#' itself, hashed, and do not depend on this.
#' @noRd
url_shown <- function(x) {
  sub("?[query hidden ", " [query hidden ", report_url(x), fixed = TRUE)
}

#' `x` with every web address in it shown as url_shown() shows one: for the
#' reason a request failed, which may quote the address.
#' @noRd
url_shown_text <- function(x) {
  gsub("?[query hidden ", " [query hidden ", report_url_text(x), fixed = TRUE)
}

#' Types that say nothing about the format: plain text is what servers call
#' anything they do not recognise, and the rest mean "some bytes".
#' @noRd
.gr_url_vague_types <- c("", "text/plain", "application/octet-stream", "binary/octet-stream",
                         "application/download", "application/x-download",
                         "application/force-download")

#' What a download is read as.
#'
#' A specific type the server declared comes first. Then the extension in the
#' address, then the first bytes of the file, which settle a PDF, a Word file
#' and an HTML page whatever they were labelled. Only then does a vague type
#' count: text that is not one of those is read as text, and bytes that are not
#' text are refused, as a file no extractor claims is. An extension no extractor
#' claims is passed over at each step.
#' @noRd
url_extension <- function(type, url, path) {
  known <- unique(tolower(unlist(lapply(gr_state$extractors, `[[`, "extensions"))))
  type_full <- as_chr1(type, "")
  type <- tolower(trimws(sub(";.*$", "", type_full)))
  if (!type %in% .gr_url_vague_types) {
    ext <- unname(.gr_url_types[type])
    if (length(ext) == 1L && !is.na(ext) && ext %in% known) return(ext)
  }
  addr <- sub("^https?://[^/]*", "", sub("[?#].*$", "", url), ignore.case = TRUE)
  ext <- tolower(tools::file_ext(addr))
  if (nzchar(ext) && ext %in% known) return(ext)
  head <- tryCatch(readBin(path, "raw", 1024L), error = function(e) raw(0))
  starts <- function(bytes) length(head) >= length(bytes) &&
    identical(head[seq_along(bytes)], bytes)
  if (starts(charToRaw("%PDF"))) return("pdf")
  # A Word file is a zip holding word/document.xml. Other zips (a spreadsheet,
  # a slide deck, an archive) are not documents readgpt reads.
  if (starts(as.raw(c(0x50, 0x4b, 0x03, 0x04)))) {
    parts <- tryCatch(utils::unzip(path, list = TRUE)$Name, error = function(e) character(0))
    if ("word/document.xml" %in% parts) return("docx")
    return(NA_character_)
  }
  # Matched on the bytes: lower-casing them first failed on a byte-order mark,
  # which is not valid UTF-8, and a UTF-16 page went unrecognised.
  txt <- tryCatch(rawToChar(head[head != as.raw(0)]), error = function(e) "")
  if (grepl("<(!doctype html|html|head|body)", txt, ignore.case = TRUE, useBytes = TRUE)) {
    return("html")
  }
  # UTF-16 and UTF-32 text has zero bytes all through it and is still text:
  # told by a byte-order mark, a zero in every other byte, or the charset the
  # server declared. Refused as bytes that are not text, the error blamed the
  # type ("text/plain; charset=utf-16").
  cs <- charset_known(url_charset(type_full))
  if (!is.null(byte_order_mark(head)) || !is.null(utf16_by_zeros(head)) ||
      grepl("^utf-(16|32)", cs %||% "")) {
    return("txt")
  }
  if (any(head == as.raw(0))) return(NA_character_)
  "txt"
}

#' The charset a Content-Type declares ("text/html; charset=windows-1252"), or
#' "" when it declares none.
#' @noRd
url_charset <- function(type) {
  type <- as_chr1(type, "")
  at <- regexpr("(?i);[[:space:]]*charset[[:space:]]*=[[:space:]]*\"?[^\";[:space:]]+", type, perl = TRUE)
  if (at < 0L) return("")
  sub("(?i)^.*=[[:space:]]*\"?", "", regmatches(type, at), perl = TRUE)
}

#' The request itself: the body written to `dest`, the status and the declared
#' type returned. Kept apart so the rest can be tested without a network.
#'
#' Two limits, which a download had none of but a total time:
#'
#'   - Size. A server that streams without end (or a large file) was written to
#'     the temporary directory at whatever speed the line allowed for two
#'     minutes; one test wrote 5.5 GB in three seconds. A response over
#'     `max_bytes` is refused as soon as it is known: from its declared length
#'     before anything is written, or as it passes the limit.
#'   - Where it goes. An address on the machine itself or its private network
#'     (localhost, 10.x, 192.168.x, 169.254.169.254, where cloud machines serve
#'     their credentials) is refused, and so is a redirect to one: redirects
#'     are followed here, one at a time, so each address is checked before it
#'     is fetched. What a download holds is sent to a model and shown in
#'     answers and reports, and any one-line address in a list of texts is
#'     fetched. A host name is looked up first and the request is held to the
#'     addresses checked, so a public name for a private address
#'     ("169.254.169.254.nip.io") is refused too, and the name cannot be
#'     pointed elsewhere between the check and the request.
#'     `options(readgpt.allow_local_urls = TRUE)` lifts this for a document
#'     server on your own network. Behind a proxy that looks names up itself,
#'     a name that cannot be looked up here is fetched unchecked.
#'
#' Its errors show each address as url_shown() does, the one a redirect
#' pointed to included: a redirect to a presigned link is how most storage
#' services hand out a download.
#' @noRd
url_download <- function(url, dest, max_bytes = .gr_max_download_bytes,
                         allow_local = isTRUE(getOption("readgpt.allow_local_urls")),
                         lookup = url_addresses) {
  agent <- sprintf("readgpt/%s", tryCatch(as.character(utils::packageVersion("readgpt")),
                                          error = function(e) "dev"))
  too_big <- function() {
    gr_abort(sprintf(paste0("'%s' is larger than %s, the most readgpt downloads. Download it ",
                            "yourself and pass its path."), url_shown(url), format_bytes(max_bytes)),
             class = c("gr_too_large", "gr_url_error"))
  }
  refuse <- function(where) {
    gr_abort(sprintf(paste0("Refusing to fetch '%s': %s on this machine or a private network. ",
                            "Download the file yourself and pass its path, or set ",
                            "options(readgpt.allow_local_urls = TRUE) to fetch such addresses."),
                     url_shown(url), where), class = "gr_url_error")
  }
  for (hop in 0:10) {
    cfg <- list(followlocation = 0L, maxfilesize_large = max_bytes)
    if (!allow_local) {
      if (url_is_local(url)) refuse("it is")
      host <- url_host(url)
      if (is.na(ipv4_number(host$name)) && !grepl(":", host$name, fixed = TRUE)) {
        ips <- lookup(host$name)
        if (length(ips)) {
          bad <- ips[vapply(ips, ip_is_local, logical(1))]
          if (length(bad)) refuse(sprintf("its host resolves to %s,", bad[1]))
          cfg$resolve <- sprintf("%s:%d:%s", host$name, host$port,
                                 paste(ifelse(grepl(":", ips, fixed = TRUE), paste0("[", ips, "]"), ips),
                                       collapse = ","))
        }
      }
    }
    # The body is written as it arrives, and the write stops at the limit. A
    # progress callback that says stop makes curl raise an interrupt, not an
    # error, which a script cannot catch; and curl before 8.4 checks its own
    # size limit only against a declared length.
    out <- file(dest, "wb")
    written <- 0
    # Written this way, curl reports why a request failed in a warning ("Failed
    # to open ...: Could not resolve host") ahead of a bare "cannot open the
    # connection"; the reason is kept for the error.
    why <- character(0)
    res <- tryCatch(
      withCallingHandlers(
        httr::GET(url, httr::write_stream(function(x) {
                    written <<- written + length(x)
                    if (written > max_bytes) too_big()
                    writeBin(x, out)
                  }),
                  httr::timeout(as_num1(gr_options("request_timeout"), 120)),
                  httr::user_agent(agent),
                  do.call(httr::config, cfg)),
        warning = function(w) {
          if (!startsWith(conditionMessage(w), "Failed to open")) return()
          why <<- c(why, sub("^Failed to open '[^']*': ", "", conditionMessage(w)))
          invokeRestart("muffleWarning")
        }),
      error = function(e) {
        close(out)
        if (inherits(e, "gr_too_large") ||
            any(grepl("maximum file size", c(why, conditionMessage(e)), ignore.case = TRUE))) {
          too_big()
        }
        if (length(why)) stop(simpleError(why[length(why)]))
        stop(e)
      })
    close(out)
    status <- httr::status_code(res)
    to <- as_chr1(httr::headers(res)[["location"]], "")
    if (!status %in% c(301L, 302L, 303L, 307L, 308L) || !nzchar(to)) {
      return(list(status = status, type = as_chr1(httr::headers(res)[["content-type"]], "")))
    }
    from <- url
    url <- url_resolve(to, url)
    if (!grepl("^https?://", url, ignore.case = TRUE)) {
      # Named by its scheme alone: url_shown() hides the credentials of a web
      # address and would pass another kind through whole. And the address
      # that redirected, not the one it pointed to, which this named twice.
      gr_abort(sprintf("'%s' redirected to an address that is not a web address ('%s:').",
                       url_shown(from), sub(":.*$", "", url)),
               class = "gr_url_error")
    }
  }
  gr_abort(sprintf("'%s' redirected more than 10 times.", url_shown(url)), class = "gr_url_error")
}

#' The most a download may be: 512 MB.
#' @noRd
.gr_max_download_bytes <- 512 * 1024^2

#' The address a redirect's Location header points to, from the address that
#' gave it: absolute, scheme-relative ("//host/x"), host-relative ("/x"), a
#' new query ("?x") or relative to the directory ("x").
#' @noRd
url_resolve <- function(to, from) {
  to <- trimws(to)
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*:", to)) return(to)
  scheme <- sub("^([A-Za-z][A-Za-z0-9+.-]*):.*$", "\\1", from)
  if (startsWith(to, "//")) return(paste0(scheme, ":", to))
  origin <- sub("^([A-Za-z][A-Za-z0-9+.-]*://[^/?#]*).*$", "\\1", from)
  if (startsWith(to, "/")) return(paste0(origin, to))
  path <- sub("[?#].*$", "", substring(from, nchar(origin) + 1L))
  if (grepl("^[?#]", to)) return(paste0(origin, path, to))
  paste0(origin, sub("[^/]*$", "", if (nzchar(path)) path else "/"), to)
}

#' Is the host of `url`, as written, this machine or a private network?
#'
#' Names that mean the machine itself (localhost and anything under
#' .localhost), cloud metadata names, and addresses written as numbers in the
#' loopback, private, link-local, shared (100.64/10) or reserved ranges, IPv4
#' or IPv6, in any form an address can be written (2130706433, 0x7f.1 and
#' ::ffff:127.0.0.1 are all 127.0.0.1). Any other name is looked up by
#' url_download().
#' @noRd
url_is_local <- function(url) {
  host <- url_host(url)$name
  if (!nzchar(host) || host %in% c("localhost", "localhost.localdomain", "ip6-localhost",
                                   "ip6-loopback", "metadata.google.internal", "metadata",
                                   "instance-data") ||
      endsWith(host, ".localhost")) {
    return(TRUE)
  }
  if (grepl(":", host, fixed = TRUE)) return(ipv6_is_local(host))
  v4 <- ipv4_number(host)
  !is.na(v4) && ipv4_is_local(v4)
}

#' The host of `url`, lower-cased, without brackets, user, zone or trailing
#' dot (`name`), and the port it is reached on (`port`).
#' @noRd
url_host <- function(url) {
  url <- as_chr1(url, "")
  scheme <- lower_text(sub("^([A-Za-z][A-Za-z0-9+.-]*)://.*$", "\\1", url))
  auth <- sub("^.*@", "", sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/?#]*).*$", "\\1", url))
  if (startsWith(auth, "[")) {
    name <- sub("^\\[([^]]*)\\].*$", "\\1", auth)
    port <- sub("^\\[[^]]*\\]:?", "", auth)
  } else {
    name <- sub(":[0-9]*$", "", auth)
    port <- if (grepl(":[0-9]+$", auth)) sub("^.*:", "", auth) else ""
  }
  name <- tryCatch(utils::URLdecode(name), error = function(e) name)
  name <- lower_text(sub("\\.$", "", sub("%.*$", "", name)))
  port <- suppressWarnings(as.integer(port))
  if (is.na(port)) port <- if (identical(scheme, "https")) 443L else 80L
  list(name = name, port = port)
}

#' Whether one address, as a lookup returns it, is local; see url_is_local().
#' @noRd
ip_is_local <- function(ip) {
  if (grepl(":", ip, fixed = TRUE)) return(ipv6_is_local(lower_text(ip)))
  v4 <- ipv4_number(ip)
  is.na(v4) || ipv4_is_local(v4)
}

#' The addresses a host name resolves to, or NULL when it cannot be looked up
#' here (behind a proxy that looks names up itself, say).
#' @noRd
url_addresses <- function(host) {
  if (!requireNamespace("curl", quietly = TRUE)) return(NULL)
  tryCatch(curl::nslookup(host, multiple = TRUE, error = FALSE), error = function(e) NULL)
}

#' An IPv4 address as the number it stands for, read as the system reads one:
#' one to four parts, each decimal, octal (a leading 0) or hex (0x). NA when
#' `x` is not an address.
#' @noRd
ipv4_number <- function(x) {
  parts <- strsplit(x, ".", fixed = TRUE)[[1]]
  if (!length(parts) || length(parts) > 4L || any(!nzchar(parts))) return(NA_real_)
  digits <- function(s, base) {
    d <- match(strsplit(s, "")[[1]], c(0:9, letters[1:6])) - 1L
    if (anyNA(d) || any(d >= base)) return(NA_real_)
    sum(d * base^(rev(seq_along(d)) - 1))
  }
  v <- vapply(parts, function(p) {
    if (grepl("^0x", p)) { if (p == "0x") 0 else digits(substring(p, 3L), 16) }
    else if (grepl("^0[0-9]+$", p)) digits(substring(p, 2L), 8)
    else digits(p, 10)
  }, numeric(1), USE.NAMES = FALSE)
  if (anyNA(v)) return(NA_real_)
  k <- length(v)
  # The last part fills the bytes the others leave.
  room <- 256^(5L - k)
  if (any(v[-k] > 255) || v[k] >= room) return(NA_real_)
  sum(v[-k] * 256^(3:(4L - k + 1L))[seq_len(k - 1L)]) + v[k]
}

#' Whether an IPv4 address (as a number) is loopback, private, link-local,
#' shared, benchmarking, "this network" or multicast and above.
#' @noRd
ipv4_is_local <- function(n) {
  block <- function(a, b, bits) {
    start <- a * 256^3 + b * 256^2
    n >= start && n < start + 2^(32 - bits)
  }
  n < 256^3 || block(10, 0, 8) || block(100, 64, 10) || block(127, 0, 8) ||
    block(169, 254, 16) || block(172, 16, 12) || block(192, 168, 16) ||
    (n >= 192 * 256^3 && n < 192 * 256^3 + 256) || block(198, 18, 15) || n >= 224 * 256^3
}

#' Whether an IPv6 address is unspecified, loopback, link- or site-local,
#' unique-local or multicast, or carries a local IPv4 address (mapped,
#' compatible, or NAT64). An address that cannot be read counts as local.
#' @noRd
ipv6_is_local <- function(x) {
  if (grepl(".", x, fixed = TRUE)) {
    v4 <- ipv4_number(sub("^.*:", "", x))
    if (is.na(v4)) return(TRUE)
    x <- sub("[^:]*$", sprintf("%x:%x", v4 %/% 65536, v4 %% 65536), x)
  }
  gaps <- gregexpr("::", x, fixed = TRUE)[[1]]
  if (length(gaps) > 1L) return(TRUE)
  side <- function(s) if (nzchar(s)) strsplit(s, ":", fixed = TRUE)[[1]] else character(0)
  if (gaps[1] > 0L) {
    left <- side(substr(x, 1L, gaps[1] - 1L))
    right <- side(substring(x, gaps[1] + 2L))
    g <- c(left, rep("0", max(0L, 8L - length(left) - length(right))), right)
  } else {
    g <- side(x)
  }
  if (length(g) != 8L || !all(grepl("^[0-9a-f]{1,4}$", g))) return(TRUE)
  g <- strtoi(g, 16L)
  v4 <- g[7] * 65536 + g[8]
  all(g[1:7] == 0) ||                                       # :: and ::1
    bitwAnd(g[1], 0xffc0) %in% c(0xfe80, 0xfec0) ||        # link- and site-local
    bitwAnd(g[1], 0xfe00) == 0xfc00 || g[1] >= 0xff00 ||    # unique-local, multicast
    (all(g[1:5] == 0) && g[6] %in% c(0, 0xffff) && ipv4_is_local(v4)) ||
    (g[1] == 0x64 && g[2] == 0xff9b && all(g[3:6] == 0) && ipv4_is_local(v4))
}

#' Download an address to a temporary file whose extension says how to read it.
#'
#' The errors show the address as url_shown() does; see there.
#' @noRd
fetch_url <- function(url) {
  url <- trimws(url)
  shown <- url_shown(url)
  dest <- tempfile("readgpt_url_")
  # A space in an address is not allowed on the wire. Encoded here, and left
  # alone when the address is encoded already.
  got <- tryCatch(url_download(utils::URLencode(url), dest), error = function(e) e)
  if (inherits(got, "error")) {
    unlink(dest)
    # A refusal (too large, a private address) already says what to do.
    if (inherits(got, "gr_url_error")) stop(got)
    # The reason is curl's, and it can quote the address it was given.
    gr_abort(sprintf("Could not fetch '%s': %s", shown, url_shown_text(conditionMessage(got))),
             class = "gr_url_error")
  }
  status <- as_int1(got$status, NA_integer_)
  if (is.na(status) || status >= 400L) {
    unlink(dest)
    gr_abort(sprintf(paste0("Fetching '%s' returned HTTP %s. Check the address, or download ",
                            "the file yourself and pass its path."), shown,
                     if (is.na(status)) "with no status" else status),
             class = "gr_url_error")
  }
  ext <- url_extension(got$type, url, dest)
  if (is.na(ext)) {
    unlink(dest)
    gr_abort(sprintf(paste0("'%s' served %s, which no registered extractor reads. Registered ",
                            "extensions: %s. Add one with gr_register_extractor()."),
                     shown, if (nzchar(trimws(as_chr1(got$type, "")))) sprintf("a file of type '%s'",
                       trimws(as_chr1(got$type, ""))) else "a file of unknown type",
                     paste(sort(unique(unlist(lapply(gr_state$extractors, `[[`, "extensions")))),
                           collapse = ", ")),
             class = "gr_unsupported_format")
  }
  out <- paste0(dest, ".", ext)
  file.rename(dest, out)
  # The charset the server declared travels with the file, for the text and
  # HTML extractors: a page whose encoding is given only there was read as
  # UTF-8, and every accented letter became U+FFFD.
  cs <- url_charset(got$type)
  if (nzchar(cs)) attr(out, "gr_charset") <- cs
  out
}
