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
  type <- tolower(trimws(sub(";.*$", "", as_chr1(type, ""))))
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
  txt <- tryCatch(tolower(rawToChar(head[head != as.raw(0)])), error = function(e) "")
  if (grepl("<(!doctype html|html|head|body)", txt, useBytes = TRUE)) return("html")
  if (any(head == as.raw(0))) return(NA_character_)
  "txt"
}

#' The request itself: the body written to `dest`, the status and the declared
#' type returned. Kept apart so the rest can be tested without a network.
#' @noRd
url_download <- function(url, dest) {
  agent <- sprintf("readgpt/%s", tryCatch(as.character(utils::packageVersion("readgpt")),
                                          error = function(e) "dev"))
  res <- httr::GET(url, httr::write_disk(dest, overwrite = TRUE),
                   httr::timeout(as_num1(gr_options("request_timeout"), 120)),
                   httr::user_agent(agent))
  list(status = httr::status_code(res),
       type = as_chr1(httr::headers(res)[["content-type"]], ""))
}

#' Download an address to a temporary file whose extension says how to read it.
#' @noRd
fetch_url <- function(url) {
  url <- trimws(url)
  dest <- tempfile("readgpt_url_")
  # A space in an address is not allowed on the wire. Encoded here, and left
  # alone when the address is encoded already.
  got <- tryCatch(url_download(utils::URLencode(url), dest), error = function(e) e)
  if (inherits(got, "error")) {
    unlink(dest)
    gr_abort(sprintf("Could not fetch '%s': %s", url, conditionMessage(got)),
             class = "gr_url_error")
  }
  status <- as_int1(got$status, NA_integer_)
  if (is.na(status) || status >= 400L) {
    unlink(dest)
    gr_abort(sprintf(paste0("Fetching '%s' returned HTTP %s. Check the address, or download ",
                            "the file yourself and pass its path."), url,
                     if (is.na(status)) "with no status" else status),
             class = "gr_url_error")
  }
  ext <- url_extension(got$type, url, dest)
  if (is.na(ext)) {
    unlink(dest)
    gr_abort(sprintf(paste0("'%s' served %s, which no registered extractor reads. Registered ",
                            "extensions: %s. Add one with gr_register_extractor()."),
                     url, if (nzchar(trimws(as_chr1(got$type, "")))) sprintf("a file of type '%s'",
                       trimws(as_chr1(got$type, ""))) else "a file of unknown type",
                     paste(sort(unique(unlist(lapply(gr_state$extractors, `[[`, "extensions")))),
                           collapse = ", ")),
             class = "gr_unsupported_format")
  }
  out <- paste0(dest, ".", ext)
  file.rename(dest, out)
  out
}
