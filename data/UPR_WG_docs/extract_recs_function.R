# Extract UPR recommendations from a single document into a tidy rds file.
#
# v5 -- wraps the v4 extraction machinery (extract_recs_refactored.R) in a
#       single user-facing function, extract_upr_recs(), which processes ONE
#       document at a time, stamps it with caller-supplied metadata, and
#       saves the result to data/UPR_WG_docs/extracted_recs/.
#
#   extract_upr_recs(
#     input,                  # docx / pdf / legacy .doc path, or a URL
#     state_under_review,     # e.g. "Somalia"
#     document_symbol,        # source document, e.g. "A/HRC/63/12"
#     upr_session,            # UPR Working Group session, e.g. 52
#     provisional             # TRUE for draft/provisional documents
#   )
#
# Output: a tibble (also saved as "<state_under_review>_<upr_session>.rds"
# in data/UPR_WG_docs/extracted_recs/) with the columns
#
#   state_under_review   caller-supplied
#   recommendation       original text: paragraph prefix + recommendation +
#                        the recommending state(s), e.g.
#                        "6.1 Speed up its effort to ratify key human
#                         rights treaties (India);"
#                        (For auto-numbered reports, where Word generates
#                        the numbering and it never appears as literal
#                        text, the computed paragraph label is prepended so
#                        every format starts with its prefix. The trailing
#                        "Source of Position: ..." reference of the matrix
#                        documents is always removed.)
#   recommendation_clean recommendation text only -- no paragraph prefix,
#                        no trailing "(State); (State);" list
#   paragraph            paragraph label, e.g. "6.1"
#   recommending_states  "; "-separated recommending states
#   position             factor: Supported / Supported/Noted / Noted /
#                        Under consideration / NA (state has not responded)
#   document_symbol      caller-supplied
#   upr_session          caller-supplied
#   provisional          caller-supplied
#
# The extraction machinery extends extract_recs_refactored.R (v4) and
# handles, auto-detected:
#   - original Working Group draft reports (.docx), auto-numbered or
#     literal-prefix, scoped to the "Conclusions and/or recommendations"
#     section, with positions from the lead-in paragraphs;
#   - final adopted reports as PDFs (e.g. from docs.un.org symbol links);
#   - cycle-1 reports (e.g. A/HRC/12/2), where recommendations are
#     numbered "1.", "2.", ... restarting under each lead-in paragraph
#     ("sub-numbered" format; labels become 74.1, 74.2, ...) and
#     recommending states are often attributed inline
#     ("Consider ratifying (Turkey) / Ratify (Mexico) ...");
#   - OHCHR "matrix of recommendations" tables (.docx, legacy .doc
#     converted on the fly via Microsoft Word, or PDF -- for PDFs the
#     table columns are rebuilt from the word coordinates), with positions
#     from the matrix's Position column;
#   - URLs for any of the above.

suppressPackageStartupMessages({
  library(xml2)
  library(stringr)
  library(tibble)
  library(dplyr)
  library(here)
})

# Patterns
.STATE_RE   <- "\\([^()]*(?:\\([^()]*\\)[^()]*)*\\)"
.TRAIL_RE   <- paste0("(", .STATE_RE, "(?:\\s*;\\s*", .STATE_RE, ")*)\\s*;?\\s*$")
.PREFIX_RE  <- "^\\s*(\\d+)\\.(\\d+)\\s*"
.LEADIN_RE  <- "^\\s*(\\d+)\\.(?!\\d)"   # top-level "6." (but not "6.1")
# (allows trailing footnote markers, e.g. "II. Conclusions and/or recommendations**")
.SECTION_RE <- "^\\s*(?:[IVXLC]+\\.)?\\s*Conclusions?\\s+and/?or\\s+recommendations\\s*\\**\\s*$"
.END_RE     <- "All conclusions and/or recommendations contained in the present report"
.ANNEX_RE   <- "^\\s*Annexe?s?\\s*$"
.MODE_THRESHOLD <- 0.8   # >= 80% to commit to a format

# "Supported/Noted" is the mixed (partially supported) category used by the
# OHCHR matrix documents; report-based extraction never produces it.
.POSITION_LEVELS <- c("Supported", "Supported/Noted", "Noted", "Under consideration")

# Like .PREFIX_RE, but tolerates a trailing dot after the paragraph number,
# as used in the older matrix documents ("166.6. Ratify ...")
.MATRIX_PREFIX_RE <- "^\\s*(\\d+)\\.(\\d+)\\.?\\s*"

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# ---- Recommendation parser --------------------------------------------------
# Splits "rec text (State); (State); ..." into rec text + state list.
# Caller is responsible for stripping any leading "6.N\t" prefix beforehand.

parse_recommendation <- function(x) {
  x <- str_replace(x, "[;.\\s]+$", "")
  m <- str_match(x, .TRAIL_RE)
  if (is.na(m[1, 1])) return(list(text = x, states = NA_character_))
  trailing <- m[1, 2]
  rec_text <- str_trim(str_sub(x, 1L, str_length(x) - str_length(trailing)))
  states   <- str_extract_all(trailing, .STATE_RE)[[1]]
  states   <- str_trim(str_sub(states, 2L, -2L))
  list(text = rec_text, states = paste(states, collapse = "; "))
}

# ---- Lead-in classifier -----------------------------------------------------
# Maps a lead-in paragraph ("6.The recommendations ... enjoy the support of
# X:") to a position. Order matters: the Supported and Noted lead-ins both
# also contain "have been examined", so "will be examined" (future tense)
# must be checked last.

classify_leadin <- function(text) {
  case_when(
    # "did/do not enjoy the support" (older reports) must be checked before
    # the plain "enjoy the support" pattern
    str_detect(text, regex("(did|do(es)?)\\s+not\\s+enjoy\\s+(the|its|their)\\s+support",
                           ignore_case = TRUE))                                ~ "Noted",
    str_detect(text, regex("enjoys?\\s+(the|its|their)\\s+support",
                           ignore_case = TRUE))                                ~ "Supported",
    str_detect(text, regex("(have\\s+been|are)\\s+noted", ignore_case = TRUE)) ~ "Noted",
    str_detect(text, regex("will\\s+be\\s+examined", ignore_case = TRUE))      ~ "Under consideration",
    .default = NA_character_
  )
}

# ---- Input handling ---------------------------------------------------------
# Turns whatever the caller gave us (docx path, pdf path, legacy .doc path,
# or URL) into a local file path plus a type ("docx" or "pdf"). Legacy .doc
# files are converted to .docx on the fly.

.convert_doc_to_docx <- function(path, verbose = TRUE) {
  if (.Platform$OS.type != "windows") {
    stop("Legacy .doc files are converted via Microsoft Word COM ",
         "automation, which is only available on Windows. Please save '",
         basename(path), "' as .docx and try again.")
  }
  if (verbose) cat("Converting legacy .doc to .docx via Microsoft Word...\n")
  src <- normalizePath(path, winslash = "\\", mustWork = TRUE)
  dst <- normalizePath(tempfile(fileext = ".docx"), winslash = "\\",
                       mustWork = FALSE)
  # A .ps1 script file sidesteps command-line quoting issues
  ps_file <- tempfile(fileext = ".ps1")
  writeLines(c(
    '$ErrorActionPreference = "Stop"',
    '$word = New-Object -ComObject Word.Application',
    '$word.Visible = $false',
    'try {',
    sprintf('  $doc = $word.Documents.Open("%s", $false, $true)', src),
    sprintf('  $doc.SaveAs2("%s", 16)', dst),   # 16 = wdFormatXMLDocument
    '  $doc.Close($false)',
    '  [System.Runtime.InteropServices.Marshal]::ReleaseComObject($doc) | Out-Null',
    '} finally {',
    '  $word.Quit()',
    '  # Release the COM reference so the hidden WINWORD process exits',
    '  [System.Runtime.InteropServices.Marshal]::ReleaseComObject($word) | Out-Null',
    '  [GC]::Collect(); [GC]::WaitForPendingFinalizers()',
    '}'
  ), ps_file)
  suppressWarnings(system2(
    "powershell",
    c("-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass",
      "-File", ps_file),
    stdout = FALSE, stderr = FALSE
  ))
  unlink(ps_file)
  if (!file.exists(dst)) {
    stop("Converting '", basename(path), "' from .doc to .docx failed. ",
         "The conversion requires Microsoft Word; alternatively, open the ",
         "file in Word or LibreOffice, save it as .docx, and pass that.")
  }
  dst
}

.resolve_input <- function(input, verbose = TRUE) {
  if (str_detect(input, "^https?://")) {
    # docs.un.org symbol links serve an HTML viewer; the PDF itself lives
    # behind the documents.un.org access API. Rewrite directly.
    m <- str_match(input,
                   "^https?://docs\\.un\\.org/([a-z]{2})/(.+?)/?$")
    url <- if (!is.na(m[1, 1])) {
      sprintf("https://documents.un.org/api/symbol/access?s=%s&l=%s&t=pdf",
              m[1, 3], m[1, 2])
    } else {
      input
    }
    if (verbose) cat("Downloading:", url, "\n")
    path <- tempfile(fileext = ".tmp")
    utils::download.file(url, path, mode = "wb", quiet = TRUE)
  } else {
    stopifnot(file.exists(input))
    path <- input
  }

  # Sniff the file type from the magic bytes ("%PDF", "PK" zip = docx,
  # OLE2 compound document = legacy .doc), falling back to the extension.
  magic <- readBin(path, "raw", n = 8L)
  is_ole2 <- length(magic) >= 8L &&
    identical(magic, as.raw(c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1)))
  type <- if (length(magic) >= 4L && rawToChar(magic[1:4]) == "%PDF") {
    "pdf"
  } else if (length(magic) >= 2L && rawToChar(magic[1:2]) == "PK") {
    "docx"
  } else if (is_ole2) {
    "doc"
  } else if (str_detect(input, regex("\\.pdf$", ignore_case = TRUE))) {
    "pdf"
  } else if (str_detect(input, regex("\\.docx$", ignore_case = TRUE))) {
    "docx"
  } else if (str_detect(input, regex("\\.doc$", ignore_case = TRUE))) {
    "doc"
  } else {
    stop("Cannot determine file type of '", input,
         "' (expected a PDF, a docx, or a legacy .doc).")
  }

  if (type == "doc") {
    path <- .convert_doc_to_docx(path, verbose = verbose)
    type <- "docx"
  }
  list(path = path, type = type)
}

# ---- Paragraph readers --------------------------------------------------------
# Both return a tibble(idx, nid, text): one row per non-empty paragraph.
# nid is the Word numbered-list id (always NA for PDFs, which have no list
# metadata -- their recommendations carry literal "N.M" prefixes instead).

.read_docx_paragraphs <- function(path) {
  # Read document.xml straight out of the .docx zip. (A unz() connection
  # rather than utils::unzip(): no temp files, and it copes with archives
  # that crash R's unzip-to-disk path.)
  doc <- read_xml(unz(path, "word/document.xml"))
  ns  <- xml_ns(doc)

  paras <- xml_find_all(doc, ".//w:p", ns = ns)
  tibble(
    idx  = seq_along(paras),
    nid  = vapply(paras, function(p) {
      ids <- xml_find_all(p, ".//w:numId", ns = ns)
      if (length(ids) == 0L) NA_character_ else xml_attr(ids[[1L]], "val")
    }, character(1)),
    text = vapply(paras, function(p) {
      str_trim(paste(xml_text(xml_find_all(p, ".//w:t", ns = ns)),
                     collapse = ""))
    }, character(1))
  ) |>
    filter(nzchar(text))
}

.read_pdf_paragraphs <- function(path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Reading PDFs requires the 'pdftools' package.")
  }
  pages <- pdftools::pdf_text(path)

  lines <- unlist(lapply(pages, function(pp) {
    ls <- strsplit(pp, "\n", fixed = TRUE)[[1]]
    # Footnotes sit at the bottom of the page: drop everything from the
    # first footnote-marker line onwards. Markers are asterisks on their
    # own line / followed by text ("**", "* The annex ..."), or a small
    # number hanging at low indentation ("1   The recommendations were
    # not edited.") -- body text is indented much further right.
    fn <- which(
      str_detect(ls, "^\\s*\\*+\\s*$") |
        str_detect(ls, "^\\s*\\*+\\s+\\S") |
        str_detect(ls, "^\\s{0,4}\\d{1,2}\\s{2,}\\S")
    )
    if (length(fn) > 0L) ls <- ls[seq_len(fn[1L] - 1L)]
    # Page furniture: the document symbol repeated on every page
    # ("A/HRC/28/16"), bare page numbers, "page 17"-style footers (older
    # documents), and "GE."-style print codes.
    ls[
      !str_detect(ls, "^\\s*[A-Z]+(/[A-Za-z0-9.]+)+\\s*$") &
        !str_detect(ls, "^\\s*\\d+\\s*$") &
        !str_detect(ls, regex("^\\s*page\\s+\\d+\\s*$", ignore_case = TRUE)) &
        !str_detect(ls, "^\\s*GE\\.[0-9]") &
        nzchar(str_trim(ls))
    ]
  }))

  # Reassemble the wrapped lines into paragraphs. A new paragraph starts at
  # a numbered lead-in or recommendation ("166.", "166.1"), a roman-numeral
  # heading ("II. Conclusions ..."), or an Annex heading; every other line
  # continues the current paragraph. Lines broken on a hyphen are rejoined
  # without inserting a space ("OP-" + "CAT" -> "OP-CAT").
  starts <- str_detect(lines, "^\\s*\\d{1,3}\\.(?:\\d{1,3})?\\s") |
    str_detect(lines, "^\\s*[IVXLC]+\\.\\s*\\S") |
    str_detect(lines, "^\\s*Annexe?s?\\b")
  starts[1L] <- TRUE
  para_id <- cumsum(starts)

  text <- vapply(split(str_squish(lines), para_id), function(chunk) {
    out <- chunk[1L]
    for (piece in chunk[-1L]) {
      out <- if (str_detect(out, "-$")) paste0(out, piece)
      else paste(out, piece)
    }
    out
  }, character(1))

  tibble(
    idx  = seq_along(text),
    nid  = NA_character_,
    text = unname(text)
  ) |>
    filter(nzchar(text))
}

# ---- Matrix documents ---------------------------------------------------------
# OHCHR "matrix of recommendations" tables. Returns the output tibble, or
# NULL when the docx contains no table with Recommendation/Position columns
# (i.e. it is a regular report and should go through the paragraph pipeline).

.normalize_matrix_position <- function(x, verbose = TRUE) {
  x <- str_squish(x)
  # compare with all whitespace removed, so values wrapped across lines in
  # PDF cells still match ("Supported/Not" + "ed" -> "Supported/Noted")
  xc <- str_remove_all(x, "\\s")
  pos <- case_when(
    str_detect(xc, regex("^Supported/Noted$", ignore_case = TRUE)) ~ "Supported/Noted",
    str_detect(xc, regex("^Supported$", ignore_case = TRUE))       ~ "Supported",
    str_detect(xc, regex("^Noted$", ignore_case = TRUE))           ~ "Noted",
    str_detect(xc, regex("consideration|examined|pending",
                         ignore_case = TRUE))                      ~ "Under consideration",
    .default = NA_character_
  )
  unmapped <- unique(x[is.na(pos) & !is.na(x) & nzchar(x)])
  if (verbose && length(unmapped) > 0L) {
    cat(sprintf("WARNING: unrecognized Position value(s) mapped to NA: %s\n",
                paste(sQuote(unmapped), collapse = ", ")))
  }
  pos
}

# Shared tail of the matrix parsers (docx tables and PDF tables): takes one
# row per table row with the raw Recommendation cell, the raw Position cell
# and the optional "Recommending state/s" cell, and produces the output
# tibble.

.finalize_matrix_rows <- function(df, verbose = TRUE) {
  df <- df |>
    mutate(
      # Drop the trailing source reference that must never appear in the
      # text ("Source of Position: A/HRC/43/16/Add.1 - Para.7")
      raw  = str_remove(raw, regex("\\s*Source of position:?[\\s\\S]*$",
                                   ignore_case = TRUE)),
      raw  = str_squish(str_replace_all(raw, "\n", " ")),
      sec  = str_match(raw, .MATRIX_PREFIX_RE)[, 2L],
      num  = as.integer(str_match(raw, .MATRIX_PREFIX_RE)[, 3L]),
      body = str_trim(str_replace(raw, .MATRIX_PREFIX_RE, ""))
    )

  no_prefix <- filter(df, is.na(sec))
  if (verbose && nrow(no_prefix) > 0L) {
    cat(sprintf(
      "\nWARNING: %d matrix row(s) lack a leading 'N.M' paragraph number and were skipped:\n",
      nrow(no_prefix)))
    for (i in seq_len(nrow(no_prefix))) {
      cat(sprintf("  %s\n", str_trunc(no_prefix$raw[i], 160)))
    }
  }
  df <- filter(df, !is.na(sec))

  parsed <- lapply(df$body, parse_recommendation)
  out <- tibble(
    sec                  = as.integer(df$sec),
    num                  = df$num,
    paragraph            = sprintf("%s.%d", df$sec, df$num),
    recommendation       = df$raw,
    recommendation_clean = vapply(parsed, `[[`, character(1), "text"),
    recommending_states  = vapply(parsed, `[[`, character(1), "states"),
    position             = .normalize_matrix_position(df$pos_raw,
                                                      verbose = verbose)
  )

  # Fallback for recs whose text lacks the "(State);" trailer: use the
  # matrix's own "Recommending state/s" column when present
  fallback <- str_squish(str_replace_all(df$states_cell, "\n", "; "))
  use_fb   <- is.na(out$recommending_states) & !is.na(fallback) & nzchar(fallback)
  out$recommending_states[use_fb] <- fallback[use_fb]

  # Matrices repeat a recommendation under each theme it belongs to:
  # collapse identical repeats, then keep the first occurrence of any
  # paragraph number that still conflicts
  out <- distinct(out, paragraph, recommendation, recommending_states,
                  position, .keep_all = TRUE)
  n_conflict <- sum(duplicated(out$paragraph))
  if (verbose && n_conflict > 0L) {
    cat(sprintf(
      "WARNING: %d paragraph number(s) appear with conflicting content; keeping the first occurrence.\n",
      n_conflict))
  }
  out |>
    distinct(paragraph, .keep_all = TRUE) |>
    arrange(sec, num) |>
    select(paragraph, recommendation, recommendation_clean,
           recommending_states, position)
}

.extract_matrix_recs <- function(path, verbose = TRUE) {
  doc <- read_xml(unz(path, "word/document.xml"))
  ns  <- xml_ns(doc)

  # Cell text: runs within a paragraph concatenate with no separator (a
  # paragraph number can be split across runs), paragraphs within a cell
  # join with a newline.
  cell_texts <- function(row) {
    vapply(xml_find_all(row, "./w:tc", ns = ns), function(tc) {
      ps  <- xml_find_all(tc, ".//w:p", ns = ns)
      txt <- vapply(ps, function(p) {
        str_trim(paste(xml_text(xml_find_all(p, ".//w:t", ns = ns)),
                       collapse = ""))
      }, character(1))
      paste(txt[nzchar(txt)], collapse = "\n")
    }, character(1))
  }

  for (tbl in xml_find_all(doc, ".//w:tbl", ns = ns)) {
    rows <- xml_find_all(tbl, "./w:tr", ns = ns)
    if (length(rows) < 2L) next
    hdr <- cell_texts(rows[[1L]])
    rec_col    <- which(str_detect(hdr, regex("^\\s*Recommendations?\\b",
                                              ignore_case = TRUE)))[1]
    pos_col    <- which(str_detect(hdr, regex("^\\s*Position\\b",
                                              ignore_case = TRUE)))[1]
    states_col <- which(str_detect(hdr, regex("^\\s*Recommending state",
                                              ignore_case = TRUE)))[1]
    if (is.na(rec_col) || is.na(pos_col)) next

    if (verbose) {
      cat(sprintf(
        "Detected format: matrix of recommendations (%d table rows)\nColumns: %s\n",
        length(rows) - 1L, paste(str_squish(hdr), collapse = " | ")))
    }

    cells   <- lapply(rows[-1L], cell_texts)
    is_data <- vapply(cells, length, integer(1)) >=
      max(rec_col, pos_col, states_col, na.rm = TRUE)
    if (verbose && sum(!is_data) > 0L) {
      cat(sprintf("Skipping %d separator row(s) (theme headings).\n",
                  sum(!is_data)))
    }
    cells <- cells[is_data]

    df <- tibble(
      raw = vapply(cells, function(cl) cl[rec_col], character(1)),
      pos_raw = vapply(cells, function(cl)
        str_squish(str_replace_all(cl[pos_col], "\n", " ")), character(1)),
      states_cell = vapply(cells, function(cl)
        if (!is.na(states_col)) cl[states_col] else NA_character_,
        character(1))
    )
    return(.finalize_matrix_rows(df, verbose = verbose))
  }
  NULL
}

# ---- Matrix documents as PDFs (e.g. LesothoMatriceRecommendations.pdf) --------
# Same matrix, but printed to PDF: the table structure is gone, so the
# columns are rebuilt from the word coordinates of pdftools::pdf_data(),
# using the header row ("Recommendation | [Recommending state/s] |
# Position | ...") to locate the column x-boundaries. Records start at
# "N.M." anchors in the Recommendation column; "Right or area:" / "Theme:"
# separator lines (which run across the whole page width) are dropped.
# Returns the output tibble, or NULL when the PDF has no such header row.

.extract_matrix_recs_pdf <- function(path, verbose = TRUE) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Reading PDFs requires the 'pdftools' package.")
  }
  pages <- pdftools::pdf_data(path)
  if (length(pages) == 0L) return(NULL)

  # A matrix header has "Recommendation" and, to its right on the same
  # line, "Position"
  find_header <- function(w) {
    cand <- which(w$text == "Recommendation")
    for (i in cand) {
      row <- w[abs(w$y - w$y[i]) <= 3, ]
      if (any(row$text == "Position" & row$x > w$x[i])) {
        return(list(y = w$y[i], x_rec = w$x[i],
                    x_pos = row$x[row$text == "Position"][1L], row = row))
      }
    }
    NULL
  }
  hdr <- find_header(pages[[1L]])
  if (is.null(hdr)) return(NULL)

  # Column boundaries (5pt tolerance for slightly out-dented wraps)
  row      <- hdr$row
  x_states <- row$x[str_detect(row$text, "^Recommen") &
                      row$x > hdr$x_rec + 10 & row$x < hdr$x_pos]
  x_states <- if (length(x_states) > 0L) min(x_states) else NA_real_
  x_right  <- row$x[row$x > hdr$x_pos + 10]
  x_right  <- if (length(x_right) > 0L) min(x_right) else Inf
  rec_end    <- if (!is.na(x_states)) x_states - 5 else hdr$x_pos - 5
  states_end <- hdr$x_pos - 5
  pos_end    <- x_right - 5

  if (verbose) {
    cat(sprintf(
      "Detected format: matrix of recommendations (PDF table, %d pages)\n",
      length(pages)))
  }

  # Walk the pages line by line, starting a new record at each "N.M."
  # anchor in the Recommendation column (records can span page breaks)
  recs <- list()
  cur  <- NULL
  n_sep <- 0L
  for (w in pages) {
    ph <- find_header(w)
    # Page furniture: the title line plus the (possibly wrapped) column
    # header -- everything above the header bottom
    cutoff <- if (!is.null(ph)) ph$y + 14L else min(w$y) + 2L
    w <- w[w$y > cutoff, ]
    if (nrow(w) == 0L) next
    w <- w[order(w$y, w$x), ]
    w$line <- cumsum(c(TRUE, diff(w$y) > 3))
    for (ln in split(w, w$line)) {
      rec_txt <- paste(ln$text[ln$x < rec_end], collapse = " ")
      # Theme separators run across the whole page width: skip the line
      if (str_detect(rec_txt, "^\\s*(Right or area|Theme)\\s*:")) {
        n_sep <- n_sep + 1L
        next
      }
      sta_txt <- if (!is.na(x_states)) {
        paste(ln$text[ln$x >= rec_end & ln$x < states_end], collapse = " ")
      } else ""
      pos_txt <- paste(ln$text[ln$x >= states_end & ln$x < pos_end],
                       collapse = " ")
      if (str_detect(rec_txt, "^\\s*\\d+\\.\\d+\\.?(\\s|$)")) {
        if (!is.null(cur)) recs[[length(recs) + 1L]] <- cur
        cur <- list(raw = character(), states = character(),
                    pos = character())
      }
      if (is.null(cur)) next   # anything before the first record
      if (nzchar(rec_txt)) cur$raw    <- c(cur$raw, rec_txt)
      if (nzchar(sta_txt)) cur$states <- c(cur$states, sta_txt)
      if (nzchar(pos_txt)) cur$pos    <- c(cur$pos, pos_txt)
    }
  }
  if (!is.null(cur)) recs[[length(recs) + 1L]] <- cur
  if (length(recs) == 0L) return(NULL)

  if (verbose && n_sep > 0L) {
    cat(sprintf("Skipping %d separator line(s) (theme headings).\n", n_sep))
  }

  df <- tibble(
    raw = vapply(recs, function(r) paste(r$raw, collapse = "\n"),
                 character(1)),
    pos_raw = vapply(recs, function(r) paste(r$pos, collapse = " "),
                     character(1)),
    states_cell = vapply(recs, function(r)
      if (length(r$states) > 0L) paste(r$states, collapse = "\n")
      else NA_character_, character(1))
  )
  .finalize_matrix_rows(df, verbose = verbose)
}

# ---- Core extractor ---------------------------------------------------------
#
# Args:
#   input           : one of
#                       - path to a .docx (original draft report, or an
#                         OHCHR "matrix of recommendations" table)
#                       - path to a .pdf (e.g. a final adopted report)
#                       - path to a legacy .doc (converted via MS Word)
#                       - a URL, e.g. "https://docs.un.org/en/A/HRC/28/16"
#   mode            : "auto" (default) / "auto-numbered" / "literal-prefix"
#   num_id          : in auto-numbered mode, optional override (else detected)
#   section_prefix  : fallback for the "6" in "6.1, 6.2, ..." when a block
#                     has no literal lead-in number -- defaults to "6"
#   verbose         : print summary + warnings
#
# Returns: tibble with paragraph, recommendation (original text incl.
# prefix and states), recommendation_clean, recommending_states, position.

extract_upr_recommendations <- function(
    input,
    mode           = c("auto", "auto-numbered", "literal-prefix", "sub-numbered"),
    num_id         = NULL,
    section_prefix = NULL,
    verbose        = TRUE
) {
  mode <- match.arg(mode)

  # 1. Resolve URL / file type (legacy .doc is converted to docx here).
  src <- .resolve_input(input, verbose = verbose)

  # 1b. OHCHR "matrix of recommendations" documents are a table, not
  #     running text: if the document has a Recommendation/Position table
  #     (docx) or table header (PDF), parse it row by row and skip the
  #     paragraph pipeline entirely.
  if (src$type == "docx") {
    out <- .extract_matrix_recs(src$path, verbose = verbose)
    if (!is.null(out)) return(.finalize_output(out, verbose = verbose))
  } else if (src$type == "pdf") {
    out <- .extract_matrix_recs_pdf(src$path, verbose = verbose)
    if (!is.null(out)) return(.finalize_output(out, verbose = verbose))
  }

  # 2. Read one row per paragraph.
  para_df <- switch(src$type,
                    docx = .read_docx_paragraphs(src$path),
                    pdf  = .read_pdf_paragraphs(src$path))
  if (verbose) cat(sprintf("Read %d non-empty paragraphs from the %s.\n",
                           nrow(para_df), src$type))

  # 3. Scope to the "Conclusions and/or recommendations" section.
  sec_start <- which(str_detect(para_df$text,
                                regex(.SECTION_RE, ignore_case = TRUE)))
  if (length(sec_start) > 0L) {
    sec_start <- sec_start[1L]
    after     <- seq(sec_start + 1L, nrow(para_df))
    sec_end   <- after[str_detect(para_df$text[after],
                                  regex(.END_RE, ignore_case = TRUE)) |
                         str_detect(para_df$text[after], .ANNEX_RE)]
    sec_end   <- if (length(sec_end) > 0L) sec_end[1L] else nrow(para_df) + 1L
    if (verbose) {
      cat(sprintf(
        "Recommendations section: '%s' (doc-paragraph idx=%d to %s)\n",
        str_trunc(para_df$text[sec_start], 60), para_df$idx[sec_start],
        if (sec_end <= nrow(para_df))
          sprintf("idx=%d '%s'", para_df$idx[sec_end],
                  str_trunc(para_df$text[sec_end], 50))
        else "end of document"))
    }
    para_df <- para_df[seq(sec_start + 1L, sec_end - 1L), ]
  } else if (verbose) {
    cat("WARNING: no 'Conclusions and/or recommendations' heading found; ",
        "scanning the whole document.\n", sep = "")
  }

  para_df <- para_df |>
    mutate(
      text_clean = str_replace(text, "[;.\\s]+$", ""),
      is_rec     = str_detect(text_clean, .TRAIL_RE),
      pre_match  = str_match(text, .PREFIX_RE)[, 1L],
      has_prefix = !is.na(pre_match)
    )

  # 4. Mode detection
  rec_paras <- filter(para_df, is_rec)
  if (nrow(rec_paras) == 0L) {
    stop("No paragraphs match the recommendation shape '... (State);'.")
  }

  if (mode == "auto") {
    prefix_frac <- mean(rec_paras$has_prefix)
    nid_frac    <- mean(!is.na(rec_paras$nid))
    mode <- if (prefix_frac >= .MODE_THRESHOLD && nid_frac < .MODE_THRESHOLD) {
      "literal-prefix"
    } else if (prefix_frac < 0.2 && nid_frac < 0.2) {
      # no Word list metadata and no "N.M" prefixes: cycle-1 style, where
      # recommendations are numbered "1.", "2.", ... under each lead-in
      "sub-numbered"
    } else {
      "auto-numbered"
    }
    if (verbose) {
      cat(sprintf(
        "Detected format: %s (prefix coverage %.0f%%, numId coverage %.0f%%)\n",
        mode, 100 * prefix_frac, 100 * nid_frac))
    }
  }

  # 5. Cycle-1 sub-numbered documents rebuild their own lead-in blocks
  #    (their recommendations start with "N." and would confuse the generic
  #    lead-in detection below).
  if (mode == "sub-numbered") {
    out <- .extract_subnumbered(para_df, verbose = verbose)
    return(.finalize_output(out, verbose = verbose))
  }

  # 6. Position blocks. A lead-in is a non-rec paragraph starting with a
  #    literal top-level number ("6." but not "6.1"). block 0 = anything
  #    before the first lead-in.
  para_df <- para_df |>
    mutate(
      is_leadin  = !is_rec & str_detect(text, .LEADIN_RE),
      leadin_num = if_else(is_leadin, str_match(text, .LEADIN_RE)[, 2L],
                           NA_character_),
      block      = cumsum(is_leadin)
    )

  blocks <- para_df |>
    filter(is_leadin) |>
    transmute(
      block,
      block_prefix   = leadin_num,
      block_position = classify_leadin(text),
      leadin_text    = text
    )

  # NA rule: "Under consideration" only counts as a position when the State
  # has actually responded to something (i.e. there is also a Supported or
  # Noted block). A document whose only lead-in(s) say "will be examined"
  # has no positions yet -> all NA.
  has_response <- any(blocks$block_position %in% c("Supported", "Noted"))
  if (!has_response) blocks$block_position <- NA_character_

  if (verbose && nrow(blocks) > 0L) {
    cat("Lead-in paragraphs found:\n")
    for (i in seq_len(nrow(blocks))) {
      cat(sprintf("  [%s.] %s -> %s\n",
                  blocks$block_prefix[i],
                  str_trunc(blocks$leadin_text[i], 90),
                  blocks$block_position[i] %||% if (has_response)
                    "<unrecognized>" else "NA (no positions given yet)"))
    }
  }

  para_df <- para_df |>
    left_join(select(blocks, block, block_prefix, block_position),
              by = "block")

  # Recs sitting under an unrecognized lead-in deserve a loud warning
  # (an unrecognized lead-in with no recs after it is fine -- e.g. a
  # free-text "X notes the relevant recommendations as follows: ..."
  # paragraph just before the closing disclaimer).
  if (verbose && has_response) {
    orphaned <- para_df |>
      filter(is_rec, block > 0L, is.na(block_position))
    if (nrow(orphaned) > 0L) {
      bad_blocks <- blocks |> filter(block %in% unique(orphaned$block))
      cat(sprintf(
        "\nWARNING: %d recommendation(s) follow lead-in(s) whose position ",
        nrow(orphaned)))
      cat("could not be classified; their position is NA:\n")
      for (i in seq_len(nrow(bad_blocks))) {
        cat(sprintf("  [%s.] %s\n", bad_blocks$block_prefix[i],
                    str_trunc(bad_blocks$leadin_text[i], 120)))
      }
    }
  }

  # 7. Dispatch (re-filter so rec_paras carries the block columns)
  rec_paras <- filter(para_df, is_rec)
  out <- switch(mode,
                "literal-prefix" = .extract_literal_prefix(para_df, rec_paras,
                                                           section_prefix, verbose),
                "auto-numbered"  = .extract_auto_numbered(para_df, num_id,
                                                          section_prefix %||% "6", verbose)
  )
  .finalize_output(out, verbose = verbose)
}

# Shared tail of every extraction path: turn position into a factor and
# print the summary.
.finalize_output <- function(out, verbose = TRUE) {
  out <- mutate(out, position = factor(position, levels = .POSITION_LEVELS))
  if (verbose) {
    n_pairs <- sum(str_count(out$recommending_states, ";"), na.rm = TRUE) +
      sum(!is.na(out$recommending_states))
    cat(sprintf("\nExtracted %d recommendations (%d rec-state pairs).\n",
                nrow(out), n_pairs))
    pos_counts <- table(out$position, useNA = "ifany")
    pos_names  <- names(pos_counts)
    pos_names[is.na(pos_names)] <- "NA"
    cat("Positions: ",
        paste(sprintf("%s = %d", pos_names, pos_counts), collapse = ", "),
        "\n", sep = "")
  }
  out
}

# ---- Format A: auto-numbered (Mozambique-style) -----------------------------
# Paragraph labels restart at each lead-in block: "<lead-in number>.<n>".
# The numbering is generated by Word and never appears as literal text, so
# the computed label is prepended to form the "original" recommendation.

.extract_auto_numbered <- function(para_df, num_id, section_prefix, verbose) {
  if (is.null(num_id)) {
    rec_in_lists <- para_df |> filter(is_rec, !is.na(nid))
    if (nrow(rec_in_lists) == 0L) {
      stop("auto-numbered mode forced, but no rec-shaped paragraphs are in ",
           "any numbered list. Try mode = \"literal-prefix\".")
    }
    nid_counts <- sort(table(rec_in_lists$nid), decreasing = TRUE)
    num_id <- names(nid_counts)[1L]
    if (verbose) {
      cat(sprintf("Auto-detected numId='%s' (%d rec-shaped paragraphs)\n",
                  num_id, nid_counts[[1L]]))
      if (length(nid_counts) > 1L) {
        runners <- paste(sprintf("%s (%d)", names(nid_counts)[-1L],
                                 as.integer(nid_counts)[-1L]),
                         collapse = ", ")
        cat(sprintf("Other numIds with rec-shaped paragraphs: %s\n", runners))
      }
    }
  }

  numbered <- para_df |> filter(is_rec, !is.na(nid), nid == num_id) |>
    arrange(idx) |>
    group_by(block) |>
    mutate(paragraph = sprintf("%s.%d", coalesce(block_prefix, section_prefix),
                               row_number())) |>
    ungroup()
  unnumbered <- para_df |> filter(is_rec, is.na(nid) | nid != num_id) |>
    arrange(idx) |>
    mutate(paragraph = sprintf("%s.U%d", section_prefix, row_number()))

  if (verbose && nrow(unnumbered) > 0L) {
    cat(sprintf(
      "\nWARNING: %d rec-shaped paragraph(s) NOT in numId='%s'. ",
      nrow(unnumbered), num_id))
    cat(sprintf("Labelled %s.U1..%s.U%d:\n",
                section_prefix, section_prefix, nrow(unnumbered)))
    for (i in seq_len(nrow(unnumbered))) {
      cat(sprintf("  [%s | doc-paragraph idx=%d, numId=%s]\n",
                  unnumbered$paragraph[i], unnumbered$idx[i],
                  unnumbered$nid[i] %||% "<none>"))
      cat(sprintf("    %s\n", str_trunc(unnumbered$text[i], 160)))
    }
  }
  in_list_not_rec <- para_df |> filter(!is.na(nid), nid == num_id, !is_rec)
  if (verbose && nrow(in_list_not_rec) > 0L) {
    cat(sprintf(
      "\nWARNING: %d paragraph(s) in numId='%s' do NOT match rec shape:\n",
      nrow(in_list_not_rec), num_id))
    for (i in seq_len(nrow(in_list_not_rec))) {
      cat(sprintf("  [doc-paragraph idx=%d]: %s\n",
                  in_list_not_rec$idx[i],
                  str_trunc(in_list_not_rec$text[i], 160)))
    }
  }

  combined <- bind_rows(numbered, unnumbered) |> arrange(idx)
  parsed   <- lapply(combined$text, parse_recommendation)
  tibble(
    paragraph            = combined$paragraph,
    recommendation       = str_squish(paste(combined$paragraph, combined$text)),
    recommendation_clean = vapply(parsed, `[[`, character(1), "text"),
    recommending_states  = vapply(parsed, `[[`, character(1), "states"),
    position             = combined$block_position
  )
}

# ---- Format B: literal-prefix (Somalia/Niger-style) --------------------------

.extract_literal_prefix <- function(para_df, rec_paras,
                                    section_prefix, verbose) {
  prefixed <- rec_paras |> filter(has_prefix) |>
    mutate(
      sec = str_match(text, .PREFIX_RE)[, 2L],
      num = as.integer(str_match(text, .PREFIX_RE)[, 3L]),
      # Recommendation text with the "X.Y\t" prefix stripped:
      body = str_trim(str_replace(text, .PREFIX_RE, ""))
    ) |>
    arrange(idx)

  if (is.null(section_prefix)) {
    section_prefix <- names(sort(table(prefixed$sec), decreasing = TRUE))[1L]
    if (verbose && dplyr::n_distinct(prefixed$sec) > 1L) {
      cat(sprintf(
        "Note: literal prefixes use %d different section numbers (%s), ",
        dplyr::n_distinct(prefixed$sec),
        paste(unique(prefixed$sec), collapse = ", ")))
      cat("as expected when recommendations are grouped by position.\n")
    }
  }

  # Sanity checks on the literal numbering (within each section number,
  # since numbering restarts at each lead-in block)
  if (verbose) {
    for (s in unique(prefixed$sec)) {
      nums      <- sort(prefixed$num[prefixed$sec == s])
      expected  <- seq_len(max(nums))
      gaps      <- setdiff(expected, nums)
      dup_count <- sum(duplicated(nums))
      cat(sprintf("Literal prefixes span %s.%d to %s.%d (%d paragraphs).\n",
                  s, min(nums), s, max(nums), length(nums)))
      if (length(gaps) > 0L) {
        cat(sprintf("WARNING: %d gap(s) in %s.N numbering: %s%s\n",
                    length(gaps), s,
                    paste(head(gaps, 10), collapse = ", "),
                    if (length(gaps) > 10L) ", ..." else ""))
      }
      if (dup_count > 0L) {
        cat(sprintf("WARNING: %d duplicate paragraph number(s) in %s.N.\n",
                    dup_count, s))
      }
    }
  }

  # Anything rec-shaped without a literal prefix? (shouldn't happen, but
  # worth flagging if it does)
  unprefixed <- rec_paras |> filter(!has_prefix)
  if (verbose && nrow(unprefixed) > 0L) {
    cat(sprintf(
      "\nWARNING: %d rec-shaped paragraph(s) lack a literal '%s.N' prefix:\n",
      nrow(unprefixed), section_prefix))
    for (i in seq_len(nrow(unprefixed))) {
      cat(sprintf("  [doc-paragraph idx=%d]: %s\n",
                  unprefixed$idx[i],
                  str_trunc(unprefixed$text[i], 160)))
    }
  }

  parsed <- lapply(prefixed$body, parse_recommendation)
  labels <- sprintf("%s.%d", prefixed$sec, prefixed$num)
  tibble(
    paragraph            = labels,
    recommendation       = str_squish(paste(labels, prefixed$body)),
    recommendation_clean = vapply(parsed, `[[`, character(1), "text"),
    recommending_states  = vapply(parsed, `[[`, character(1), "states"),
    position             = prefixed$block_position
  )
}

# ---- Format C: sub-numbered (cycle-1 style, e.g. A/HRC/12/2) ------------------
# In the earliest UPR reports the recommendations are numbered "1.", "2.",
# ... restarting inside each lead-in paragraph ("74. The recommendations
# ... enjoy its support:"), so both lead-ins and recommendations start with
# a top-level "N." -- this extractor tells them apart by whether
# classify_leadin() recognizes the paragraph. Labels become
# "<lead-in number>.<sub-number>" (74.1, 74.2, ...).
#
# Recommending states are often attributed inline ("Consider ratifying
# (Turkey) / Ratify (Mexico) the Optional Protocol ..."), and parentheses
# are also used for treaty acronyms ("(CEDAW)", "(OP-CAT)") and list
# markers ("(a)"), so a parenthetical only counts as a recommending state
# when it starts with an upper-case letter followed by a lower-case one
# ("Turkey", "United Kingdom" -- but not "CAT", "IDPs" or "(a)").

.extract_subnumbered <- function(para_df, verbose = TRUE) {
  df <- para_df |>
    mutate(
      top_num    = str_match(text, .LEADIN_RE)[, 2L],
      leadin_pos = classify_leadin(text),
      is_leadin  = !is.na(top_num) & !is.na(leadin_pos),
      block      = cumsum(is_leadin)
    )

  blocks <- df |>
    filter(is_leadin) |>
    transmute(
      block,
      block_prefix   = top_num,
      block_position = leadin_pos,
      leadin_text    = text
    )
  if (nrow(blocks) == 0L) {
    stop("sub-numbered mode, but no lead-in paragraph ('The recommendations ",
         "... enjoy the support of X / will be examined by X ...') was ",
         "found. Try forcing another mode.")
  }

  # Same NA rule as the other report formats
  has_response <- any(blocks$block_position %in% c("Supported", "Noted"))
  if (!has_response) blocks$block_position <- NA_character_

  if (verbose) {
    cat("Lead-in paragraphs found:\n")
    for (i in seq_len(nrow(blocks))) {
      cat(sprintf("  [%s.] %s -> %s\n",
                  blocks$block_prefix[i],
                  str_trunc(blocks$leadin_text[i], 90),
                  blocks$block_position[i] %||% "NA (no positions given yet)"))
    }
  }

  recs <- df |>
    filter(block > 0L, !is_leadin, !is.na(top_num)) |>
    left_join(select(blocks, block, block_prefix, block_position),
              by = "block") |>
    mutate(
      sub_num   = as.integer(top_num),
      body      = str_trim(str_replace(text, .LEADIN_RE, "")),
      paragraph = sprintf("%s.%d", block_prefix, sub_num)
    ) |>
    arrange(idx)
  if (nrow(recs) == 0L) {
    stop("sub-numbered mode: no numbered recommendation paragraphs found ",
         "after the lead-in(s).")
  }

  stray <- df |> filter(block > 0L, !is_leadin, is.na(top_num))
  if (verbose && nrow(stray) > 0L) {
    cat(sprintf(
      "\nWARNING: %d unnumbered paragraph(s) inside the recommendation blocks were skipped:\n",
      nrow(stray)))
    for (i in seq_len(nrow(stray))) {
      cat(sprintf("  [doc-paragraph idx=%d]: %s\n",
                  stray$idx[i], str_trunc(stray$text[i], 160)))
    }
  }

  # Numbering sanity: sub-numbers should run 1..n within each block
  if (verbose) {
    for (b in unique(recs$block)) {
      nums <- sort(recs$sub_num[recs$block == b])
      pref <- recs$block_prefix[recs$block == b][1L]
      gaps <- setdiff(seq_len(max(nums)), nums)
      cat(sprintf("Sub-numbers span %s.%d to %s.%d (%d paragraphs).\n",
                  pref, min(nums), pref, max(nums), length(nums)))
      if (length(gaps) > 0L) {
        cat(sprintf("WARNING: %d gap(s) in %s.N numbering: %s%s\n",
                    length(gaps), pref,
                    paste(head(gaps, 10), collapse = ", "),
                    if (length(gaps) > 10L) ", ..." else ""))
      }
      if (anyDuplicated(nums) > 0L) {
        cat(sprintf("WARNING: duplicate sub-number(s) under lead-in %s.\n",
                    pref))
      }
    }
  }

  parsed <- lapply(recs$body, parse_recommendation)
  # Recommending states = every plausible-state parenthetical in the body,
  # covering inline "X (Turkey) / Y (Mexico)" attributions and trailing
  # "(State); (State);" lists alike
  states <- vapply(recs$body, function(b) {
    grp <- str_extract_all(b, .STATE_RE)[[1]]
    grp <- str_trim(str_sub(grp, 2L, -2L))
    grp <- unique(grp[str_detect(grp, "^[A-Z][a-z]")])
    if (length(grp) == 0L) NA_character_ else paste(grp, collapse = "; ")
  }, character(1), USE.NAMES = FALSE)

  tibble(
    paragraph            = recs$paragraph,
    recommendation       = str_squish(paste(recs$paragraph, recs$body)),
    recommendation_clean = vapply(parsed, `[[`, character(1), "text"),
    recommending_states  = states,
    position             = recs$block_position
  )
}

# ---- User-facing wrapper ------------------------------------------------------
#
# Extracts ONE document, adds the caller-supplied metadata, and saves the
# result as "<state_under_review>_<upr_session>.rds" in output_dir.
# Returns the tibble invisibly.

extract_upr_recs <- function(
    input,
    state_under_review,
    document_symbol,
    upr_session,
    provisional,
    output_dir     = here("data", "UPR_WG_docs", "extracted_recs"),
    mode           = c("auto", "auto-numbered", "literal-prefix", "sub-numbered"),
    num_id         = NULL,
    section_prefix = NULL,
    verbose        = TRUE
) {
  stopifnot(
    is.character(state_under_review), length(state_under_review) == 1L,
    length(document_symbol) == 1L,
    length(upr_session) == 1L,
    is.logical(provisional), length(provisional) == 1L, !is.na(provisional)
  )

  recs <- extract_upr_recommendations(
    input,
    mode           = mode,
    num_id         = num_id,
    section_prefix = section_prefix,
    verbose        = verbose
  )

  out <- recs |>
    mutate(
      state_under_review = .env$state_under_review,
      document_symbol    = .env$document_symbol,
      upr_session        = .env$upr_session,
      provisional        = .env$provisional
    ) |>
    select(state_under_review, recommendation, recommendation_clean,
           paragraph, recommending_states, position,
           document_symbol, upr_session, provisional)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  out_file <- file.path(output_dir,
                        paste0(state_under_review, "_", upr_session, ".rds"))
  saveRDS(out, out_file)
  if (verbose) cat("Saved:", out_file, "\n")

  invisible(out)
}

# ---- Run --------------------------------------------------------------------

# if (sys.nframe() == 0) {
#   # Draft (provisional) WG report circulated after the review
#   extract_upr_recs(
#     here("data", "UPR_WG_docs",
#          "Somalia - Draft report circulated on 12 May 2026_original.docx"),
#     state_under_review = "Somalia",
#     document_symbol    = "A/HRC/63/12",
#     upr_session        = 52,
#     provisional        = TRUE
#   )
# 
#   # OHCHR matrices of recommendations (final positions), cycle 2 and 3.
#   # The cycle-2 matrix is a legacy .doc, converted via Microsoft Word.
#   cat("\n")
#   extract_upr_recs(
#     "https://www.ohchr.org/sites/default/files/lib-docs/HRBodies/UPR/Documents/Session20/EG/EgyptMatriceRecommendations.doc",
#     state_under_review = "Egypt",
#     document_symbol    = "A/HRC/28/16",
#     upr_session        = 20,
#     provisional        = FALSE
#   )
# 
#   cat("\n")
#   extract_upr_recs(
#     "https://www.ohchr.org/sites/default/files/lib-docs/HRBodies/UPR/Documents/Session34/EG/UPR34_Egypt_Thematic_list_of_Recommendations.docx",
#     state_under_review = "Egypt",
#     document_symbol    = "A/HRC/43/16",
#     upr_session        = 34,
#     provisional        = FALSE
#   )
# 
#   # A final adopted report fetched as a PDF also works (this one would
#   # save to the same Egypt_20.rds as the cycle-2 matrix above, so it is
#   # left commented out):
#   # extract_upr_recs(
#   #   "https://docs.un.org/en/A/HRC/28/16",
#   #   state_under_review = "Egypt",
#   #   document_symbol    = "A/HRC/28/16",
#   #   upr_session        = 20,
#   #   provisional        = FALSE
#   # )
# }
