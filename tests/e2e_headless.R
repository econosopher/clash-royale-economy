#!/usr/bin/env Rscript

suppressWarnings(suppressMessages({
  library(jsonlite)
  library(processx)
  library(chromote)
}))

ok <- TRUE
fail <- function(msg){
  cat("[fail]", msg, "\n"); ok <<- FALSE
}
pass <- function(msg){
  cat("[ok]", msg, "\n")
}

url <- "http://127.0.0.1:7789/"

cat("== E2E Headless Checks (chromote) ==\n")

# Launch app
cat("Starting app...\n")
p <- processx::process$new("Rscript", c("app.R"), stdout = "|", stderr = "|")
on.exit({ if (p$is_alive()) p$kill() }, add = TRUE)

# Wait for port
ready <- FALSE
for (i in 1:60) {
  con <- try(url(url), silent = TRUE)
  if (!inherits(con, "try-error")) { close(con); ready <- TRUE; break } else Sys.sleep(1)
}
if (!ready) stop("App did not start in time")

session <- ChromoteSession$new()
session$Page$navigate(url)

# Wait for DOM ready
for (i in 1:200) {
  res <- try(session$Runtime$evaluate("document.readyState"), silent = TRUE)
  if (!inherits(res, "try-error")) {
    state <- tryCatch(res$result$value, error = function(e) NULL)
    if (is.character(state) && identical(state, "complete")) break
  }
  Sys.sleep(0.1)
}
Sys.sleep(0.3)

# Install simple browser error collector
session$Runtime$evaluate(
  "window.__errors__ = []; window.addEventListener('error', e => { try { window.__errors__.push(String(e.error?.stack || e.message || e)); } catch(_){} });"
)

# Helper: wait for JS predicate to be truthy
wait_js <- function(expr, timeout_ms = 15000, interval_ms = 100) {
  t0 <- as.numeric(Sys.time())
  repeat {
    res <- try(session$Runtime$evaluate(sprintf("(function(){try{return !!(%s)}catch(e){return false}})()", expr)), silent = TRUE)
    if (!inherits(res, "try-error")) {
      v <- tryCatch(isTRUE(res$result$value), error = function(e) FALSE)
      if (isTRUE(v)) return(TRUE)
    }
    if ((as.numeric(Sys.time()) - t0) * 1000 > timeout_ms) return(FALSE)
    Sys.sleep(interval_ms/1000)
  }
}

# Helper: assert and log
assert_js <- function(expr, label, timeout_ms = 15000) {
  if (wait_js(expr, timeout_ms = timeout_ms)) pass(label) else fail(paste0(label, " (expr: ", expr, ")"))
}

# Helper: assert no browser errors so far
assert_no_errors <- function(label){
  res <- session$Runtime$evaluate("Array.isArray(window.__errors__) ? window.__errors__.length : 0")
  n <- tryCatch(as.integer(res$result$value), error = function(e) 999L)
  if (!is.na(n) && n == 0) pass(paste0(label, ": no console errors"))
  else {
    txt <- session$Runtime$evaluate("(window.__errors__||[]).slice(-3).join('\n')")
    msg <- tryCatch(as.character(txt$result$value), error=function(e) "(unable to read)")
    fail(paste0(label, ": console errors detected\n", msg))
  }
}

# 1) Daily Table populated
session$Runtime$evaluate("Shiny.setInputValue('tabs','Daily Table', {priority: 'event'})")
assert_js("document.querySelectorAll('#tbl_daily table tbody tr').length > 0", "Daily Table has rows")
assert_no_errors("Daily Table")

# 2) Visualizations: check key metrics rendered (robust across devices)
session$Runtime$evaluate("Shiny.setInputValue('tabs','Visualizations', {priority: 'event'})")
assert_js("(function(){try{const t=JSON.parse(document.getElementById('e2e_beacon')?.innerText||'{}'); return t.has_daily===true && (t.daily_rows||0) > 0;}catch(e){return false}})()", "Beacon: daily data ready")
assert_js("(function(){const el=document.getElementById('metric_trophies'); if(!el) return false; const t=el.innerText||''; return /^[-0-9,]+$/.test(t) && t.length>0;})()", "Metric trophies rendered")
assert_js("(function(){const el=document.getElementById('metric_gold'); if(!el) return false; const t=el.innerText||''; return /^[-0-9,]+$/.test(t) && t.length>0;})()", "Metric gold rendered")
assert_js("(function(){const el=document.getElementById('metric_winrate'); if(!el) return false; const t=el.innerText||''; return /%$/.test(t) && t.length>0;})()", "Metric winrate rendered")
assert_no_errors("Visualizations")

# 3) Compare: run for Defaults and expect plot after recompute
session$Runtime$evaluate("Shiny.setInputValue('tabs','Compare', {priority: 'event'})")
session$Runtime$evaluate("Shiny.setInputValue('cmp_select',['Defaults'], {priority: 'event'})")
session$Runtime$evaluate("document.getElementById('cmp_run')?.click()")
assert_js("(function(){try{const t=JSON.parse(document.getElementById('e2e_beacon')?.innerText||'{}'); return t.cmp_ready===true;}catch(e){return false}})()", "Beacon: compare ready", timeout_ms = 30000)
assert_js("(function(){const el=document.querySelector('#cmp_upg_summary'); if(!el) return false; if (el.classList.contains('recalculating')) return false; return document.querySelectorAll('#cmp_upg_summary table tbody tr').length > 0;})()", "Compare summary table rendered", timeout_ms = 30000)
assert_no_errors("Compare")

# 4) Data Editor table loads
session$Runtime$evaluate("Shiny.setInputValue('tabs','Data Editor', {priority: 'event'})")
assert_js("(function(){const el=document.querySelector('#edit_table_view'); if(!el) return false; if (el.classList.contains('recalculating')) return false; return document.querySelectorAll('#edit_table_view table tbody tr').length > 0;})()", "Data Editor has rows", timeout_ms = 20000)
assert_no_errors("Data Editor")

# 5) Assumptions form present
session$Runtime$evaluate("Shiny.setInputValue('tabs','Assumptions Editor', {priority: 'event'})")
assert_js("!!document.getElementById('cfg_matches')", "Assumptions form present")
assert_no_errors("Assumptions Editor")

cat("\n== E2E Summary ==\n")
cat(if (ok) "All E2E checks passed.\n" else "Failures detected — see details above.\n")
quit(save = "no", status = if (ok) 0 else 1)
