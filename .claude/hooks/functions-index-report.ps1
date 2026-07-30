# SessionStart / PostCompact hook: inject the FUNCTIONS.md drift report into
# context, so the map of R/ is known to be accurate (or known to be stale)
# without anyone having to remember what changed.
#
# SessionStart covers a fresh session; PostCompact covers the case where a long
# session is summarized and the record of this session's own edits is lost.
#
# Runs the deterministic R generator only - no model, no cost beyond ~3s.
# Any failure exits 0 silently: a broken hook must never block a session start.
#
# ASCII only. Windows PowerShell 5.1 reads .ps1 as ANSI unless the file has a
# BOM, so a stray non-ASCII character here becomes a parse error.

$eventName = "SessionStart"
try {
    $raw = [Console]::In.ReadToEnd()
    if (-not [string]::IsNullOrWhiteSpace($raw)) {
        $payload = $raw | ConvertFrom-Json
        if ($payload.hook_event_name) { $eventName = [string]$payload.hook_event_name }
    }
} catch {
    # fall through with the default event name
}

$root    = "C:\Users\kdgc151\OneDrive - AZCollaboration\CovariateSearcher\CovariateSearcher"
$rscript = "C:\Users\kdgc151\AppData\Local\Programs\R\R-4.4.2\bin\Rscript.exe"
$script  = Join-Path $root ".claude\scripts\functions-index.R"

if (-not (Test-Path $rscript) -or -not (Test-Path $script)) { exit 0 }

try {
    $report = (& $rscript $script $root | Out-String)
} catch {
    exit 0
}
if ([string]::IsNullOrWhiteSpace($report)) { exit 0 }

# A large refactor can produce a very long report; cap what gets injected.
$limit = 8000
if ($report.Length -gt $limit) {
    $note = "`n[report truncated at " + $limit + " chars - run the script directly for the rest]`n"
    $report = $report.Substring(0, $limit) + $note
}

$header = @"
FUNCTIONS.md status - produced just now by .claude/scripts/functions-index.R.
This is measured from the source, not recalled from memory.

FUNCTIONS.md maps every function in R/ (signature, purpose, side effects, call
graph). Consult it before writing a new helper - reimplementing something that
already exists is the most common avoidable mistake in this package.

Any function listed under MISSING_FROM_DOC, STALE_IN_DOC, SIGNATURE_DRIFT or
BODY_CHANGED is NOT accurately described in the doc. Read its source directly,
and run the functions-index agent to bring the doc back in sync. Everything not
listed below was verified accurate as of the recorded baseline.
"@

$context = $header + "`n" + $report

$out = @{
    hookSpecificOutput = @{
        hookEventName     = $eventName
        additionalContext = $context
    }
} | ConvertTo-Json -Depth 5 -Compress

Write-Output $out
exit 0
