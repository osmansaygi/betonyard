$content = Get-Content -Path "c:\Users\BEYKENT\Documents\CURSOR\DENEME_OSM\ST4_Aks_Ciz.lsp" -Raw
$depth = 0
$line = 1
$col = 0
$inComment = $false
$inString = $false
$escapeNext = $false
$openStack = @()
$rawOpen = 0
$rawClose = 0

for ($i = 0; $i -lt $content.Length; $i++) {
    $c = $content[$i]
    if ($c -eq "`n") {
        $line++
        $col = 0
        $inComment = $false
        continue
    }
    $col++

    if ($escapeNext) {
        $escapeNext = $false
        continue
    }
    if ($inComment) { continue }
    if ($inString) {
        if ($c -eq '\') { $escapeNext = $true }
        elseif ($c -eq '"') { $inString = $false }
        continue
    }
    if ($c -eq ';') {
        $inComment = $true
        continue
    }
    if ($c -eq '"') {
        $inString = $true
        continue
    }
    if ($c -eq '(') {
        $depth++
        $openStack += [PSCustomObject]@{Line=$line; Col=$col}
    }
    elseif ($c -eq ')') {
        $depth--
        if ($openStack.Count -gt 0) { $openStack = $openStack[0..($openStack.Count-2)] }
        if ($depth -lt 0) {
            Write-Host "Extra ) at line $line, column $col"
            exit 0
        }
    }
}

# Raw count (all chars, no skip)
$rawOpen = ([regex]::Matches($content, '\(')).Count
$rawClose = ([regex]::Matches($content, '\)')).Count
Write-Host "Raw counts: $rawOpen open, $rawClose close (diff: $($rawOpen - $rawClose))"

if ($depth -gt 0) {
    Write-Host "Extra ( - $depth unclosed (excluding comments/strings)."
    Write-Host "Unclosed ( positions (oldest first):"
    $start = [Math]::Max(0, $openStack.Count - $depth)
    for ($j = $start; $j -lt $openStack.Count; $j++) {
        Write-Host "  Line $($openStack[$j].Line), col $($openStack[$j].Col)"
    }
    Write-Host "Missing ) should close the form that starts at line $($openStack[-1].Line), column $($openStack[-1].Col)."
}
