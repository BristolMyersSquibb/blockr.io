# Test Coverage Audit for blockr.io

## Purpose
Ensure ALL block arguments are comprehensively tested with testServer.

## write_block Arguments

### Constructor Parameters

| Parameter | Values | Status | Test Location |
|-----------|--------|--------|---------------|
| `directory` | non-empty path | ✅ TESTED | All tests use temp directories |
| `directory` | empty (use default) | ❌ MISSING | - |
| `filename` | non-empty | ✅ TESTED | Multiple tests |
| `filename` | empty (auto-timestamp) | ✅ TESTED | test-write-block-server.R:297 |
| `format` | "csv" | ✅ TESTED | Multiple tests |
| `format` | "excel" | ✅ TESTED | test-write-block-server.R:47, 412 |
| `format` | "parquet" | ✅ TESTED | test-write-block-server.R:171 |
| `format` | "feather" | ✅ TESTED | test-write-block-server.R:453 |
| `mode` | "browse" | ✅ TESTED | All tests use browse mode |
| `mode` | "download" | ❌ MISSING | - |
| `auto_write` | TRUE | ✅ TESTED | Multiple tests (implicit default) |
| `auto_write` | FALSE | ✅ TESTED | test-write-block-server.R:1, 44 |

### CSV args Parameter

| Arg | Status | Test Location |
|-----|--------|---------------|
| `sep` / `delim` | ✅ TESTED | test-write-block-server.R:92 |
| `quote` | ❌ MISSING | - |
| `na` | ❌ MISSING | - |
| `append` | ❌ MISSING | - |
| `col_names` | ❌ MISSING | - |

### Excel args Parameter

| Arg | Status | Test Location |
|-----|--------|---------------|
| (basic multi-sheet) | ✅ TESTED | test-write-block-server.R:47 |
| (single sheet) | ✅ TESTED | test-write-block-server.R:412 |
| Format options | ❌ MISSING | - |

### Parquet/Feather args

| Arg | Status | Test Location |
|-----|--------|---------------|
| Basic write | ✅ TESTED | test-write-block-server.R:171, 453 |
| Compression | ❌ MISSING | - |
| Other options | ❌ MISSING | - |

### UI Interactions

| Interaction | Status | Test Location |
|-------------|--------|---------------|
| Submit button click (auto_write=FALSE) | ✅ TESTED | test-write-block-server.R:44 |
| Mode pills change | ⚠️ PARTIAL | test-write-block-server.R:333 (only state check) |
| Directory selection (shinyFiles) | ❌ MISSING | - |
| Format dropdown change | ❌ MISSING | - |
| Advanced options toggle | ❌ MISSING | - |

---

## read_block Arguments

### Constructor Parameters

| Parameter | Values | Status | Test Location |
|-----------|--------|--------|---------------|
| `path` | single file | ✅ TESTED | Multiple tests |
| `path` | multiple files | ✅ TESTED | test-read-block-server.R:45 |
| `path` | empty | ❌ MISSING | - |
| `source` | "upload" | ⚠️ IMPLIED | Default but not explicitly tested |
| `source` | "path" | ✅ TESTED | Implied by path-based tests |
| `source` | "url" | ❌ MISSING | - |
| `combine` | "auto" | ⚠️ IMPLIED | Default but not explicit |
| `combine` | "rbind" | ✅ TESTED | test-read-block-server.R:45 |
| `combine` | "cbind" | ❌ MISSING | - |
| `combine` | "first" | ❌ MISSING | - |

### CSV args Parameter

| Arg | Status | Test Location |
|-----|--------|---------------|
| `sep` / `delim` | ✅ TESTED | test-read-block-server.R:92, 233 |
| `skip` | ✅ TESTED | test-read-block-server.R:171 (in state check) |
| `n_max` | ❌ MISSING | - |
| `col_names` | ❌ MISSING | - |
| `col_types` | ❌ MISSING | - |
| `na` | ❌ MISSING | - |
| `quote` | ❌ MISSING | - |

### Excel args Parameter

| Arg | Status | Test Location |
|-----|--------|---------------|
| `sheet` | ✅ TESTED | test-read-block-server.R:135 |
| `range` | ❌ MISSING | - |
| `col_names` | ❌ MISSING | - |
| `col_types` | ❌ MISSING | - |
| `skip` | ❌ MISSING | - |
| `n_max` | ❌ MISSING | - |

### Other Formats

| Format | Status | Test Location |
|--------|--------|---------------|
| TSV | ❌ MISSING | - |
| Parquet | ❌ MISSING | - |
| Feather | ❌ MISSING | - |
| RDS | ❌ MISSING | - |

### UI Interactions

| Interaction | Status | Test Location |
|-------------|--------|---------------|
| csv_sep dropdown change | ✅ TESTED | test-read-block-server.R:233 |
| Tab switching (upload/path/url) | ❌ MISSING | - |
| File browser selection (shinyFiles) | ❌ MISSING | - |
| Upload file input | ❌ MISSING | - |
| URL input change | ❌ MISSING | - |
| combine method change | ❌ MISSING | - |
| Advanced options toggle | ❌ MISSING | - |

---

## Priority: HIGH (Must Add)

### write_block
1. **mode = "download"** - Critical parameter completely untested
2. CSV args: quote, na, col_names
3. UI: Format dropdown change
4. UI: Mode pills interaction (full test, not just state)

### read_block
1. **source = "url"** - Critical parameter completely untested
2. **combine = "cbind"** - Important combine method
3. **combine = "first"** - Important combine method
4. CSV args: n_max, col_names, na
5. Excel args: range, skip, n_max
6. Other formats: TSV, Parquet, Feather, RDS
7. UI: Tab switching between upload/path/url
8. UI: combine method dropdown

## Priority: MEDIUM (Should Add)

### write_block
1. directory = "" (default directory)
2. Parquet/Feather compression options
3. UI: Advanced options toggle
4. ZIP creation with multiple formats

### read_block
1. path = character() (empty)
2. Explicit source = "upload" test
3. col_types for CSV/Excel
4. UI: Advanced options toggle

## Priority: LOW (Nice to Have)

### Both
1. shinyFiles browser interactions (complex, may need integration test)
2. File upload widget (fileInput - may need browser)
3. Error handling for invalid inputs
4. Edge cases (missing files, malformed data, etc.)

---

## Next Steps

1. Add HIGH priority tests first
2. Document any discoveries about testServer limitations
3. Update this audit as tests are added
4. Create comprehensive test patterns document
