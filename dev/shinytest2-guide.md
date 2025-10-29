# shinytest2 Tests - Fully Migrated to testServer ✅

## Migration Complete

**All shinytest2 tests have been successfully migrated to testServer.**

### Files Deleted (2025-01)

The following shinytest2 files were deleted after confirming equivalent testServer coverage exists:

- ✅ `test-shinytest2-read-block-integration.R` → Covered by test-read-block-server.R
- ✅ `test-shinytest2-write-block-integration.R` → Covered by test-write-block-server.R
- ✅ `helper-shinytest2.R` → No longer needed

**Total deleted:** ~300 lines of slow browser-based tests + helper infrastructure

## Why We Migrated

1. **Speed**: testServer is 20-50x faster than shinytest2
2. **Simplicity**: No browser setup (chromote) required
3. **Debugging**: Direct access to server state and reactives
4. **Coverage**: testServer can test all Shiny logic, including UI interactions via `session$setInputs()`

## Current Testing Strategy

**Two-tier approach:**

1. **Unit Tests** - Pure R functions (expression builders, helpers)
2. **testServer** - ALL Shiny reactivity and UI interactions

**No shinytest2** - It's almost never needed. See [testing-guide.md](testing-guide.md) for details.

## If You Think You Need shinytest2

**Stop and ask yourself:**
- Can I test this with `session$setInputs()` in testServer? → Almost always YES
- Am I testing logic or just visual UI layout? → Logic = testServer
- Do I need a browser? → Almost always NO

**See [testing-guide.md](testing-guide.md) for the complete decision tree.**

## Testing Approach

**blockr.io uses a two-tier testing strategy:**
1. **Unit tests** for pure R functions (expression builders, file format helpers)
2. **testServer** for ALL Shiny logic and UI interactions

All scenarios previously tested with shinytest2 are now covered by faster, simpler testServer tests.

## Key Learnings

**Misconception:** "UI button clicks need shinytest2 with a browser"
**Reality:** `session$setInputs(button_id = 1)` in testServer works perfectly

**Misconception:** "Changing input values in UI needs shinytest2"
**Reality:** `session$setInputs(input_id = new_value)` in testServer handles this

**Critical discovery:** Input IDs may be namespaced. For blockr blocks:
- Button in expr module: Use `expr-button_name` not just `button_name`
- Example: `session$setInputs(\`expr-submit_write\` = 1)`
