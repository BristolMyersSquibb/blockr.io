# Is this path one the app itself manages?

Uploads and URL downloads land in the app's own sandboxes, so the
deployment file-access policy (which governs paths the USER chooses)
does not apply to them.

## Usage

``` r
in_app_sandbox(path, upload_path)
```
