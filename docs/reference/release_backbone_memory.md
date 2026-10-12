# Forget the in-memory backbone

Drops the process-wide copy held by \[load_backbone_cache()\]. The memo
invalidates itself when the cache file changes, so this is only needed
to hand the memory back — after a long matching session, say.

## Usage

``` r
release_backbone_memory()
```

## Value

Invisibly TRUE if a copy was held, FALSE otherwise.
