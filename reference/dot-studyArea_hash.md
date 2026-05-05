# Stable short hash of a study-area object

Returns a short hexadecimal hash (`xxhash64`) of the serialized object,
used to namespace climate caches so that distinct study areas don't
collide. Two study-area objects that are equal under
[`identical()`](https://rdrr.io/r/base/identical.html) return the same
hash; otherwise the hashes differ.

## Usage

``` r
.studyArea_hash(studyArea)
```

## Arguments

- studyArea:

  any R object — typically an `sf` polygons object.

## Value

character scalar — a 16-character hex hash.
