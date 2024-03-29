# UNF 2.0.5

- Exported new function `unf_equal()` to better comply with R CMD check.
- Converted package to use roxygen documentation.
- Improved implementation of raw vector handling.
- Expanded test suite to cover raw, AsIs, and time-series vectors. (#17)

# UNF 2.0.4

- Added a `unfmatch` element to the return value of `%unf%`.
- Expanded test suite to cover `%unf%`. (#17)

# UNF UNF 2.0.3

- Expanded test suite to cover `as.unfvector()`. (#2)

# UNF 2.0.2

- Allow `%unf%` to be used to compare against a character string containing a UNF signature (#16, h/t Dominik Vogel)
- Converted `unf()` to an S3 generic, with matrix, list, and data.frame methods.
- Exposed new function `as.unfvector()`, which coerces data types to UNF character vector specifications. (#15)
- Added base package imports to NAMESPACE. (#14)

# UNF 2.0.1

- Fix error in datetime tests due to use `%F` (not supported on Windows for R3.0.3). (#13)

# UNF 2.0

- Support UNF version 6. (#2)
- Entirely rewrite codebase in R using `digest`.

# UNF 1.02

- Standardizes representation of non-finite numbers as : +inf, -inf, +nan
- Allows for separate specification of rounding levels for character and numeric
  vectors
- Composite unfs are sorted by unf value which disambiguates ordering
- Discovered that sprintf() internal rounding behavior is inconsistent.
  Now use explicit rounding using IEE 753 "round to nearest".
- UNF Version 4, replaces MD5 with SHA256 algorithm
- check with codetools()
- remove -static (request from Brian Ripley)

# UNF 1.04 
- Set locale for sort order
- Add vignette
- Move citation

# UNF 1.05
- fix bug in as.unf

# UNF 1.06
- workaround for lack of posix locale under windows

# UNF 1.07
- Protocol v. 4.1, perf, as.unf bug

# UNF 1.08
- code comments 

# UNF 1.09
- fix prototyping bug, add other languages

# UNF 1.10
- fix suppression of digits for default under v3

# UNF 1.11
- fix non C99 #includes

# UNF 1.13
- inital changes for Splus compatability

# UNF 1.14
- Namespace imports, other  SPLUS changes

# UNF 1.15
- Splus for linux debug
