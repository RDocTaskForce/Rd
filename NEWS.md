# Rd 0.2.0

* Improved canonization
* Added Rd_compact for compacting results of a map/apply function.
* Added `testthat::compare` methods for Rd and Rd_tag
* Added conversion Rd_tag to Rd
* Added convenience functions:
    + Rd_list
    + Rd_enumerate
    + Rd_itemize
* Removed support for tag indexing in `[[` methods
* bug fixes:
    + new line checking

# Rd 0.1.0

* Initial Release
* Added a `NEWS.md` file to track changes to the package.
* Includes 
    + creation functions
    + S3 extensions 
        - `[[`
        - `[`
        - format
