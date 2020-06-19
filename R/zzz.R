# Unit types, e.g. 'mm', 'inch', 'npc' etc. are stored as character attribute in
# R <= 3.6.3, but as integer attributes in R >= 4.0.0 and have a dedicated reader
# function. To get unit operations to work in both versions, we'll need to branch
# the unit type getter
unit_type <- NULL
.onLoad <- function(libname, pkgname) {
    if (as.numeric(version$major) >= 4) {
        unit_type <- getFromNamespace("unitType", "grid")
    } else {
        unit_type <- function(x) {attr(x, "unit", exact = TRUE)}
    }
    utils::assignInMyNamespace("unit_type", unit_type)
}
