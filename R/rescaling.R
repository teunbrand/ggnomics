setGeneric(
    "S4Rescale",
    function(x, to = c(0, 1), from = S4Range(x, na.rm = TRUE, finite = TRUE),
             aes = "z") standardGeneric("S4Rescale"),
    signature = c("x", "to", "from")
)

setMethod(
    "S4Rescale",
    signature = c(x = "knownContinuous"),
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE),
             aes = "z") {
        scales::rescale(x, to = to, from = from)
    }
)

setMethod(
    "S4Rescale",
    signature = c(x = "NULL"),
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE),
             aes = "z") {
        NULL
    }
)

setMethod(
    "S4Rescale",
    signature = c(x = "WoodenHorse"),
    function(x, to = c(0, 1), from = S4Range(x, na.rm = TRUE, finite = TRUE)) {
        x <- Nightfall(x)
        x <- callGeneric(x, to = to, from = from)
        x
    }
)

## TODO: Shouldn't actually be this, but seems to work for now
setMethod(
    "S4Rescale",
    signature = c(x = "GenomicRanges", to = "numeric", from = "GenomicRanges"),
    function(x, to = c(0, 1), from = S4Range(x, na.rm = TRUE, finite = TRUE)) {
        S4ForceFlat(x, limits = from)
    }
)

setMethod(
    "S4Rescale",
    signature = c(x = "Rle", to = "numeric_OR_missing", from = "numeric"),
    function(x, to = c(0, 1), from = S4Range(x, na.rm = TRUE, finite = TRUE)) {
        runValue(x) <- scales::rescale(runValue(x), to = to, from = from)
        as.vector(x)
    }
)
