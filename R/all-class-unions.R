setClassUnion(
  "numeric_OR_missing",
  c("numeric", "missing")
)

setClassUnion(
  "character_OR_NULL",
  c("character", "NULL")
)

setClassUnion(
  "missing_OR_NULL", 
  c("NULL", "missing")
)

setClassUnion(
  "knownDiscretes",
  c("logical", "character", "factor", "ordered")
)

setClassUnion(
  "knownContinuous",
  c("integer", "double", "numeric", "complex", "POSIXt", "Date", "dist")
)

setClassUnion(
  "ANYGenomic",
  c("GenomicRanges", "GRangesFactor")
)