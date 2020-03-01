setClassUnion(
  "missing_OR_NULL", 
  c("NULL", "missing")
)

setClassUnion(
  "numeric_OR_missing",
  c("numeric", "missing_OR_NULL")
)

setClassUnion(
  "character_OR_NULL",
  c("character", "NULL")
)

setClassUnion(
  "knownDiscretes",
  c("logical", "character", "factor", "ordered")
)

setClassUnion(
  "knownDiscretes_OR_missing",
  c("knownDiscretes", "missing_OR_NULL")
)

setClassUnion(
  "knownContinuous",
  c("integer", "double", "numeric", "complex", "POSIXt", "Date", "dist")
)

setClassUnion(
  "ANYGenomic",
  c("GenomicRanges", "GRangesFactor")
)

setClassUnion(
  "GRanges_OR_missing",
  c("GRanges", "missing", "missing_OR_NULL")
)

setClassUnion(
  "WoodenHorse",
  c("BeechHorse", "OakHorse")
)

