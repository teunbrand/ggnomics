# ggnomics 0.1.2

## Major changes

* Started with a clean slate, removed previous ggnomics content.
* Previous functionality unrelated to genomics now lives in the 
  [ggh4x](https://github.com/teunbrand/ggh4x) GitHub.

## Plotting

* Added fortify methods for `S4Vectors::DataFrame` and `S4Vectors::Vector` 
  classes, preserving S4 classes where possible. 
* Added `ggplot()` method for `S4Vectors::Vector` class, which automatically 
  adds `coord_S4()`.
  
### Geoms

* Added `geom_range()`, which is effectively `geom_rect()`, but can be 
  parameterised by `IRanges::Ranges`-class for the x/y aesthetic instead of
  the xmin/xmax and ymin/ymax parameterisation.
* Added `geom_genemodel()` to display exon/intron structure of genes. 
  Controlling the thickness of CDSs vs UTRs can be done 
  using `scale_exontype_discrete()`.
  
### Stats

* Added `stat_coverage()` to calculate how often a space is covered by a set of
  ranges. Treats `IRanges::Ranges`-class as closed-interval integers and doubles
  as open-interval.
  
### Positions

* Added `position_disjoint_ranges()` to segregate overlapping ranges. Works 
  similar to `IRanges::disjointBins()`, but doesn't require integer ranges.
  
### Scales

* Added tree of 'sister' classes for ggplot's discrete and continuous scales. 
  It is the task of a scale to take S4 classes as input and wrap them inside an 
  S3 representation and expose the correct training (range updating).
* Added `scale_(x/y)_S4_continuous()` to handle continuous S4-class positions.
* Added `scale_(x/y)_S4_discrete()` to handle discrete S4-class positions.
* Added `scale_(colour/fill)_S4_continuous()` and 
  `scale_(colour/fill)_S4_discrete()` to handle S4-class values.
* Added `scale_(x/y)_genomic()`, which is almost identical to the 
  `scale_(x/y)_S4_continuous()` scale except for labels, breaks and guide 
  defaults.
* Added `guide_genomic_axis()` to display sequence name - position relation more 
  clearly.
* Dispatch rules for scale types; for `S4Vectors::Factor` and `S4Vectors::Rle`, 
  the levels/value slot is inspected to decide. Genomic classes in the 
  `GenomicRanges` package go to `*_genomic` scales. Anything else is currently 
  defaulting to the `*_S4_continuous` scales.

### Coordinates

* Added `coord_S4()` to control class specific final transformations. This 
  coordinate system is added automatically when `ggplot()` is called on 
  a `S4Vectors::Vector` class. Should be added manually when S4 Vector data is
  added in, for example, layers. This allows the plot to 'know' that e.g. an 
  `IRanges::IRanges` object underneath an `xmin` aesthetic should give the 
  start, underneath `xmax` should give the end and under a plain `x` should give 
  the middle.

## Internals

* Added custom `ViewScale` ggprotos that can give minor labels and handle 
  discontinuous limits (for e.g. `GenomicRanges::GRanges`).
* Added `forceflat` generic to map discontinuous classes to linear [0,1] 
  interval. For genomic classes, mimicks `GenomicRanges::absoluteRanges()`; 
  otherwise `scales::rescale()`.
* Added range related generics for getting the range of a class, expanding 
  a range and determining empty ranges.
* Added plot arithmetic for WoodenHorse, so that Ranges obey e.g. nudging rules.
* Added generic out-of-bounds variants for `censor()`, `squish()`, 
  `squish_infinite()` and `discard()` from the scales package.
* Added major/minor breaks `S4BreaksMajor()/S4BreaksMinor()` and labels 
  `S4LabelFormat()` generics. They default to the scales package behaviour, 
  but differ for genomic classes (e.g. GRanges).
* Added `S4TransIdentity()` identity transformation that uses the major/minor 
  breaks and label generics.
* S3 wrapper-representation of S4 Vector class, so that they are compatible with 
  data.frame operations. The S3 class is called `WoodenHorse` as they smuggle 
  S4 Vectors into an S3 framework. Constructor is `GreekSoldier()`, reversor is 
  `Nightfall()` and inspector of the underlying class in `HelenOfTroy()`. There
  are two `WoodenHorse` variants; `BeechHorse` which carries the S4 Vector as a 
  parallel attribute, and `OakHorse` which is an index into the S4 Vector 
  attribute.
* Added a `NEWS.md` file to track changes to the package. 
