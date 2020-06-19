#' Example cytobands data
#'
#' Generates some toy cytoband data to plot ideograms with.
#'
#' @return A \code{data.frame} containing cytoband data.
#' @export
#'
#' @details Returns the cytoband data for the first two chromosomes of the human
#'   genome (build hg38). Full data is available as the \code{cytobands.txt.gz}
#'   file from the
#'   \href{http://hgdownload.cse.ucsc.edu/goldenpath/hg38/database/}{UCSC
#'   database}.
#'
#' @seealso \code{\link[ggnomics]{example_cytoband_colours}}
#'   \code{\link[ggnomics]{setup_cytobands}}
#'
#' @examples
#' cyto <- example_cytobands()
example_cytobands <- function() {

  chr1 <- c(0, 23, 53, 71, 91, 125, 159, 201, 236, 276, 299, 323, 343,
            396, 437, 463, 502, 556, 585, 608, 685, 693, 844, 879, 915, 943,
            993, 1018, 1067, 1112, 1155, 1172, 1204, 1217, 1234, 1251, 1432,
            1475, 1506, 1551, 1566, 1591, 1605, 1655, 1672, 1709, 1730, 1761,
            1803, 1858, 1908, 1938, 1987, 2071, 2113, 2144, 2239, 2244, 2268,
            2305, 2346, 2364, 2435, 2489.56422)
  chr2 <- c(0, 44, 69, 120, 165, 190, 238, 277, 298, 318, 363, 383, 415,
            475, 526, 547, 610, 639, 684, 713, 733, 748, 831, 918, 939, 960,
            1021, 1053, 1067, 1087, 1122, 1181, 1216, 1291, 1317, 1343, 1361,
            1415, 1434, 1479, 1490, 1496, 1540, 1589, 1629, 1689, 1771, 1797,
            1821, 1885, 1911, 1966, 2025, 2041, 2082, 2145, 2207, 2243, 2252,
            2301, 2347, 2364, 2421.93529)
  data.frame(
    chrom = factor(rep.int(1:2, c(63, 62)), labels = c("chr1", "chr2")),
    chromStart = c(head(chr1, -1),  head(chr2, -1)) * 1e5,
    chromEnd   = c(tail(chr1, - 1), tail(chr2, -1)) * 1e5,
    name = "",
    gieStain = factor(
      c(2, 4, 2, 4, 2, 5, 2, 4, 2, 4, 2, 4, 2, 4, 2, 6, 2, 5, 2, 5,
        2, 3, 2, 6, 2, 6, 2, 3, 2, 5, 2, 5, 2, 1, 1, 7, 2, 5, 2, 5, 2,
        5, 2, 5, 2, 6, 2, 5, 2, 3, 2, 3, 2, 4, 2, 3, 2, 4, 2, 5, 2, 6,
        2, 2, 5, 2, 6, 2, 6, 2, 4, 2, 6, 2, 5, 2, 3, 2, 3, 2, 5, 2, 5,
        2, 3, 2, 1, 1, 2, 5, 2, 4, 2, 5, 2, 5, 2, 4, 2, 3, 2, 3, 2, 4,
        2, 6, 2, 6, 2, 5, 2, 6, 2, 6, 2, 5, 2, 3, 2, 6, 2, 3, 2, 5, 2),
      labels = c("acen", "gneg", "gpos100", "gpos25",
                 "gpos50", "gpos75", "gvar")
    ),
    stringsAsFactors = FALSE
  )
}


#' Example colours for cytobands.
#'
#' Gives a selection of colours compatible with the \code{gieStain} column of
#' \code{example_cytobands}.
#'
#' @return A character vector of colours named by \code{gieStain} levels.
#' @export
#'
#' @seealso \code{\link[ggnomics]{example_cytobands}}
#' @examples
#' cytocols <- example_cytoband_colours()
example_cytoband_colours <- function() {
  setNames(c("#E41A1C", "grey90", "black", "grey65",
             "grey50",  "grey25", "grey1", "grey51", "grey66", "grey33"),
           c("acen",   "gneg",   "gpos100", "gpos25",
             "gpos50", "gpos75", "gvar",    "stalk", "gpos33", "gpos66"))
}

#' Generate fake Hi-C experiment
#'
#' Randomly generate data in the format of a GENOVA Hi-C experiment.
#'
#' @return A \code{list} with the skeleton of a GENOVA Hi-C experiment.
#'
#' @details The data is formatted as the first 120 Mb of 'chr1' at 1 Mb
#'   resolution. Since data is randomly generated, use \code{set.seed()} for
#'   reproducibility. Qualifies as the bare minimum requirement for the
#'   \code{\link[ggnomics]{geom_hictriangle}}'s \code{exp} argument.
#' @param version When this is set to \code{"old"}, it formats the object as pre
#'   v1.0.0 GENOVA experiment, else it formats the object as a \code{contacts}
#'   object.
#'
#' @seealso \code{\link[GENOVA]{load_contacts}} for how to construct
#'   proper Hi-C experiment objects.
#'
#' @export
#'
#' @examples
#' exp <- example_HiC()
example_HiC <- function(version = "old") {
  try_require("MASS", "example_HiC")
  try_require("reshape2", "example_HiC")
  try_require("data.table", "example_HiC")
  ev <- cbind(cumsum(rt(120, 3)),
              cumsum(rt(120, 3)),
              cumsum(rt(120, 3)),
              cumsum(rt(120, 3)))
  ev <- ev %*% diag(c(300, 150, 75, 35)) %*% MASS::ginv(ev)
  ev <- ev + abs(min(ev))
  ev <- ev ^ 2

  # Add a diagonal
  max <- max(ev)
  for (i in seq.int(0, ncol(ev) - 1, by = 1)) {
    test <- row(ev)  + i == col(ev)
    ev[test] <- (ev[test] + sqrt(dcauchy(i, 0, 5)) * max * 3)
  }
  ev <- ev - min(ev)

  # Format as symmetric matrix in triplet form as data.table
  ev[lower.tri(ev)] <- NA
  ev <- reshape2::melt(ev)
  ev <- ev[!is.na(ev$value), ]
  rownames(ev) <- NULL
  ev <- data.table::data.table(
    V1 = ev$Var1,
    V2 = ev$Var2,
    V3 = ev$value
  )
  data.table::setkey(ev, "V1", "V2")

  # Setup genomic coordinates
  coords <- data.frame(V1 = "chr1",
                       V2 = as.integer(seq.int(0, 119e6, by = 1e6)),
                       V3 = as.integer(seq.int(1e6, 120e6, by = 1e6)),
                       V4 = 1:120)
  
  if (version == "old") {
    exp <- list(ICE = ev,
                ABS = coords,
                RES = 1e6)
    
  } else {
    data.table::setDT(coords)
    data.table::setkeyv(coords, c("V1", "V2"))
    exp <- structure(
      list(
        MAT = ev,
        IDX = coords,
        CHRS = "chr1",
        CENTROMERES = NULL
      ),
      class = "contacts", znorm = FALSE, samplename = "test",
      resolution = 1e6,
      colour = "black", rmChrom = TRUE, balanced = TRUE, scale_cis = FALSE,
      package = "ggnomics"
    )
  }
  return(exp)
}

#' Example gene models
#'
#' Generates a \code{data.frame} compatible with \code{geom_genemodel}.
#'
#' @return A \code{data.frame}
#' @export
#'
#' @details The result contains about one megabase's worth of genes on human
#'   chromosome 17. Data was taken from GRCh38.92 (hg38) gene models from
#'   \href{https://www.ensembl.org/}{Ensembl} and was edited to somewhat
#'   simplify data. Original \code{.gtf} file is available at the
#'   \href{ftp://ftp.ensembl.org/pub/release-97/gtf/homo_sapiens/}{Ensembl
#'   database}.
#'
#' @seealso \code{\link[ggnomics]{geom_genemodel}}
#'
#' @examples
#' gm <- example_genemodels()
example_genemodels <- function() {
  data.frame(
    chrom = "chr17",
    start = c(63728411L, 63751940L, 63702845L, 63706635L, 63710728L, 63713406L,
              63723298L, 63726638L, 63484868L, 63493436L, 63507056L, 63484823L,
              63507629L, 63513382L, 63518928L, 63477095L, 63493436L, 63477061L,
              63484840L, 63479859L, 63493436L, 63746884L, 63751871L, 63760914L,
              63764016L, 63756311L, 63760914L, 63764016L, 63772777L, 63745250L,
              63751770L, 63929229L, 63932262L, 63928740L, 63895025L, 63895755L,
              63894909L, 63872129L, 63872859L, 63872012L, 63909714L, 63918767L,
              63910444L, 63918777L, 63909597L, 63433349L, 63437461L, 63445898L,
              63432304L, 63550678L, 63578470L, 63583502L, 63589000L, 63550461L,
              63589173L, 63787050L, 63792412L, 63798038L, 63805044L, 63815563L,
              63773603L, 63787034L, 63792412L, 63800470L, 63805059L, 63818068L,
              63819805L, 63826903L, 63819433L, 63825343L, 63917312L, 63918777L,
              63917200L, 63880236L, 63881802L, 63880215L, 63523414L, 63530091L,
              63533886L, 63542241L, 63523334L, 63530115L, 63533886L, 63542241L,
              63548802L, 63698555L, 63698876L, 63695902L, 63622760L, 63632681L,
              63646034L, 63652557L, 63657794L, 63666940L, 63681766L, 63685517L,
              63688527L, 63622415L, 63652565L, 63657794L, 63666940L, 63672845L,
              63681766L, 63685517L, 63688527L, 63998365L, 63998351L, 63827491L,
              63827152L, 63829915L, 63940774L, 63957162L, 63963672L, 63971162L,
              63972842L, 63938554L, 63832941L, 63842459L, 63837617L, 63842381L,
              63832081L, 63703602L, 63710491L, 63713406L, 63717075L, 63723298L,
              63726638L, 63713424L, 63717093L, 63726638L, 63737396L, 63741168L,
              63682336L, 63689093L, 63702819L, 63710491L, 63713406L, 63717075L,
              63601084L, 63604534L, 63600872L, 63608003L, 63009560L, 63073943L,
              63099175L, 63151270L, 63193991L, 63200771L, 63237814L, 63267748L,
              63314388L, 63318957L, 63340101L, 63351250L, 63354783L, 63379718L,
              63388635L, 63395743L, 63398821L, 63405122L, 63411511L, 63418307L,
              63009556L, 63399043L, 63420062L),
    end = c(63728413L, 63752097L, 63704582L, 63707418L, 63710836L, 63714108L,
            63723326L, 63728407L, 63491381L, 63496985L, 63507625L, 63484867L,
            63510832L, 63514094L, 63521848L, 63491381L, 63497363L, 63477541L,
            63484867L, 63491381L, 63498380L, 63746961L, 63756570L, 63761351L,
            63766175L, 63756570L, 63761351L, 63766194L, 63776351L, 63746880L,
            63751867L, 63932261L, 63932354L, 63929225L, 63896511L, 63896661L,
            63895157L, 63873614L, 63873766L, 63872528L, 63911196L, 63918776L,
            63911341L, 63918838L, 63909846L, 63440199L, 63442852L, 63446378L,
            63435110L, 63550815L, 63579943L, 63585328L, 63589169L, 63550677L,
            63594266L, 63787270L, 63792562L, 63800617L, 63813454L, 63818395L,
            63775178L, 63787049L, 63792547L, 63800617L, 63813290L, 63819317L,
            63826902L, 63827671L, 63821828L, 63826642L, 63918776L, 63918838L,
            63917423L, 63881801L, 63881935L, 63880456L, 63524369L, 63530542L,
            63538662L, 63545899L, 63523413L, 63530542L, 63538662L, 63546727L,
            63548977L, 63699298L, 63701172L, 63698551L, 63622763L, 63634801L,
            63646074L, 63652656L, 63657907L, 63668162L, 63681899L, 63685590L,
            63693774L, 63622759L, 63652656L, 63657907L, 63668334L, 63672914L,
            63681899L, 63685590L, 63696303L, 63999074L, 63998984L, 63831966L,
            63828137L, 63832026L, 63951900L, 63961431L, 63968355L, 63972841L,
            63972918L, 63940770L, 63838681L, 63842674L, 63838728L, 63843065L,
            63837210L, 63707418L, 63710836L, 63714108L, 63717154L, 63723326L,
            63728369L, 63714108L, 63717154L, 63728413L, 63737436L, 63741986L,
            63682758L, 63690597L, 63707510L, 63710836L, 63714108L, 63717125L,
            63601363L, 63607999L, 63601083L, 63608365L, 63009626L, 63074014L,
            63099357L, 63151380L, 63194139L, 63200957L, 63238077L, 63267873L,
            63314669L, 63319090L, 63340332L, 63351416L, 63355390L, 63379826L,
            63389544L, 63395928L, 63399039L, 63406277L, 63415674L, 63421952L,
            63009559L, 63399286L, 63427699L),
    strand = factor(rep(c(2, 1, 2, 1, 2, 1, 2, 1, 2, 1),
                        c(8, 13, 28, 17, 10, 9, 3, 22, 28, 27)),
                    labels = c("+", "-")),
    type = factor(rep(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
                        2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L,
                        1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
                        2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
                      c(2L, 6L, 3L, 4L, 2L, 4L, 4L, 6L, 1L, 2L, 1L, 2L, 1L,
                        2L, 2L, 3L, 1L, 3L, 4L, 2L, 5L, 6L, 1L, 3L, 1L, 2L,
                        1L, 2L, 4L, 5L, 1L, 2L, 9L, 8L, 1L, 1L, 1L, 2L, 4L,
                        2L, 2L, 3L, 6L, 11L, 2L, 2L, 20L, 3L)),
                  labels = c("CDS", "UTR")),
    name = rep(c("AC046185.1", "AC113554.1", "ACE", "CCDC47", "CD79B", "CSH1",
                 "CSH2", "CSHL1", "CYB561", "DCAF7", "DDX42", "FTSJ3", "GH1",
                 "GH2", "KCNH6", "LIMD2", "MAP3K3", "PRR29", "PSMC5", "SCN4A",
                 "SMARCD2", "STRADA", "TACO1", "TANC2"),
               c(8L, 7L, 6L, 10L, 3L, 3L, 3L, 5L, 4L, 6L, 11L, 4L, 3L, 3L, 9L,
                 3L, 17L, 2L, 3L, 6L, 5L, 17L, 4L, 23L))
    )
}
