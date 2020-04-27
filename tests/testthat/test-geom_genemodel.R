df <- example_genemodels()
df <- DataFrame(x = granges(df), mcols(df))

base <- ggplot(df, aes(x = ranges(x), strand = strand(x),
                       exontype = type, group = name))

test_that("geom_genemodel can be added to a plot", {
    g <- base + geom_genemodel()

    expect_is(g$layers[[1]]$geom, "GeomGenemodel")

    gt <- ggplotGrob(g)

    expect_is(gt, "gtable")
})

test_that("geom_genemodel intron styles can be set", {

    a <- base + geom_genemodel(intron.style = "arrowline")
    b <- base + geom_genemodel(intron.style = "line")
    c <- base + geom_genemodel(intron.style = "chevron")

    a <- layer_grob(a)[[1]]$children[[1]]
    b <- layer_grob(b)[[1]]$children[[1]]
    c <- layer_grob(c)[[1]]$children[[1]]

    expect_is(a, "arrowline")
    expect_is(b, "segments")
    expect_is(c, "polyline")
})

test_that("geom_genemodel chevron height is accepted", {
    a <- base + geom_genemodel(intron.style = "chevron", chevron.height = 1)
    b <- base + geom_genemodel(intron.style = "chevron", chevron.height = 0.2)

    a <- layer_grob(a)[[1]]$children[[1]]
    b <- layer_grob(b)[[1]]$children[[1]]

    diff <- as.numeric(a$y) - as.numeric(b$y)

    # Test if there are differences
    expect_true(sum(abs(diff)) > 0)
})

test_that("geom_genemodel propogates arrows", {
    arrow <- arrow(angle = 10, type = "closed")
    g <- base + geom_genemodel(arrow = arrow)
    g <- layer_grob(g)[[1]]$children[[1]]
    expect_identical(g$arrow.template, arrow)
})

test_that("geom_genemodel makeContent works", {
    g <- base + geom_genemodel(intron.style = "arrow")
    grob <- layer_grob(g)[[1]]
    expect_is(grob, "genemodel")
    expect_is(grob$children[[2]], "rect")

    input <- grob
    output <- grid::makeContent(input)
    expect_is(input$children[[1]], "arrowline")
    expect_is(output$children[[1]], "segments")
})
