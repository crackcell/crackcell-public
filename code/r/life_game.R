#'
#' Copyright (c) 2012, Menglong Tan <tanmenglong@gmail.com>
#'
#' This program is free software; you can redistribute it
#' and/or modify it under the terms of the GPL licence
#'
#'
#' @file life_game.R
#' @author Tan Menglong <tanmenglong@gmail.com>
#' @date Wed Apr  4 17:29:29 2012
#' @brief Life Game implemented by R
#'

InitWorld <- function(nrow, x) {
    #' @brief produce a n*n matrix with x bacteria

    x <- ifelse(x > nrow * nrow, nrow * nrow, x)
    matrix.slots <- rep(0, nrow * nrow)
    for (i in sample(1 : (nrow * nrow), x,
                     replace = FALSE)) {
        matrix.slots[i] = 1
    }
    return(matrix(data = matrix.slots, nrow = nrow, ncol = nrow))
}

InitPlot <- function(world) {
    #' @brief Prepare an empty plot

    plot(c(1 : nrow(world)), c(1 : nrow(world)),
         type = 'n', xlab = '', ylab = '')
}

PlotWorld <- function(world) {
    #' @brief Plot world matrix in a scatter plot

    for (x in 1 : nrow(world)) {
        for (y in 1 : nrow(world)) {
            if (world[x, y] == 1) {
                points(x, y)
            }
        }
    }
}

GetNeighbourNum <- function(world, x, y) {
    #' @brief Get number of neighbours

    left.corner.x <- ifelse(x == 1, 1, x - 1)
    left.corner.y <- ifelse(y == 1, 1, y - 1)
    right.corner.x <- ifelse(x == nrow(world), nrow(world), x + 1)
    right.corner.y <- ifelse(y == ncol(world), ncol(world), y + 1)

    count <- 0
    for (i in left.corner.x : right.corner.x) {
        for (j in left.corner.y : right.corner.y) {
            if (i == x && j == y) {
                next
            }
            if (world[i, j] == 1) {
                count <- count + 1
            }
        }
    }
    return(count)
}

Reproduce <- function(world) {
    #' @brief Reproduce the world

    changed <- FALSE
    bacteria.count.diff <- 0
    for (i in 1 : nrow(world)) {
        for (j in 1 : ncol(world)) {
            n <- GetNeighbourNum(world, i, j)
#            print(paste("[", i, ",", j, "]=", n))
            if (world[i, j] == 1) {
                if(n == 2 || n == 3) {
                    next
                }
                if (n < 2 || n > 3) {
                    changed <- TRUE
                    bacteria.count.diff <- bacteria.count.diff - 1
                    world[i, j] = 0
                }
            } else {
                if (n == 3) {
                    changed <- TRUE
                    bacteria.count.diff <- bacteria.count.diff + 1
                    world[i, j] = 1
                }
            }
        }
    }
    return(list(world, changed, bacteria.count.diff))
}

IsAllDead <- function(world) {
    #' @brief Is all bacteria dead?

    for (i in 1 : nrow(world)) {
        for (j in 1 : ncol(world)) {
            if (world[i, j] == 1) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

generation.max <- 50
bacteria.count <- 150
world <- (InitWorld(30, bacteria.count))
generation <- 1
generation.vector <- c()
bacteria.count.vector <- c()
while (1) {
    InitPlot(world)
    PlotWorld(world)
    print(paste("generation: ", generation, ", bacteria: ", bacteria.count))
    generation.vector <- append(generation.vector, generation)
    bacteria.count.vector <- append(bacteria.count.vector, bacteria.count)
    if (IsAllDead(world)) {
        print("all dead")
        break
    }
    if (generation >= generation.max) {
        print("max generation")
        break
    }
    Sys.sleep(0.5)
    ret <- Reproduce(world)
    changed <- ret[[2]]
    if (!changed) {
        print("stable")
        break
    }
    bacteria.count <- bacteria.count + ret[[3]]
    world <- ret[[1]]
    generation <- generation + 1
}
plot(generation.vector, bacteria.count.vector, type = 'l')
