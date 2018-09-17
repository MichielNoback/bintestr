data.folder <- "external_data"


create.test.data <- function() {
    create.snake.sizes()
}

create.test.data()

create.snake.sizes <- function() {
    data.file <- "snakes.csv"
    path <- file.path(data.folder, file)
    snakes <- read.table(file.path(data.folder, file), sep = ";", header = T)
    #head(snakes)
    devtools::use_data(snakes)
}
