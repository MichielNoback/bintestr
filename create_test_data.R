data.folder <- "external_data"

create.snake.df <- function() {
    data.file <- "snakes.csv"
    path <- file.path(data.folder, data.file)
    snakes <- read.table(path, sep = ";", header = T)
    devtools::use_data(snakes, overwrite = TRUE)
}

create.test.data <- function() {
    create.snake.df()
}

create.test.data()

