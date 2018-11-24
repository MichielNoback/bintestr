.progress.file <- ".bintestr.Rdata"
.userdata <- NULL

.onLoad <- function(libname, pkgname) {
    if (file.exists(.progress.file)) {
        load(.progress.file)
        #message("loading your progress...")
        message(paste0("Welcome back, ", userdata$username, "!"))
        .userdata <<- userdata
        instructions()
    } else {
        message("no progress file found. Did you forget to set the working directory?")
        answer <- readline(prompt="Type N if you are a New user or E to Exit: ")
        if (answer == "N") {
            .initUser()
            instructions()
        } else {
            detach(package:bintestr, character.only=TRUE)
        }
    }
}

.initUser <- function() {
    username <- readline(prompt="What will be your name in this course? ")
    userdata <- list("username" = username)
    save(userdata, file = .progress.file)
}

#' Display instructions
#'
#' This help page lists the functions you can use to navigate this course.
#'
#' - \code{start()} - To proceed where you left last time (or at question 1 if
#'  you just started).
#'
#' - \code{check.me()} - Will verify your current question, and maybe give some
#' feedback if it is not correct
#'
#' - \code{next.question()} - Will proceed to the next question
#'
#' - \code{bye()} - Saves your progress and quits - but you will not want to quit!
#'
#' @export
instructions <- function() {
    message("There are only a few functions needed to navigate these assignments;")
    message("They are shown in the Help viewer right now (I hope).")
    message("Otherwise, type ?instructions to view them")
    help(instructions)
}



