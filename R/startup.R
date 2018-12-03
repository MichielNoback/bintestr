.progress.file <- ".bintestr.Rdata"
.userdata <- NULL
.questions <- NULL

.onLoad <- function(libname, pkgname) {
    if (file.exists(.progress.file)) {
        load(.progress.file)
        #message("loading your progress...")
        message(paste0("Welcome back, ", userdata$username, "!"))
        .userdata <<- userdata
        instructions()
    } else {
        message("no progress file found. Did you forget to set the working directory?")
        answer <- readline(prompt="Type N if you are a New user or any other key to Exit: ")
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
    current_lesson <- "intro"
    scores <- list()
    scores[[current_lesson]] <- 0

    userdata <<- list(
        "username" = username,
        "lesson" = current_lesson,
        "question" = 1,
        "scores" = scores)
    .userdata <<- userdata
    save(userdata, file = .progress.file)
}

#' Display bintestr instructions
#'
#' This help page lists the functions you can use to navigate this course.
#'
#' - \code{start()} - To proceed where you left last time (or at question 1 if
#'  you just started).
#'
#' - \code{check.me()} - Will verify your current question, and maybe give some
#' feedback if it is not correct
#'
#' - \code{next_question()} - Will proceed to the next question
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

#' report status
#'
#' Reports the status of the current registered lessons
#'
#' @export
status <- function() {
    message(paste0("status for user ", .userdata$username, ":"))
    message(paste0("   your current lesson is: ", .userdata$lesson))
    message(paste0("   your current question is: ", .userdata$question))
    score <- .userdata$scores[[.userdata$lesson]]
    message(paste0("   current score of this lesson is: ", score))
}

#' loads the lesson registered as current
#'
#' Loads the current lesson (or the one specified as argument)
#'
#' @export
init_lesson <- function(lesson = "__current__", question = "__current__") {
    lesson <- .userdata$lesson
    question <- .userdata$question

    lesson_function <- paste0("lesson_", lesson)

    if(is_defined(lesson_function)) {
        #calling the lesson will return a list of questions
        .questions <<- get(lesson_function)()
        print(paste0(length(questions), " found for this lesson"))
    } else {
        message(paste0("the requested lesson is not found: ", lesson))
    }
}

