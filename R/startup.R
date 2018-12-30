## local progress file
.progress.file <- ".bintestr.Rdata"
## cache for userdata stores username, progress, and questions
.userdataEnv <- new.env()

.onLoad <- function(libname, pkgname) {
    if (file.exists(.progress.file)) {
        load(.progress.file)
        message(paste0("Welcome back, ", userdata$username, "!"))
        assign("userdata", userdata, envir=.userdataEnv)
        instructions()
    } else {
        message("no progress file found. Did you forget to set the working directory?")
        answer <- readline(prompt="Type N if you are a New user or any other key to Exit: ")
        if (answer == "N") {
            .initUser()
            instructions()
        } else {
            detach(package:bintestr, unload=TRUE)
        }
    }
}

.initUser <- function() {
    username <- readline(prompt="What will be your name in this course? ")
    current_lesson <- "intro"
    scores <- list()
    scores[[current_lesson]] <- 0

    userdata <- list(
        "username" = username,
        "lesson" = current_lesson,
        "question" = 1,
        "scores" = scores)
    assign("userdata", userdata, envir=.userdataEnv)
    save(userdata, file = .progress.file)
}

#' Display bintestr instructions
#'
#' This help page lists the functions you can use to navigate this course.
#'
#' - \code{status()} - To see the your status in the current lesson.
#'
#' - \code{start_lesson()} - To proceed where you left last time (or start at question 1
#' of lesson 1 if you just started).
#'
#' - \code{check.me()} - Will verify your current question, and maybe give some
#' feedback if it is not correct
#'
#' - \code{next_question()} - Will proceed to the next question
#'
#' - \code{save_and_exit()} - Saves your progress and quits - but you will not want to quit!
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
    userdata <- get("userdata", envir = .userdataEnv)
    message(paste0("status for user ", userdata$username, ":"))
    message(paste0("   your current lesson is: ", userdata$lesson))
    message(paste0("   your current question is: ", userdata$question))
    score <- userdata$scores[[userdata$lesson]]
    message(paste0("   current score of this lesson is: ", score))
}

#' loads the lesson registered as current
#'
#' Loads the current lesson (or the one specified as argument)
#'
#' @export
start_lesson <- function(lesson = "__current__", question = "__current__") {
    userdata <- get("userdata", envir = .userdataEnv)
    lesson <- userdata$lesson
    question <- userdata$question
    lesson_function <- paste0("lesson_", lesson)
    if(lesson_function %in% ls("package:bintestr")) {
        message(paste0("loading lesson ", lesson_function))
        # calling the lesson will return a list of questions
        questions <- get(lesson_function)()
        assign("questions", questions, envir = .userdataEnv)
    } else {
        message(paste0("the requested lesson is not found: ", lesson))
    }
}

#' leaves the lesson
#'
#' Leaves the lesson and stores progress locally.
#'
#' @export
save_and_exit <- function() {
    answer <- readline(prompt="Sure you want to quit (y/n)? ")
    if (answer == "y") {
        userdata <- get("userdata", envir = .userdataEnv)
        save(userdata, file = .progress.file)
        status()
        message("see you soon!")
        detach(package:bintestr, unload=TRUE)
    } else if (answer == "n") {
        message("You changed your mind! Good luck proceeding")
    } else {
        message(paste0("option ", answer, " is not y/n"))
    }
}
