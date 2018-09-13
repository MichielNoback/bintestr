.progress.file <- ".bintestr.Rdata"

.onLoad <- function(libname, pkgname) {
    message("initializing your lessons...")

    if (file.exists(.progress.file)) {
        load(.progress.file)
        message("loading your progress...")
        message(paste0("welcome back, ", bintestr.userdata$username, "!"))
    } else {
        message("no progress file found. Did you forget to set the working directory?")
        answer <- readline(prompt="Enter N for a new progress file or E to exit: ")
        if (answer == "N") {
            bintestr.username <- readline(prompt="What will be your name in this course? ")

        } else {
            detach(package:bintestr, character.only=TRUE)
        }
    }
}
