

new_QuestionTester <- function(tester_function, feedback_generator) {
    stopifnot(is.function(tester_function))
    stopifnot(is.function(feedback_generator) || is.character(feedback_generator))
    qt_data <- list(feedback_message = "FEEDBACK", answer_correct = FALSE)
    check <- function(answer) {
        if (tester_function(answer)){
            qt_data$feedback_message <<- "That is correct!"
            qt_data$answer_correct <<- TRUE
        }
        else {
            if (is.character(feedback_generator)) qt_data$feedback_message <<- feedback_generator
            else qt_data$feedback_message <<- feedback_generator(answer)
        }
    }
    correct <- function() qt_data$answer_correct
    message <- function() qt_data$feedback_message

    t <- list(check = check, is_correct = correct, message = message)
    structure(t, class="QuestionTester")
}

QuestionTester <- function(tester, feedback_generator) {
    new_QuestionTester(tester, feedback_generator)
}

new_Question <- function(q_text, tester, type, hints) {
    stopifnot(is.character(q_text))
    stopifnot(class(tester) == "QuestionTester" )
    stopifnot(is.character(type))
    stopifnot(is.list(hints))
    ask <- function(){
        message(q_text)
        if (type == "console-input") {
            answer <- readline("")
            tester$check(answer)
            message(tester$message())
        }
    }
    verify <- function() {
        tester()
    }
    q <- list(ask = ask,
              q_text = q_text,
              tester = tester,
              hints = hints)
    structure(q, class="Question")
}

Question <- function(question_text, tester_function, type="console-input", hints = NULL) {
    if(is.null(hints)) hints <- list("Hints have no power here")

    new_Question(question_text, tester_function, type, hints)
}


t1 <- QuestionTester(
    function(x){is.character(x) && nchar(x)==5},
    function(x){
        if(! is.character(x)) "that is no word"
        else if(nchar(x) < 5) paste0(x, " has ", nchar(x), " characters and that is not 5")
    }
)
q1 <- Question("Please give a word of length 5", t1)


q1$ask()


