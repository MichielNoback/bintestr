#### QUESTION TESTER

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

#' QuestionTester constructor
#'
#' This S3 class should be instantiated and passed to the Question constructor.
#' It will be used to verify the users' answer and also provide feedback when a
#' given answer is incorrect.
#'
#' @param tester the function that will be called to verify the question answer.
#' It should only return \code{TRUE} or \code{FALSE}
#' @param feedback_generator this argument is used to rovide relevant feedback
#' to the user. It can be either a simple \code{character} or a \code{function}.
#' If it is a character, it is simply displayed when the answer is incorrect.
#' If it is a function, it will be called when \code{tester} returns
#' \code{FALSE}. It will be passed the user's incorrect answer and should print some
#' appropriate feedback message.
#'
#' @examples
#' ## Instantiate a QuestionTester that tests for a character vector with a single
#' ## "word" of length 5
#' ## The feedback_generator generates some appropriate messages depending on the answer
#' t1 <- QuestionTester(
#'     function(x){is.character(x) && length(x)==1 && nchar(x)==5},
#'     function(x){
#'         if(! is.character(x)) "that is no word"
#'         else if(nchar(x) < 5) paste0(x, " has ", nchar(x), " characters and that is not 5")
#'     }
#' )
#' q1 <- Question("Please give a word of length 5", t1)
#'
#' @export
QuestionTester <- function(tester, feedback_generator) {
    new_QuestionTester(tester, feedback_generator)
}


#### QUESTION
new_Question <- function(q_text, tester, q_type, hints) {
    stopifnot(is.character(q_text))
    stopifnot(class(tester) == "QuestionTester" )
    stopifnot(is.character(q_type))
    stopifnot(is.list(hints))
    hint_index = 0
    max_hints = length(hints)
    ask <- function(){
        message(q_text)
        if (q_type == "console-input") {
           while (! tester$is_correct()) {
                answer <- readline("")
                if(nchar(answer) == 0) {
                    message("It seems you want to quit this question. A shame.")
                }
                tester$check(answer)
                message(tester$message())
                if (! tester$is_correct()) {
                    hint()
                }
            }
        } else if (q_type != "environment-eval") {
            message(paste0("unknown question type: ", q_type))
        }
    }
    verify <- function() {
        tester()
    }
    hint <- function() {
        hint_index <<- hint_index + 1
        if(hint_index > max_hints) {
            message("Sorry, no more hints")
        } else {
            message(hints[[hint_index]])
        }
    }
    q <- list(ask = ask,
              q_text = q_text,
              tester = tester,
              hint = hint)
    structure(q, class="Question")
}

#' Question constructor
#'
#' This S3 class encapsulates a question for interactive lesson scenarios.
#'
#' @param question_text the text that will be displayed in the console prompting the user
#' to action.
#'
#' @param question_tester an instance of class \link[bintestr]{QuestionTester}.
#'
#' @param type the type can be one of \code{console-input}, \code{environment-eval}. For
#' \code{console-input}, execution will be halted until the user has entered some text
#' and hit enter. For \code{environment-eval}, the user is required to explicitly call
#' \code{verify()} on the question instance.
#'
#' @param hints a list of hints that will be displayed in consecutive order for each
#' incorrect answer.
#'
#' @examples
#' ## Instantiate a QuestionTester that tests for a character vector with a single
#' ## "word" of length 5
#' ## The feedback_generator generates some appropriate messages depending on the answer
#' t1 <- QuestionTester(
#'     function(x){is.character(x) && length(x)==1 && nchar(x)==5},
#'     function(x){
#'         if(! is.character(x)) "that is no word"
#'         else if(nchar(x) < 5) paste0(x, " has ", nchar(x), " characters and that is not 5")
#'     }
#' )
#' q1 <- Question("Please give a word of length 5", t1)
#'
#' @export
Question <- function(question_text, question_tester, type="console-input", hints = NULL) {
    if(is.null(hints)) hints <- list("Hints have no power here")

    new_Question(question_text, question_tester, type, hints)
}




t1 <- QuestionTester(
    function(x){is.character(x) && length(x)==1 && nchar(x)==5},
    function(x){
        if(! is.character(x)) "that is no word"
        else if(nchar(x) < 5) paste0(x, " has ", nchar(x), " characters and that is not 5")
    }
)
q1 <- Question("Please give a word of length 5",
               t1,
               hints=list("A WORD of FIVE characters. Please...", "I suggest you go do something else"))

#q1$ask()

t2 <- QuestionTester(
    tester_function = function(x){
        NULL
    },
    feedback_generator = function(x){
        if(! is.character(x)) "that is no word"
        else if(nchar(x) < 5) paste0(x, " has ", nchar(x), " characters and that is not 5")
    }
)
q2 <- Question("Define a variable called x with value 42",
               t2,
               hints=list("a variable called x with value 42. Please...", "I suggest you don't quit your day job"))

q2$ask()


