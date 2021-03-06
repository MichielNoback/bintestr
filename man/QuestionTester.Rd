% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/question_model.R
\name{QuestionTester}
\alias{QuestionTester}
\title{QuestionTester constructor}
\usage{
QuestionTester(tester_function, feedback_generator)
}
\arguments{
\item{feedback_generator}{this argument is used to rovide relevant feedback
to the user. It can be either a simple \code{character} or a \code{function}.
If it is a character, it is simply displayed when the answer is incorrect.
If it is a function, it will be called when \code{tester} returns
\code{FALSE}. It will be passed the user's incorrect answer and should print some
appropriate feedback message.}

\item{tester_fun}{the function that will be called to verify the question answer.
It should only return \code{TRUE} or \code{FALSE}}
}
\description{
This S3 class should be instantiated and passed to the Question constructor.
It will be used to verify the users' answer and also provide feedback when a
given answer is incorrect.
}
\examples{
## Instantiate a QuestionTester that tests for a character vector with a single
## "word" of length 5
## The feedback_generator generates some appropriate messages depending on the answer
t1 <- QuestionTester(
    function(x){is.character(x) && length(x)==1 && nchar(x)==5},
    function(x){
        if(! is.character(x)) "that is no word"
        else if(nchar(x) < 5) paste0(x, " has ", nchar(x), " characters and that is not 5")
    }
)
q1 <- Question("Please give a word of length 5", t1)

}
