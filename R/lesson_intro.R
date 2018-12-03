lesson_intro <- function() {
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


    t2 <- QuestionTester(
        tester_function = function() is_defined_with_value("var", 44, c("numeric", "integer")),
        feedback_generator = function() {
            if(! is_defined("var")) return("no object with name 'var' found")
            if(! is_defined_with_value("var", 44)) return("object 'var' does not have value 44")
            return("There does not seem to be any problem here")
        }
    )
    q2 <- Question("Define a variable called 'var' with value 44",
                   t2,
                   q_type="environment-eval",
                   hints=list("a variable called 'var' with value 44. Please...",
                              "I suggest you don't quit your day job"))

    list(q1, q2)
}

