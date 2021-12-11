Rock_Paper_Scissors <- function(your_choice) {
    if(missing(your_choice)) {stop("Please make your choice")}
    rock <- "rock"
    paper <- "paper"
    scissors <- "scissors"
    com_choice <- sample(c(rock, paper, scissors), 1)
        if (your_choice == rock) { 
            if (com_choice == rock) {
                "Tie."
            }  else {
                ifelse(com_choice == paper, "You win!", "Computer win :(")
            } 
    }   else if (your_choice == paper) {  
            if (com_choice == rock) {
                "You win!"
            } else {
                ifelse(com_choice == paper, "Tie.", "Computer win :(")
            }
    }   else {
            if (com_choice == rock) {
                "Computer win :("
                } else {
                    ifelse(com_choice == paper, "You win!", "Tie.")
            }
    }
}
#test !chr
Rock_Paper_Scissors(paper)
Rock_Paper_Scissors(rock)
Rock_Paper_Scissors(scissors)
#test chr
Rock_Paper_Scissors("paper")
Rock_Paper_Scissors("rock")
Rock_Paper_Scissors("scissors")

com <- list(sample(c(rock, paper, scissors))
Rock_Paper_Scissors(com)

