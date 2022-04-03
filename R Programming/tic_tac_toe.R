options(warn=-1)

# Function for asking for the user preferred symbol and establishing order of players.
ask_symbol <- function(){
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  cat("Pick a symbol! X or O? ")
  symbol_User <- readLines(con = con, n = 1)
  symbol_comp <- symbols[!symbols %in% symbol_User]
  
  if (symbol_User=="X"){
    player1<<-"User"
    player2 <<-"Computer"
  }
  else if (symbol_User=="O"){
    player2<<-"User"
    player1 <<-"Computer"
  }
  else{
    print("Wrong symbol. Please pick either X or O")
    ask_symbol()
  }
}


# Function executed every time a player has to play, if player is "user" then it asks for coordinates input. 
player_plays <- function(board, player, symbol){
  if (player=="User"){
    cat( "\n","Current board:", "\n")
    print(board)
    if (interactive()) {
      con_row <- stdin()
      con_col <- stdin()
    } else {
      con_row <- "stdin"
      con_col <- "stdin"
    }
    cat("\n","Row? ")
    n_row <- as.integer(readLines(con = con_row, n = 1))
    
    cat("Column? ")
    n_col <- as.integer(readLines(con = con_col, n = 1))
    
    if ((is.integer(n_row)!=T | n_row >3 | n_row <1 | is.na(n_row)) | (is.integer(n_col)!=T | n_col >3 | n_col <1 | is.na(n_col))){
      cat("\n","Incorrect row or column number. Please input numbers from 1 to 3 to indicate row and column.", "\n")
      board<-player_plays(board, player, symbol)
    }
    else if (is.na(board[n_row, n_col]) ==F){
      cat("\n","That place is already taken. Please choose a different one.", "\n")
      board<-player_plays(board, player, symbol)
    }
    else{
      board[n_row, n_col] <- symbol
      cat("\n", "Move placed!", "\n")
      print(board)
    }

  }
  
  else if (player=="Computer"){
    cat("\n","Computer:","\n")
    ind_na <- which(is.na(board), TRUE)
    comp_pos <- ind_na[sample(1:nrow(ind_na),1),]
    board[comp_pos[1], comp_pos[2]]<- symbol
    print(board)
  }

  return(board)
}

# Function for checking whether a player has won in the last turn.
is.win <- function(board){
  win <- sum(apply(board, 2, function(a) length(unique(a))==1 & sum(is.na(a)) != 3))>0 |
    sum(apply(board, 1, function(a) length(unique(a))==1 & sum(is.na(a)) != 3)) >0|
    (length(unique(diag(board))) == 1 & sum(is.na(diag(board))) != 3)|
    (length(unique(board[(n<-nrow(board))^2-(1:n)*(n-1)])) == 1 & sum(is.na(board[(n<-nrow(board))^2-(1:n)*(n-1)])) != 3)
  return(win)
}

# Establishing board and round counter
board <- matrix(ncol=3, nrow=3)
symbols <- c("X", "O")
ask_symbol()
round <- 1

while (NA %in% board){
  Sys.sleep(3)
  cat(paste0("\n","##################  Round ", round, " ##################", "\n"))

  Sys.sleep(1)
  board<-player_plays(board, player1, "X")
  if (is.win(board)){
    Sys.sleep(2)
    cat(paste0("\n","Player ", player1, " wins!"))
    break
  }
  else if (!NA %in% board){
    cat("\n","It's a tie!","\n")
    break
  }
  Sys.sleep(2)
  board<-player_plays(board, player2, "O")
  if (is.win(board)){
    Sys.sleep(2)
    cat(paste0("\n","Player ", player2, " wins!"))
    break
  }
  else if (!NA %in% board){
    cat("\n","It's a tie!","\n")
    break
  }
  
  round <- round +1  
}

  
