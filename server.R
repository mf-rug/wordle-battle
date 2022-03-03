function(input, output) {
  # forceword <- toupper('blind')
  # forceScore <- '13eb330e5b145fe37a224551c8c1e24c.csv'
  # debg <- 'TRUE'
  
  js$getidcookie()
  
  plog <- function(...) {
    cat(file = stderr(), paste(paste(..., collapse = ' '), '\n'))
  }
  
  observe({
    if (exists('df_rV')) {
      if (is.null(df_rV$table)) { 
        choose_words()
        delay(100, {
        getcookie <- input$cookie
        if (!is.null(getcookie)) {
          cat(file = stderr(), 'cookie exists\n')
          df_rV$userID <- getcookie
          prevScore <- file.path('/home/shiny/ScoreLogs',paste0(df_rV$userID,'.csv'))
          cat(file = stderr(), paste('checking if this file exists:', prevScore, '\n'))
          cat(file = stderr(), paste(file.exists(prevScore), '\n'))
          if (file.exists(prevScore)) {
            cat(file = stderr(), 'previous records found\n\n\n\n')
            df_rV$score <- read_csv(prevScore, show_col_types = FALSE) %>% as.data.frame()
            if (ncol(df_rV$score) == 9) {
              cat(file = stderr(), 'previous records with old format, adding 4 columns YAI etc\n')
              df_rV$score[,c('YAI', 'YUser', 'GAI', 'GUser' )] <- 0
            }
          } else {
            cat(file = stderr(), 'no previous records found. Making records with new 13 col format\n')
            df_rV$score <- data.frame('Game' = 1, 'Winner' = NA, 'Winround' = NA, '1' = NA, '2' = NA, '3' = NA, '4' = NA, '5' = NA, '6' = NA, 'YAI' = NA, 'YUser' = NA, 'GAI' = NA, 'GUser' = NA)
          }
          if (exists('forceScore') && file.exists(forceScore)) {
            df_rV$score <- read_csv(forceScore, show_col_types = FALSE) %>% as.data.frame()
          }
          paste('cookie exists and is', getcookie)
        } else {
          cat(file = stderr(), 'no cookie exists\n')
          rid <- ids::random_id(n = 1, bytes = 16, use_openssl = TRUE)
          shinyjs::runjs(paste0("
                              Cookies.set('id', '",rid,"', { expires: 5000, sameSite: 'None', secure: true, path: '/wordle_battle_en'});
                              "))    
          df_rV$userID <- rid
          df_rV$score <- data.frame('Game' = 1, 'Winner' = NA, 'Winround' = NA, '1' = NA, '2' = NA, '3' = NA, '4' = NA, '5' = NA, '6' = NA, 'YAI' = NA, 'YUser' = NA, 'GAI' = NA, 'GUser' = NA)
          paste('cookie does NOT exist and was set to be', rid)
          }
        })
      }
    }
  })

  mes.df <- data.frame('EnU' = NA, 'EsU' = NA, 'DeU' =NA,'EnA' = NA, 'EsA' = NA, 'DeA' =NA)
  mes.df['Welcome',] <- c('Welcome to Wordle Battle. You begin!', 'Bienvenido a Worlde Battle. Empiezas.', 'Willkommen zu Wordle Battle. Du beginnst.','Welcome to Wordle Battle. The AI begins.', 'Bienvenido a Worlde Battle. Empieza la AI.', 'Willkommen zu Wordle Battle. Die AI beginnt.')
  mes.df['gameno1', ] <- c('Game no ', 'Juego nro ', 'Spiel nr ','Game no ', 'Juego nro ', 'Spiel nr ')
  mes.df['gameno2', ] <- c('You begin.', 'Empiezas.', 'Du beginnst.','The AI begins.', 'Empieza la AI.', 'Die AI beginnt.')
  mes.df['Guessed', ] <- c('You guessed "','Has adivinado "','Du rätst "','The AI guessed "','La AI ha adivinado "','Die AI rät "')
  mes.df['noword', ] <- c('" is not in the word list.','" no está en la lista de palabras.','" ist nicht in der Wörterliste.', '" is not in the word list.','" no está en la lista de palabras.','" ist nicht in der Wörterliste.')
  mes.df['lost1', ] <- c('You won against the AI in ', 'Has ganado contra la AI en ', 'Du hast die AI besiegt in ','You lost against the AI in ', 'Has perdido contra la AI en ', 'Du hast verloren gegen die AI in ')
  mes.df['lost2', ] <- c(' round', ' ronda', ' Runde',' round', ' ronda', ' Runde')
  mes.df['lost31', ] <- c('! Lucky bastard!\n', '! Suertoso!\n', '! Glückspilz!\n', '! Sorry!\n', '! Lo siento!\n', '! Schade!\n')
  mes.df['lost32', ] <- c('! Unbelievable!\n', '! Increíble!\n', '! Unglaublich!\n', '! Sorry!\n', '! Lo siento!\n', '! Schade!\n')
  mes.df['lost33', ] <- c('! Impressive!\n', '! Impresionante!\n', '! Beeindruckend!\n', '! Sorry!\n', '! Lo siento!\n', '! Schade!\n')
  mes.df['lost34', ] <- c('! Excellent!\n', '! Excelente!\n', '! Exzellent!\n', '! Sorry!\n', '! Lo siento!\n', '! Schade!\n')
  mes.df['lost35', ] <- c('! Well done!\n', '! Bien hecho!\n', '! Gut gemacht!\n', '! Sorry!\n', '! Lo siento!\n', '! Schade!\n')
  mes.df['lost36', ] <- c('! Congratulations!\n', '! Felicitaciones!\n', '! Glückwunsch!\n', '! Sorry!\n', '! Lo siento!\n', '! Schade!\n')
  mes.df['noone1', ] <- c('Nobody guessed the word "','Nadie ha adivinado la palabra "','Niemand hat das Wort "','Nobody guessed the word "','Nadie ha adivinado la palabra "','Niemand hat das Wort "')
  mes.df['noone2', ] <- c('" in 6 rounds.', '" en 6 rondas.', '" in 6 Runden erraten.','" in 6 rounds.', '" en 6 rondas.', '" in 6 Runden erraten.')
  
  firstguess <- function() {
    if (!file.exists('startword.csv')) {
      w <- cbind(as.data.frame(wordle_dict), 
                 as.data.frame(matrix(unlist(str_split(wordle_dict[1:length(wordle_dict)], '')), ncol = 5, byrow = TRUE)))
      f <- cbind(as.data.frame(letters), 
                 as.data.frame(apply(w[,2:6], 2, function(x) table(x) / nrow(w) *100))) 
      
      f$mean <- apply(f[,2:6], 1, mean)
      
      for (i in 1:nrow(w)) {
        for (o in 1:5) {
          w[i, paste('p', o+1+5)] <- f[which(f$letter == w[i, o+1]), o+1]
          w[i, paste('s', o+1+5)] <- f[f$letters == w[i, o+1], 'mean']
        }
      }
      w$sumf <- apply(w[, c(7,9,11,13,15)], 1, sum)
      
      for (i in 1:nrow(w)) {
        w[i, 'sum_uniqs'] <- sum(w[i, match(unique(as.character(w[i,2:6])),w[i,2:6]) * 2 + 6])
      }
      
      w$unique <- apply(w[, 2:6], 1, function(x) length(unique(x)))
      w$score <- w$unique * w$sumf
      w$rank <- nrow(w) - rank(w$score) + 1
      
      w$UxSUxSF <- w$unique * w$sum_uniqs * w$sumf
      w$rank2 <- nrow(w) - rank(w$UxSUxSF) + 1
      write_csv(w, 'startword.csv')
    }
    w <- read_csv('startword.csv', show_col_types = FALSE)
    w <<- w[toupper(w$wordle_dict) %in% play_dict,]
    toupper(sample(w$wordle_dict, size = 1, prob = w$UxSUxSF ** 10))
  }
  
  choose_words <- function(reactive=TRUE) {
    if (exists('debg')) { print('making words') }
    df_rV$lang <- 'En'
    if (exists('debg')) { print(df_rV$lang) }
    if (exists('forceword')) { 
      word_dict <- data.frame("X1" = forceword, 'X2' = 1, 'X3' = 1, 'X4' = 1)
    } else {
      if (exists('debg')) { print('English selected!') }
      if (file.exists('wb_english.csv')) {
        word_dict <- read.csv('wb_english.csv')
      } else {
        word_dict <- read_tsv('http://norvig.com/ngrams/count_1w.txt', col_names = FALSE)
        word_dict <- rbind(word_dict, data.frame('X1' = setdiff(wordle_dict, word_dict$X1), 'X2' = 10000))
        word_dict <- word_dict[word_dict$X1 %in% wordle_dict,]
        ttags <- c("[0-0.0001)", "[0.0001-0.0008)","[0.0008-0.008)", "[0.008-0.06)", "[0.06-1)")
        breaks <- c(0,0.0001, 0.0008, 0.008, 0.06, 1)
        prob <- c(0.01,0.05, 0.1, 0.5 ,1)
        
        word_dict$X2 <- word_dict$X2 / max(word_dict$X2)
        word_dict$X3 <- cut(word_dict$X2, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = ttags)
        word_dict$X4 <- cut(word_dict$X2, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = prob)
        word_dict$X4 <- as.numeric(as.character(word_dict$X4))
        write.csv(word_dict,'wb_english.csv')
      }
      print('now making win_dict')
      if (file.exists('wb_winwords.csv')) {
        win_dict <- read.csv('wb_winwords.csv')
      } else {
        win_dict <- read.csv('https://gist.githubusercontent.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b/raw/5d752e5f0702da315298a6bb5a771586d6ff445c/wordle-answers-alphabetical.txt', header=FALSE)
        write.csv(win_dict,'wb_winwords.csv', row.names = FALSE)
      }
      win_dict <<- toupper(win_dict$V1)
    }
    word_dict$X1 <- toupper(word_dict$X1)
    if (exists('debg')) {  print('word_dict look like this:') }
    if (exists('debg')) { print(head(word_dict)) }
    if (exists('debg')) { print('now initiating') }
    word_dict <<- word_dict
    if (exists('forceword')) {
      play_dict <<- toupper(wordle_dict)
    } else {
      play_dict <<- word_dict$X1
    }
    initiate()
  }
  
  
  initiate <- function() {
    #Initial values of df_rV
    if (exists('debg')) { print('Initiating') }
    if (exists('debg')) { print('Choosing playword') }
    if (exists('forceword')) {
      playword <- forceword
    } else { 
      playword <- sample(win_dict, size = 1)
    }
    df_rV$playword <- playword 
    if (exists('debg')) { print('resetting table within df_rV') }
    df_rV$table <- matrix(str_split(df_rV$playword, '')[[1]], nrow = 1) %>%  as.data.frame()
    df_rV$cols <- data.frame()
    # df_rV$yels1 <- df_rV$yels2 <- df_rV$yels3 <- df_rV$yels4 <- df_rV$yels5 <- df_rV$yels6 <- NULL

    if (exists('debg')) { print('resetting current') }
    df_rV$table['current',] <- NA
    df_rV$keyboard <- data.frame(row.names = c(LETTERS,'Nje', 'Ñ', 'Ü', 'Ä', 'Ö', 'Oe', 'Ae', 'Ue'), 'col' = rep('#dadada', 34))

    if (exists('debg')) { print('choosing start player') }
    sprob <- apply(df_rV$score[,2:3],1, function(x) if (x[1] == 'User' && x[2] %in% c(2,4,6)) { 'AI' } else { 'User'}) %>% table() %>% as.numeric()
    if (length(sprob) != 2) {
      sprob <- c(1,1)
    }
    sprob[is.na(sprob)] <- 10
    if (exists('debg')) { print(paste0('probability user:', sprob[1] , ', AI: ',sprob[2])) }
    outo <- paste0('probability user:', sprob[1] , ', AI: ',sprob[2])
    cat(file = stderr(), paste( outo, '\n'))
    start_user <- sample(c(TRUE, FALSE),1, prob = sprob)
    df_rV$start_user <- start_user
    df_rV$user_rounds <- ifelse(rep(df_rV$start_user, 3), paste0('guess',c(1,3,5)), paste0('guess',c(2,4,6)))
    
    game <- max(c(1,df_rV$score$Game))
    if (game == 1) {
      #this is true upon starting the app
      df_rV$message <- ifelse(start_user, mes.df['Welcome', paste0(df_rV$lang,'U')], mes.df['Welcome', paste0(df_rV$lang,'A')])
    } else {
      df_rV$message <- ifelse(start_user,
                              paste0(mes.df['gameno1', paste0(df_rV$lang, 'U')], game + 1, '. ', mes.df['gameno2', paste0(df_rV$lang, 'U')]),
                              paste0(mes.df['gameno1', paste0(df_rV$lang, 'A')], game + 1, '. ', mes.df['gameno2', paste0(df_rV$lang, 'A')]))
    }
    
    if (exists('debg')) { print(paste('probability of current word:', word_dict[word_dict$X1 == playword, c("X2", "X4")])) }
    df_rV$won <- FALSE
    df_rV$updateSquares <- FALSE
    df_rV$dfo <- NULL
    cat(file = stderr(), paste('----->', playword, '<-----\n'))
    if (exists('debg')) { 
      if (exists('debg')) { print(paste('----->', playword, '<-----')) }
      print('doing guess after initiate')
    }
    guess()
  }
  
  
  if (!exists('debg')) {
    shinyalert("Welcome to Wordle Battle!", text = HTML("It works like normal Wordle, but you play against an KI, and you take turns in guessing. \n\nIf you or the KI get to guess first is randomly chosen. \n\nFirst one to guess the word wins. \n\n Let the battle begin."), 
               closeOnClickOutside = TRUE, 
               showConfirmButton = TRUE)
  }
  #first population of df_rV
  if (exists('debg')) { print('making df_rV') }
  df_rV <- reactiveValues()
  df_rV$score <- data.frame('Game' = 1, 'Winner' = NA, 'Winround' = NA, '1' = NA, '2' = NA, '3' = NA, '4' = NA, '5' = NA, '6' = NA)

  output$title <- renderUI({
    if (exists('debg')) { print('hsv') }
    if (exists('word_dict') && is.data.frame(word_dict)) {
      HTML(paste('<div style = "text-align:center; line-height:1"><strong><big><big>&nbsp&nbsp&nbsp&nbspWORDLE&nbsp&nbsp<font color="', ColorPalette::hsv2rgb(359.99 * as.numeric(word_dict[word_dict$X1 == df_rV$playword, "X2"]),1,1), '">BATTLE&nbsp&nbsp&nbsp&nbsp</font></big></big></strong></div>'))
    } else {
      HTML(paste('<div style = "text-align:center; line-height:1"><strong><big><big>&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbspWORDLE&nbsp&nbsp<font color="black">BATTLE&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp<a href="https://www.fuerstlab.com" target="_blank"> <i class="fa fa-question-circle"></i></a></font></big></big></strong></div>'))
    }
  })
  
  output$message <- renderUI({
    if (exists('debg')) { print('rendering message') }
    HTML('<strong><font color="#484c4d">',df_rV$message,'</font><strong>')
  })
  
  
  is_yellow <- function(cword, pword) {
    pchars <- str_split(pword, '')[[1]]
    cchars <- str_split(cword, '')[[1]]
    out <- rep(FALSE, 5)
    for (i in 1:length(cchars)) {
      if (cchars[i] == pchars[i]) {
        pchars[i] <- 'is_green'
      }
    }
    for (i in 1:length(cchars)) {
      if (pchars[i] != 'is_green' && cchars[i] %in% pchars) {
        pos <- which(pchars == cchars[i])[1]
        out[i] <- TRUE
        pchars[pos] <- 'is_yellow'
      }
    }
    out
  }
  
  is_green <- function(cword, pword) {
    pchars <- str_split(pword, '')[[1]]
    cchars <- str_split(cword, '')[[1]]
    out <- rep(FALSE, 5)
    for (i in 1:length(pchars)) {
      if (cchars[i] == pchars[i]) {
        out[i] <- TRUE
      }
    }
    out
  }
  
  guessnum <- function() {
    no <- grep('^guess', rownames(df_rV$table), value = T) %>% str_extract('[0-9]') %>% as.numeric()
    max(c(1,no + 1))
  }
  
  update_keyboard <- function() {
    df_rV$keyboard[df_rV$cols[df_rV$cols$col == 'grey50', 'letter'], 'col'] <- '#767676'
    df_rV$keyboard[df_rV$cols[df_rV$cols$col == '#baac00', 'letter'], 'col'] <- '#baac00'
    df_rV$keyboard[df_rV$cols[df_rV$cols$col == '#08ac00', 'letter'], 'col'] <- '#08ac00'
    if (exists('debg')) { print('updated keyboard') }
    if (exists('debg')) { print(paste('df_rV$won is now', df_rV$won)) }
  }
  
  
  new_ys <- function(df, start) {
    if (exists('debg')) { print(df) }
    y.df <- df[grepl('^match', rownames(df)),]
    names.df <- df[grepl('^guess', rownames(df)),]
    if (start == 1) {
      vec<- c(3,5)
      ye1 <- sum(as.logical(y.df[1,]))
    } else if (start == 2) {
      vec <- c(2,4,6)
      ye1 <- c()
    } else {
      return()
    }

    vec <- vec[vec <= sum(!is.na(y.df$V1))]
    tabo <- as.logical(c(t(y.df)))
    names(tabo) <- c(t(df[grepl('^guess', rownames(df)),]))

    ron <- c()
    for (ro in vec) {
      for (co in 1:5) {
        cur <- y.df[ro,co]
        names(cur) <- names.df[ro,co]
        prev <- tabo[1:(((ro - 1) * 5) + co -1)]
        if (as.logical(cur) && !names(cur) %in% names(prev)) {
          ron <- c(ron, TRUE)
        }
      }
    }
    sum(c(ye1, ron))
  }



  new_gs <- function(df, start) {
    g.df <- df[grepl('^exactmatch', rownames(df)),]
    if (start == 1) {
      vec <- c(3,5)
      gr1 <- sum(as.logical(g.df[1,]))
    } else if (start == 2) {
      vec <- c(2,4,6)
      gr1 <- c()
    } else {
      return()
    }
    vec <- vec[vec <= nrow(g.df)]
    ron <- c()
    for (ro in vec) {
      for (co in 1:5) {
        cur <- as.logical(g.df[ro,co])
        prev <- as.logical(g.df[1:(ro-1),co])
        if (cur && all(!prev)) {
          ron <- c(ron, TRUE)
        }
      }
    }
    sum(c(gr1, ron))
  }

  check_win <- function(guess, guessno, who) {
    df_rV$updateSquares <- TRUE
    out <- FALSE
    df_rV$message <- paste0(ifelse(who == 'User', mes.df['Guessed', paste0(df_rV$lang,'U')], mes.df['Guessed', paste0(df_rV$lang,'A')]), guess, '".')
    if (exists('debg')) { print(paste('checking win round', guessno)) }
    if (all(as.logical(df_rV$table[paste0('exactmatch',guessno), ]))) {
      df_rV$won <- TRUE
      out <- TRUE
      if (who == 'AI') {
        df_rV$message <- paste0(mes.df['lost1', paste0(df_rV$lang,'A')], guessno, mes.df['lost2', paste0(df_rV$lang,'A')], ifelse(guessno == 1, '', ifelse(df_rV$lang == 'De', 'n', 's')), mes.df[paste0('lost3', guessno), paste0(df_rV$lang,'A')])
        
        if (exists('debg')) { print('ai won') }
      } else {
        df_rV$message <- paste0(mes.df['lost1', paste0(df_rV$lang,'U')], guessno, mes.df['lost2', paste0(df_rV$lang,'U')], ifelse(guessno == 1, '', ifelse(df_rV$lang == 'De', 'n', 's')), mes.df[paste0('lost3', guessno), paste0(df_rV$lang,'U')])
        if (exists('debg')) { print('user won') }
      }
      
      if (exists('debg')) { print('now setting p5') }
      df_rV$p5 <- data.frame(col = c('#baac00', '#08ac00'),
                             c(new_ys(df_rV$table, 1), new_gs(df_rV$table, 1)),
                             c(new_ys(df_rV$table, 2), new_gs(df_rV$table, 2)))
      colnames(df_rV$p5)[2:3] <- c(ifelse(df_rV$start_user, 'User', 'AI'), ifelse(df_rV$start_user, 'AI', 'User'))
      df_rV$p5 <- pivot_longer(df_rV$p5, cols =2:3)
      df_rV$p5[is.na(df_rV$p5)] <- 0
      df_rV$p5 <- df_rV$p5[order(rev(df_rV$p5$col), df_rV$p5$name),]
      if (exists('debg')) { print('df_rV$p5$value is __________________________________________________________') }
      if (exists('debg')) { print(df_rV$p5$value) }
  
      if (is.na(df_rV$score[1,2])) {
        if (exists('debg')) { print('first round score') }
        df_rV$score[nrow(df_rV$score) + 0, c(1:(3+ guessno), 10:13)] <- c(1,who, guessno, apply(df_rV$table[grep('^guess', rownames(df_rV$table)),], 1, function(x) paste0(x, collapse='')), df_rV$p5$value)
      } else {
        if (exists('debg')) { print('non first round score') }
        df_rV$score[nrow(df_rV$score) + 1, c(1:(3+ guessno), 10:13)] <- c(as.numeric(df_rV$score[nrow(df_rV$score),'Game']) + 1,who, guessno, apply(df_rV$table[grep('^guess', rownames(df_rV$table)),], 1, function(x) paste0(x, collapse='')), df_rV$p5$value)
      }
      # fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
      # cat(file=stderr(), paste0("WD: ",getwd(), "\n"))
      if (dir.exists('/home/shiny/ScoreLogs/')) {
        write_csv(df_rV$score, file.path('/home/shiny/ScoreLogs/',paste0(df_rV$userID,'.csv')))
      }
      
      # drop_upload(file.path('/home/shiny/ScoreLogs/',fileName), 'WB_ONLINE_DATA_COLLECTED')
      if (exists('debg')) { print('score done') }
      show("restart")
    } else {
      if (exists('debg')) { print('nobody won in this round') }
      if (guessno == 6) {
        df_rV$won <- TRUE
        wikurl <- paste0('https://', tolower(df_rV$lang), '.wiktionary.org/wiki/', tolower(df_rV$playword))
        wikurl <- paste0('https://', tolower(df_rV$lang), '.wiktionary.org/wiki/', 
                         ifelse(httr::GET(wikurl)$status_code == 404,
                                paste0(toupper(str_extract(df_rV$playword, '^.')), tolower(str_extract(df_rV$playword, '....$'))),
                                paste0(str_extract(df_rV$playword, '^.'), tolower(str_extract(df_rV$playword, '....$')))))
        df_rV$message <- paste0(mes.df['noone1', paste0(df_rV$lang, 'U')], '<a href ="', wikurl, '" target="_blank">', df_rV$playword, '</a>', mes.df['noone2', paste0(df_rV$lang, 'U')])
        df_rV$score[nrow(df_rV$score) + 1, 1:(3+ guessno)] <- c(as.numeric(df_rV$score[nrow(df_rV$score),'Game']) + 1,'Noone', 6, apply(df_rV$table[grep('^guess', rownames(df_rV$table)),], 1, function(x) paste0(x, collapse='')))
        if (exists('debg')) { print('nobody won') }
        show("restart")
      }
    }
    df_rV$score$Game <- as.numeric(df_rV$score$Game)
    df_rV$table['current',] <- NA
    out
  }
  
  yels <- function(x) {
    df <- x[grep("^guess",rownames(x)),]
    chars <- c(t(df))
    ylogicals <- as.logical(c(t(x[grep("^match",rownames(x)),])))
    glogicals <- as.logical(c(t(x[grep("^exactmatch",rownames(x)),])))
    chars[!ylogicals] <- NA
    chars[glogicals] <- NA
    as.data.frame(matrix(chars, nrow= nrow(df), byrow = T))
  }
  
  
  aiguess <- function(no, newletters = NULL, out = 'sample') {
    no <- no -1
    df <- df_rV$table[1:((no * 3) + 2),]
    if (no == 0) {
      if (out == 'sample') {
        firstguess()
      } else if (out == 'dict') {
        play_dict
      } else if (out == 'length') {
        length(play_dict)
      } else if (out == 're') {
        ''
      }
    } else {
      if (!is.null(newletters)) {
        re <- unique(c(t(df[grep('guess', rownames(df)),]))) %>% paste0(., collapse = '') %>% paste0('[', ., ']')
        if (newletters == 'goodletters') {
          if (!exists('w')) {
            w <- read.csv('startword.csv')
          }
          remain_dict <- w[!grepl(re, toupper(w$wordle_dict), perl = T),]
          remain_words <- toupper(remain_dict$wordle_dict)
        } else {
          remain_words <- play_dict[!grepl(re, play_dict)]
        }
        if (out == 'sample') {
          if (newletters == 'goodletters') {
            remain_dict$prob <- round(scales::rescale((remain_dict$sum_uniqs * remain_dict$sumf) ** remain_dict$unique, c(0,100)), 5)
            sample(remain_words,1, prob = remain_dict$prob)
          } else {
            sample(remain_words,1)
          }
        } else if (out == 'dict') {
          remain_words
        } else if (out == 'length') {
          length(remain_words)
        } else if (out == 're') {
          re
        }
      } else {
        reA <- lapply(1:no, function(x) as.character(df[paste0('guess',x), which(as.logical(df[paste0('match',x),]))])) %>% unlist() %>% unique()
        reB <- lapply(1:no, function(x) as.character(df[paste0('guess',x), which(!as.logical(df[paste0('match',x),]))])) %>% unlist() %>% unique()
        reC <- lapply(1:no, function(x) as.character(df[paste0('guess',x), which(as.logical(df[paste0('exactmatch',x),]))])) %>% unlist() %>% unique()
        
        reB <- reB[!reB %in% intersect(reB,reA)]
        reB <- reB[!reB %in% intersect(reB,reC)]
        
        reA <- gsub('([A-ZÖÄÜÑ])', '(?=.*?\\1)', reA)
        reB <- gsub('([A-ZÖÄÜÑ])', '(?!.*?\\1)', reB)
        
        re <- paste0(c(reA,reB), collapse='')
        
        for (i in 1:no) {
          yandg <- c(as.character(df[paste0('guess', i), as.logical(df[paste0('exactmatch', i),])]), df[paste0('guess', i), as.logical(df[paste0('match', i),])]) %>% unlist() %>% table() %>% as.matrix() %>% as.data.frame() %>% rownames_to_column(var = 'letter')
          yandg <- yandg[yandg$V1 > 1,]
          for (o in seq_len(nrow(yandg))) {
            re <- gsub(paste0("\\(\\?\\=\\.\\*\\?", as.character(yandg[o,"letter"]), "\\)", collapse = ''), paste0('(?=.*?(', as.character(yandg[o,"letter"]), '.*){', yandg[o,"V1"], '})', collapse = ''), re, perl = T)
          }
        }
        
        re2 <- paste0('[^',as.character(apply(yels(df),2, function(x) paste0(x[!is.na(x)], collapse = ''))), ']')
        re2[re2 == "[^]"] <- "."
        for (i in 1:no) {
          re2[as.logical(df[paste0('exactmatch',i),])] <- as.character(df[paste0('guess', i), as.logical(df[paste0('exactmatch',i),])])
        }
        refull <- paste0(re,paste(re2, collapse=''))
        
        guessai <- grep(refull,play_dict, value=T, perl = T)
        guessai <- guessai[!guessai %in% apply(df[grep('^guess', rownames(df)),], 1, function(x) paste0(x, collapse = ''))[1:no]]
        
        if (out == 'sample') {
          sample(guessai,1)
        } else if (out == 'dict') {
          guessai
        } else if (out == 'length') {
          length(guessai)
        } else if (out == 're') {
          refull
        }
      }
    }
  }
  
  
  guess <- function() {
    #cat('\ndoing guess()\n')
    last_guess <- c('..0', rownames(df_rV$table)) %>% str_extract('(?<=..)[0-9]') %>% as.numeric() %>% max(na.rm = TRUE)
    guessno <- last_guess +1
    cur <- paste0(df_rV$table['current',], collapse = '')
    if (exists('debg')) { print(paste('guess is ', guessno, 'current is', cur)) }
    df <- df_rV$table
    if (exists('debg')) { print(df) }
    if (guessno == 1 && !df_rV$start_user) {
      #first guess by AI, this is the only case when AI plays first within guess(); 
      if (exists('debg')) { print('this is round 1 and the ai starts') }
      guess <- firstguess()
      if (exists('debg')) { print(paste('ai guesses as first word', guess)) }
      df[paste0('guess', guessno), ] <- str_split(guess, '')[[1]]
      df[paste0('exactmatch', guessno), ] <- is_green(guess, df_rV$playword)
      df[paste0('match', guessno), ] <- is_yellow(guess, df_rV$playword)

      if (exists('debg')) { print('disabling') }
      delay(22,disable("keyb"))
      delay(1000, df_rV$table['current', 1] <- str_split(guess, '')[[1]][1])
      delay(1400, df_rV$table['current', 2] <- str_split(guess, '')[[1]][2])
      delay(1800, df_rV$table['current', 3] <- str_split(guess, '')[[1]][3])
      delay(2200, df_rV$table['current', 4] <- str_split(guess, '')[[1]][4])
      delay(2600, df_rV$table['current', 5] <- str_split(guess, '')[[1]][5])
      # 
      delay(3000, {
        if (exists('debg')) { print(paste('guess dr_rV$table change for guess', guessno)) }
        df_rV$table <- df
        if (exists('debg')) { print(paste('check_win() with guessno', guessno)) }
        check_win(guess, guessno, 'AI')

      })
      
    } else {
      if (sum(!is.na(df_rV$table['current',])) == 5) {
        guess <- cur
        if (exists('debg')) { print(paste('user submitted', cur)) }
        df[paste0('guess', guessno), ] <- str_split(guess, '')[[1]]
        df[paste0('exactmatch', guessno), ] <- is_green(guess, df_rV$playword)
        df[paste0('match', guessno), ] <- is_yellow(guess, df_rV$playword)

        if (exists('debg')) { print(paste('guess dr_rV$table change for guess', guessno)) }
        df_rV$table <- df
        
        if (!check_win(guess, guessno, 'User')) {
          guessno <- guessno + 1
          if (guessno < 7) {
            #next guesses the  AI
            if (exists('debg')) { cat('\nnow ai guesses\n') }
            if (aiguess(guessno, out = 'length') > 50) {
              guess <- aiguess(guessno, newletters = 'goodletters')
            } else {
              guess <- aiguess(guessno)
            }
            if (exists('debg')) { print(paste('ai guesses', guess)) }
            df[paste0('guess', guessno), ] <- str_split(guess, '')[[1]]
            df[paste0('exactmatch', guessno), ] <- is_green(guess, df_rV$playword)
            df[paste0('match', guessno), ] <- is_yellow(guess, df_rV$playword)

            if (exists('debg')) { print('disabling') }
            delay(22,disable("keyb"))
            delay(1000, df_rV$table['current', 1] <- str_split(guess, '')[[1]][1])
            delay(1400, df_rV$table['current', 2] <- str_split(guess, '')[[1]][2])
            delay(1800, df_rV$table['current', 3] <- str_split(guess, '')[[1]][3])
            delay(2200, df_rV$table['current', 4] <- str_split(guess, '')[[1]][4])
            delay(2600, df_rV$table['current', 5] <- str_split(guess, '')[[1]][5])
            # 
            delay(3000, {
              df_rV$table <- df
              
              if (exists('debg')) { print(paste('check_win() with guessno', guessno)) }
              check_win(guess, guessno, 'AI')
            })
          } 
        }
      } 
    }
  }
  
  observeEvent(df_rV$cols, {
    if (sum(is.na(df_rV$table['current',])) == 5 || sum(is.na(df_rV$table['current',])) == 0) {
      update_keyboard()
      if (exists('debg')) { print('----------------------------------------------------------------------------change') }
    }
  })
  
  observeEvent(df_rV$table, {
    if (sum(is.na(df_rV$table['current',])) == 5 || sum(is.na(df_rV$table['current',])) == 0) {
      if (exists('debg')) { print(max(df_rV$score$Game)) }
      if (exists('debg')) { print(paste('fixing yellows, sum is',sum(is.na(df_rV$table['current',])))) }
      # if (exists('debg')) { print(df_rV$table['current',]) }
      # if (exists('debg')) { print('---') }
      # if (exists('debg')) { print(df_rV$table) }
      if (guessnum() - 1 > 0) {
        # print('changing yels from')
        if (exists('debg')) { print(df_rV$table[paste0('match', guessnum() -1),]) }
        if (exists('debg')) { print('to') }
        # if (exists('debg')) { print(df_rV[[paste0('yels', guessnum() -1)]]) }
        if (exists('debg')) { print(paste('using guessnum() -1 = ', guessnum() -1)) }
        # df_rV$table[paste0('match', guessnum() -1),] <- df_rV[[paste0('yels', guessnum() -1)]]
      }
      # cat('\n\n\n')
    }
  })

  output$squares <- renderPlot({
    if (exists('debg')) { 
      print('making squares') 
      print(df_rV$table[grep('^match', rownames(df_rV$table)),]) 
    }
    df <- df_rV$table[c('guess1', 'guess2', 'guess3', 'guess4', 'guess5', 'guess6'),] 
    cur <- df_rV$table[ 'current',] 
    row.names(df) <- c('guess1', 'guess2', 'guess3', 'guess4', 'guess5', 'guess6')
    colnames(df) <- 1:5
    if (nrow(df_rV$table) > 2) {
      curno <- guessnum() -1
    } else {
      curno <- 0
    }
    if (curno < 6) {
      df[curno + 1,] <- cur
    }
    # if (!is.null(df_rV$dfo) && ((!all(is.na(cur))) || (df_rV$start_user && curno %in% c(1,3,5) && !all(is.na(cur))) || (!df_rV$start_user && curno %in% c(0,2,4,6) && !all(is.na(cur))))) {
    if (!df_rV$updateSquares && !is.null(df_rV$dfo)) {
      df <- df_rV$dfo 
      df[grep(paste0('guess', guessnum()), df$rowname), 'letter'] <- c(as.character(cur[which(!is.na(cur))]), rep('', length(which(is.na(cur)))))
      df[grep(paste0('guess', guessnum()), df$rowname), 'text'] <- 'black'
      df[grep(paste0('guess', guessnum()), df$rowname), 'sqcol'] <- ifelse(all(!is.na(cur)),'grey50', 'grey85')
      
    } else {

      enable('keyb')
      df <- df %>% rownames_to_column() %>% pivot_longer(cols = 2:6, names_to = 'x', values_to = 'letter') %>% as.data.frame()
      df$y <- gsub('[a-z]', '', df$rowname) %>% as.numeric()
      df$col <- 'grey50'
      df$sqcol <- 'grey85'
      
      gr_which <- df_rV$table[grep('exactmatch', rownames(df_rV$table)),] %>% t() %>% c() %>% as.logical()
      y_which <- df_rV$table[grep('^match', rownames(df_rV$table)),] %>% t() %>% c() %>% as.logical()
      
      df[!is.na(df$letter), 'sqcol'] <- 'grey50'
      df[1:length(gr_which),'col'][gr_which] <- '#08ac00'
      df[1:length(gr_which),'sqcol'][gr_which] <- '#08ac00'
      df[1:length(y_which),'col'][y_which] <- '#baac00'
      df[1:length(y_which),'sqcol'][y_which] <- '#baac00'
      
      df$text <- 'white'
      df[df$y == curno +1, 'text'] <- 'black'
      
      df[is.na(df$letter), 'col'] <- 'white'
      df$x <- as.numeric(df$x)
      df$y <- as.numeric(df$y)
      df[is.na(df)] <- ''
      df[df$rowname == paste0("guess", curno +1), 'col'] <- 'white'
      
      df$Player <- 'AI'
      df[df$rowname %in% df_rV$user_rounds, 'Player'] <- 'U'
      df$PlayerC <- '#4eadd3'
      df[df$rowname %in% df_rV$user_rounds, 'PlayerC'] <- '#5214b8'
      
      df$PlayerF <- df$PlayerC
      df[df$y > curno,'PlayerF'] <- str_replace(df[df$y > curno,'PlayerC'], '$', '06')
      df[df$y <= curno,'PlayerC'] <- 'white'
      
      df$outlCur <- 0.01
      df[df$y == curno +1, 'outlCur'] <- 2
      
      df_rV$cols <- df
      dfo <<- df
      dfu <<- df_rV$table

      df_rV$dfo <- df

      dfrv <<- reactiveValuesToList(df_rV)
      
      #plog(paste('!!!!', "longcut", '!!!!'))
      #plog('yellows I just processed are:')
      #plog(df[(((guessnum() -1) * 5) - 4):((guessnum() -1) * 5),'col'])
      #plog(paste0('yels', guessnum() -1, ' is:'))
      #plog(df_rV[[paste0('yels', guessnum() -1)]])
      #plog('agreement')
      #plog('------------------')
      #plog((df[(((guessnum() -1) * 5) - 4):((guessnum() -1) * 5),'col'] == '#baac00') == df_rV[[paste0('yels', guessnum() -1)]])
      #plog('------------------')
      #plog(' \n ')
      
      
    }
    wleft <- grep(paste0('^', paste0(cur[!is.na(cur)], collapse = '')),play_dict, value = T)
    left.df <- data.frame(x = 0.35, y= 1:6, "label" = c(rep('', min(5,guessnum() -1)), ifelse(guessnum() == 7, '', length(wleft)), rep('', max(0, 6 - guessnum()))))

    if (exists('debg')) { print('rendering plot') }
    


    p1 <- ggplot(df) +
      geom_text(data = left.df, aes(x = x, y= y, label = label), size = 3.6, fontface = 'bold', color = 'grey50', angle = 90) +
      geom_tile(aes(x=x, y=y, fill = col), color = 'white', size=2) +
      geom_rect(aes(xmin = x - 0.45, xmax = x + 0.45, ymin = y - 0.45, ymax = y + 0.45, colour = sqcol), size = 0.8, fill ='transparent') +
      geom_point(aes(x=5.85, y=y, color = PlayerC, fill = PlayerF, stroke = outlCur), shape = 21, size=11) +
      geom_text(aes(x=5.85, y=y, label = Player, color = PlayerC), size=5) +
      geom_text(aes(x=x, y=y, label = letter, color = text), size=12, fontface ='bold') +
      scale_y_reverse() +
      expand_limits(x=6.2) +
      scale_fill_identity() +
      scale_color_identity() +
      coord_fixed() +
      theme_void() +
      theme(plot.margin=unit(c(-0.4,-0.55,-0.4,-0.55), 'cm'))
    
    df_rV$updateSquares <- FALSE
    suppressWarnings(p1)
  
  })
  
  
  output$out <- renderPlot({
    if (exists('debg')) { print('making stat plots') }
    plog(df_rV$score[1,])
    if (!is.na(df_rV$score[1,2])) {
      p <- df_rV$score 
      p$Winround <- as.numeric(p$Winround)
      p <- p[!is.na(p$Winner),1:3]
      
      p2 <- rbind(p, c(1, 'User', NA), c(1, 'AI', NA), c(1, 'Noone', NA))
      p2 <- p2 %>% count(Winner)
      p2$n <- p2$n -1
      
      g1 <- ggplot(p2) +
        geom_col(aes(x=factor(Winner, levels = c('Noone','AI', 'User')), y= n, fill = Winner),show.legend = F) +
        scale_fill_manual(values = c('User' = '#5214b8', 'AI' = '#4eadd3', 'Noone' = '#d5e0d8')) +
        scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))), expand = c(0,0)) +
        scale_x_discrete(expand=c(0,0)) +
        xlab('') +
        coord_flip() +
        ggtitle(label = 'Overall wins') +
        theme_minimal() +
        theme(
          plot.margin = unit(c(1,1,2,0), "lines"),
          panel.background = element_rect(color = 'grey92'),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 13),
          axis.title = element_blank(),
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
        )
      
      p3 <- rbind(p, c(1, 'User', 0), c(1, 'AI', 0), c(1, 'Noone', 0))
      p3$Winround <- as.numeric(p3$Winround)
      p3 <- p3[p3$Winner %in% c('User', 'AI'),]
      if (nrow(p3) > 0) {
        g2 <- ggplot(p3) +
          geom_histogram(aes(x = Winround,y=..count.., fill = Winner), 
                         show.legend = F, position = 'dodge', color = 'black', bins = 8, size=0.1) +
          scale_x_reverse(limits = c(7,0), breaks=c(7.001,6:1, -0.001), expand = c(0,0)) +
          scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1), 3))), expand=c(0,0)) +
          scale_fill_manual(values = c('User' = '#5214b8', 'AI' = '#4eadd3')) +
          facet_grid(~Winner, scales = 'free') +
          ggtitle(label = 'Win distribution by rounds') +
          coord_flip() +
          theme(
            plot.margin = unit(c(0,1,2,2.1), "lines"),
            panel.spacing = unit(2.5, "lines"),
            panel.background = element_rect(fill = 'grey99', color = 'grey95'),
            strip.background = element_rect(fill = 'grey94', color = 'grey90'),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color='grey80'),
            axis.text = element_text(size = 11),
            axis.title = element_blank(),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
          )
        mu <- p3[p3$Winner == 'User' & p3$Winround != 0, ]
        mum <- mean(mu$Winround)
        if (nrow(mu) > 0) {
          g2 <- g2 + geom_vline(data = mu, aes(xintercept = mum), color = 'grey50', linetype = 'dashed') +
                     geom_text(data = mu, aes(x=mum - 0.6, y= 0.91*max(table(mu$Winround)), label = round(mum,2)), 
                               color = 'grey50', size = 3.5)
        }
        ma <- p3[p3$Winner == 'AI' & p3$Winround != 0, ]
        mam <- mean(ma$Winround)
        if (nrow(ma) > 0) {
          g2 <- g2 + geom_vline(data = ma, aes(xintercept = mam), color = 'grey50', linetype = 'dashed') +
            geom_text(data = ma, aes(x=mam - 0.6, y= 0.91*max(table(ma$Winround)), label = round(mam,2)), 
                      color = 'grey50', size = 3.5)
        }
        
        p4 <- apply(df_rV$score[,2:3],1, function(x) if (x[1] == 'User' && x[2] %in% c(2,4,6)) { 'AI' } else { 'User'}) %>% table() %>% as.data.frame()
        colnames(p4) <- c('Starter', 'n')
        g3 <- ggplot(p4) +
          geom_col(aes(x=factor(Starter, levels = c('AI', 'User')), y= n, fill = Starter),show.legend = F) +
          scale_fill_manual(values = c('User' = '#5214b8', 'AI' = '#4eadd3')) +
          scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(p4$n) + 1) * 1.1)))), expand = c(0,0)) +
          scale_x_discrete(expand=c(0,0)) +
          xlab('') +
          coord_flip() +
          ggtitle(label = 'First round stats') +
          theme_minimal() +
          theme(
            plot.margin = unit(c(0,1,2,0.4), "lines"),
            panel.background = element_rect(color = 'grey92'),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text = element_text(size = 13),
            axis.title = element_blank(),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
          )
        # print('now plotting p5')
        
        p5 <- data.frame(col= c('#baac00','#baac00','#08ac00', '#08ac00'), name=c('User', 'AI', 'User', 'AI'), 
                         value =c(sum(as.numeric(df_rV$score$YUser), na.rm = T),
                                  sum(as.numeric(df_rV$score$YAI), na.rm = T),
                                  sum(as.numeric(df_rV$score$GUser), na.rm = T),
                                  sum(as.numeric(df_rV$score$GAI), na.rm = T)))
        if (exists('debg')) { print('plot') }
        if (exists('debg')) { print(p5) }
        g4 <- ggplot(p5) + 
          geom_col(aes(x=name, y=value, fill=col), position = 'dodge', width = 0.65) +
          scale_fill_identity() +
          scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(p5$value) + 1) * 1.1)))), expand = c(0,0)) +
          scale_x_discrete(expand=c(0,0)) +
          xlab('') +
          coord_flip() +
          ggtitle(label = 'New letters discovered') +
          theme_minimal() +
          # facet_wrap(~name, nrow = 1) + 
          theme(
            plot.margin = unit(c(0,1,0.1,0.4), "lines"),
            panel.background = element_rect(color = 'grey92'),
            panel.grid.major.y = element_blank(),
            strip.background = element_rect(fill = 'grey94', color = 'grey90'),
            panel.grid.minor.x = element_blank(),
            panel.spacing = unit(2.5, "lines"),
            axis.text = element_text(size = 13),
            axis.title = element_blank(),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
          )
        if (df_rV$won) {
          p6 <- data.frame("Round" = 1:(max(which(!is.na(df_rV$score[nrow(df_rV$score), 4:9])))))
          p6$n <- apply(p6, 1, function(x) aiguess(x['Round'], out = 'length'))
          p6$y1 <- (max(p6$n) /2) - (p6$n /2)
          p6$y2 <- (max(p6$n) /2) + (p6$n /2)

          p7 <- pivot_longer(p6, cols = 3:4)
          p7$value <- as.numeric(p7$value)

          p8 <- cbind(p6,apply(p6, 1, function(x) aiguess(x['Round'], out = 'dict')[1:40]) %>% t())
          p8[,5:44] <- apply(p8[,5:44],1, function(x) if (!any(is.na(x))) { sample(x) } else { rev(x) } ) %>% t()
          repl <- apply(p6, 1, function(x) ifelse(aiguess(x['Round'], out = 'length') > 40, '...', NA))
          p8[1:length(repl[!is.na(repl)]),5] <- repl[!is.na(repl)]
          p8$y1 <- as.character(p8$y1)
          p8$y2 <- as.character(p8$y2)
          p9 <- pivot_longer(p8, cols = 5:44)

          if (max(p7$Round) > 1) {
            lims <- as.numeric((p7[p7$Round ==2, 'value'] %>% unlist()) + ((p7[p7$Round ==2, 'n'] %>% unlist()) * c(-0.25,0.25)))
          } else {
            lims <- c(0, length(play_dict))
          }
          
          p9$yw <- rep(seq(lims[1], lims[1] + (lims[2] - lims[1]) * 0.475, length.out = 40), max(p6$Round))
          p7 <- rbind(p7[order(p7$Round),][p7$name == 'y1',], p7[order(p7$Round, decreasing = T),][p7$name == 'y2',])

          plotlist <<- list(p6, p7, p8, p9)
          
          g5 <- ggplot(p7) + 
            geom_area(aes(x=value, y=Round), fill ='#739cfb', position = 'dodge' ,color='blue', size=0.25, show.legend = FALSE) +
            geom_path(aes(x=value, y=Round), color='blue', size=0.25, show.legend = FALSE) +
            geom_text(data = p9, aes(y=Round -0.5, x= yw, label=value), size=1.4, family = 'mono', angle = 90) +
            
            scale_x_continuous( expand = c(0,0)) +
            scale_y_reverse(expand=c(0,0), breaks = 6:1) +
            expand_limits(y = c(-0.1, 6.5)) +

            geom_hline(yintercept = c(1:6), color = 'black', alpha=0.2, size=0.2) +
            geom_text(data = p7[p7$name == 'y2',], aes(y=Round, x = value, label = n), vjust = 0.75, hjust = 'outward', size=3, fontface = 'bold') +
            ylab('Round') +
            ggtitle(label = 'Last round\'s solution funnel') +
            theme_minimal() +
            coord_cartesian(xlim = lims) +
            theme(
              plot.margin = unit(c(0,1,0.1,0.4), "lines"),
              panel.grid.major.y = element_blank(),
              panel.grid = element_blank(),
              axis.text.x = element_blank(),
              panel.spacing = unit(2.5, "lines"),
              axis.text = element_text(size = 11),
              axis.title =element_text(size = 11),
              axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
            )
          # g6 <- g5
          # g6$layers <- g6$layers[1:2]
          # g6 <- g6 +
          #   geom_text(data = p9, aes(x=Round, y= yw, label=value), size=1.32, hjust = 'inward', family = 'mono') +
          #   coord_cartesian(ylim = range(p7[p7$Round == 2, 'value']) + c(- max(p7[p7$Round == 2, 'n']) * 0.2, max(p7[p7$Round == 2, 'n']) * 0.2)) +
          #   ggtitle(label = 'Zoom') +
          #   ylab('') +
          #   theme(axis.text.y = element_text(size =6)) +
          #   geom_text(data = p7[p7$name == 'y2',], aes(x=Round, y = value +1, label = n), vjust = 0, size = 3, hjust = 'inward', fontface = 'bold')
        } 
        if (!exists('g5')) {
          g5 <- NULL
        }

        suppressWarnings(cowplot::plot_grid(g5,g1,g2,g3,g4,nrow =5, rel_heights = c(0.225, 0.20, 0.25, 0.155, 0.17)))
        # suppressWarnings(cowplot::plot_grid(cowplot::plot_grid(g5, g6, nrow=1),g1,g2,g3,g4,nrow =5, rel_heights = c(0.22, 0.20, 0.25, 0.155, 0.175)))
      } else { 
        suppressWarnings(print(g1))
      }
    }
  })
  
  Btntxt <- function(text) {
    HTML(paste('<strong>',text,'</strong>'))
  }
  lh <- '7vh'
  btnbord = 'thin'
  btnwdth = '7.5vw'
  maxbtnwdth = '50px'
  
  output$Q <- renderUI({
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('Q', Btntxt("Q"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:", lh, "; background-color: ",df_rV$keyboard['Q', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white; margin:0, padding:0"))))
    } 
  })
  output$W <- renderUI({
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('W', Btntxt("W"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['W', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$E <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('E', Btntxt("E"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['E', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$R <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('R', Btntxt("R"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['R', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$T <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('T', Btntxt("T"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['T', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$Y <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('Y', Btntxt("Y"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['Y', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$U <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('U', Btntxt("U"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['U', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$I <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('I', Btntxt("I"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['I', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$O <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('O', Btntxt("O"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['O', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$P <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('P', Btntxt("P"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['P', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })  
  output$A <- renderUI({
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('A', Btntxt("A"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['A', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$S <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('S', Btntxt("S"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['S', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$D <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('D', Btntxt("D"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['D', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$F <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('F', Btntxt("F"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['F', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$G <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('G', Btntxt("G"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['G', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$H <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('H', Btntxt("H"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['H', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$J <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('J', Btntxt("J"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['J', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$K <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('K', Btntxt("K"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['K', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$L <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('L', Btntxt("L"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['L', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })  
  output$Nje <- renderUI({    
    if (is.data.frame(df_rV$keyboard) && (df_rV$lang == 'Es')) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('Nje', Btntxt("Ñ"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['Ñ', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    }
  })  
  output$Ph <- renderUI({    
    if (is.data.frame(df_rV$keyboard) && (df_rV$lang == 'De' || df_rV$lang == 'Es')) {
      HTML(paste0('<div style = "width: 3.626vw; max-width:25px">&nbsp</div>'))
    } 
  })  
  output$Oe <- renderUI({    
    if (is.data.frame(df_rV$keyboard) && (df_rV$lang == 'De')) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('Oe', Btntxt("Ö"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['Ö', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })  
  output$Ue <- renderUI({    
    if (is.data.frame(df_rV$keyboard) && (df_rV$lang == 'De')) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('Ue', Btntxt("Ü"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['Ü', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })  
  output$Ae <- renderUI({    
    if (is.data.frame(df_rV$keyboard) && (df_rV$lang == 'De')) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('Ae', Btntxt("Ä"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['Ä', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })  
  output$Z <- renderUI({
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('Z', Btntxt("Z"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['Z', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$X <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('X', Btntxt("X"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['X', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$C <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('C', Btntxt("C"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['C', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$V <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('V', Btntxt("V"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['V', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    }
  })
  output$B <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('B', Btntxt("B"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['B', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))))
    } 
  })
  output$N <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('N', Btntxt("N"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['N', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$M <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('M', Btntxt("M"), style=paste0("width:", btnwdth,"; max-width:", maxbtnwdth,"; color: #111111; line-height:",lh,"; background-color: ",df_rV$keyboard['M', 'col'],"; border-radius: 9px; border-width: ", btnbord, "; border-color:white"))))
    } 
  })
  output$Enter <- renderUI({    
    if (is.data.frame(df_rV$keyboard)) {
      HTML(gsub('id=', ifelse(df_rV$won,'disabled id=', 'id='), actionButton('Enter', Btntxt('ENTER'), style=paste0("width:16vw; max-width:100px; color: #111111; line-height:",lh,"; background-color: #dadada; border-radius: 9px; border-width: ", btnbord, "; border-color:white"), width = "65px", padding='0,0')))
    } 
  })
  output$bs <- renderUI({
    if (is.data.frame(df_rV$keyboard)) {
      actionButton('bs', label = NULL, icon = icon('backspace', style = 'font-size: 2.5ch; line-height:0.3'), style=paste0("width:12vw; max-width:75px; color: #111111; line-height:7.1vh; background-color: #dadada; border-radius: 9px; border-width: ", btnbord, "; border-color:white"), width = "50px")
    } 
  })
  
  observeEvent(input$restart, {
    print('restart pressed, enabling')
    hide('restart')
    initiate()
  })
  
  observeEvent(input$Enter, {
    if (exists('debg')) {
      cat('\n\nUser hit Enter\n\n')
    }
    if (sum(!is.na(df_rV$table['current',])) == 5) {
      if (paste0(df_rV$table['current',], collapse = '') %in% word_dict$X1 || exists('forceword')) {
        guess()
      } else {
        if (exists('debg')) { print(paste0(df_rV$table['current',], collapse = '')) }
        df_rV$message <- paste0('"', paste0(df_rV$table['current',], collapse = ''), (mes.df['noword', paste0(df_rV$lang,'U')]))
      }
    }
  })
  
  observeEvent(input$bs, {
    if (!all(is.na(df_rV$table['current', ]))) {
      df_rV$table['current', max(which(!is.na(df_rV$table['current', ])))] <- NA
    }
  })
  
  observeEvent(input$A, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'A'
    }
  })
  
  observeEvent(input$B, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'B'
    }
  })  
  observeEvent(input$C, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'C'
    }
  })  
  observeEvent(input$D, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'D'
    }
  })  
  observeEvent(input$E, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'E'
    }
  })  
  observeEvent(input$F, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'F'
    }
  })  
  observeEvent(input$G, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'G'
    }
  })  
  observeEvent(input$H, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'H'
    }
  })  
  observeEvent(input$I, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'I'
    }
  })  
  observeEvent(input$J, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'J'
    }
  })  
  observeEvent(input$K, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'K'
    }
  })  
  observeEvent(input$L, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'L'
    }
  })  
  observeEvent(input$M, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'M'
    }
  })  
  observeEvent(input$N, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'N'
    }
  })  
  observeEvent(input$O, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'O'
    }
  })  
  observeEvent(input$P, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'P'
    }
  })  
  
  observeEvent(input$Q, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'Q'
    }
  })  
  observeEvent(input$R, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'R'
    }
  })
  observeEvent(input$S, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'S'
    }
  })
  observeEvent(input$T, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'T'
    }
  })
  observeEvent(input$U, {
    
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'U'
    }
  })
  observeEvent(input$V, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'V'
    }
  })
  observeEvent(input$W, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'W'
    }
  })
  observeEvent(input$X, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'X'
    }
  })
  observeEvent(input$Y, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'Y'
    }
  })
  observeEvent(input$Z, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'Z'
    }
  })
  observeEvent(input$Nje, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'Ñ'
    }
  })
  observeEvent(input$Oe, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'Ö'
    }
  })
  observeEvent(input$Ue, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'Ü'
    }
  })
  observeEvent(input$Ae, {
    if (!all(!is.na(df_rV$table['current',]))) {
      df_rV$table['current', which(is.na(df_rV$table['current',]))[1]] <- 'Ä'
    }
  })
}