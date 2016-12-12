library(PhViD)
library(MCMCpack)

as.PhViD_HCSC <-  function(DATA.FRAME, MARGIN.THRES = 1){
  coln = names(DATA.FRAME)
  DATA.FRAME[[3]] %<>% as.numeric() # leaving as integer can result in overflow during calculations
  threshold_satisfied <- FALSE
  output <- data.frame()
  
  while (!threshold_satisfied) {
    # calculate marginals
    n1._df <- aggregate(DATA.FRAME[3], DATA.FRAME[1] , sum) %>%
      dplyr::filter(count >= MARGIN.THRES) %>%
      dplyr::rename(n1. = count) %>%
      as.data.frame()
    n.1_df <- aggregate(DATA.FRAME[3], DATA.FRAME[2] , sum) %>%
      dplyr::filter(count >= MARGIN.THRES) %>%
      dplyr::rename(n.1 = count) %>%
      as.data.frame()
    
    # generate new count table
    output <- DATA.FRAME %>%
      dplyr::left_join(n1._df, by = coln[1]) %>%
      dplyr::left_join(n.1_df, by = coln[2]) %>%
      # drop all rows for which a marginal doesn't exist
      dplyr::filter(!is.na(n1.) & !is.na(n.1))
    
    if ( identical(dplyr::select(output, 1:3), DATA.FRAME) ) {
      output %<>% dplyr::rename(n11 = count)
      # all marginals satisfy threshold since no entries dropped
      threshold_satisfied = TRUE
    } else {
      # recalculate marginals based on which terms were kept and
      #   check again that marginal threshold is satisified
      DATA.FRAME <- dplyr::select(output, 1:3)
    }
  }
  RES <- vector(mode="list")
  RES$L <- output %>% dplyr::select(1:2)
  RES$data <- output %>% dplyr::select(n11, n1., n.1)
  RES$N <- sum(output$n11)
  RES
}

########## BCPNN_HCSC function to include upper bound of IC and return the actual IC value #################
BCPNN_HCSC <- function(DATABASE, RR0 = 1, MIN.n11 = 1,
                      MC = FALSE, NB.MC = 10000) {
  
  DATA <- DATABASE$data
  N <- DATABASE$N
  L <- DATABASE$L
  
  n11 <- DATA[,1]
  n1. <- DATA[,2] 
  n.1 <- DATA[,3] 
  n10 <- n1. - n11
  n01 <- n.1 - n11
  n00 <- N - (n11+n10+n01)
  E <- n1. * n.1 / N # les counts attendus
  
  if(MIN.n11 > 1) {
    E <- E[n11 >= MIN.n11]
    n1. <- n1.[n11 >= MIN.n11]
    n.1 <- n.1[n11 >= MIN.n11]
    n10 <- n10[n11 >= MIN.n11]
    n01 <- n01[n11 >= MIN.n11]
    n00 <- n00[n11 >= MIN.n11]
    #  LL <- data.frame(drugs=L[,1],events=L[,2],n11)
    #  LL1 <- LL[,1][n11 >= MIN.n11]
    #  LL2 <- LL[,2][n11 >= MIN.n11]
    #  rm(list="L")
    #  L <- data.frame(LL1,LL2)
    L <- L[n11 >= MIN.n11,]
    n11 <- n11[n11 >= MIN.n11]
  }
  
  Nb.Cell <- length(n11)
  
  if (MC == FALSE) {
    post.H0 <- matrix(nrow=Nb.Cell,ncol=length(RR0))
    p1  <- 1 + n1.
    p2  <- 1 + N - n1.
    q1  <- 1 + n.1
    q2  <- 1 + N - n.1
    r1  <- 1 + n11
    r2b <- N - n11 -1 + (2+N)^2/(q1*p1)
    EICb <- log(2)^(-1) * (digamma(r1) - digamma(r1+r2b) - (digamma(p1) - digamma(p1+p2) + digamma(q1) - digamma(q1+q2)))
    VICb <- log(2)^(-2) * (trigamma(r1) - trigamma(r1+r2b) + (trigamma(p1) - trigamma(p1+p2) + trigamma(q1) - trigamma(q1+q2)))
    post.H0 <- pnorm(log(RR0),EICb,sqrt(VICb))
    # Calculation of the Lower Bound
    LB <- qnorm(0.025,EICb,sqrt(VICb))
  }
  
  if (MC == TRUE) { # Advanced option MC
    n1. <- n11 + n10
    n.1 <- n11 + n01
    
    ## Nouvelles priors
    q1. <- (n1. +.5)/(N +1)
    q.1 <- (n.1 +.5)/(N +1)
    q.0 <- (N - n.1 +.5)/(N +1)
    q0. <- (N - n1. +.5)/(N +1)
    
    a.. <- .5/(q1.*q.1) ## le .5 devrait pouvoir être changé
    
    a11 <- q1.*q.1* a..
    a10 <- q1.*q.0* a..
    a01 <- q0.*q.1* a..
    a00 <- q0.*q.0* a..
    
    g11 <- a11 + n11
    g10 <- a10 + n10
    g01 <- a01 + n01
    g00 <- a00 + n00
    g1. <- g11 + g10
    g.1 <- g11 + g01
    
    post.H0 <- vector(length=Nb.Cell)
    LB <- vector(length=Nb.Cell)
    UB <- vector(length=Nb.Cell)
    IC <- vector(length=Nb.Cell)
    for (m in 1 : Nb.Cell){
      p <- rdirichlet(NB.MC,c(g11[m],g10[m],g01[m],g00[m]))
      p11 <- p[,1]
      p1. <- p11 + p[,2]
      p.1 <- p11 + p[,3]	
      IC_monte <- log(p11/(p1.* p.1))
      temp <- IC_monte < log(RR0)
      post.H0[m] <- sum(temp)/NB.MC
      LB[m] <- sort(IC_monte)[round(NB.MC * 0.025)]
      UB[m] <- sort(IC_monte)[round(NB.MC * 0.975)]
      IC[m] <- sort(IC_monte)[round(NB.MC * 0.5)]
    }
    rm(p11,p1.,p.1,temp)
    gc()
  }
  
  # # Need to choose a way to discriminate false/true positive to determine FDR, etc.
  # if (RANKSTAT==1) {
  #   FDR <- (cumsum(post.H0[order(post.H0)]) / (1:length(post.H0)))
  #   FNR <- rev(cumsum((1-post.H0)[order(1-post.H0)])) / (Nb.Cell - 1:length(post.H0))
  #   Se <- cumsum((1-post.H0)[order(post.H0)]) / (sum(1-post.H0))
  #   Sp <- rev(cumsum(post.H0[order(1-post.H0)])) / (Nb.Cell - sum(1-post.H0))
  # }
  # 
  # if (RANKSTAT==2) {
  #   FDR <- (cumsum(post.H0[order(LB,decreasing=TRUE)]) / (1:length(post.H0)))
  #   FNR <- rev(cumsum((1-post.H0)[order(1-LB,decreasing=TRUE)])) / (Nb.Cell - 1:length(post.H0))
  #   Se <- cumsum((1-post.H0)[order(LB,decreasing=TRUE)]) / (sum(1-post.H0))
  #   Sp <- rev(cumsum(post.H0[order(1-LB,decreasing=TRUE)])) / (Nb.Cell - sum(1-post.H0))
  # }
  
  ############################ SORTIE DE LA FONCTION #############################
  RES <- data.frame(L[,1], L[,2], n11, E, (n11/E), n1., n.1, post.H0, LB, UB, IC)
  colnames(RES) <- c("drug code","event effect","count","expected count",
                     "n11/E","drug margin","event margin",
                     "postH0","Q_0.025(log(IC))","Q_0.975(log(IC))","median_IC")
  RES
}

RFET_HCSC <- function(DATABASE, OR0 = 1, MIN.n11 = 1) {
    
    # DATABASE :  object. It is the object returned by the function transform_data. It contains :
    #             DATABASE$PARAM : the parameters used when calling the function transform_data
    #             DATABASE$data :  matrix. The first column of DATA must contain the number of notifications n11, the second
    #                              column the row marges n10 and the third column the column marges n01
    #             DATABASE$N :     Nb de notifications total
    #             DATABASE$L :     LIBELLES
    
    # OR0 :  positive double. The value of the risk you want to consider. By default, OR0=1
    # OR0 <-c(1,2,5) ## seuil pour la definition des associations
    
    require("LBE")
    
    # Initialization              
    DATA <- DATABASE$data
    N <- DATABASE$N
    L <- DATABASE$L
    
    n11 <- DATA[,1]
    n1. <- DATA[,2] # les marges lignes (effets indésirables)
    n.1 <- DATA[,3] # les marges colonnes (médicaments)
    n10 <- n1. - n11
    n01 <- n.1 - n11
    n00 <- N - (n11+n10+n01)
    E <- n1. * n.1 / N # les effectifs attendus
    
    if(MIN.n11 > 1) {
      E <- E[n11 >= MIN.n11]
      n1. <- n1.[n11 >= MIN.n11]
      n.1 <- n.1[n11 >= MIN.n11]
      n10 <- n10[n11 >= MIN.n11]
      n01 <- n01[n11 >= MIN.n11]
      n00 <- n00[n11 >= MIN.n11]
      LL <- data.frame(drugs=L[,1],events=L[,2],n11)
      LL1 <- LL[,1][n11 >= MIN.n11]
      LL2 <- LL[,2][n11 >= MIN.n11]
      rm(list="L")
      L <- data.frame(LL1,LL2)
      n11 <- n11[n11 >= MIN.n11]
    }
    
    Nb.Cell <- length(n11)
    pval.uni <- vector(length=Nb.Cell)
    for (p in 1 : Nb.Cell) {
      pval.uni[p] <-  fisher.test(matrix(c(n11[p],n10[p],n01[p],n00[p]),ncol=2,byrow=TRUE),or=OR0,alternative="g")$p.value
    }
    
    midpval.uni <- vector(length=Nb.Cell)
    # require(MCMCpack)
    for (p in 1 : Nb.Cell) {
      midpval.uni[p] <- pval.uni[p] - 0.5 * dnoncenhypergeom(x = n11[p], n1 = n11[p] + n01[p] , n2 = n10[p] + n00[p], m1 = n11[p] + n10[p], psi = OR0)
    }
    
    pval.uni[pval.uni>1] <-1
    pval.uni[pval.uni<0] <-0
    midpval.uni[midpval.uni>1] <-1
    midpval.uni[midpval.uni<0] <-0
    
    
    ############################ SORTIE DE LA FONCTION #############################
    # SIGNALS RESULTS and presentation
    RES <- data.frame(L[,1], L[,2], n11, E, pval.uni, midpval.uni)
    colnames(RES) <- c("drug_code",
                       "event_effect",
                       "count",
                       "expected_count",
                       "RFET",
                       "midRFET")
    RES
  }
