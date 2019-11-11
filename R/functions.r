#Quelques fonction bien utiles...
#Build binary c'est mieux sous windows


# library(tidyverse)

# TODO --------------------------------------------------------------------

 # --- Faire un roxygen combiné pour les fonctions de base
 # --- Réécrire l'opérateur + pour ne pas géner ggplot ?
 # --- Faire un purrr::nested_pmap -> plusieurs listes et pour chaque niveau un sapply = modify depth ?
 # --- nestedlist to dataframe



# Opérateurs --------------------------------------------------------------

?`%==%`
?`%!=%`
?`%!in%`

##' @name dan.operators
##' @rdname dan.operators
##'
##' @title Operators not indluded in R
##'
##' @param x, y vectors to be compared
##'
##' @return na-proof equality or difference
NULL

#TODO : https://gist.github.com/jefferys/b79fe87314b0dc72fec9
# https://stackoverflow.com/questions/15932585/multiple-functions-in-one-rd-file


#' Not in
#' @name not_in
#' @description The inverse of \code{\%in\%}, oddly not native in R
#' @export
"%!in%" = function(x,y){
  !('%in%'(x,y))
}


#' Exactly equals (NA-proof)
#' @rdname dan.operators
#' @title egalite en comptant les NA. Ne peut pas retourner de NA
#'
#' @return never NA
#' @export
#'
#' @examples
#' data.frame(a=c(NA,1,NA,1,1), b=c(NA,NA,1,1,2)) %>% mutate(
#' equal = a==b,
#' different = a!=b,
#' real_equal = a%==%b,
#' real_different = a%!=%b
#' )
"%==%" = function(a, b){
  ifelse(is.na(a)|is.na(b),
         is.na(a)==is.na(b),
         a==b)
}

#' Exactly inequal (NA-proof)
#' @rdname dan.operators
#' @title inegalite en comptant les NA. Ne peut pas retourner de NA
#' @export
"%!=%" = function(a, b){
  ifelse(is.na(a)|is.na(b),
         is.na(a)!=is.na(b),
         a!=b)
}



#' Addition2
#' Reecriture de l'addition pour prendre en compte les characters (paste0)
#' @seealso
#' \url{https://stackoverflow.com/a/18175871/3888000}
#' @export
#' @examples
#' "a"+"b"
"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}



# Fonctions de base -------------------------------------------------------

#' Table2
#'
#' reecriture de base::table pour mettre isany par defaut
#'
#' @param ...
#'
#' @export
table = function(...){
  if("useNA" %in% names(list(...))) {
    base::table(...)
  }else{
    base::table(..., useNA = "ifany")
  }
}

#' @export
print0 = function(...){
  print(paste0(...))
}

#' @export
cat0 = function(...){
  cat(..., sep="")
}

#' equivalent de grep(needle, str, value=T)
#' @export
match.str = function(str, needle, ...){str[grepl(needle, str, ...)]} #utiliser | pour un multiple pattern

#' @export
match.str.not = function(str, needle, ...){str[!grepl(needle, str, ...)]}



#' Recode une variable en percentiles
#'
#' @param x un vecteur à transformer
#' @param n le nombre de groupes (5 pour des quintiles)
#' @param letter le prefixe des groupes
#'
#' @export
#'
#' @examples
#' percentiles(iris$Sepal.Length, 3, "T")
#' percentiles(iris$Sepal.Length, 5, "Q")
percentiles = function(x, n, letter="Q"){
  labs = paste0(letter, 1:n)
  cut(x,
      breaks=quantile(x, probs=seq(0, 1, by=1/n), na.rm = TRUE),
      labels=labs, include.lowest = T)
}

#' Recode une variable en quantiles, en groupant ou non
#'
#' @description en quintiles non groupes par défaut
#' @param x la variable à recoder
#' @param by la variable de groupement (ou FALSE sinon)
#' @param data data.frame
#'
#' @export
#' @examples
#' getQuantiles(Sepal.Length, by=Species, data=iris)
#' getQuantiles(Sepal.Length, by=FALSE, data=iris)
#' getQuantiles(Sepal.Length, by=FALSE, n=3, letter="T", data=iris)
getQuantiles = function(x, data, by=FALSE, n=5, letter="Q"){
  data %>%
    group_by(!!enquo(by)) %>%
    mutate(rtn = percentiles(!!enquo(x), n, letter)) %>%
    pull(rtn)
}


#' getQuintiles
#' @description for compatibility
#' @export
getQuintiles = function (...) {
  getQuantiles(..., n = 5, letter = "Q")
}


dan.makepath = function(path){
  dir.create(file.path(path), showWarnings = FALSE, recursive = TRUE)
}


#' Cast to factor, ordering by frequency
#'
#' @param x a character or a factor
#'
#' @return a factor, ordered by frquency
#' @export
#'
#' @examples
#' x=iris$Sepal.Length %>% cut(c(-Inf, 4.5, 5.4, Inf))
#' x %>% table
#' x %>% factor_by_freq %>% table
#' x %>% factor_by_freq %>% levels
factor_by_freq = function(x) {
  stopifnot(is.character(x)|is.factor(x))
  ordered_levels = x %>% table %>% sort(decreasing = T) %>% names
  x %>% factor(levels=ordered_levels)
}


#' Intersections des NA
#' donne les intersection des NA
#' utiliser apres un select sur une dataframe
#' Voir aussi mice::md.pattern
#'
#' @export
#'
dan.na.table = function(df.na, pnames=FALSE){
  rtn.li = list()
  for(x in df.na %>% names){
    lin = c()
    for(y in df.na %>% names){
      lin = lin %>% c((is.na(db[x]) & is.na(db[y])) %>% sum)
    }
    rtn.li[[x]] = lin
  }
  rtn = do.call(rbind, rtn.li) %>% as.data.frame.matrix()
  if(pnames[1]==FALSE){
    colnames(rtn) = rownames(rtn)
  }else{
    colnames(rtn) = rownames(rtn) = pnames
  }
  return(rtn)
}

#' @export
describe.contained = function(x, y){
  stopifnot((is.vector(x)||is.factor(x)) && (is.vector(y)||is.factor(y)))
  xiny = (x %in% y) %>% sum(na.rm=T)
  xinypercent = (x %in% y) %>% mean(na.rm=T) %>% percent
  yinx = (y %in% x) %>% sum(na.rm=T)
  yinxpercent = (y %in% x) %>% mean(na.rm=T) %>% percent
  return(c(
    paste0("x in y = ", round(xiny, 2), "/", length(x), " (=", xinypercent, ")"),
    paste0("y in x = ", round(yinx, 2), "/", length(y), " (=", yinxpercent, ")")
  ))
  # cat0("x in y = ", round(xiny, 2), "/", length(x), " (=", xinypercent, ") \ny in x = ", round(yinx, 2), "/", length(y), " (=", yinxpercent, ")")
  invisible(NA)
}

#' Formula to variable list
#'
#' @param fo a single formula or a string to be parsed as formula.
#'
#' @return a character vector of formula members, with used functions as attribute
#' @export
#' @importFrom assertthat assert_that
#' @importFrom stringr str_match str_replace_all
#'
#' @examples
#' fo = formula("Surv(start, stop, event) ~ X + log(Y) + Z + rcs(W, 3)")
#' formula_to_varlist(fo)
formula_to_varlist = function(fo){
  assert_that(is.formula(fo) | is.character(fo))
  fo = as.character(fo) %>% paste(collapse = "+")
  fo1 = fo %>% strsplit("\\+|\\*|:") %>% .[[1]]
  fo2 = fo1 %>% str_match("(\\w+)\\(([\\w\\._, ]+)\\)")
  fct = fo2[,2] %>% na.omit %>% unique
  fo3 = ifelse(is.na(fo2[,3]), fo1, fo2[,3])
  fo4 = fo3 %>% str_replace_all("\\s", "") %>% strsplit("\\,") %>% unlist
  rtn = fo4[!grepl("^\\d+$", fo4) & fo4!="~"]
  attr(rtn, "functions") = fct
  return(rtn)
}

#' get.formula
#' @export
#' @examples
#' get.formula(c("score.pnns4.C"), c("age_inclus_dan", "sexe.cl"))
#' get.formula("score.pnns4.C", "age_inclus_dan")
get.formula = function(LHS, RHS, ...){
  as.formula(paste0(paste0(LHS, collapse="+"), " ~ ", paste0(RHS, collapse="+")), ...)
}


#' @export
geometric.mean <- function(x, na.rm = FALSE, trim = 0, ...){
  exp(mean(log(x, ...), na.rm = na.rm, trim = trim, ...))
}

#' @export
geometric.sd <- function(x, na.rm = FALSE, ...){
  exp(sd(log(x, ...), na.rm = na.rm, ...))
}




#' affiche les etoiles en fonction de la significativite de la pvalue
#' @export
pval.star = function (pval, digits = 3, limit=0.0001, onlystar=F) {
  if (any(pval > 1, na.rm=T))
    stop("Attention : p-value > 1")
  if (any(pval < 0, na.rm=T))
    stop("Attention : p-value < 0")
  star = pval %>% cut(breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1,
                                 1.0001), labels = c("***", "**", "*", ".", ""))
  if(onlystar)
    return(star)

  pval.txt = formatC(pval, format = "f", digits = digits)
  pval.txt = ifelse(as.numeric(pval.txt) == 0,
                    signif(pval, digits = digits),
                    pval.txt)
  pval.txt = ifelse(pval<limit, paste0("<", format(limit, scientific = FALSE)), pval.txt) %>%
    replace_na("")
  return(paste(pval.txt, star))
}


#' @export
sem = function(x, na.rm = TRUE){
  sd(x, na.rm=na.rm)/sqrt(length(x))
}


#' @export
get.IC = function(v){
  m = mean(v, na.rm=T)
  s = sd(v, na.rm=T)
  n = length(v[!is.na(v)])
  error <- qt(0.975,df=n-1)*s/sqrt(n)
  return(paste0("[", round(m-error, 3), ";", round(m+error, 3), "]"))
}

#' supprime tous les labels de HMisc pour eviter les problemes avec dplyr
#'
#' @param x data.frame
#'
#' @export
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled')
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}


# Fonctions pour GGPlot ---------------------------------------------------


#' à ajouter à un ggplot
#' #TODO ajouter sd
#'
#' @param type sem ou ic
#'
#' @export
geom_point_and_error = function(type=c("sem", "ic")){
  type = match.arg(type)
  coef = if(type=="ic") 1.96 else 1
  list(
    stat_summary(fun.y = mean, fun.ymin = function(x) mean(x) - coef*sem(x), fun.ymax = function(x) mean(x) + coef*sem(x),
                 geom = "errorbar", colour="black", width=.1),
    stat_summary(fun.y = mean, geom = "point")
  )
}



# Equation de Schofield ---------------------------------------------------


#' Basal Metabolic Rate as per Schofield's equations
#'
#' @param age in years
#' @param sex "male" or "female"
#' @param weight in kg
#'
#' @return BMR, in kcal/day
#' @export
bmr.schofield = function(age, sex=c("male","female"), weight){
  sex=match.arg(sex)
  if(age<3){
    x1 = ifelse(sex==0, 59.512, 58.317)
    x2 = ifelse(sex==0, -30.4, -31.1)
    SEE = ifelse(sex==0, 70, 59)
  }else if(age<10){
    x1 = ifelse(sex==0, 22.706, 20.315)
    x2 = ifelse(sex==0, 504.3, 485.9)
    SEE = ifelse(sex==0, 67, 70)
  }else if(age<18){
    x1 = ifelse(sex==0, 17.686, 13.384)
    x2 = ifelse(sex==0, 658.2, 692.6)
    SEE = ifelse(sex==0, 105, 111)
  }else if(age<29){
    x1 = ifelse(sex==0, 15.057, 14.818)
    x2 = ifelse(sex==0, 692.2, 486.6)
    SEE = ifelse(sex==0, 153, 119)
  }else if(age<59){
    x1 = ifelse(sex==0, 11.472, 8.126)
    x2 = ifelse(sex==0, 873.1 , 845.6)
    SEE = ifelse(sex==0, 167, 111)
  }else{
    x1 = ifelse(sex==0, 11.711, 9.082)
    x2 = ifelse(sex==0, 587.7, 658.5)
    SEE = ifelse(sex==0, 164, 108)
  }
  list(
    BMR=x1*weight+x2,
    SEE=SEE
  )
}



# Useless -----------------------------------------------------------------



#' formatDateFromSAS
#' @export
formatDateFromSAS = function(dateAsString, format="%d%b%y", lang="en"){
  rtn=dateAsString %>% readr::parse_date(format, locale = locale(lang))
  if((dateAsString %>% as.character %>% as.Date(format="%Y-%m-%d") %>% is.na) %>% any %>% `!`)
    rtn=dateAsString %>% as.character %>% as.Date(format="%Y-%m-%d")
  rtn=as.Date(ifelse(rtn > "2016-12-31", format(rtn, "19%y-%m-%d"), format(rtn)))
  return(rtn)
}
# formatDateFromSAS = function(dateAsString, format="%d%b%y", lang="en"){
#   if((dateAsString %>% as.character %>% as.Date(format="%Y-%m-%d") %>% is.na) %>% any %>% `!`)
#     return(dateAsString %>% as.character %>% as.Date(format="%Y-%m-%d"))
#   return(dateAsString %>% readr::parse_date(format, locale = locale(lang)))
# }




get.label = function(x){label(db[as.vector(x)])}


par.dan = function(nb.total, nb.colonnes){
  par(mfrow = c(ceiling(nb.total/nb.colonnes),nb.colonnes))
}




cronbach2 = function(v1){ #je ne sais pas si j'ai le droit...
  v1 <- na.omit(v1)
  nv1 <- ncol(v1)
  pv1 <- nrow(v1)
  alpha <- (nv1/(nv1 - 1)) * (1 - sum(apply(v1, 2, var), na.rm = T)/var(apply(v1, 1, sum, na.rm = T), na.rm = T))
  resu <- list(sample.size = pv1, number.of.items = nv1, alpha = alpha)
  resu
}



coalesce2 = function(...){ #si tout est NA on retourne NA, si y'a que du FALSE|NA on retourne FALSE, sinon il y a au moins un TRUE et on retourne TRUE
  x=data.frame(...)
  return((NA^!rowSums(!is.na(x)))*rowSums(x, na.rm = TRUE)>0)
  # do.call(pmax, c(as.data.frame(x), na.rm = TRUE)) > 0 #ca marche aussi, et en un peu plus vite
}

formatDate <- function(dateAsString, format="%d%b%Y"){
  # return(as.Date(as.character(dateAsString), format=format))
  # return(as.Date(dateAsString, format=format))
  if((dateAsString %>% as.character %>% as.Date() %>% is.na) %>% any %>% `!`)
    return(dateAsString %>% as.character %>% as.Date())
  if((dateAsString %>% as.character %>% as.Date(format="%Y-%m-%d") %>% is.na) %>% any %>% `!`)
    return(dateAsString %>% as.character %>% as.Date(format="%Y-%m-%d"))
  return(as.Date(chron(format(as.Date(dateAsString, format), "%m/%d/%y"))))
}





#version modifiee pour les grandes databases (as.numeric vers la ligne 200)
#' @export
mantelhaen.test2 = function (x, y = NULL, z = NULL,
                             alternative = c("two.sided", "less", "greater"),
                             correct = TRUE, exact = FALSE, conf.level = 0.95) {
  DNAME <- deparse(substitute(x))
  if (is.array(x)) {
    if (length(dim(x)) == 3L) {
      if (anyNA(x))
        stop("NAs are not allowed")
      if (any(dim(x) < 2L))
        stop("each dimension in table must be >= 2")
    }
    else stop("'x' must be a 3-dimensional array")
  }
  else {
    if (is.null(y))
      stop("if 'x' is not an array, 'y' must be given")
    if (is.null(z))
      stop("if 'x' is not an array, 'z' must be given")
    if (any(diff(c(length(x), length(y), length(z))) !=
            0L))
      stop("'x', 'y', and 'z' must have the same length")
    DNAME <- paste(DNAME, "and", deparse(substitute(y)),
                   "and", deparse(substitute(z)))
    OK <- complete.cases(x, y, z)
    x <- factor(x[OK])
    y <- factor(y[OK])
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L))
      stop("'x' and 'y' must have at least 2 levels")
    else x <- table(x, y, z[OK])
  }
  if (any(apply(x, 3L, sum) < 2))
    stop("sample size in each stratum must be > 1")
  I <- dim(x)[1L]
  J <- dim(x)[2L]
  K <- dim(x)[3L]
  if ((I == 2) && (J == 2)) {
    alternative <- match.arg(alternative)
    if (!missing(conf.level) && (length(conf.level) != 1 ||
                                 !is.finite(conf.level) || conf.level < 0 || conf.level >
                                 1))
      stop("'conf.level' must be a single number between 0 and 1")
    NVAL <- c(`common odds ratio` = 1)
    if (!exact) {
      s.x <- apply(x, c(1L, 3L), sum)
      s.y <- apply(x, c(2L, 3L), sum)
      n <- as.double(apply(x, 3L, sum))
      DELTA <- sum(x[1, 1, ] - s.x[1, ] * s.y[1, ]/n)
      YATES <- if (correct && (abs(DELTA) >= 0.5))
        0.5
      else 0
      STATISTIC <- ((abs(DELTA) - YATES)^2/sum(apply(rbind(s.x,
                                                           s.y), 2L, prod)/(n^2 * (n - 1))))
      PARAMETER <- 1
      if (alternative == "two.sided")
        PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
      else {
        z <- sign(DELTA) * sqrt(STATISTIC)
        PVAL <- pnorm(z, lower.tail = (alternative ==
                                         "less"))
      }
      names(STATISTIC) <- "Mantel-Haenszel X-squared"
      names(PARAMETER) <- "df"
      METHOD <- paste("Mantel-Haenszel chi-squared test",
                      if (YATES)
                        "with"
                      else "without", "continuity correction")
      s.diag <- sum(x[1L, 1L, ] * x[2L, 2L, ]/n)
      s.offd <- sum(x[1L, 2L, ] * x[2L, 1L, ]/n)
      ESTIMATE <- s.diag/s.offd
      sd <- sqrt(sum((x[1L, 1L, ] + x[2L, 2L, ]) * x[1L,
                                                     1L, ] * x[2L, 2L, ]/n^2)/(2 * s.diag^2) + sum(((x[1L,
                                                                                                       1L, ] + x[2L, 2L, ]) * x[1L, 2L, ] * x[2L, 1L,
                                                                                                                                              ] + (x[1L, 2L, ] + x[2L, 1L, ]) * x[1L, 1L,
                                                                                                                                                                                  ] * x[2L, 2L, ])/n^2)/(2 * s.diag * s.offd) +
                   sum((x[1L, 2L, ] + x[2L, 1L, ]) * x[1L, 2L,
                                                       ] * x[2L, 1L, ]/n^2)/(2 * s.offd^2))
      CINT <- switch(alternative, less = c(0, ESTIMATE *
                                             exp(qnorm(conf.level) * sd)), greater = c(ESTIMATE *
                                                                                         exp(qnorm(conf.level, lower.tail = FALSE) *
                                                                                               sd), Inf), two.sided = {
                                                                                                 ESTIMATE * exp(c(1, -1) * qnorm((1 - conf.level)/2) *
                                                                                                                  sd)
                                                                                               })
      RVAL <- list(statistic = STATISTIC, parameter = PARAMETER,
                   p.value = PVAL)
    }
    else {
      METHOD <- paste("Exact conditional test of independence",
                      "in 2 x 2 x k tables")
      mn <- apply(x, c(2L, 3L), sum)
      m <- mn[1L, ]
      n <- mn[2L, ]
      t <- apply(x, c(1L, 3L), sum)[1L, ]
      s <- sum(x[1L, 1L, ])
      lo <- sum(pmax(0, t - n))
      hi <- sum(pmin(m, t))
      support <- lo:hi
      dc <- .Call(C_d2x2xk, K, m, n, t, hi - lo + 1L)
      logdc <- log(dc)
      dn2x2xk <- function(ncp) {
        if (ncp == 1)
          return(dc)
        d <- logdc + log(ncp) * support
        d <- exp(d - max(d))
        d/sum(d)
      }
      mn2x2xk <- function(ncp) {
        if (ncp == 0)
          return(lo)
        if (ncp == Inf)
          return(hi)
        sum(support * dn2x2xk(ncp))
      }
      pn2x2xk <- function(q, ncp = 1, upper.tail = FALSE) {
        if (ncp == 0) {
          if (upper.tail)
            return(as.numeric(q <= lo))
          else return(as.numeric(q >= lo))
        }
        if (ncp == Inf) {
          if (upper.tail)
            return(as.numeric(q <= hi))
          else return(as.numeric(q >= hi))
        }
        d <- dn2x2xk(ncp)
        if (upper.tail)
          sum(d[support >= q])
        else sum(d[support <= q])
      }
      PVAL <- switch(alternative, less = pn2x2xk(s, 1),
                     greater = pn2x2xk(s, 1, upper.tail = TRUE),
                     two.sided = {
                       relErr <- 1 + 10^(-7)
                       d <- dc
                       sum(d[d <= d[s - lo + 1] * relErr])
                     })
      mle <- function(x) {
        if (x == lo)
          return(0)
        if (x == hi)
          return(Inf)
        mu <- mn2x2xk(1)
        if (mu > x)
          uniroot(function(t) mn2x2xk(t) - x, c(0, 1))$root
        else if (mu < x)
          1/uniroot(function(t) mn2x2xk(1/t) - x, c(.Machine$double.eps,
                                                    1))$root
        else 1
      }
      ESTIMATE <- mle(s)
      ncp.U <- function(x, alpha) {
        if (x == hi)
          return(Inf)
        p <- pn2x2xk(x, 1)
        if (p < alpha)
          uniroot(function(t) pn2x2xk(x, t) - alpha,
                  c(0, 1))$root
        else if (p > alpha)
          1/uniroot(function(t) pn2x2xk(x, 1/t) - alpha,
                    c(.Machine$double.eps, 1))$root
        else 1
      }
      ncp.L <- function(x, alpha) {
        if (x == lo)
          return(0)
        p <- pn2x2xk(x, 1, upper.tail = TRUE)
        if (p > alpha)
          uniroot(function(t) pn2x2xk(x, t, upper.tail = TRUE) -
                    alpha, c(0, 1))$root
        else if (p < alpha)
          1/uniroot(function(t) pn2x2xk(x, 1/t, upper.tail = TRUE) -
                      alpha, c(.Machine$double.eps, 1))$root
        else 1
      }
      CINT <- switch(alternative, less = c(0, ncp.U(s,
                                                    1 - conf.level)), greater = c(ncp.L(s, 1 - conf.level),
                                                                                  Inf), two.sided = {
                                                                                    alpha <- (1 - conf.level)/2
                                                                                    c(ncp.L(s, alpha), ncp.U(s, alpha))
                                                                                  })
      STATISTIC <- c(S = s)
      RVAL <- list(statistic = STATISTIC, p.value = PVAL)
    }
    names(ESTIMATE) <- names(NVAL)
    attr(CINT, "conf.level") <- conf.level
    RVAL <- c(RVAL, list(conf.int = CINT, estimate = ESTIMATE,
                         null.value = NVAL, alternative = alternative))
  }
  else {
    df <- (I - 1) * (J - 1)
    n <- m <- double(length = df)
    V <- matrix(0, nrow = df, ncol = df)
    for (k in 1:K) {
      f <- x[, , k]
      # ntot <- sum(f)
      ntot <- as.numeric(sum(f))
      rowsums <- apply(f, 1L, sum)[-I]
      colsums <- apply(f, 2L, sum)[-J]
      n <- n + c(f[-I, -J])
      m <- m + c(outer(rowsums, colsums, "*"))/ntot
      V <- V + (kronecker(diag(ntot * colsums, nrow = J -
                                 1) - outer(colsums, colsums), diag(ntot * rowsums,
                                                                    nrow = I - 1) - outer(rowsums, rowsums))/(ntot^2 *
                                                                                                                (ntot - 1)))
    }
    n <- n - m
    x=qr.solve(V, n)
    STATISTIC <- c(crossprod(n, qr.solve(V, n)))
    PARAMETER <- df
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Cochran-Mantel-Haenszel M^2"
    names(PARAMETER) <- "df"
    METHOD <- "Cochran-Mantel-Haenszel test"
    RVAL <- list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL)
  }
  RVAL <- c(RVAL, list(method = METHOD, data.name = DNAME))
  class(RVAL) <- "htest"
  return(RVAL)
}



#' Recursive length of a list of lists
#'
#' @description count the number of terminal children (not list) of a list of lists
#' @param x a list of lists
#'
#' @export
recursive_length = function(x){
  f=function(x){
    if(is.list(x[[1]])) return(sapply(x, function(x) recursive_length(x)))
    return(1)
  }
  sum(f(x))
}





