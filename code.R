library("zoo")
library("textcat")
library("tm")
library("openNLP")
FILE <- "df.Rdata"

if (FILE %in% list.files()) load(FILE) else {
  df <- read.csv("TripAdvisor.csv", stringsAsFactors = FALSE)
  dim(df)
  names(df) <- c("id", names(df[-1]))
  
  df$r_lang <- textcat::textcat(df$text)
  df$res_lang <- textcat::textcat(df$response)
  
  df$responded <- ifelse(is.na(df$response_date), F, T)
  
  nreviews <- xtabs(~review_date + hotel, data = df)
  nreviews <- apply(nreviews, 2, function(x) rev(cumsum(rev(x))))
  df$nreviews <- nreviews[cbind(as.character(df$review_date), df$hotel)]
  
  df[ , c("mobile", "stay_type", "hotel")] <- lapply(df[ , c("mobile", "stay_type", "hotel")], as.factor)
  df[ , c("stay_date", "review_date", "response_date")] <- lapply(df[ , c("stay_date", "review_date", "response_date")], as.Date, format = "%Y-%m-%d")
  df$stay_date <- as.yearmon(df$stay_date)
  summary(df)
  sapply(df[, c("value", "rooms", "sleep_quality", "location",  "cleanliness", "service_r")], table, useNA = "always")
  tr <- function(x) {
    is.na(x[which(x == 0)]) <- TRUE
    return(x)
  }
  df[, c("value", "rooms", "sleep_quality", "location",  "cleanliness", "service_r")] <- lapply(df[ , c("value", "rooms", "sleep_quality", "location",  "cleanliness", "service_r")], tr)
  sapply(df[, c("value", "rooms", "sleep_quality", "location",  "cleanliness", "service_r")], table, useNA = "always")     
  summary(df)       
  
  O <- with(df, order(hotel, review_date, id))
  odata <- df[O, c("hotel", "review_date", "id")]
  endday <- df$review_date[which.max(df$review_date)]
  odata$firstpage <- unlist(with(odata, tapply(review_date, hotel, 
                                               function(x) if (length(x) > 10) c(x[-c(1:10)], rep(endday, 10)) - x else endday - x)))
  odata$untilend <- unlist(with(odata, tapply(review_date, hotel, 
                                              function(x) if (length(x) > 10) rep(0:1, c(length(x) - 10, 10)) else rep(1, length(x)))))				     
  df <- merge(df, odata[, c("id", "firstpage", "untilend")], by = "id", all.x = TRUE)
  df$rev_window <- (as.yearmon(df$review_date)-df$stay_date)*12
  str(df)
  save(df, file = FILE)
}

dim(df)
form <- "votes ~ log(nreviews) * I(log(firstpage + 1)) + rating + responded + hotel"
fit <- glm(formula = form, family = "poisson", data = df)
summary(fit)

#form <- gsub(pattern = " + hotel", replacement = "", x = form, fixed = TRUE)
#glmm <- glmer(paste(form, "+ (1 | hotel)", sep = ""),
#              verbose = 2, data = df, family = "poisson")

df <- df[df$r_lang == "english" , ]
dim(df)
fit <- glm(formula = form, family = "poisson", data = df)
summary(fit)

reviews <- removePunctuation(x = df$text, preserve_intra_word_dashes = TRUE)
df$review_words <- sapply(reviews, function(x) length(MC_tokenizer(x)))
response <- removePunctuation(x = df$response, preserve_intra_word_dashes = TRUE)
df$response_words <- sapply(response, function(x) length(MC_tokenizer(x)))
usedata <- df[df$review_words >= 50 & df$review_words <= 1000, ]
dim(usedata)
usedata <- usedata[!is.na(usedata$response), ]
dim(usedata)

#sentences2 <- strsplit(df$text, "(\\. |!|\\?)")

table(usedata$untilend)
prop.table(table(usedata$untilend))

vars <- usedata[, c("votes", "nreviews", "firstpage", "rating", "window", "review_words", "response_words")]
descriptive <- t(apply(vars, 2, function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))))
round(descriptive, digits = 2)

form <- "votes ~ log(nreviews) * I(log(firstpage + 1)) + rating + window + review_words + response_words + hotel"
fit <- glm(formula = form, family = "poisson", data = usedata)
summary(fit)


glmm <- glmer(paste(form, "+ (1 | hotel)", sep = ""),
              verbose = 2, data = usedata, family = "poisson")

#Not working

library("lme4")
r_squared <- function(model) {
  X <- model.matrix(model)
  rand_formula <- reformulate(sapply(findbars(formula(model)), function(x) paste0("(", deparse(x), ")")), response=".")
  null_mod <- update(model, rand_formula, data = usedata)
  VarF <- var(predict(model, re.form = FALSE, type = "link"))
  sigma <- unlist(VarCorr(model))
  if ("unit" %in% names(sigma)) {
    VarRand <- sum(sigma[-grep("unit", names(sigma))])
    VarResid <- sigma["unit"] + log(1 + 1/exp(fixef(null_mod)))
  } else {
    VarRand <- sum(sigma)
    VarResid <- log(1 + 1/exp(fixef(null_mod)))
  }
  Rm <- VarF/(VarF + VarRand + VarResid)
  Rc <- (VarF + VarRand)/(VarF + VarRand + VarResid)
  c(Rm = Rm, Rc = Rc)
}
newdata <- data.frame(review_words = rep(c(50, 100, 150, 200), each = 6),
                      breadth = rep(0:5, 4),
                      rating = mean(usedata$rating, na.rm = TRUE),
                      nreviews = mean(usedata$nreviews),
                      firstpage = mean(usedata$firstpage))
formula <- c("votes ~ log(nreviews) * I(log(firstpage + 1)) + rating",
             "votes ~ log(nreviews) * I(log(firstpage + 1)) + rating + window + review_words + I(review_words^2)")
FILE <- c("helpfulness-null.rda", "helpfulness-base.rda")
for (i in 1:2) {
  if (FILE[i] %in% list.files()) load(FILE[i]) else {
    glmm <- glmer(paste(formula[i], "+ (1 | hotel)", sep = ""),
                  verbose = 2, data = usedata, family = "poisson")
    r2 <- r_squared(glmm)
    save(glmm, r2, file = FILE[i])
  }
  cat(gsub(".rda", "", FILE[i]), "\n")
  assign(gsub("-", "_", gsub(".rda", "", FILE[i])), glmm)
  printCoefmat(round(coef(summary(glmm)), digits = 4)) 
  
  names(r2) <- c("Marginal R2", "Conditional R2")
  print(round(r2, digits = 4))
  assign(paste("r2", gsub("-", "_", gsub(".rda", "", FILE[i])), sep = "_"), r2)
  
  pred <- cbind(newdata, p = predict(glmm, newdata, REform = NA, type = "response"))
  assign(paste("pred", gsub("-", "_", gsub(".rda", "", FILE[i])), sep = "_"), pred)
  
  if (i != 1) {
    change1 <-  sapply(c(50, 100, 150, 200), function(words) {
      p <- subset(pred, review_words == words)$p
      c("0 to 5" = p[6] / p[1],
        "1 to 2" = p[3] / p[2], 
        "2 to 5" = p[6] / p[3])
    })
    colnames(change1) <- c("50", "100", "150", "200")
    print(change1)
    change2 <-  sapply(c(1, 3, 5), function(complete) {
      p <- subset(pred, breadth == complete)$p
      c("50 to 100" = p[2] / p[1],
        "100 to 150" = p[3] / p[2],
        "150 to 200" = p[4] / p[3],
        "100 to 200" = p[4] / p[2])
    })
    colnames(change2) <- c("1", "3", "5")
    print(change2)
  }
}


Coefs <- list(null = coef(summary(helpfulness_null))[,c(1, 2, 4)],
                base = coef(summary(helpfulness_base))[,c(1, 2, 4)])
Max <- max(sapply(Coefs, nrow))
mat <- do.call("cbind", lapply(Coefs, function(x) {
  xorig <- x
  xnames <- rownames(x)
  x[, 1:2] <- format(round(x[, 1:2], digits = 4), nsmall = 4)
  x[, 2] <- paste("(", x[,2], ")", sep = "")
  x[, 3] <- as.character(cut(xorig[,3], c(0, 0.001, 0.01, 0.05, 1), c("***", "**", "*", ""), include.lowest = TRUE))
  x <- apply(x[, c(1, 3, 2)], 1, paste, collapse = " ")
  y <- vector("character", length = nrow(Coefs[[2]]))
  names(y) <- rownames(Coefs[[2]])
  y[xnames] <- x
  y
}))
mat <- rbind(mat,
             cbind(r2_helpfulness_null, r2_helpfulness_base))
write.csv(mat, file = "regcoefs.csv")


  myplot <- function(review_words, breadth = 0:5, phi = 30, theta = -30) {
    weight <- xtabs(~ review_2012$breadth + cut(review_2012$review_words, c(-Inf, review_words)))
    weight[weight > 100] <- 100
    weight <- weight / max(weight)
    
    p <- predict(glmm, newdata = data.frame(breadth = rep(breadth, n),
                                            review_words = rep(review_words, each = length(breadth)),
                                            nreviews = mean(review_2012$nreviews),
                                            firstpage = mean(review_2012$firstpage), ratings.overall = mean(review_2012$ratings.overall)),
                 REform = NA)
    
    nbcol <- 100
    color <- rev(colorspace::heat_hcl(nbcol))
    z <- matrix(p, nrow = length(breadth))
    nrz <- nrow(z)
    ncz <- ncol(z)
    zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
    wfacet <- (weight[-1, -1] + weight[-1, -ncz] + weight[-nrz, -1] + weight[-nrz, -ncz])/ 4
    facetcol <- cut(zfacet, nbcol)
    cols <- color[facetcol]
    colsalpha <- col2rgb(cols)
    colsalpha <- sapply(seq_along(facetcol), function(i)
      do.call("rgb", as.list(c(colsalpha[,i] / 255, alpha = wfacet[i] / max(wfacet)))))
    
    persp(breadth, review_words, z, col = colsalpha, phi = phi, theta = theta,
          ticktype = "detailed", ylab = "Review Depth", xlab = "Review Completeness", zlab = "Effect")
  }


  n <- 50
review_words <- seq(50, 200, length.out = n)
myplot(review_words, phi = 0, theta = -30)

