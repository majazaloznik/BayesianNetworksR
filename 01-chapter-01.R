library(bnlearn)
library(vcd)
#' note that some caution must be exercised in interpreting both direct and 
#' indirect dependencies. The presence of arrows or arcs seems to imply, at 
#' an intuitive level, that for each arc one variable should be interpreted
#' as a cause and the other as an effect. This interpretation, which is called
#' causal, is difficult to justify in most situations; for this reason in 
#' general we speak about dependence relationships instead of causal effects. 
#' 
#' 

dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
dag
dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")
dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "E", to = "R")
dag <- set.arc(dag, from = "O", to = "T")
dag <- set.arc(dag, from = "R", to = "T")
modelstring(dag)
arcs(dag)
A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train","other")

A.prob <- array(c(0.3, 0.5, 0.2), dim = 3,
                dimnames = list(A = A.lv))

S.prob <- array(c(0.6, 0.4), dim = 2,
                dimnames = list(S = S.lv))

R.prob <- array(c(0.25, 0.75, 0.2, 0.8), dim = c(2,2),
                dimnames = list(R = R.lv, E = E.lv))

O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim = c(2,2),
                dimnames = list(O = O.lv, E = E.lv))

E.prob <- array(c(.75, .25, .72, .28, .88, .12, .64, .36, .70, .30, .90, .10), 
                dim = c(2,3,2),
                dimnames = list(E = E.lv, A = A.lv, S = S.lv))

T.prob <- array(c(.48, .42, .10, .56, .36, .08, .58, .24, .18, .70, .21, .09), 
                dim = c(3,2,2),
                dimnames = list(T = T.lv, O = O.lv,  R = R.lv))

dag3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")
all.equal(dag, dag3)


cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob, T = T.prob)

bn <- custom.fit(dag, cpt)

nparams(bn)

arcs(bn)
bn$R

coef(bn$R)

## 1.4 estimating parameters

survey <- read.table("data/survey.txt", header = TRUE)
head(survey)


#' for discrete BNs in general, teh paremeters to estimate are the conditional
#' probabilities in the local distributions. These can be estimated directly 
#' using the empirical frequencies in the dataset - this are the classic
#' frequentist or maximum likelihood estimates of the parameters. 
#' 
bn.mle <- bn.fit(dag, data = survey, method = "mle")

#' this is what you could also calculate manually e.g. 

prop.table(table(survey[, c("O", "E")]), margin = 2)

mosaic(coef(bn.mle$O))


#' alternative is to estimate the conditional probabilities in a bayesian setting 
#' using their posterior distributions. 
#' iss is the imaginariy sample size, that is effectively the weight given
#' to a uniform prior relative to the survey data. the smaller it is, the more 
#' the survey data gets to dominate the result, the larger it is, the closer
#' the posterior will be to the uniform

bn.bayes  <- bn.fit(dag, data = survey, method = "bayes", iss = 500)
mosaic(coef(bn.bayes$O))

#' so these posterior estimates are good because they prevent sparse CPTs,
#' they also are more robist than mle and have better predictive power (p13)
#' 

# 1.5 learning the DAG structure from the


#' the space of possible DAGs increases super-exponentially as the number of 
#' nodes grows. And it is very different from real spaces, it isn't 
#' continuous but has finite number of elements, so ad-hoc algorithms are
#' required to explore it. And decide the criteria used to evaulate them: 
#' conditional independence tests and network scores


# 1.5.1 Conditional Independence tests

#' the idea here is to test the null hypothesis that two variables are 
#' conditionally independent (independent conditional on its parents)
#' so e.g. H_0 : Travel is independent of education | given Occupation and 
#' Residence. This can then be tesetd using e.g. Pearson's X^2 or 
#' log-likelihood ratio G^2 (this is equivalent to the mutual information
#' test from information theory 
#' but test for conditional instead of marginal indepenence (this means 
#' the "E" in O-E is what would be expected if conditional ind. was true. 
#' 

#' degrees of freedom
(nlevels(survey[, "T"]) - 1) * (nlevels(survey[, "E"]) - 1) * 
  (nlevels(survey[, "O"]) * nlevels(survey[, "R"]))

ci.test("T", "E", c("O", "R"), test = "mi", data = survey)
ci.test("T", "E", c("O", "R"), test = "x2", data = survey)

#' ERRATA - PAGE 16 THE CI.TEST X2 RESULTS ARE WRONG, AS ARE THE ONES ON PAGE 17
#' both are very large, indicating that the education/travel dependence
#' relationship is not significant (conditional on occupation and residence)
#' let's test an existing one:

ci.test("T", "O", "R", test = "mi", data = survey)
ci.test("T", "O", "R", test = "x2", data = survey)

# so not significant
 
arc.strength(dag, data = survey, criterion = "x2")

# 1.5.2 networks scores
#' instead of looking just at individual arcs, network scores look at the
#' network as a whole. TWO main types are BIC - bayesian information criterion and 
#' bayesian dirichlet equivalent uniform posterior probability of the DG 
#' associated with a uniform prior
score(dag, data = survey, type = "bic")
score(dag, data = survey, type = "bde", iss = 10)

dag4 <- set.arc(dag, from = "E", to = "T")
nparams(dag4, survey)
score(dag4, data = survey, type = "bic")


rnd <- random.graph(nodes = c("A", "S", "E", "O", "R", "T"))
modelstring(rnd)
score(rnd, data = survey, type = "bic")

learned <- hc(survey)

modelstring(learned)
score(learned, data = survey, type = "bic")
