#' ---
#' title: "Chapter 1---The discrete case: mulitnomial Bayesian networks"
#' author: "mz"
#' date: ""
#' output: pdf_document
#' ---
#' #  1.1 Introductiory example: Train use survey
#' 
#+ intro, message = FALSE
# preliminaries: 
suppressMessages(library(bnlearn))
suppressMessages(library(vcd))
suppressMessages(library(gRain))
suppressMessages(library(here))

#' Toy example, six variables: **Age** (y,o), **Sex** (m/f), **Education** (high/uni), 
#' **Occupation** (emp/self), **residence** (small/big), **Travel** (car/train/other). 
#'
#'# 1.2. Graphical representation
#' 
#' A **Directed graph** is used to represent these relationships, variables are
#' nodes and direct dependende relatioships are arcs between the *parent* and *child* nodes. 
#' 
#' Indirect relationships are read from sequences of arcs or paths--with no cyclical
#' paths allowed. #' 
#' *"Note that some caution must be exercised in interpreting both direct and 
#' indirect dependencies. The presence of arrows or arcs seems to imply, at 
#' an intuitive level, that for each arc one variable should be interpreted
#' as a cause and the other as an effect. This interpretation, which is called
#' causal, is difficult to justify in most situations; for this reason in 
#' general we speak about **dependence relationships** instead of causal effects."* 
#' 
#' 
#' 
#' 

#' Create empty graph with no arcs
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
#' Have a look at it:
dag

#' add arcs: age affects education level, as does sex...
dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")
dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "E", to = "R")
dag <- set.arc(dag, from = "O", to = "T")
dag <- set.arc(dag, from = "R", to = "T")

#' Here's what it looks like now with the arcs added
dag 

#' You can extract only the dependecies from the graph:
modelstring(dag)

#' Another function that could be useful is `arcs()`
#' also becasue you cam use `arcs(dag) <- matrix() `
#' to add arcs to a dag more conveniently than one by one
arcs(dag)


#' # 1.3 Probabilistic represetntion
#' 
#' We first define the levels of each of the variables: they are discrete and nonordered *levels*
#' 
#' 
A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train","other")

#' *"Therefore the natural choice for the joint probability distribution is a multinomial 
#' distribution"*--here called the **global distribution**. But the number of combinations of levels
#' in this example is 3 x 2 x 2 x 2 x 2 x 3 - 1 = 143 parameters. But we can break this down into 
#' smaller local distributions, one for each variable. So this is the **factorisation** of the 
#' global distribution:
#' 
#' $Pr(A,S,E,O,R,T) = Pr(A) Pr(S) Pr(E|A,S) Pr(O|E) Pr(R|E) Pr(T|O,R)$
#' 
#' All the local distributions is a nested model of the global distribution: has fewer paremeters than
#' it because it makes assumptions about the dependencies between the variables. 
#' 
#' Age and sex are sinple unidimensional probability tables:

#+ echo = FALSE
A.prob <- array(c(0.3, 0.5, 0.2), dim = 3,
                dimnames = list(A = A.lv))

S.prob <- array(c(0.6, 0.4), dim = 2,
                dimnames = list(S = S.lv))
#+
A.prob
S.prob
#' Occupation and residence are twodimensional conditional probability distributions, each 
#' column representing one level of the parent
#' 
#' 
#+ echo = FALSE
R.prob <- array(c(0.25, 0.75, 0.2, 0.8), dim = c(2,2),
                dimnames = list(R = R.lv, E = E.lv))

O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim = c(2,2),
                dimnames = list(O = O.lv, E = E.lv))
#+
R.prob
O.prob

#' And the three dimensional tables with two aprents each and each
#' column is one combination of parents:
#+ echo = FALSE
E.prob <- array(c(.75, .25, .72, .28, .88, .12, .64, .36, .70, .30, .90, .10), 
                dim = c(2,3,2),
                dimnames = list(E = E.lv, A = A.lv, S = S.lv))

T.prob <- array(c(.48, .42, .10, .56, .36, .08, .58, .24, .18, .70, .21, .09), 
                dim = c(3,2,2),
                dimnames = list(T = T.lv, O = O.lv,  R = R.lv))

#+ 
E.prob
T.prob

#'So these take a total of 21 parameters. Let's put them all together:
#'
#'`model2network()` is the inverse of `modelstring()`, so it creates a BBN model
#'out of a string describing the network:
#'
dag <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")

#' Then we combine the model with the conditional probability tables which we have 
#' to store in a list first. 
cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob, T = T.prob)

#' Then use `custom.fit()` which is the fucntion to 
#' specify a custom BN (e.g. from experts) instead of learning it from the dataset (That would 
#' be `bn.fit()`, we'll do that alter).
bn <- custom.fit(dag, cpt)

#' This produces an object of class `bn.fit` which is strucutred as a list with an emelent
#' for each node that includes:
#' 
#' * `$node` - the name of the node
#' * `$parents` - a vector of parent nodes
#' * `$children` - a vector of children nodes
#' * `$prob` - the local distribution. 
#' 
#' Ohter useful methods for bn.fit objects is `coef()` to get the parameters out e.g. 

coef(bn$E)

#' Also the numebr of parameters: 

nparams(bn)

#' # 1.4 Estimating Parameters: Conditional Probability Tables
#' 
#' 
#'Most of the time you have to learn the parameters of the local distrubutions
#'*learn* them from the data. Here's a dataset (N= 500) fo the survey:
#'
#'
survey <- read.table(here("data", "survey.txt"), header = TRUE)
head(survey)


#' In the case of this survey, and for discrete BNs in general,
#' the paremeters to estimate are the conditional
#' probabilities in the local distributions. These can be estimated, for example, directly 
#' using the empirical frequencies in the dataset---this yields the *classic
#' frequentist* or *maximum likelihood estimates* of the parameters:
#' 
#' 
#'\[ 
#'\widehat{Pr}(O=emp | E = high) = \frac{\widehat{Pr}(O = emp, E = high)}{\widehat{Pr}(E = high)} = 
#'\frac{N observations (O = emp, E = high)}{N observations ( E = high)}
#'\]
#' 
#' So now we use the `bn.fit()` function to estimate the parameters from the data 
#' (as opposed to using a custom set of parameters specified by the user). 
#' 
bn.mle <- bn.fit(dag, data = survey, method = "mle")

bn.mle
#' this is what you could also calculate manually e.g. 

prop.table(table(survey[, c("O", "E")]), margin = 2)

#' Which is the same as  the *fitted* one:
#' 

bn.mle$O

#+ fig.dim = c(3,3)
mosaic(coef(bn.mle$O))

#' This is fitting the CPTs from the data, but assuming the network structure is known, we passed it 
#' via the dag object. 
#' 
#' #### Bayesian estimate of CPTs
#' 
#'  An alternative is to estimate the conditional probabilities in a Bayesian setting 
#' using their posterior distributions. 
#' `iss` is the *Imaginariy Sample Size*, that is effectively the weight given
#' to a uniform prior relative to the survey data. the smaller it is, the more 
#' the survey data gets to dominate the result, the larger it is, the closer
#' the posterior will be to the uniform.

#'\[ 
#'\widehat{p}_{O=emp, E = high} = \frac{N observations (O = emp, E = high)}{N}\\
#'\widehat{p}_{E = high} = \frac{N observations (E = high)}{N}
#'\]
#'
#'and the prior probabilities are then:
#'
#'
#'\[ 
#'\pi_{O=emp, E = high} = \frac{1}{nlevels(O) \times nlevels(E)}\\
#'\pi_{E = high} = \frac{nlevels(O)}{nlevels(O) \times nlevels(E)}
#'\]
#'
#'ERRATA p13: `nlevels(bn.bayes$O)` doesnt' work (any more?) instead `nlevels(survery$O)` is OK.
#'
#'In this case e.g. p is 
{{prop.table(table(survey[, c("O", "E")]), margin = 2)[[1]]}}
#' and $\pi$ is 1/4. So then the posterior probability is calculated: 
#' 
#'\[ 
#'\widehat{Pr}_{O=emp, E = high} = \frac{iss}{N+is}\times \pi_{O=emp, E = high} + \frac{N}{N+is}\times \hat{p}_{O=emp, E = high}  \\
#'\widehat{Pr}_{E = high} = \frac{iss}{N+is}\times \pi_{ E = high} + \frac{N}{N+is}\times \hat{p}_{E = high}  \\
#'\]
#'
#'And then the estimated posterior probability of being in employment if you have a high school education is 
#'the porbability of both divided by the education one. as before. So the **iss** if it's small, then the $\pi$ probability 
#'has very little influence (e.g. 10/510 vs 500/510), but if you increase it, it has more. 
#'
#'
bn.bayes  <- bn.fit(dag, data = survey, method = "bayes", iss = 10)
#+ fig.dim = c(3,3)
bn.bayes$O
mosaic(coef(bn.bayes$O))

#' So these posterior estimates are good because they prevent sparse CPTs,
#' they also are more robust than mle and have better predictive power (p13). 
#' So even with a small data set this prevents empty cells. Curious about
#' the predictive power?
#' 
#' 
#' # 1.5 learning the DAG structure: Tests and scores

#' Before we assumed the DAG is known, but you might now, the structure of the DAG might
#' be what is the object of investigation. But this is a complex task: because the state space
#' is very very big. The space of possible DAGs increases super-exponentially as the number of 
#' nodes grows. 
#' 
#' And it is very different from real spaces, it isn't 
#' continuous but has finite number of elements, so ad-hoc algorithms are
#' required to explore it. And decide the criteria used to evaulate them: 
#' *conditional independence tests * and *network scores* are two clases of statistical 
#' criteria used by the algorithms. 
#' 
#' ## 1.5.1 Conditional Independence Tests
#' 
#' * Focuses on individual arcs - tests if they are presnt or not
#' 
#' The idea here is to test the null hypothesis that two variables are 
#' conditionally independent (independent conditional on its parents)
#' so e.g. 
#' 
#' *H_0 : Travel is independent of education | given Occupation and Residence. *
#' 
#' \[
#' H_0 : T {\perp\!\!\!\perp}_P E | \{O,R\}. \\
#' H_1: T {\not\!\perp\!\!\!\perp} _P E | \{O,R\}. 
#' \]
#' 
#' This can then be tesetd adapting either. Pearson's $\chi^2$ or 
#' log-likelihood ratio $G^2$ (this is equivalent to the mutual information
#' test from information theory) but test for *conditional* instead of *marginal* 
#' indepenence (this means the "E" in O-E is what would be expected if conditional ind. was true. 
#' 
#+ fig.dim = c(2,4), echo = FALSE,fig.cap = "The plot of the basic dag",out.extra='angle=90'
graphviz.plot(dag)


#' Both tests have an asymptotic $\chi^2$ distribution with have 8 degrees of freedom:
(nlevels(survey[, "T"]) - 1) * (nlevels(survey[, "E"]) - 1) * 
  (nlevels(survey[, "O"]) * nlevels(survey[, "R"]))

#' You can do a *Conditional Independence* (ci) test from bnelarn using `ci.test()`. `mi` stands for 
#' mutual information, that's the $G^2$ test. The tests sum up the individual independence
#' statistics across the levels of [O,R]. 
ci.test("T", "E", c("O", "R"), test = "mi", data = survey)
ci.test("T", "E", c("O", "R"), test = "x2", data = survey)

#' ERRATA - PAGE 16 THE CI.TEST X2 RESULTS ARE WRONG, AS ARE THE ONES ON PAGE 17
#' 
#' The larger the test statistic, the stronger the dependece. But both tests have large p-values 
#' (low test statistics) so we cannot reject the null hypothesis that education/travel are independet.
#'  (conditional on occupation and residence). 

#' Let's test an existing arc and see if the dependence it ecnodes is supported by the data:

ci.test("T", "O", "R", test = "mi", data = survey)
ci.test("T", "O", "R", test = "x2", data = survey)

#' So it's also not significant, meaning we cannot reject the null hypothesis.
#' 
#' We can automate that for the whole dag using `arc.strength()`:

#+ echo = -(1)
options(digits = 2)

arc.strength(dag, data = survey, criterion = "x2")

#' So the lower the strength (these are the p-values) the higher the *probabilistic dependence*
#' corresponding to each arc. It is measured by removing it from the DAG and 
#' quantifying the change in some probabilistic criterion. The test is for the child 
#' node to be independent from the parent node, conditional on the remaining parents (and 
#' keeping the rest of the network structure fixed). 
#' 
#' 
#' 

#' ## 1.5.2 networks scores ####################################################
#' 
#' Instead of looking just at individual arcs, like conditional independence tests do,
#' network scores look at the network as a whole, measuring how well the DAG
#' mirrors the dependence structure of the data. First popular type is BIC - **bayesian information criterion** 
#' 
#' \[
#' BIC = log{\widehat{Pr}(A, S, E, O, R, T)} - \frac{d}{2} log {n}
#' \]
#' 
#' Where $d$ is the number of parameters in the network (i.e.21) and $n$ is the sample size. 
#' And this distribution decomposes into the local distributions making it easy to 
#' calculate. 
#' 
#' \[
#' BIC = (log {\widehat{Pr}(A)} - \frac{d_A}{2} log{n}) - (log{\widehat{Pr}(S)} - \frac{d_S}{2} log n) (log \widehat{Pr}(E|A,S) - \frac{d_E}{2} log n) - (log \widehat{Pr}(O|E) - \frac{d_O}{2} log n)...
#' \]  
#' 
#' Second popular type is  **Bayesian dirichlet equivalent uniform** (BDeu) posterior probability of the DAG
#' associated with a uniform prior over both the space of the DAGs and the parameters. Again the iss
#' controls how much weight the prior is given. Both BIC aand BDeu assign
#' higher scores to better fitting DAGs:

score(dag, data = survey, type = "bic")
score(dag, data = survey, type = "bde", iss = 10)

#' Lets try adding an arc from E-> T: 
dag4 <- set.arc(dag, from = "E", to = "T")
#+ fig.dim = c(2,4), echo = FALSE, fig.cap = "T\\label{fig:2}he plot of the new dag with E->T added",out.extra='angle=90'
graphviz.plot(dag4)

#' Now we have eight more parameters--but the increased fit (increase in the $log{\widehat{Pr}}$) is not engouh
#' to ofset the increased number of parameters (the second term). See figure \ref{fig:2} for the model. 
#' I still need to do the math of why it's eight parameters: T already has two parents, O and R, so each of them
#' has two levels, so in total there are 4 possible parent combinations. Each comination has three
#' possible T outcomes (that have to add up to 100), so that's two parameters. In the old DAG
#' T therefore has 8 parameters associated with it.
#' 
#' So When you aff E->T now T has three parents. Each with two levels, so now there are eight possible
#' parent combinations, therefore 16 parameters associated with it. That's where the extra 8 come from. 

nparams(dag4, survey)
score(dag4, data = survey, type = "bic")

#' So these scores take the increasad numbers of parameters into acocunt automatically. 
#' 
#' This was comparing two networks with one arc difference, but we can also compare completley different networks
#' like this random one we make up:

set.seed(42)
rnd <- random.graph(nodes = c("A", "S", "E", "O", "R", "T"))
modelstring(rnd)

#' And the score is not surprisingly worse than the first dag:
score(rnd, data = survey, type = "bic")

#' We can see what the random plot looks like in Figure \ref{fig:3} (or rather below). 
#+ fig.dim = c(2,4), echo = FALSE, fig.cap = "\\label{fig:3}The plot of the randomly generated dag",out.extra='angle=90'
graphviz.plot(rnd)

#' What about an empty dag? 

dag0 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
score(dag0, data = survey, type = "bic")
score(dag, data = survey, type = "bic")
#' Not what I expected actually: So the fact that there are 21-8 = 13 fewer parameters 
#' means the empty DAG has a higher BIC!
score(dag0, data = survey, type = "bde", iss = 10)
score(dag, data = survey, type = "bde", iss = 10)

#' alhtough the BDeu is lower. 

### Learning the structure from the data

#' So far we had a manually drawn model and a random one. How about learing  it from the survey data.
#' There are several algorithms available to do this, looking for DAGs that maximise a given
#' network score. A simple one is **hill climbing**: Starts with an empty graph and adds individual 
#' arcs one by one and keeps the change that increases the network score most. `hc()` is the function
#' the default of which is to maximise the BIC score. 
#' 
learned <- hc(survey)

modelstring(learned)
score(learned, data = survey, type = "bic")
#+ fig.dim = c(2,4), echo = FALSE, fig.cap = "\\label{fig:4}The plot of the dag learned by default hill-climbing",out.extra='angle=90'
graphviz.plot(learned)

#' We can pick a different score as the optimisation criterion, which in this case produces the same result
learned2 <- hc(survey, score = "bde")
all.equal(learned, learned2)

#' The function `arc.strength()` produces values based on the criterion of the generation algorithm! SO 
#' it's not always a p-value. This doesn't seem to be obviously well documented. If you explicitly state
#' the criterion, that's obviously what you get. But it seems criterion also picks up as the default
#' the core used in the generation (learning) algorithm if there is one. And if there isn't one
#' then it's a conditional independence test, looks like `mi` is the default, mutual information. 

arc.strength(learned, data = survey, criterion = "bic")

#' Here we can see all the arcs--if we removed them--would decrease the network score (that's what the
#' negative values mean). So just like the conditional independence tests test what would happen
#' if each individual arc were removed, the netwokr scores tell you what would happen to the
#' network score if you rewmoved that arc. So let's see how BIC would score the strength of our 
#' original DAG:
#' 
arc.strength(dag, data = survey, criterion = "bic")

#' Now here we see a very different picture. A whole four arcs' removal would increase the network score
#' the most dramatically the 0-> T one, which we saw also had the highest p-value indicating that the
#' two variables are conditionally independent, so the arc really shouldn't be there. 
#' 
#' 
#' ## 1.6 Using Discrete BNs
#' 
#' So what do we do with these? **Inference**. This can be done two ways: (i) through the DAG or (ii) through
#' the set of local distributions. This is also known as **queryin** (in computer science), i.e. treating
#' a BBN as an *expert system* and asking it questions. Three types of querries are possible:
#' 
#' 1. *conditional probability querries*: ask for the probabilities of an event under specific conditions
#' 2. *conditional independence querries*: check the association between variables after the 
#' influence of other variables is removed. 
#' 3. *most likely explanation querries*: identify the most likely state of one or more variables. 
#' 
#' ### 1.6.1 Usinge the DAG structure for these querries
#' 
#' 
#' You can use the DAg to look for associations between variables:
#' 
#' * *direct* associations mean there is a single arc connecting the corresponding nodes.
#' * *indirect* association mean there will be two or more arcs passing through the nodes
#' that mediate the association. 
#' 
#' In general two variables X,Y are independent given a third, Z,if there are no arcs connecting
#' them that are not blocked by Z. Conditioning on Z is the same as *fixing* Z's value, so it
#' is known. 
#' 
#' $X{\perp\!\!\!\perp}_G Y| \ Z$
#' 
#' So we say X and Y are *separated* by Z, and because the BN is based on a DAG we call it *d-separation*
#' to mean directed separation. The $G$ above refers to **graphical separation** ${\perp\!\!\!\perp}_G$ and that *implies*
#' **probabilistic independence** ${\perp\!\!\!\perp}_P$.
#' 
#' *If all the paths between X and Y are blocked, they are (conditionally) independent (i.e. d-separated).*
#' 
#' #### *Serial connections**
#' `desp()` is the function to test d-separation. So e.g. test if S is d-separated (independent) from R. They 
#' are not independent:
#' 
dsep(dag, x = "S", y = "R")
#' 
#' The answer is FALSE, they are not d-separated. The association between Sex and Residence is clear from the 
#' direct path 
#' 
path(dag, from = "S", to = "R")
#' If we codntion on Education, the path is blocked and Sex and Residence become d-separated/independent.
dsep(dag, x = "S", y = "R", z = "E")
#' 
#' The joint probability distribution of S and R, once E is known, decomposes into two parts, one that depends
#' only on S and one that depends only on R:
#' 
#' \[
#' Pr(S,R | E) = Pr(S|E)Pr(R|E)
#' \]
#' 
#' Thereby probing that S and R are independent conditional on E being known. (The book here says we
#' get this from equation 1.1 and i'm not actually at all clear on how you get that from there?)
#' #### *Divergent connections**
#' But the same applies to the connection between occupation and residence: knowing
#' the education makes them independent:
dsep(dag, x = "O", y = "R")
dsep(dag, x = "O", y = "R", z = "E")

#' #### *convergent connections**
#' But, on the other hand conditioning can make two variables dependent when they are marginally
#' independent:
#' 
dsep(dag, x = "A", y = "S")
dsep(dag, x = "A", y = "S", z = "E")
#' 
#' So Age and Sex are independent, but within a certain level of education they are not. If E
#' is known we cannot decompose the joint distribution of A and S in a part that depends
#' only on A and a part that depends only on S. (hm, it's not clear to me again). 
#' 
#' when the parents in a convergent connection are not connected it is called a *v-structure*. 
#' 
#' These questions about conditional independence can be answered only using the DAG structure.
#' But more complicated queries requrire the local distributions (which are of course
#' indirectly connected to the DAG)
#' 
#' ### 1.6.2. Using the Conditional Probability tables
#' 
#' So the most common types are conditional probability and most likely explanation querries, both under 
#' *non-trivial* conditions. These querries can be answered either using **exact** or ** approximate** inference. 
#' 
#' #### 1.6.2.1 Exact Inference
#' 
#' Implemented using the `gRain` package (**gRa**phical model **in**ference), which "relies on transforming 
#' the BN into a specially crafted tree--a **junciton tree**--to speed up the computation of conditional probabilities.
#' 
#' Here's how you do it: First you start with a list of CPTs, in our case `bn`, and using the4`as.grain()` function 
#' to  create a junction tree, and then compile to compute it's probability tables
#' 
junction <- compile(as.grain(bn))

#' OK, now I don't know what a junciton tree is, nor is it really explained here.. Also at this point
#' it's not clear why we are using gRain, not bnlearn, what is the difference between the packages..
#' 
#' Anyway, we can now query the model. E.g. what are the attitutes of women to different
#' modes of transport relative to the wholes survey. First the whole survey, then to 
#' the set evidenev version of the junction tree:
#' 
#' 
#+  echo = -(1)
options(digits = 4)

#' $Pr(T)$
querygrain(junction, nodes = "T")
#' $Pr(T \ S = F)$
jsex <- setEvidence(junction, nodes = "S", states = "F")
querygrain(jsex, nodes = "T")
#' essentially no difference. What about how does living in a small city affect car and train use?
#' $Pr(T\ R = S)$
#'
jtownS <- setEvidence(junction, nodes = "R", states = "small")
querygrain(jtownS, nodes = "T")
jtownB <- setEvidence(junction, nodes = "R", states = "big")
querygrain(jtownB, nodes = "T")

#+ fig.dim = c(6, 4.5), fig.cap = "Exact pronanility distributions of Travel (T) given no evidence, town  is small, or town in big"
x<- barplot(t(rbind(
  querygrain(junction, nodes = "T")$T,
  querygrain(jtownS, nodes = "T")$T,
  querygrain(jtownB, nodes = "T")$T)),
  beside = TRUE,
  density = 20)

text(colMeans(x), -.1, c("no evidence", "small town ","big town"), xpd = TRUE)
text(as.vector(x), -.05, rep(jtownS$universe$levels$T, 2), xpd = TRUE)
text(colMeans(x), .62, c("P(T)","P(T | R = small) ","P(T | R = big) "), xpd = TRUE)

#'
#' A bit of text I don't understandt at the moment: "extending this query to provide the most likely explanation
#' we conclude that form people living in small cities the car is the preferred mode of transport". Is that all 
#' most likely explanationis? The largest probabaility given a certain distribution?
#' 
#' 
#' OK, now we can also test conditinal independence like we did befor with dsep: So let's see again sex and T, this
#' time conditioning on E = high. 
#' 
jedu <- setEvidence(junction, nodes = "E", states = "high")
SxT.cpt <- querygrain(jedu, nodes = c("S","T"), type = "joint")
SxT.cpt
#'
#' The `type` of distribution is also `marginal` and `conditional:
#' 
querygrain(jedu, nodes = c("S","T"), type = "marginal")
querygrain(jedu, nodes = c("S","T"), type = "conditional")

#' Erratum: this might have changed, but the text says columns should sum up to 1, but now
#' i'm getting rows summing up to 1. Nevermind. 
#' 
#' But yeah, here you cen see that all the conditional probabilities are the same, so conditional
#' on education being high, S and T are d-separated. 
#' 
dsep(bn, x = "S", y = "T", z = "E")
#' 
#' Or we can do a chi-square test on the joint probability distribution:
chisq.test(SxT.cpt * nrow(survey))

#' Also independent. Perfectly so!
#'
#' ## List of functions in this chapter 
#' 
#' ### from `bnlearn`
#' 
#' `empty.graph()` - produces a dag
#' 
#' `random.graph()`- produces a dag
#' 
#' `set.arc()` - adds an arc to a dag
#' 
#' `modelstring()` 
#' 
#' `arcs()`
#' 
#' `model2network()`
#' 
#' `custom.fit()` - a dag & cpt combo
#' 
#' `bn.fit()` - a dag and cpt from data using `method`
#' 
#' `ci.test()` - conditional independece test
#' 
#' `arc.strenght()` - individual arc strenght based on `criterion`
#' 
#' `nparams()` - in a dag & data combo. 
#' 
#' `score()` - network score
#' 
#' `hc()` - learnes a dag from data
#' 
#' `desp()` - test d-separation
#' 
#' `as.grain()` converts a `bn.fit` object to a  `grain` object
#' 
#' ### from `gRain`
#' 
#' `compile()` compute the probability tables from a junction tree (grain == independence network). 
#' 
#' `setEvidence()`
