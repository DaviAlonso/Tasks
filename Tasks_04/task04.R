trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)

trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)

Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
boxplot(Sample1, Sample2)
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da") 
head(MatGrandma)
nrow(MatGrandpa)
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
ToMom <- length(grep("mom", Focus)) / length(Focus)
ToMomMom <- length(grep("grandma_mom", Focus)) / length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus)) / length(Focus)
#half I would assume.
ToMom
Focus
#I didnt expect a list of eggs and sperm... at all. I'm not sure I did this one correctly
Sibling_01 <- makeBaby(Brenda, Alan)
Sibling_01
#I dont know how much DNA would be shared between my sibling and I but I think it would be much higher than 500
ToSib <- length( intersect( Focus, Sibling_01)) / length(Focus)
ManySiblings <- replicate(1e3, length( intersect(Focus, makeBaby(Brenda, Alan))) / length(Focus))
quantile(ManySiblings)
#what does this indicate?
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
#ask mitchell for help with line 116
HWE <- function(p) {
  aa <- p^2
  ab <- 2*p*(1-p)
  bb <- (1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab=" freq. allele a", ylab="geno. freq")
p <- seq(from=0, to=1, by=0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#answer question on line 144
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=22, bg="red")
#install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
