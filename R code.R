
id = read.csv(file.choose(), header=T)  					#id = Internet_dataset
View(id)
head(id)
nrow(id)
ncol(id)

#PROB1
summary(id)

contfreq = sort(table(id$Continent), T)

plot(contfreq, col= rainbow(6), type = "b", xlab = "Continents",
     ylab = "Visit frequency from continents")

sg=sort(table(id$Sourcegroup),T)

lab = c("Google", "Directly", "Others", "Tableau","t.co",
        "site:publictableau","visualising","Reddit", "Facebook")

barplot(sg, col = "lightblue", xlab="Source Group", ylab = "Frequency", width = 3
        ,legend.text = "Frequency of Sourcegroup",space = .2,
        border = "darkblue", names.arg = lab)

names(id)
tail(id)
attach(id)

#PROB2

prob2 = aov(Uniquepageviews~Visits, data = id)
prob2
summary(prob2)

cor(Uniquepageviews, Visits)

#PROB3

prob3 = aov(Exits~., data = id)
summary(prob3)


#PROB4

prob4 = lm(Timeinpage~.,data = id)
summary(prob4)

#PROB5

table(Bounces)
Bouncesnew = Bounces * 0.01
table(Bouncesnew)


prob5 = glm(Bouncesnew~Timeinpage+Visits+Exits+
              Uniquepageviews+Sourcegroup+Continent,
            family = "binomial", data = id)
summary(prob5)



