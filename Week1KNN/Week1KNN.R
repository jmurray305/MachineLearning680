library(ggplot2)
library(DescTools)
MyDF <- (iris) 


ggplot(MyDF, aes(x=Petal.Length, y = Petal.Width, color=Species)) +
  geom_point()

MyK = 3

MyNewIris= data.frame("Petal.Length" = 2.25, "Petal.Width" = 0.75)

ggplot(MyDF, aes(x = Petal.Length, y = Petal.Width, col = Species)) + 
  geom_point() +
  geom_point(aes(x=2.25, y=0.75), colour="black", shape=4)

myTestIris = iris
euclDist <- function(x,y){
  return(
    sqrt((MyNewIris$Petal.Length - x)^2 + (MyNewIris$Petal.Width - y)^2) 
        )
}
euclDist(2,1)
euclDist(2.25,0.75)

MyIris3 <- iris
MyIris3$Dist <- mapply(FUN = euclDist, MyIris3$Petal.Width, MyIris3$Petal.Length)
attach(MyIris3); MyIris3 <- MyIris3[order(Dist),];detach(MyIris3)

paste('The new Iris is classified as belonging to the',
      DescTools::Mode(MyIris3[1:MyK, 5]),
      'species.'
)


# trying a more challenging test. from the plot (4.8,1.625) is on the margin between versicolor and virginica.
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) + 
  geom_point() +
  annotate("text", x=4.8, y=1.625, label="X")

MyK = 9
MyNewIris = data.frame("Petal.Length"=4.8, "Petal.Width"=1.625)

MyIris3 <- iris
MyIris3$Dist <- mapply(FUN = euclDist, MyIris3$Petal.Width, MyIris3$Petal.Length)
attach(MyIris3); MyIris3 <- MyIris3[order(Dist),];detach(MyIris3)

paste('The new Iris is classified as belonging to the',
      DescTools::Mode(MyIris3[1:MyK, 5]),
      'species.'
)


