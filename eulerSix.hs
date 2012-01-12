sumNaturalNumbers n = sum [1..n]
squareNaturalNumbers n = map (^2) [1..n]

eulerSix n = (sumNaturalNumbers n)^2 - (sum (squareNaturalNumbers n))