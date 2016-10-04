module PolygonArea where

computeArea :: [(Double,Double)] -> Double
computeArea [] = error "Empty list"
computeArea [q1] = error "List contains single vertex"
computeArea [q1,q2] = error "List describes a line"
computeArea [q1,q2,q3] = (det q1 q2 + det q2 q3 + det q3 q1)/2
computeArea qs = computeArea (take 3 qs) + computeArea ([head qs] ++ tail (tail qs))

det :: (Double,Double) -> (Double,Double) -> Double
det q1 q2 = (fst q1 * snd q2) - (fst q2 * snd q1)
