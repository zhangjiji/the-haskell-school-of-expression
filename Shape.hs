module Shape (Shape) where

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving Show

type Radius = Float
type Side = Float
type Vertex = (Float, Float)

square s = Rectangle s s
circle r = Ellipse r r
rectangle s1 s2 = Polygon [s1, s2]
rtTriangle s1 s2 = Polygon [s1, s2]

regularPolygon :: Int -> Side -> Shape
regularPolygon n s = undefined

area :: Shape -> Float
area (Rectangle s1 s2) = s1 * s2
area (RtTriangle s1 s2) = s1 * s2 /2
area (Ellipse r1 r2) = pi * r1 * r2
area (Polygon (v1:vs)) = polyArea vs
  where polyArea :: [Vertex] -> Float
        polyArea (v2:v3:vs') = triArea v1 v2 v3 + polyArea (v3:vs')
        polyArea _ = 0

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3 = sqrt (s * (s-a) * (s-b) *(s-c))
  where s = (a+b+c)/2
        a = distBetween v2 v1
        b = distBetween v3 v2
        c = distBetween v1 v3

distBetween :: Vertex -> Vertex -> Float
distBetween v1 v2 = sqrt (dx ^ 2 + dy ^ 2)
  where dx = fst v2 - fst v1
        dy = snd v2 - snd v1
