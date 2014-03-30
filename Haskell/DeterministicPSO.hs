import Data.List

{-
Hilfsfunktionen auf Listen. Fuer Vektoroperationen wird generell als Vorbedingung
angenommen, dass die Vektoren die gleiche Dimension haben.
-}

{-
Addition von Vektoren
-}
vAdd :: [[Double]] -> [Double]
vAdd vectors = map addComponent (transpose vectors)
  where
  addComponent :: [Double] -> Double
  addComponent c = foldl1 (+) c

{-
Skalarmultiplikation eines Vektors v mit einem Skalar s
-}
vScalarMult :: Double -> [Double] -> [Double]
vScalarMult s v = map multComponent v
  where
  multComponent x = s*x

{-
Multipliziert einen Vektor mit -1
-}
vSign :: [Double] -> [Double]
vSign vector = map (\x -> -x) vector

{-
gibt Listen mathematica-konform aus
-}
mathematicaList :: Show a => a -> String
mathematicaList l = (changeBraces.show) l
  where
  changeBraces t = map changeBrace t
    where
    changeBrace c
      | c == '[' = '{'
      | c == ']' = '}'
      | otherwise = c

{-
Die Fitnessfunktion f:R^2 -> R
-}
f:: [Double] -> Double
f [x,y] = sin(x*y)

{-
Partikel pi hat das Format {phi1,phi2,x,v,lbest}, wobei phi1, phi2 \
Beschleunigungsfaktoren, x die Position, v die Geschwindigkeit, und \
lbest die Position der besten Loesung sind.
Selectors:
selectAcceleration1 : gibt phi1 zurueck
selectAcceleration2 : gibt phi2 zurueck
selectPosition : gibt Position x zurueck
selectVecolity : gibt Geschwindigkeit v zurueck
selectBest : gibt beste Loesung des Parikels lbest zurueck
-}
data Particle = Particle Double Double [Double] [Double] [Double] deriving Show

selectAcceleration1 :: Particle -> Double
selectAcceleration1 (Particle c1 _ _ _ _) = c1

selectAcceleration2 :: Particle -> Double
selectAcceleration2 (Particle _ c2 _ _ _) = c2

selectPosition :: Particle -> [Double]
selectPosition (Particle _ _ x _ _) = x

selectVecolity :: Particle -> [Double]
selectVecolity (Particle _ _ _ v _) = v

selectBest :: Particle -> [Double]
selectBest (Particle _ _ _ _ b) = b

{-
Der Zustand eines Partikelschwarms mit Partikel pi , i aus {1,m}, zu \
einem Zeitpunkt ist von der Gestalt {{p1 ... pm},gbest}.
getters:
selectSwarm : gibt die liste aller Partikel zurueck
selectGlobalBest : gibt die globale beste Loesung gbest zurueck
getSwarmPositions : gibt die positionen der partikel in einem schwarm zurueck
getPositionsOfStates : gibt fuer jeden Zustand des partikelschwarmes die positionen der Partikel zurueck
-}
data ParticleSwarm = ParticleSwarm [Particle] [Double] deriving Show

selectSwarm :: ParticleSwarm -> [Particle]
selectSwarm (ParticleSwarm swarm _) = swarm

selectGlobalBest :: ParticleSwarm -> [Double]
selectGlobalBest (ParticleSwarm _ gbest) = gbest

getSwarmPositions :: ParticleSwarm -> [[Double]]
getSwarmPositions (ParticleSwarm swarm _) = map selectPosition swarm

getPositionsOfStates :: [ParticleSwarm] -> [[[Double]]]
getPositionsOfStates swarms = map getSwarmPositions swarms

{-
----------
Ein Particle Swarm Optimizer ohne stochastische Beschleunigungsfaktoren
----------
-}

{-
Der folgende Particle Swarm Optimizer sucht Minima der \
Fitnessfunktion f auf einem 2-dimensionalen Suchraum. Elemente des Suchraumes
werden durch Listen von Double Elementen dargestellt.
-}

{-
Gibt die Liste der besten Loesungen des Schwarms Zurueck
-}
bestSolutions :: ParticleSwarm -> [[Double]]
bestSolutions (ParticleSwarm swarm gbest) = (map selectBest swarm) ++ [gbest]

{-
Berechnet den Wert der besten Position des Schwarms
-}
swarmMinimum :: ParticleSwarm -> Double
swarmMinimum (ParticleSwarm swarm gbest) = minimum ( (map (f.selectBest) swarm) ++ [f gbest] )

{-
Gibt eine Position mit der momentan optimalen Loesung zurueck
-}
swarmMinimumPosition :: ParticleSwarm -> [Double]
swarmMinimumPosition pswarm = (!! )[x | x <- (bestSolutions pswarm) , (f x) == (swarmMinimum pswarm)] 0

{-
Bildet ein Partikel auf ein Partikel mit Geschwindigkeit und Position \
zum naechsten Zeitpunkt mit ansonsten gleichen Zustand ab.
-}
updateParticlePositionVecolity :: ParticleSwarm -> Particle -> Particle
updateParticlePositionVecolity (ParticleSwarm swarm gbest) (Particle c1 c2 x v lbest) = Particle c1 c2 (vAdd [x , v , (vScalarMult c1 (vAdd [lbest,vSign x]) ) , (vScalarMult c2 (vAdd [gbest,vSign x])) ]) (vAdd [ v , (vScalarMult c1 (vAdd [lbest,vSign x]) ) , (vScalarMult c2 (vAdd [gbest,vSign x])) ]) lbest

{-
Berechnet den Schwarm zum naechsten Zeitpunkt mit den besten \
Loestungen aus dem vorhergehenden Zeitpunkt
-}
updatePositionVecolity :: ParticleSwarm -> ParticleSwarm
updatePositionVecolity (ParticleSwarm swarm gbest) = ParticleSwarm (map (updateParticlePositionVecolity (ParticleSwarm swarm gbest)) swarm) gbest

{-
Updated die lokalen besten Loesungen
-}
updateLocalBest :: ParticleSwarm -> ParticleSwarm
updateLocalBest (ParticleSwarm swarm gbest) = ParticleSwarm (map selectBestPosition swarm) gbest
  where
  selectBestPosition :: Particle -> Particle
  selectBestPosition (Particle c1 c2 x v lbest)
    | (f x) < (f lbest) = Particle c1 c2 x v x
    | otherwise = Particle c1 c2 x v lbest

{-
Updated die globale beste Loesung
-}
updateGlobalBest :: ParticleSwarm -> ParticleSwarm
updateGlobalBest (ParticleSwarm swarm gbest) = ParticleSwarm swarm (swarmMinimumPosition (ParticleSwarm swarm gbest))

{-
Berechnet den Zustand des Schwarmes zum naechsten Zeitpunkt vollstaendig.
-}
updateParticleSwarm :: ParticleSwarm -> ParticleSwarm
updateParticleSwarm particleSwarm = (updateGlobalBest.updateLocalBest.updatePositionVecolity) particleSwarm

{-
Berechnet die Zustaende des Partikelschwarmes zu den ersten n Iterationen.
-}
pso :: ParticleSwarm -> Int -> [ParticleSwarm]
pso particleSwarm n = take (n+1) (iterate updateParticleSwarm particleSwarm)

{-
----------
Berechnung der ersten n Zustaende einer Beispielinstanz eines Partikelschwarms
----------
-}
phi1 = 0.01
phi2 = 0.001
exampleSwarm = ParticleSwarm [Particle phi1 phi2 [0.5,0.5] [0.08,0.01] [0.5,0.5] , Particle phi1 phi2 [1,-1] [0,0.01] [1,-1] , Particle phi1 phi2 [-0.75,0.1] [0.05,0.04] [-0.75,0.1]] [0,0]
exampleSwarmStates = pso exampleSwarm 50
exampleSwarmTrajectories = (transpose.getPositionsOfStates) exampleSwarmStates
exampleMathematicaInput = mathematicaList exampleSwarmTrajectories