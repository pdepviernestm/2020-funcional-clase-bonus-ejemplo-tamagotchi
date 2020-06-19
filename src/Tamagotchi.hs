module Tamagotchi where
import PdePreludat
-- Codigo del modelo (lo que hacemos en PdeP)

data Tamagotchi = Tamagotchi { hambre :: Number, felicidad :: Number, haciendo :: Actividad }
  deriving (Eq, Show)

data Actividad = Cantar | Correr | Nada
  deriving (Eq, Show)

data Estado = Feliz | Triste | Hambriento | Haciendo Actividad
  deriving (Eq, Show)

type Accion = Tamagotchi -> Tamagotchi

hambriento :: Tamagotchi -> Bool
hambriento = (>= 100) . hambre

feliz :: Tamagotchi -> Bool
feliz = (>= 100) . felicidad

triste :: Tamagotchi -> Bool
triste = (== 0) . felicidad

estado :: Tamagotchi -> Estado
estado tamagotchi | feliz tamagotchi = Feliz
                  | triste tamagotchi = Triste
                  | hambriento tamagotchi = Hambriento
                  | otherwise = Haciendo (haciendo tamagotchi)

cansarse :: Number -> Accion
cansarse cuanto tamagotchi = tamagotchi { hambre = hambre tamagotchi + cuanto }

aumentarFelicidad :: Number -> Accion
aumentarFelicidad cuanto tamagotchi = tamagotchi { felicidad = felicidad tamagotchi + cuanto }

hacer :: Actividad -> Accion
hacer actividad tamagotchi = tamagotchi { haciendo = actividad }

comer :: Accion
comer tamagotchi = tamagotchi { hambre = max 0 $ hambre tamagotchi - 20 }

cantar :: Accion
cantar = hacer Cantar . aumentarFelicidad 10

correr :: Accion
correr = hacer Correr . cansarse 20
