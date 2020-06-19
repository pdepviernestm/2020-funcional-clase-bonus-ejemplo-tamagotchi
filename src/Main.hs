module Main where
import PdePreludat
import Tamagotchi
-- Codigo para interactuar con el usuario
tamagotchiInicial :: Tamagotchi
tamagotchiInicial = Tamagotchi { felicidad = 0, hambre = 0, haciendo = Nada } 

main :: IO ()
main = interactuarCon tamagotchiInicial

-- Maneja el flujo del programa
interactuarCon :: Tamagotchi -> IO ()
interactuarCon tamagotchi = do
  mostrar tamagotchi
  putStrLn "Ingresa una acción:"
  accion <- getLine
  let nuevoTamagotchi = (interpretarAccion accion) tamagotchi
  interactuarCon nuevoTamagotchi

-- Convierte lo que nos llega de afuera a algo de nuestro dominio
interpretarAccion :: String -> Accion
interpretarAccion accion = case accion of
  "correr" -> correr
  "cantar" -> cantar
  "comer" -> comer
  _ -> error "Esa no es una opcion valida :("

-- Convierte algo de nuestro dominio en algo que vamos a mandar para afuera
estadoComoKaomoji :: Tamagotchi -> String
estadoComoKaomoji tamagotchi = case estado tamagotchi of
  Feliz -> "♡＼(￣▽￣)／♡"
  Triste -> "(╥﹏╥)"
  Hambriento -> "(￣﹃￣)"
  Haciendo Correr -> "ε=ε=ε=ε=ε=┌(；　・＿・)┘"
  Haciendo Cantar -> "♬♩♪♩(　◜◒ ◝　 )♩♪♩♬"
  Haciendo Nada -> "(◕‿◕)"

mostrar :: Tamagotchi -> IO ()
mostrar tamagotchi = do
  putStrLn $ show tamagotchi
  putStrLn $ estadoComoKaomoji tamagotchi
  putStrLn ""