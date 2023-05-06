import Text.Show.Functions ()
import Foreign (lengthArray0)

data Persona = UnaPersona { 
    edad :: Int,
    sueniosQueCumplir :: [Suenio],
    nombre :: String,
    habilidades :: [Habilidad],
    felicidonios :: Int
} deriving Show

type Suenio = String
type Habilidad = String

cantidadDeSuenios :: Persona -> Int
cantidadDeSuenios unaPersona = length (sueniosQueCumplir unaPersona)

tipoDeFelicidad :: Persona -> String
tipoDeFelicidad unaPersona  
    | felicidonios unaPersona > 100 = "Muy Feliz"
    | felicidonios unaPersona <= 50 = "Poco Feliz"
    | otherwise = "Moderadamente Feliz"

coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion unaPersona 
    | tipoDeFelicidad unaPersona == "Muy Feliz" =  edad unaPersona * felicidonios unaPersona
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" = cantidadDeSuenios unaPersona * felicidonios unaPersona
    | tipoDeFelicidad unaPersona == "Poco Feliz" = div (felicidonios unaPersona) 2

gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion unaPersona
    | tipoDeFelicidad unaPersona == "Muy Feliz" = felicidonios unaPersona * cantidadDeSuenios unaPersona
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" = edad unaPersona * cantidadDeSuenios unaPersona 
    | tipoDeFelicidad unaPersona == "Poco Feliz" = 2 * cantidadDeSuenios unaPersona

-- pto 2

--a

saberSiTieneNombreLargo :: Persona -> String
saberSiTieneNombreLargo unaPersona
    | (length . nombre) unaPersona > 10 = "Tiene nombre largo"
    | otherwise = "No tiene nombre largo"

--b



--c

nombreLindo :: Persona -> String
nombreLindo unaPersona
    | (last . nombre) unaPersona == 'a' = "Tiene lindo Nombre"
    | otherwise = "No tiene lindo nombre"


--pto3

recibirseDeUnaCarrera :: Persona -> String -> Persona
recibirseDeUnaCarrera unaPersona unaCarrera = unaPersona {felicidonios = felicidonios unaPersona + ((*1000) . length) unaCarrera , habilidades = unaCarrera : habilidades unaPersona}

type Ciudad = String

viajarAUnaListaDeCiudades :: Persona -> [Ciudad] -> Persona
viajarAUnaListaDeCiudades unaPersona ciudades = unaPersona {felicidonios = felicidonios unaPersona + ((+100) . length) ciudades, edad = edad unaPersona + 1}

enamorarseDeOtraPersona :: Persona -> Persona -> Persona
enamorarseDeOtraPersona  unaPersona otraPersona = unaPersona {felicidonios = felicidonios unaPersona + felicidonios otraPersona}
