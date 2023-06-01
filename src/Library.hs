module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f camp v1 v2 = comp (f v1) (f v2)

rimaAsonante :: Palabra -> Palabra -> Bool
rimaAsonante = cumplen (ultimasVocales 2) (==)

rimaConsonante :: Palabra -> Palabra -> Bool
rimaConsonante = cumplen (ultimasLetras 3) (==)



ultimasLetras :: Number -> Palabra -> String
ultimasLetras n = reverse . take n . reverse


ultimasVocales :: Number -> Palabra -> String
ultimasVocales n = ultimasLetras n . filter vocal

rima :: Palabra -> Palabra -> Bool
rima palabra1 palabra2 = palabra1 /= palabra2 && (rimaAsonante palabra1 palabra2 || rimaConsonante palabra1 palabra2)

vocal :: Char -> Bool
vocal letra = esVocal letra || tieneTilde letra

type Conjugacion = Verso -> Verso -> Bool

conjugaRima :: Conjugacion
conjugaRima verso1 verso2 = rima (ultimaPalabra verso1) (ultimaPalabra verso2) 

anadiplosis :: Conjugacion
anadiplosis verso1 verso2 = (ultimaPalabra verso1) == (primeraPalabra verso2)

ultimaPalabra :: Verso -> Palabra
ultimaPalabra = last . words

primeraPalabra :: Verso -> Palabra
primeraPalabra = head . words

type Patron = Estrofa -> Bool

type Par = (Number, Number)

simple :: Par -> Patron
simple (n1, n2) estrofa = versoAt n1 estrofa `conjugaRima` versoAt n2 estrofa

versoAt :: Number -> Estrofa -> Verso
versoAt n estrofa = estrofa !! (n-1)

esdrujula :: Palabra -> Bool
esdrujula = tieneTilde . head . ultimasVocales 3

patronEsdrujula :: Patron
patronEsdrujula = all (esdrujula . ultimaPalabra)

anafora :: Patron
anafora versos = all ((primeraPalabra (head versos)==).primeraPalabra) versos

cadena :: Conjugacion -> Patron
cadena _ [] = False
cadena _ [_] = True
cadena conjugacion (verso1 : verso2 : versos) = conjugacion verso1 verso2 && cadena conjugacion (verso2 : versos)

combinado :: Patron -> Patron -> Patron
combinado patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa

aabb :: Patron
aabb = simple (1, 2) `combinado` simple (3, 4)

abab :: Patron
abab = simple (1, 3) `combinado` simple (2, 4)

abba :: Patron
abba = simple (1,4) `combinado` simple (2,3)

hardcore :: Patron
hardcore =  cadena conjugaRima `combinado` patronEsdrujula

data PuestaEnEscena = UnaPuestaEnEscena{
    artista :: Artista,
    potencia :: Number,
    publicoExaltado :: Bool,
    freestyle :: Estrofa
}

puestaBase :: Artista -> Estrofa -> PuestaEnEscena
puestaBase mc estrofa = UnaPuestaEnEscena { artista = mc, potencia = 1, publicoExaltado = False, freestyle = estrofa}

aumentarPotencia :: Number -> PuestaEnEscena -> PuestaEnEscena
aumentarPotencia n puesta = puesta {potencia = potencia puesta * (1 + n)}

exaltarPublico :: Bool -> PuestaEnEscena -> PuestaEnEscena
exaltarPublico exaltado puesta = puesta {publicoExaltado = exaltado}
