module Library where
import PdePreludat

data Ingrediente =
    PanIntegral | Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | TofuBacon
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente TofuBacon = 10
precioIngrediente PanIntegral = 3


data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


{-agrandar: cada vez que se agranda una hamburguesa se agrega otro ingrediente base 
    (por ahora, son Carne o Pollo), 
    se elige el ingrediente base a agregar según lo que ya haya en la hamburguesa -}

agrandar :: Hamburguesa -> Hamburguesa
agrandar = agregarIngrediente . ingredienteBase 

ingredienteBase :: Hamburguesa -> (Hamburguesa, Ingrediente)
ingredienteBase hamburguesa = (hamburguesa, primerBase(ingredientes hamburguesa))

primerBase :: [Ingrediente] -> Ingrediente
primerBase (x:xs)
    | esBase x = x
    | otherwise = primerBase xs

esBase :: Ingrediente -> Bool
esBase Carne = True
esBase Pollo = True
esBase PatiVegano = True
esBase _ = False

-- agregarIngrediente: recibe un ingrediente y una hambrugesa lo agrega a la hamburguesa.

agregarIngrediente :: (Hamburguesa, Ingrediente) -> Hamburguesa
agregarIngrediente (hamburguesa, nuevoIngrediente) = 
    hamburguesa {ingredientes = nuevoIngrediente : ingredientes hamburguesa }

--descuento: recibe un % de descuento, 
 -- y devuelve la hamburguesa con ese descuento aplicado al precio base.

descuento :: (Number,Hamburguesa) -> Hamburguesa
descuento (descuento,hamburguesa) = 
    hamburguesa {
        precioBase = precioBase hamburguesa - (precioBase hamburguesa * (descuento / 100))
    }

-- la pdepBurger, que es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento. 
-- Su precio final deberia ser 110.

cuartodelibra :: Hamburguesa
cuartodelibra = Hamburguesa {
    precioBase = 137.5 ,
    ingredientes = [Pan, Carne, Cheddar, Pan]
}

pdepBurger :: Hamburguesa
pdepBurger = descuento (20, agrandar (agrandar (agregarIngrediente (cuartodelibra, Panceta)) ))


-- dobleCuarto = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84.

dobleCuarto :: Hamburguesa
dobleCuarto = agrandar (agregarIngrediente (agregarIngrediente(cuartodelibra,Cheddar) , Panceta ))

-- bigPdep = es un doble cuarto con curry. El precio final deberia ser 89.

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente(dobleCuarto,Curry)

-- delDia = es una promo que, dada una hamburguesa, le agrega Papas y un descuento del 30%.

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento (30 , agregarIngrediente(hamburguesa,Papas) )

-- hacerVeggie : cambia todos los ingredientes base que hayan en la hamburguesa por PatiVegano ,
-- el cheddar lo cambia por queso de almendras y la panceta por bacon de tofu.

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = hamburguesa {ingredientes = auxVeggie (ingredientes hamburguesa) }

auxVeggie :: [Ingrediente] -> [Ingrediente]
auxVeggie [] = []
auxVeggie (x:xs)
    | esIngredienteConvertible x = convertirIngrediente x : auxVeggie xs
    | otherwise = x: auxVeggie xs


esIngredienteConvertible :: Ingrediente -> Bool 
esIngredienteConvertible Carne = True 
esIngredienteConvertible Pollo  = True 
esIngredienteConvertible Cheddar = True 
esIngredienteConvertible Panceta = True 
esIngredienteConvertible _ = False 

convertirIngrediente :: Ingrediente -> Ingrediente
convertirIngrediente Carne = PatiVegano
convertirIngrediente Pollo = PatiVegano
convertirIngrediente Cheddar = QuesoDeAlmendras
convertirIngrediente Panceta = TofuBacon
convertirIngrediente Pan = PanIntegral


-- cambiarPanDePati : cambia el Pan que haya en la hamburguesa por PanIntegral (ingrediente de precio 3).

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = hamburguesa { ingredientes = aux (ingredientes hamburguesa)}

aux :: [Ingrediente] -> [Ingrediente]
aux [] = []
aux (x:xs)
    | esPan x = convertirIngrediente x : aux xs
    | otherwise = x: aux xs

esPan :: Ingrediente -> Bool
esPan Pan = True


-- hacer el dobleCuartoVegano, que es un dobleCuarto veggie con pan integral.

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = hacerVeggie (cambiarPanDePati dobleCuarto)








