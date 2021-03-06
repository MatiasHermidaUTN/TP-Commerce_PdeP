module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
---------------------------------------------------------------------------------------------------------------------------------------------------

type Nombre = String
type Precio = Float
type Producto = (Nombre, Precio)

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal producto cantidad descuento costoEnvio =
 (aplicarCostoDeEnvio costoEnvio).(* cantidad).(aplicarDescuento descuento).snd $ producto

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio unCostoDeEnvio unPrecio = unPrecio + unCostoDeEnvio

aplicarDescuento :: Float -> Float -> Float 
aplicarDescuento unDescuento unPrecio = (*unPrecio).(1-) $ unDescuento

entregaSencilla :: String -> Bool
entregaSencilla = (even.length)

productoDeElite :: Producto -> Bool
productoDeElite producto = ((&&).productoDeLujo $ producto).((&&).productoCodiciado $ producto).not.productoCorriente $ producto

productoDeLujo :: Producto -> Bool
productoDeLujo producto = (contieneAlgunaDeLasLetras "xzXZ").fst $ producto

contieneAlgunaDeLasLetras :: String -> String -> Bool
contieneAlgunaDeLasLetras listaLetras nombre = algunoVerdadero.(map (estaEnListaLetras listaLetras)) $ nombre

estaEnListaLetras :: String -> Char -> Bool
estaEnListaLetras listaLetras letra = elem letra listaLetras 

algunoVerdadero :: [Bool] -> Bool
algunoVerdadero = any (== True)

productoCodiciado :: Producto -> Bool
productoCodiciado = (>10).length.fst

productoCorriente :: Producto -> Bool
productoCorriente = esLetraVocal.head.fst

esLetraVocal :: Char -> Bool
esLetraVocal unaLetra = estaEnListaLetras "aeiouAEIOU" unaLetra 

productoXL :: Producto -> Producto
productoXL producto = alterarNombre (++ "XL") producto

versionBarata :: Producto -> Producto
versionBarata = (alterarNombre reverse).descodiciarProducto

descodiciarProducto :: Producto -> Producto
descodiciarProducto producto = alterarNombre (take 10) producto

alterarNombre :: (String -> String) -> Producto -> Producto
alterarNombre unaFuncion producto = (unaFuncion.fst $ producto, snd producto)