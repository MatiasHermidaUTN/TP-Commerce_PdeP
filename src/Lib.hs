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
aplicarDescuento unDescuento unPrecio = (*unPrecio).(1-) $ unDescuento --0<=descuento<=1

entregaSencilla :: String -> Bool
entregaSencilla = (even.length)

productoDeElite :: Producto -> Bool
productoDeElite producto = ((&&).productoDeLujo $ producto).((&&).productoCodiciado $ producto).not.productoCorriente $ producto

productoDeLujo :: Producto -> Bool
productoDeLujo producto = ((elem 'x').(fst)) producto || ((elem 'z').(fst)) producto || ((elem 'X').(fst)) producto || ((elem 'Z').(fst)) producto

productoCodiciado :: Producto -> Bool
productoCodiciado = (>10).length.fst

productoCorriente :: Producto -> Bool
productoCorriente = letraVocal.head.fst

letraVocal :: Char -> Bool
letraVocal unaLetra = elem unaLetra "aeiouAEIOU"

productoXL :: Producto -> Producto
productoXL producto = ((++ "XL").fst $ producto, snd producto)

versionBarata :: Producto -> Producto
versionBarata producto = (reverse.fst.descodiciarProducto $ producto, snd producto)

descodiciarProducto :: Producto -> Producto
descodiciarProducto producto = ((take 10).fst $ producto, snd producto)