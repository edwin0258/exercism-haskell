module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex a = Complex { real :: a
                          ,imaginary :: a
                         } deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex x = Complex (fst x) (snd x)

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex rl im) = Complex rl (-im)

abs :: Floating a => Complex a -> a
abs (Complex rl im) = sqrt(rl^2 + im^2)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex arl aim) (Complex brl bim) = 
      Complex (arl * brl - aim * bim) (aim * brl + arl * bim)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex arl aim) (Complex brl bim) = Complex(arl + brl) (aim + bim)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex arl aim) (Complex brl bim) = Complex(arl - brl) (aim - bim)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex arl aim) (Complex brl bim) = 
      Complex ((arl * brl + aim * bim) / (brl^2 + bim^2)) 
              ((aim * brl - arl * bim) / (brl^2 + bim^2))
