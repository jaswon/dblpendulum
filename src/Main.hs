{-# LANGUAGE RecordWildCards #-}
module Main where

import Graphics.Gloss

density = 10 :: Float

win :: Display
win = InWindow "Double Pendulum" (600,600) (100,100)

data State = State 
  { l1  :: !Float
  , l2  :: !Float
  , m1  :: !Float
  , m2  :: !Float
  , t1  :: !Float
  , t2  :: !Float
  , t'1 :: !Float
  , t'2 :: !Float
  , g   :: !Float
  , s   :: !Float
  , tl  :: !Int
  , p2t :: ![Point]
  , p1t :: ![Point]
  } deriving (Eq, Show)

getP1 :: State -> Point
getP1 State{..} = (l1*(sin t1),-l1*(cos t1))

getP2 :: State -> Point
getP2 State{..} = (l1*(sin t1) + l2 * sin t2, -l1*(cos t1) - l2 * cos t2)

initState :: State
initState = State 100 100 20 20 (pi/2) (pi/2) 0 0 9.8 10 500 [] []

fadingLine :: [Point] -> Float -> Float -> Picture
fadingLine ( p1:p2:ps ) alpha factor
  = pictures [ color (withAlpha alpha white) $ line [ p1,p2 ]
             , fadingLine ( p2:ps ) ( alpha*factor ) factor
             ]
fadingLine _ _ _ = Blank

drawState :: State -> Picture
drawState state@State{..}
  = let
    r1 = sqrt $ density * m1 / pi
    r2 = sqrt $ density * m2 / pi
  in 
    color white $ pictures 
      [ line [ (0,0), getP1 state, getP2 state ]
      , fadingLine p2t 1 0.99
      , uncurry translate (getP1 state) $ circleSolid r1
      , uncurry translate (getP2 state) $ circleSolid r2
      ]

stepState :: Float -> State -> State
stepState t state@State{..}
  = let
    den = 2*m1 + m2 - m2 * (cos $ 2*t1 - 2*t2)
    t''1 = (-g*(2*m1+m2)*(sin t1) - 
           m2*g*(sin $ t1-2*t2) - 
           2*(sin $ t1-t2)*m2*(t'2^2*l2+t'1^2*l1*(cos$t1-t2)))/den/l1
    t''2 = 2*(sin $ t1-t2)*(
             t'1^2*l1*(m1+m2) +
             g*(m1+m2)*(cos t1) +
             t'2^2*l2*m2*(cos $ t1-t2)
           )/den/l2
    nt'1 = t'1 + t''1 * t * s
    nt'2 = t'2 + t''2 * t * s
  in state { t1 = t1 + nt'1 * t * s
           , t2 = t2 + nt'2 * t * s
           , t'1 = nt'1
           , t'2 = nt'2 
           , p1t = take tl (getP1 state : p1t)
           , p2t = take tl (getP2 state : p2t)
           }

main :: IO ()
main = simulate win black 60 initState drawState (const stepState)
