module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import System.Environment
import System.IO

-- constants
------------------------------------------------

-- level depandant
------------------------------------------------

--
data World = World {squareX :: Float, squareY :: Float, squareCounter :: Int, walls :: [Wall], wallCounter :: Int, wallUp :: Bool, isOver :: Bool, score :: Int}
data Wall = Wall {wallX :: Float, wallY :: Float, wallH :: Float}
------------------------------------------------


gen :: Int -> Int -> IO Int
gen a b = getStdRandom . randomR $ (a,b)

square :: (Float, Float) -> Float -> Picture
square (p1, p2) a = Polygon [(p1, p2), (p1 + a, p2), (p1 + a, p2 - a), (p1, p2 - a)]

rectangle :: (Float, Float) -> Float -> Float -> Picture
rectangle (p1, p2) w h = Polygon [(p1, p2), (p1 + w, p2), (p1 + w, p2 - h), (p1, p2 - h)]

moveWall :: (Float, Float) -> Wall -> Wall
moveWall (x, y) w = Wall (wallX w + x) (wallY w + y) (wallH w)

findColor1 t w = find t w 255 255 -- t w max_wart domyślna_wart
findColor2 t w = find t w 255 0
findWidth t w = find2 t w 1920 1500
findHeight t w = find2 t w 1080 1000
findLevel t w = find2 t w 10 3

find:: [String] -> String -> Int -> Int -> Int
find [] w o d = d
find (x:xs) w o d = if x=="" then find xs w o d
                else let (x1:x2:x3) = words x in
                        if x1==w && (read (head x3)) <o && (read (head x3))>0 then (read (head x3))
                        else find xs w o d

find2:: [String] -> String -> Float -> Float -> Float
find2 [] w o d = d
find2 (x:xs) w o d = if x=="" then find2 xs w o d
                    else let (x1:x2:x3) = words x in
                    if x1==w && (read (head x3)) <o && (read (head x3))>0 then (read (head x3))
                    else find2 xs w o d
    
--main :: IO ()
main = do
      contents <- readFile "config.txt"

      let x = lines contents
    
      let bgColorR = findColor1 x "bgColorR"
      let bgColorG = findColor1 x "bgColorG"
      let bgColorB = findColor1 x "bgColorB"
      let squareColorR = findColor2 x "squareColorR"
      let squareColorG = findColor2 x "squareColorG"
      let squareColorB = findColor1 x "squareColorB"
      let wallColorR = findColor1 x "wallColorR"
      let wallColorG = findColor2 x "wallColorG"
      let wallColorB = findColor2 x "wallColorB"
      let windowWidth = findWidth x "windowWidth"
      let windowHeight = findHeight x "windowHeight"
      let level = findLevel x "level"

      let bgColor = makeColorI bgColorR bgColorG bgColorB 255
      
      let squareColor = makeColorI squareColorR squareColorG squareColorB 255
      
      let wallColor = makeColorI wallColorR wallColorG wallColorB 255
      
      let windowXPos = 0
      
      let windowsYPos = 0
      
      let step = 100
      
      let squareSize = windowHeight / 10
      
      let jump = windowHeight / 100
      
      let fall = windowHeight / 300
      
      let fallCounter = round $ windowHeight / 100

      let wallWidth = windowWidth * (1 + level / 10) / 10          
               
      let wallHeight = windowHeight * (1 + level / 8) / 2.5

      let wallShift = wallHeight / 2  
         
      let newWallCounter = round $ 200 * (1 - level / 20)         
               
      let wallStep = windowHeight * (1 + level / 10) / 300    
      
      let moveSquare (x, y) sc wc w = do
            v <- w
            let st = squareY v + y
            let sb = st - squareSize
            let b = windowHeight / 2
            if st <= b && sb >= (-b) 
                then return $ World (squareX v + x) (squareY v + y) (squareCounter v + sc) (walls v) (wallCounter v + wc) (wallUp v) (isOver v) (score v)
                else return $ World (squareX v) (squareY v) (0) (walls v) (wallCounter v + wc) (wallUp v) (isOver v) (score v)
      
      let getWall u = do
                a <- gen  (round $ windowHeight / 2) (round $ windowHeight / 2 + wallShift)
                b <- gen  (round $ wallHeight - wallShift - windowHeight / 2) (round $ wallHeight - windowHeight / 2)
                if u == True
                    then return $ Wall (windowWidth / 2) (fromIntegral a) wallHeight
                    else return $ Wall (windowWidth / 2) (fromIntegral b) wallHeight

      let startWorld = do
            g <- (getWall False)
            return $ World (-200) 0 0 [g] newWallCounter  True False 0

          
      let moveWalls (x, y) w = do
            v <- w
            if any (\a -> squareX v >= wallX a + wallWidth && squareX v <= wallX a + wallStep + wallWidth) (walls v)
                then return $ World (squareX v) (squareY v) (squareCounter v) (fmap (moveWall (x, y)) (walls v)) (wallCounter v) (wallUp v) (isOver v) (score v + 1)
                else return $ World (squareX v) (squareY v) (squareCounter v) (fmap (moveWall (x, y)) (walls v)) (wallCounter v) (wallUp v) (isOver v) (score v)
      
      let isVisible w = wallX w + wallWidth  >=  -(windowWidth / 2)
      
      let isOverlapping sx sy w = 
           let
            st = sy
            sb = sy - squareSize
            sl = sx
            sr = sx + squareSize
            wt = wallY w
            wb = wallY w - wallH w
            wl = wallX w
            wr = wallX w + wallWidth
           in (sb <= wt && st >= wb) && (sr >= wl && sl <= wr)
      
      let drawSquare w = Color squareColor . square  (squareX w, squareY w) $ squareSize
      
      let drawWall w = Color wallColor . rectangle (wallX w, wallY w) wallWidth $ wallH w

      let drawWalls w = fmap drawWall $ walls w

      
      let draw w  = do
            v <- w
            if isOver v
                then
                    let a = translate (-(windowWidth / 4)) 0 $ text "Game over!"
                        b = translate (-(windowWidth / 3.5)) (-(windowWidth / 6)) . text $ "Your score: " ++ show (score v) 
                    in return $ mappend a b
                else return $ mappend (drawSquare v) (mconcat (drawWalls v))
      
      let handleInput (EventKey (SpecialKey KeyUp) Down _ _) w = do
            v <- w
            if squareCounter v == 0
                then return $ moveSquare (0, 0) fallCounter 0 w
                else return w
      
      let handleInput (EventKey (SpecialKey KeyEnter) Down _ _) w = do
            v <- w
            if isOver v
                then do
                g <- getWall False
                return . return $ World (-200) 0 0 [g] newWallCounter True False 0
                else return w
      
      let handleInput _ w = return w

      let stepGame _ w = do
            v <- w
            z <- getWall (wallUp v)
            let nw = filter isVisible $ walls v
            case (squareCounter v, wallCounter v, any (isOverlapping (squareX v) (squareY v)) (walls v)) of 
                (_, _, True) -> return ( moveWalls ((0), 0) ( (moveSquare (0, (0)) 0 0 (return (World (squareX v) (squareY v) 0 (nw) 0 (not (wallUp v)) True (score v) )))))
                (0, 0, _) ->  return ( moveWalls ((-wallStep), 0) ( (moveSquare (0, (-fall)) 0 newWallCounter (return (World (squareX v) (squareY v) 0 (z : nw) 0 (not (wallUp v)) (isOver v) (score v))  ))))
                (0, wc, _) ->  return (moveWalls ((-wallStep), 0) ( (moveSquare (0, (-fall)) 0 (-1) (return(World (squareX v) (squareY v) 0 nw wc (wallUp v) (isOver v) (score v) )))))
                (sc, 0, _) ->   return ( moveWalls ((-wallStep), 0) ( (moveSquare (0, (-fall)) (-1) newWallCounter (return (World (squareX v) (squareY v) sc (z : nw) 0 (not (wallUp v)) (isOver v) (score v) ) ))))
                (sc, wc, _) -> return ( moveWalls ((-wallStep), 0) ( (moveSquare (0, (jump)) (-1) (-1) (return (World (squareX v) (squareY v) sc nw wc (wallUp v) (isOver v) (score v) )))))

      playIO
            (InWindow "flappy haskell" (round windowWidth, round windowHeight) (windowXPos,windowsYPos))
            bgColor
            step
            startWorld
            draw
            handleInput
            stepGame   
              