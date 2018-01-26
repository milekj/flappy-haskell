module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random -- for random numbers generator

-----------------------------------------
-- constants

-- position of window
windowXPos :: Int
windowXPos = 0

windowsYPos :: Int
windowsYPos = 0

-- number of animation steps per second
step :: Int
step = 100

squareSize :: Float
squareSize = windowHeight / 10

-- number of pixels square flies up per one step
jump :: Float
jump = windowHeight / 100

-- number of pixels square falls down per step
fall :: Float
fall = windowHeight / 300

-- number of steps square continues to fly up
fallCounter :: Int
fallCounter = round $ windowHeight / 100

-- initial state of screen
startWorld :: IO World
startWorld = do
    g <- (getWall False)
    return $ World (-200) 0 0 [g] newWallCounter True False 0

-- shift used in creating new walls    
wallShift :: Float
wallShift = wallHeight / 2
-----------------------------------------

-- should be entered from file 

-- window size
windowWidth :: Float
windowWidth = 1500

windowHeight :: Float
windowHeight = 1000

-- colors of elemenets
bgColor :: Color
bgColor = makeColorI 140 202 235 255

squareColor :: Color
squareColor = makeColorI 255 64 64 255

wallColor :: Color
wallColor = makeColorI 39 174 96 255

-- difficulty level
level :: Float
level = 5

------------------------------------------------
-- level depandant


wallWidth :: Float
wallWidth = windowWidth * (1 + level / 10) / 10 

wallHeight :: Float
wallHeight = windowHeight * (1 + level / 8) / 2.5

-- number of steps before new wall is created
newWallCounter :: Int
newWallCounter = round $ 200 * (1 - level / 20)

-- number of pixels wall advances per one step
wallStep :: Float
wallStep = windowHeight * (1 + level / 10) / 300
------------------------------------------------
--                  square postion                      counter for flying up | list of walls on the screen   direction of last wall created | is game over | game score
data World = World {squareX :: Float, squareY :: Float, squareCounter :: Int, walls :: [Wall], wallCounter :: Int, wallUp :: Bool, isOver :: Bool, score :: Int}
--                  wall position                wall height
data Wall = Wall {wallX :: Float, wallY :: Float, wallH :: Float}
------------------------------------------------

-- standard random number generator [a,b]
gen :: Int -> Int -> IO Int
gen a b = getStdRandom . randomR $ (a,b)

-- drawing a square using gloss polygon
square :: (Float, Float) -> Float -> Picture
square (p1, p2) a = Polygon [(p1, p2), (p1 + a, p2), (p1 + a, p2 - a), (p1, p2 - a)]

-- moving a square
moveSquare :: (Float, Float) -> Int -> Int -> IO World -> IO World
moveSquare (x, y) sc wc w = do
    v <- w
    let st = squareY v + y
    let sb = st - squareSize
    let b = windowHeight / 2
    if st <= b && sb >= (-b) -- checking if square will still be inside the screen
        then return $ World (squareX v + x) (squareY v + y) (squareCounter v + sc) (walls v) (wallCounter v + wc) (wallUp v) (isOver v) (score v)
        else return $ World (squareX v) (squareY v) (0) (walls v) (wallCounter v + wc) (wallUp v) (isOver v) (score v) -- if not, it won't be moved

-- drawing a rectangle 
rectangle :: (Float, Float) -> Float -> Float -> Picture
rectangle (p1, p2) w h = Polygon [(p1, p2), (p1 + w, p2), (p1 + w, p2 - h), (p1, p2 - h)]

--creating new wall (direction depends on the previous one)
getWall :: Bool -> IO Wall
getWall u = do
    a <- gen  (round $ windowHeight / 2) (round $ windowHeight / 2 + wallShift) -- generating two walls - upper and down
    b <- gen  (round $ wallHeight - wallShift - windowHeight / 2) (round $ wallHeight - windowHeight / 2)
    if u == True
        then return $ Wall (windowWidth / 2) (fromIntegral a) wallHeight -- choosing one of them
        else return $ Wall (windowWidth / 2) (fromIntegral b) wallHeight

-- moving a wall
moveWall :: (Float, Float) -> Wall -> Wall
moveWall (x, y) w = Wall (wallX w + x) (wallY w + y) (wallH w)

-- moving all active walls and updating the score
moveWalls :: (Float, Float) -> IO World -> IO World
moveWalls (x, y) w = do
    v <- w
    if any (\a -> squareX v == wallX a + wallWidth) (walls v) -- checking if wall just passed a wall
        then return $ World (squareX v) (squareY v) (squareCounter v) (fmap (moveWall (x, y)) (walls v)) (wallCounter v) (wallUp v) (isOver v) (score v + 1)
        else return $ World (squareX v) (squareY v) (squareCounter v) (fmap (moveWall (x, y)) (walls v)) (wallCounter v) (wallUp v) (isOver v) (score v)

-- checking if the wall is still inside the screen
isVisible :: Wall -> Bool
isVisible w = wallX w + wallWidth  >=  -(windowWidth / 2)

-- checking if the square hit a wall
isOverlapping :: Float -> Float -> Wall -> Bool
isOverlapping sx sy w = let
    st = sy
    sb = sy - squareSize
    sl = sx
    sr = sx + squareSize
    wt = wallY w
    wb = wallY w - wallH w
    wl = wallX w
    wr = wallX w + wallWidth
    in (sb <= wt && st >= wb) && (sr >= wl && sl <= wr)

-- drawing objects using gloss
drawSquare :: World -> Picture
drawSquare w = Color squareColor . square  (squareX w, squareY w) $ squareSize

drawWall :: Wall -> Picture
drawWall w = Color wallColor . rectangle (wallX w, wallY w) wallWidth $ wallH w

-- drawing list of walls
drawWalls :: World -> [Picture]
drawWalls w = fmap drawWall $ walls w

-- linking all elements into one picture
draw :: IO World -> IO Picture
draw w  = do
    v <- w
    if isOver v -- checking if game is over
        then
            let a = translate (-(windowWidth / 4)) 0 $ text "Game over!" -- if so, writing score
                b = translate (-(windowWidth / 3.5)) (-(windowWidth / 6)) . text $ "Your score: " ++ show (score v) 
            in return $ mappend a b
        else return $ mappend (drawSquare v) (mconcat (drawWalls v)) -- else joining all elements together

-- handling keyboard input
handleInput :: Event -> IO World -> IO (IO World)
handleInput (EventKey (SpecialKey KeyUp) Down _ _) w = do -- if arrow up is pressed
    v <- w
    if squareCounter v == 0
        then return $ moveSquare (0, 0) fallCounter 0 w -- making square fly up
        else return w

handleInput (EventKey (SpecialKey KeyEnter) Down _ _) w = do -- if enter is pressed - start new game 
    v <- w
    if isOver v
        then do
        g <- getWall False
        return . return $ World (-200) 0 0 [g] newWallCounter True False 0
        else return w

handleInput _ w = return w -- if there is no key pressed don't change anything


stepGame :: Float -> IO World -> IO (IO World) -- function specifying actions during each step
stepGame _ w = do
    v <- w
    z <- getWall (wallUp v)
    let nw = filter isVisible $ walls v -- deleting walls off the screen
    case (squareCounter v, wallCounter v, any (isOverlapping (squareX v) (squareY v)) (walls v)) of -- checking all the cases including if square hit any wall
                                                                                                    -- each case involves moving the square and walls,
                                                                                                    -- changing direction on wall creation
                                                                                                     
        -- if it hit setting game over
        (_, _, True) -> return ( moveWalls ((0), 0) ( (moveSquare (0, (0)) 0 0 (return (World (squareX v) (squareY v) 0 (nw) 0 (not (wallUp v)) True (score v) )))))
        -- square is falling and it's time for a new wall
        (0, 0, _) ->  return ( moveWalls ((-wallStep), 0) ( (moveSquare (0, (-fall)) 0 newWallCounter (return (World (squareX v) (squareY v) 0 (z : nw) 0 (not (wallUp v)) (isOver v) (score v))  ))))
        -- just square is falling
        (0, wc, _) ->  return (moveWalls ((-wallStep), 0) ( (moveSquare (0, (-fall)) 0 (-1) (return(World (squareX v) (squareY v) 0 nw wc (wallUp v) (isOver v) (score v) )))))
        -- just square is flying up
        (sc, 0, _) ->   return ( moveWalls ((-wallStep), 0) ( (moveSquare (0, (-fall)) (-1) newWallCounter (return (World (squareX v) (squareY v) sc (z : nw) 0 (not (wallUp v)) (isOver v) (score v) ) ))))
        -- square is flying up and it's time for a new wall
        (sc, wc, _) -> return ( moveWalls ((-wallStep), 0) ( (moveSquare (0, (jump)) (-1) (-1) (return (World (squareX v) (squareY v) sc nw wc (wallUp v) (isOver v) (score v) )))))
    
anim = do 
    playIO -- main function connecting app with gloss
        (InWindow "flappy haskell" (round windowWidth, round windowHeight) (windowXPos,windowsYPos)) -- display mode
        bgColor -- background color
        step -- steps per second
        startWorld -- initial state
        draw -- what to draw during each step
        handleInput -- how to handle input
        stepGame -- actions during each step

main :: IO ()
main = anim

