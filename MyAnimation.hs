-- *********************************************************************************
--  An animation of visual art based on the human heart by Naveed Shihab (absd031).
-- *********************************************************************************
module MyAnimation where

import Animation
import System.Random


-- *****************************************************
--  The global variables used throughout the animation.
-- *****************************************************
-- If you want to experiment to see how the animation changes with different values of the global variables, try the following all at once:
-- gridWidth = 750, gridHeight = 450, graphSquareLength = 9, whiteBloodCellRadius = 5 and see the difference!

gridWidth :: Double                        -- Width of the graph grid enclosed within the main border. Changes to this value will allow animation to adapt. 
gridWidth = 700

gridHeight :: Double                       -- Height of the graph grid enclosed within the main border. Changes to this value will allow animation to adapt. 
gridHeight = 400

graphSquareLength :: Double                -- Length of the square grids. Changes to this value will allow animation to adapt. 
graphSquareLength = 5

whiteBloodCellRadius :: Double             -- Radius of the original circles for the white blood cell. Changes to this value will allow animation to adapt. 
whiteBloodCellRadius = 4

borderSize :: Double                       -- Width of the main border. Constant variable.
borderSize = 4

width :: Double                            -- Width of the entire animation image including the main border. Constant variable.
width = gridWidth + (borderSize*2)

height :: Double                           -- Height of the entire animation image including the main border. Constant variable.
height = gridHeight + (borderSize*2)

basePointY :: Double                       -- The base point of the polygon at the y axis. Constant variable.
basePointY = (height*0.75) - (graphSquareLength*3)


-- *****************************************************************
--  Factors that coincide with the animation's main time intervals.
-- *****************************************************************
timeIntervals = [0, 0.1, 0.2, 0.3, 0.4, (graphSquareLength/2.75)]           -- Global time intervals of the animation which mimic a heartbeat.
heartExpandCurves = [46, 52, 46, 55, 53.5, 46]                              -- Expand factors of the heart curves which gives it a throbbing/pumping effect.
heartExpandBase = [86, 90.5, 86, 96, 93, 86]                                -- Expand factors of the heart base which gives it a throbbing/pumping effect.
heartRateLineOpacity = [0.05, 0.66, 0.05, 1, 0.55, 0.05]                    -- Opacity factors of the heart rate line which gives it a glowing effect.
whiteBloodCellOpacity = [0.01, 0.05, 0.01, 0.08, 0.06, 0.01]                -- Opacity factors of the white blood cells which gives it an absorbing effect.


-- *******************************************************************************************************
--  Miscellaneous coordinate values used for the generation of the heart rate line and white blood cells.
-- *******************************************************************************************************
-- Polygon coordinates of the heart rate line (from left to right but not back to the start point). 
-- Ratios are considered for different heights/widths of the grid so if global variabls are changed, it will adapt.
heartRateLinePolygon = [((borderSize*2), basePointY), ((width/8.1), basePointY), ((width/5.5), (basePointY/1.16)), ((width/4.2), basePointY), ((width/3.1), basePointY), 
                        ((width/2.7), (basePointY/graphSquareLength)), ((width/2.4), (basePointY/0.78)), ((width/2.2), basePointY), ((width/1.7), basePointY), 
                        ((width/1.3), (basePointY/1.3)), ((width/1.2), basePointY), (width, basePointY)]

-- List of pseudo-random values for the x and y coordinates of the white blood cells' paths.
whiteBloodCellXPoints = (randomNums (floor(width/borderSize)) 7 (-(width -borderSize)) ((gridHeight*2) + borderSize))
whiteBloodCellYPoints = (randomNums (floor(height/borderSize)) 7 (-(height -borderSize)) (gridHeight+borderSize))


-- ***********************************************************************************
--  The main function calls for the collective shapes that form the entire animation.
-- ***********************************************************************************
picture :: Animation           
picture = gridRectangle `plus` whiteBloodCells `plus` heart `plus` gridLines `plus` heartRateLine `plus` mainBorder


-- **********************************************************************
--  All the functions used to draw the certain shapes for the animation.
-- **********************************************************************
-- Draws the main-view rectangle (minus the borders).
gridRectangle :: Animation
gridRectangle = translate(always (borderSize, borderSize)) (withPaint (always (rgb (col 30) 0 0)) (
                    rect (always gridWidth) (always gridHeight) ))

-- Draws all the white blood cells with a fading effect formulated by the radius. Rotate/Spinner adds a unique path directory.
whiteBloodCells :: Animation
whiteBloodCells = combine[ translate(cycleSmooth 30 [startPos, endPos]) (rotate (spinner ((graphSquareLength/2.75)/2)) (
                      withGenPaint (always white) (repeatSmooth 0.01 (zip timeIntervals whiteBloodCellOpacity)) (
                          combine[ translate(always (x, y)) (circle (always whiteBloodCellRadius))
                          | x <- [0, (whiteBloodCellRadius/2), whiteBloodCellRadius, (whiteBloodCellRadius/2)],
                            y <- [0, (whiteBloodCellRadius/2), (whiteBloodCellRadius/2), -(whiteBloodCellRadius/2)] ] ) ))
                  | startPos <- (zip whiteBloodCellXPoints whiteBloodCellYPoints),
                    endPos <- (reverse (zip whiteBloodCellXPoints whiteBloodCellYPoints)) ]

-- Draws the curves and base of the heart.
heart :: Animation
heart = withGenPaint (always (rgb (col 66) 0 0)) (always 1) (
            combine[ translate (always ((width/2)+x, height/2)) (
                circle (repeatSmooth 46 (zip timeIntervals heartExpandCurves)) ) 
            | x <- [-35, 35] ]
            `plus`
            translate (always (width/2, (height/2)-22)) (rotate (always 45) (
                rect (repeatSmooth 86 (zip timeIntervals heartExpandBase)) (
                    repeatSmooth 86 (zip timeIntervals heartExpandBase) ) )))

-- Draws the grid lines of the rectangle. Small value for graphSquareLength gives a nice cross-hatch effect.
gridLines :: Animation
gridLines = translate (always (borderSize, borderSize)) (withGenPaint (always (rgb (col 150) 0 0)) (always 0.1) ((
                combine[ translate(always (borderSize, graphSquareLength*i+borderSize)) (
                    rect (always gridWidth) (always (borderSize/2)))
                | i <- [0..(gridHeight/graphSquareLength)-1] ] )     
            `plus` (
            combine[ translate (always (graphSquareLength*j+borderSize, borderSize)) (
                rect (always (borderSize/2)) (always gridHeight))
            | j <- [0..(gridWidth/graphSquareLength)-1] ] ) ))

-- Draws the heart rate line using the polygon list which is appended with reversed version of the original. 
heartRateLine :: Animation
heartRateLine = withGenBorder (always (rgb (col 209) (col 31) (col 8))) (
                    repeatSmooth 0.05 (zip timeIntervals heartRateLineOpacity) ) (always 4) (
                        polygon ((++) heartRateLinePolygon (reverse heartRateLinePolygon)) )

-- Draws the final border and places it around the main rectangle, drawn last so white blood cells don't overlap.
mainBorder :: Animation
mainBorder = translate (always (borderSize, borderSize)) (withBorder (always (rgb (col 114) 0 0)) (always 8) (
                 withoutPaint(rect (always gridWidth) (always gridHeight)) ))


-- ************************************************************************
--  All the functions used to make the animation as efficient as possible.
-- ************************************************************************
-- Initially, I had created my own respective functions to append, reverse and tuple lists using
-- recursive definitions, however, I found it was cleaner and easier to just use the functions 
-- already specified in "Data.List".

randomNums :: Int -> Int -> Double -> Double -> [Double]       -- Creates a list of randomised doubles using a seed.
randomNums seed iterNo minim maxim = (take iterNo . randomRs (minim, maxim) . mkStdGen) seed

col :: Integer -> Double                                       -- Finds the fractional value of an RGB colour
col c = fromIntegral(c) / fromIntegral(255)

test :: IO ()                                                  -- Creates the svg file of the animation.
test = writeFile "MyAnimation.svg" (svg 800 600 picture)