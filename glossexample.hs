import Graphics.Gloss


window :: Display
window = InWindow "Game of Life" (600, 600) (500, 200)

background :: Color
background = white

drawing :: Picture
drawing = rectangleSolid 80 80

main :: IO ()
main = display window background drawingn