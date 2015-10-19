-- Asteroids game
import Graphics.Gloss

import World
import Render

main = do
    mode <- displayMode
    print mode
    -- play (displayMode width height) black startingState render handleInput stepWorld



displayMode :: IO Display
displayMode = do
    putStrLn "Enter window width and height:"
    width <- prompt "Width: "
    height <- prompt "Height: "
    return $ InWindow "Asteroids" (width, height) (50, 50)

prompt :: Read a => String -> IO a
prompt s = putStr s >> fmap read getLine