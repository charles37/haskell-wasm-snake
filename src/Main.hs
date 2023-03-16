    {-# LANGUAGE JavaScriptFFI #-}
    {-# LANGUAGE CApiFFI #-} 
    {-# LANGUAGE ScopedTypeVariables #-}


    import qualified Data.ByteString.UTF8 as BSU 
    import Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCStringLen)
    import Foreign (Ptr, copyBytes, mallocBytes, poke, malloc, free)
    import Foreign.C (CChar)
    import Data.IORef
    import System.IO.Unsafe
    import System.Random.Mersenne (randomIO)
    import Prelude hiding (Left, Right)

    data Direction = Up | Down | Left | Right deriving (Show, Eq)

    type Position = (Int, Int)

    data GameState = GameState
      { snake :: [Position]
      , direction :: Direction
      , food :: Position
      } deriving (Show)

    width :: Int
    width = 20

    height :: Int
    height = 20

    initialGameStateIO :: IO GameState
    initialGameStateIO = do 
      let initialSnake = [(width `div` 2, height `div` 2)]
      GameState initialSnake Right <$> randomPosition

    randomPosition :: IO Position
    randomPosition = do
      x <- randomIO --(0, width - 1)
      y <- randomIO --(0, height - 1)
      let x' = x `mod` width
      let y' = y `mod` height
      pure (x', y')


    initialGameState :: IORef GameState
    initialGameState = unsafePerformIO $ newIORef =<< initialGameStateIO
    {-# NOINLINE initialGameState #-}


    main :: IO ()
    main = mempty

    foreign export capi mallocPtr :: IO (Ptr (Ptr a))

    mallocPtr :: IO (Ptr (Ptr a))
    mallocPtr = malloc


    foreign export capi freePtr :: Ptr a -> IO ()

    freePtr :: Ptr a -> IO ()
    freePtr = free


    foreign export capi updateGameState :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

    updateGameState :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
    updateGameState inputPtr inputLen outputPtrPtr = do

      input <- unsafePackMallocCStringLen (inputPtr, inputLen)
      gameState <- readIORef initialGameState
      randomPosition' <- randomPosition

      let direction' = case input of
            "ArrowUp" -> Up
            "ArrowDown" -> Down
            "ArrowLeft" -> Left
            "ArrowRight" -> Right
            _ -> direction gameState
      let newSnakeHead = case direction' of
            Up -> (x, y - 1)
            Down -> (x, y + 1)
            Left -> (x - 1, y)
            Right -> (x + 1, y)
            where
              (x, y) = head $ snake gameState

      let (newFood, gotFood) = if newSnakeHead == food gameState
            then (randomPosition', True)
            else (food gameState, False)


      let newSnake = if gotFood
            then newSnakeHead : snake gameState
            else take (length $ snake gameState) $ newSnakeHead : snake gameState
      

      let newGameState = GameState newSnake direction' newFood

      let output = gameStateToString newGameState


      writeIORef initialGameState newGameState

      unsafeUseAsCStringLen (BSU.fromString output) $ \(buf, len) -> do
        outputPtr <- mallocBytes len
        poke outputPtrPtr outputPtr
        copyBytes outputPtr buf len
        pure len

    gameStateToString :: GameState -> String
    gameStateToString gameState = unlines rows
      where
        rows = [[charAt (x, y) | x <- [0..width - 1]] | y <- [0..height - 1]]
        charAt pos
          | pos `elem` snake gameState = '*'
          | pos == food gameState = '@'
          | otherwise = '.'
