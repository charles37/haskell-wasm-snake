-- import Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCStringLen)
-- import Foreign (Ptr, copyBytes, malloc, mallocBytes, poke)
-- import Foreign.C (CChar)

-- main :: IO ()
-- main = mempty

-- foreign export capi mallocPtr :: IO (Ptr (Ptr a))

-- mallocPtr :: IO (Ptr (Ptr a))
-- mallocPtr = malloc

-- foreign export capi addExclamationToInput :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

-- addExclamationToInput :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
-- addExclamationToInput inputPtr inputLen outputPtrPtr = do
--   input <- unsafePackMallocCStringLen (inputPtr, inputLen)
--   unsafeUseAsCStringLen (input <> "!") $ \(buf, len) -> do
--     outputPtr <- mallocBytes len
--     poke outputPtrPtr outputPtr
--     copyBytes outputPtr buf len
--     pure len

-- {-# LANGUAGE JavaScriptFFI #-}
-- {-# LANGUAGE CApiFFI #-} 
-- {-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}

-- import qualified Data.ByteString.UTF8 as BSU 
-- import Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCStringLen)
-- import Foreign (Ptr, copyBytes, mallocBytes, poke, malloc)
-- import Foreign.C (CChar)
-- -- import System.Random (randomRIO)
-- import System.Random.Mersenne (randomIO)
-- import Prelude hiding (Left, Right)

-- data Direction = Up | Down | Left | Right

-- type Position = (Int, Int)

-- data GameState = GameState
--   { snake :: [Position]
--   , direction :: Direction
--   , food :: Position
--   }

-- width :: Int
-- width = 20

-- height :: Int
-- height = 20

-- snakeLength :: Int
-- snakeLength = 3

-- initialGameStateIO :: IO GameState
-- initialGameStateIO = do
--   let initialSnake = [(width `div` 2, height `div` 2)]
--   initialFood <- randomPosition
--   pure $ GameState initialSnake Right initialFood

-- randomPosition :: IO Position
-- randomPosition = do
--   x <- randomRIO (0, width - 1)
--   y <- randomRIO (0, height - 1)
--   pure (x, y)

-- main :: IO ()
-- main = mempty

-- foreign export capi mallocPtr :: IO (Ptr (Ptr a))

-- mallocPtr :: IO (Ptr (Ptr a))
-- mallocPtr = malloc

-- foreign export capi updateGameState :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

-- updateGameState :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
-- updateGameState inputPtr inputLen outputPtrPtr = do
--   input <- unsafePackMallocCStringLen (inputPtr, inputLen)
--   initialGameState <- initialGameStateIO
--   randomPosition' <- randomPosition
--   print input
--   let direction' = case input of
--         "ArrowUp" -> Up
--         "ArrowDown" -> Down
--         "ArrowLeft" -> Left
--         "ArrowRight" -> Right
--         _ -> direction initialGameState
--   let newSnakeHead = case direction' of
--         Up -> (x, y - 1)
--         Down -> (x, y + 1)
--         Left -> (x - 1, y)
--         Right -> (x + 1, y)
--         where
--           (x, y) = head $ snake initialGameState
--   let newSnake = if length newSnakeHead > snakeLength
--         then take snakeLength $ newSnakeHead : snake initialGameState
--         else newSnakeHead : snake initialGameState
--   let newFood = if newSnakeHead == food initialGameState
--         then randomPosition'
--         else food initialGameState
--   let newGameState = GameState newSnake direction' newFood
--   let output = gameStateToString newGameState
--   unsafeUseAsCStringLen (BSU.fromString output) $ \(buf, len) -> do
--     outputPtr <- mallocBytes len
--     poke outputPtrPtr outputPtr
--     copyBytes outputPtr buf len
--     pure len

-- gameStateToString :: GameState -> String
-- gameStateToString gameState = unlines rows
--   where
--     rows = [[charAt (x, y) | x <- [0..width - 1]] | y <- [0..height - 1]]
--     charAt pos
--       | pos `elem` snake gameState = '*'
--       | pos == food gameState = '@'
--       | otherwise = ' '








{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CApiFFI #-} 
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.ByteString.UTF8 as BSU 
import Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCStringLen)
import Foreign (Ptr, copyBytes, mallocBytes, poke, malloc)
import Foreign.C (CChar)
-- import System.Random (randomRIO)
import Data.IORef
import System.IO.Unsafe
import System.Random.Mersenne (randomIO)
import Prelude hiding (Left, Right)
import System.Mem

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

snakeLength :: Int
snakeLength = 3

initialGameStateIO :: IO GameState
initialGameStateIO = do
  let initialSnake = [(width `div` 2, height `div` 2)]
  initialFood <- randomPosition
  pure $ GameState initialSnake Right initialFood

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
  

-- addExclamationToInput :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
-- addExclamationToInput inputPtr inputLen outputPtrPtr = do
--   input <- unsafePackMallocCStringLen (inputPtr, inputLen)
--   unsafeUseAsCStringLen (input <> "!") $ \(buf, len) -> do
--     outputPtr <- mallocBytes len
--     poke outputPtrPtr outputPtr
--     copyBytes outputPtr buf len
--     pure len


gameStateToString :: GameState -> String
gameStateToString gameState = unlines rows
  where
    rows = [[charAt (x, y) | x <- [0..width - 1]] | y <- [0..height - 1]]
    charAt pos
      | pos `elem` snake gameState = '*'
      | pos == food gameState = '@'
      | otherwise = '.'
