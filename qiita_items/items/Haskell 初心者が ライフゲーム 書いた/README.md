# Haskell で ライフゲーム

GHC拡張なしには生きていけないと思いました（こなみ

```haskell
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module LifeGame
    ( LifeGameIO(..)
    , game
    ) where


import Control.Monad (join)
import Control.Monad.Trans (MonadIO(..))
import Data.Array.IO (newArray, IOUArray, writeArray, readArray, newListArray, getBounds)
import Data.Array.Unboxed (UArray, array, listArray, bounds, (!))
import System.Random (getStdRandom, randomR)


--      + Type Constructor
--      |          + Type Parameter
--      |          |   + Data Constructor: LifeGameIO :: IO a -> LifeGameIO a
--      |          |   |          + Data Parameter (Record Statement)
--      |          |   |          |
newtype LifeGameIO a = LifeGameIO {
--    + Accessor
--    |                + Type
--    |                |
      runLifeGameIO :: IO a
    } deriving (Functor, Applicative, Monad)
--    |        |
--    |        + Super Classes (or deriving (Applicative, Monad) )
--    + Type Class auto deriving to instance method

-- FlexibleInstances
-- + LifeGameIO is instance of MonadIO Type Class
-- |
instance MonadIO LifeGameIO where
    liftIO = LifeGameIO
--  |        |
--  |        + actual method: LifeGameIO :: IO a -> LifeGameIO a
--  + ideal method: liftIO :: IO a -> m a



--   + Type Constructor
--   |      
--   |       + Mutable, unboxed, strict arrays
--   |       |        + width and height Index Types
--   |       |        |          + element type
--   |       |        |          |
type Field = IOUArray (Int, Int) Bool

newField :: Int -> Int -> LifeGameIO Field
newField width height = LifeGameIO $ newArray ((0, 0), (width-1, height-1)) False
--                      |            |
--                      |            + newArray :: Ix i => (i, i) -> e -> m (a i e)
--                      + LifeGameIO :: IO Field -> LifeGameIO Field

readField :: Field -> (Int, Int) -> LifeGameIO Bool
readField fld (x, y) = do
    ((startW, startH), (stopW, stopH)) <- liftIO $ getBounds fld
    let a = (x - startW) `mod` (stopW - startW + 1)
    let b = (y - startH) `mod` (stopH - startH + 1)
    liftIO $ readArray fld (b, a)

writeField :: Field -> (Int, Int) -> Bool -> LifeGameIO ()
writeField fld (x, y) flag = do
    ((startW, startH), (stopW, stopH)) <- liftIO $ getBounds fld
    let a = (x - startW) `mod` (stopW - startW + 1)
    let b = (y - startH) `mod` (stopH - startH + 1)
    liftIO $ writeArray fld (b, a) flag

getFieldNeighbourhoods :: Field -> (Int, Int) -> LifeGameIO [Bool]
getFieldNeighbourhoods fld (x, y) = do
    ((startW, startH), (stopW, stopH)) <- liftIO $ getBounds fld
    alive <- readField fld (x, y)
    let read (ox, oy) = readField fld (ox + x, oy + y)
    around <- mapM read neighbourhoods
    return around
    where
        neighbourhoods = [ (-1,-1), (0,-1), (1,-1)
                         , (-1, 0), (0, 0), (1, 0)
                         , (-1, 1), (0, 1), (1, 1) ]


showField :: Field -> LifeGameIO String
showField fld = do
    ((startW, startH), (stopW, stopH)) <- liftIO $ getBounds fld
    b <- mapM (\x -> do
        a <- mapM (\y -> get fld x y) [startH..stopH]
        return $ (join a) ++ "\n"
     ) [startW..stopW]
    return $ join b
    where
        get :: Field -> Int -> Int -> LifeGameIO String
        get fld x y = do
            flag <- liftIO $ readArray fld (x, y)
            LifeGameIO $ return $ if flag then "#" else "_"

putField :: Field -> LifeGameIO ()
putField fld = do
    showField fld >>= \str -> liftIO $ putStrLn str
--  |             |           |        |
--  |             |           |        + putStrLn :: String -> IO ()
--  |             |           + liftIO :: IO () -> LifeGameIO ()
--  |             + (>>=) :: LifeGameIO String -> (String -> LifeGameIO ()) -> LifeGameIO ()
--  + showField :: Field -> LifeGameIO String


updateField :: Field -> Field -> LifeGameIO ()
updateField curr next = do
    ((startW, startH), (stopW, stopH)) <- liftIO $ getBounds curr 
    mapM_ update [(x, y) | x <- [startW..stopW], y <- [startH..stopH]]
    where
        update (x, y) = do
--                   + getFieldNeighbourhoods :: Field -> (Int, Int) -> LifeGameIO [Bool]
--                   |
            bools <- getFieldNeighbourhoods curr (x, y)
            let centor = bools!!4
            let others = (count bools) - if centor then 1 else 0
            case others of
                3 -> writeField next (x, y) True
                2 -> writeField next (x, y) centor
                _ -> writeField next (x, y) False
        count [] = 0
        count (x:xs) = if x then 1 + count xs else count xs



--             + Arrays with unboxed elements. Instances of
--             |      + index type
--             |      |   + element type
--             |      |   |
type Pattern = UArray (Int, Int) Bool

-- TypeSynonymInstances
-- IncoherentInstances
instance Show Pattern where
    show pat = let
        ((startW, startH), (stopW, stopH)) = bounds pat
        in
        (join $ map (\x ->
            (join $ map (\y ->
                if pat!(x, y) then "#" else "_"
            ) [startH..stopH]) ++ "\n"
        ) [startW..stopW])



--        + UArray Int Bool
--        |
glider :: Pattern
--       + listArray :: (Int, Int) -> [Bool] -> UArray Int Bool
--       |
--       |          + glider!(0, 0) == False
--       |          |       + glider!(2, 2) == True
--       |          |       |
glider = listArray ((0, 0), (2, 2)) [ False, True,  False
                                    , False, False, True 
                                    , True,  True,  True ]

--            + IOUArray (Int, Int) Bool
--            |                      + UArray Int Bool
--            |                      |
setPattern :: Field -> Int -> Int -> Pattern -> LifeGameIO ()
setPattern fld offsetX offsetY pat = do
    let ((startW, startH), (stopW, stopH)) = bounds pat
    let offsets = [(x, y) | x <- [startW..stopW], y <- [startH..stopH]]
--  mapM_ :: ((Int, Int) -> IO ()) -> [(Int, Int)] -> IO ()
--  |
    mapM_ put offsets
    where
        put (x, y) = liftIO $ writeArray fld (offsetX + x, offsetY + y) $ pat!(x, y)
--                            |          |                                |
--                            |          |                                + UArray (Int, Int) Bool
--                            |          + IOUArray (Int, Int) Bool
--                            + writeArray :: IOUArray (Int, Int) Bool -> (Int, Int) -> Bool -> IO ()



game :: IO ()
game = runLifeGameIO $ do
--     |
--     + runLifeGameIO :: IO ()
    test
--  + liftIO :: IO () -> LifeGameIO ()
--  |        + putStrLn :: String -> IO ()
--  |        |
    liftIO $ putStrLn $ show glider
--            + newField :: Int -> Int -> LifeGameIO Field
--            |
    field1 <- newField 10 10
    field2 <- newField 10 10
--  + setPattern :: Field -> Int -> Int -> Pattern -> LifeGameIO ()
--  |
    setPattern field1 1 1 glider
    putField field1
    let step _ = do {
        updateField field1 field2;
        putField field2;
        updateField field2 field1;
        putField field1;
    }
    mapM_ step [1..100]
    return ()





test :: LifeGameIO ()
test = do
    liftIO $ print "test start"
    field1 <- newField 10 10
    setPattern field1 1 1 glider
    bools <- getFieldNeighbourhoods field1 (2, 2)
    let d = [ False, True,  False
            , False, False, True
            , True,  True,  True ]
    liftIO $ print $ map (\(a, b)-> a == b) $ zip bools d
    let a = [ (0,0)
            ,       (1,1),      (1,3)
            , (2,1),      (2,2)
            ,                   (3,3)
            ,                         (4,4) ]
    let b = map (\(a,b)->(a+10, b+10)) a
    let c = [ False
            ,       False,      True
            , True,        False
            ,                    True
            ,                         False ]
--                  + readField :: Field -> (Int, Int) -> LifeGameIO Bool
--                  |
    boolsA <- mapM (readField field1) a
    boolsB <- mapM (readField field1) b
    --liftIO $ print boolsA
    --liftIO $ print boolsB
    liftIO $ print $ all (\a -> a == True) $ map (\(a, b)-> a == b) $ zip boolsA c
    liftIO $ print $ all (\a -> a == True) $ map (\(a, b)-> a == b) $ zip boolsB c
    liftIO $ print "test end"
    return ()

--                  + IOUArray (Int, Int) Bool
--                  |
randFieldOneCell :: Field -> (Int, Int) -> LifeGameIO ()
randFieldOneCell field (x, y)  = do
--       + randInt :: LifeGameIO Int
--       |
    r <- randRange (1, 10)
    let flag = r == 1 -- 1/10 probabry
    liftIO $ writeArray field (x, y) flag
--  |        |          |            |
--  |        |          |            + Bool
--  |        |          + IOUArray (Int, Int) Bool
--  |        + writeArray :: IOUArray (Int, Int) Bool -> (Int, Int) -> Bool -> IO ()
--  + liftIO :: IO () -> LifeGameIO ()


randRange :: (Int, Int) -> LifeGameIO Int
randRange range = LifeGameIO $ getStdRandom $ randomR range
--                |            |              |
--                |            |              + randomR :: (Int, Int) -> StdGen -> (Int, StdGen)
--                |            + getStdRandom :: (StdGen -> (Int, StdGen)) -> IO Int
--                + LifeGameIO :: IO Int -> LifeGameIO Int
```


## 参考

* オライリー Real World Haskell
* [Life Game完成 - lamuuの勿忘草日記](http://d.hatena.ne.jp/lamuu/20060825/1156520199)
* [Stackage Server](https://www.stackage.org/)
* [Hoogle](https://www.haskell.org/hoogle/)
* [kurubushi/haskell_pragmas - 電気通信大学MMA](https://wiki.mma.club.uec.ac.jp/kurubushi/haskell_pragmas)
* [すごい配列楽しく学ぼう](http://www.slideshare.net/xenophobia__/ss-14558187)
* [All About Monads](http://www.sampou.org/haskell/a-a-monads/html/index.html)
