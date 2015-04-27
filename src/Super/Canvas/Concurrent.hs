module Super.Canvas.Concurrent ( CState 
                               , newCState
                               , stepCState
                               , enqueueDraws ) where

import System.IO (hPutStrLn, stderr)
import Data.Queue
import Control.Event.Handler (Handler)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Applicative
import Control.Monad

import qualified Data.Map as M

import Super.Canvas.Types
import Super.Canvas.JS

data DrawChan = DrawChan { drawQ :: TChan [Draw]
                         , delayQ :: TChan Double
                         , currentDraws :: TVar [Draw]
                         , nextTime :: TVar Double }

newDrawChan :: IO DrawChan
newDrawChan = DrawChan 
              <$> newFifo 
              <*> newFifo 
              <*> newTVarIO [] 
              <*> newTVarIO 0
                         
data CState = CState { dchans :: M.Map String DrawChan
                     , lastDraws :: TVar [Draw] }

newCState :: [String] -> IO CState
newCState chans = do empty <- CState M.empty <$> newTVarIO []
                     foldM addNewChan empty chans

-- startBrowserPageRun (stepCState cstate write)

addNewChan :: CState -> String -> IO CState
addNewChan cstate s = 
  do newm <- M.insert s <$> newDrawChan <*> (pure (dchans cstate))
     return (cstate { dchans = newm })

enqueueDraws :: CState -> String -> Int -> [[Draw]] -> IO ()
enqueueDraws cstate chan delay draws = 
  case M.lookup chan (dchans cstate) of
    Just dchan -> atomically (sequenceDraws dchan delay draws)
    _ -> return ()

sequenceDraws :: DrawChan -> Int -> [[Draw]] -> STM ()
sequenceDraws dchan delay draws = 
  let delay' = fromIntegral delay
      push :: [Draw] -> STM ()
      push = pushToChan dchan delay'
  in sequence_ (fmap push draws)

pushToChan :: DrawChan -> Double -> [Draw] -> STM ()
pushToChan dchan delay draws = enqueue (drawQ dchan) draws 
                               >> enqueue (delayQ dchan) delay

stepCState :: CState -> ([Draw] -> IO ()) -> IO ()
stepCState cstate write = 
  do time <- now
     let getDraws = atomically (orElse (sequenceSteps time (dchans cstate))
                                       (return [[]]))
     allDraws <- concat <$> getDraws
     newDraws <- atomically (updateDraws cstate allDraws)
     case newDraws of
       Just ds -> write ds -- >> hPutStrLn stderr ("writing: " ++ (show ds))
       _ -> return ()

sequenceSteps :: Double -> M.Map String DrawChan -> STM [[Draw]]
sequenceSteps time dchans = 
  let s :: DrawChan -> STM [Draw]
      s = stepChan time
  in (sequence . fmap s . fmap snd . M.toList) dchans

updateDraws :: CState -> [Draw] -> STM (Maybe [Draw])
updateDraws cstate newDraws = 
  do oldDraws <- readTVar (lastDraws cstate)
     if newDraws /= oldDraws
        then writeTVar (lastDraws cstate) newDraws 
             >> return (Just newDraws)
        else return Nothing

stepChan :: Double -> DrawChan -> STM [Draw]
stepChan time dchan = 
  do next <- readTVar (nextTime dchan)
     oldDraws <- readTVar (currentDraws dchan)
     if time >= next
        then attemptDeq dchan time oldDraws
        else return oldDraws

attemptDeq :: DrawChan -> Double -> [Draw] -> STM [Draw]
attemptDeq dchan time oldDraws = 
  do mNewDelay <- dequeue (delayQ dchan)
     mNewDraws <- dequeue (drawQ dchan)
     case (mNewDelay, mNewDraws) of
       (Just newDelay, Just newDraws) -> 
         (writeTVar (nextTime dchan) (time + newDelay)
          >> writeTVar (currentDraws dchan) newDraws
          >> return newDraws)
       _ -> return oldDraws

--      curDraws <- readTVar (currentDraws dchan)
--      mNewDelay <- dequeue (delayQ dchan)
--      mNewDraws <- dequeue (drawQ dchan)
--      case (mNewDelay, mNewDraws) of
--        (Just newDelay, Just newDraws) -> 
--          if time >= next
--             then writeTVar (nextTime dchan) (time + newDelay)
--                  >> writeTVar (currentDraws dchan) newDraws
--                  >> return newDraws
--             else return curDraws
--        _ -> return curDraws
