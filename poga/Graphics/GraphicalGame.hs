module Graphics.GraphicalGame
       (GraphicalGame(GraphicalGame, name, game, renderBoard),
        runGraphicalGame,
        liftPureStrategy,
        liftRandomStrategy,
        interactiveStrategy,
        runGraphicalTournament)
       where

import qualified Graphics.UI.GLUT as GLUT
import GameTheory.PoGa
import Control.Monad.Cont
import Control.Monad.Random
import Data.IORef


data Position p => GraphicalGame p = GraphicalGame {name :: String,
                                                    game :: Game p,
                                                    renderBoard :: p -> IO ()}


liftRandomStrategy :: Position p => (p -> Rand StdGen p) -> p ->  ContT () IO p
liftRandomStrategy randStrat pos = liftIO $ do
  pos' <- (evalRandIO $ randStrat pos)
  return pos'

-- lift a pure strategy to a graphical strategy
liftPureStrategy :: Position p => (p -> p) -> p -> ContT () IO p
liftPureStrategy strat pos = liftIO $ return $ strat pos



-- make an interactive strategy from a graphical game.
-- the strategy lets the user scroll (with mouse) trough his
-- different options, and select by clicking left mouse button
interactiveStrategy :: Position p => GraphicalGame p -> p -> ContT () IO p
interactiveStrategy gg pos = do
  strat gg pos 0
  where
  strat gg pos i = do
    let ps = choices pos
        n = length ps
        pos' = ps!!(i `mod` n)
    display $ renderBoard gg pos'
    (key, state, _, _) <- yieldInput
    case key of
      GLUT.MouseButton GLUT.WheelUp -> do
        strat gg pos (i+1)
      GLUT.MouseButton GLUT.WheelDown -> do
        strat gg pos (i-1)
      GLUT.MouseButton GLUT.LeftButton -> do
        case state of
          GLUT.Down -> return $ pos'
          _ -> strat gg pos i


runGraphicalTournament :: Position p => [GraphicalGame p] -> (p -> ContT () IO p) -> (p -> ContT () IO p) -> IO [Winner]
runGraphicalTournament ggs fststrat sndstrat = do
  GLUT.getArgsAndInitialize
  GLUT.actionOnWindowClose GLUT.$= GLUT.MainLoopReturns
  GLUT.createWindow "Tournament"
  wsRef <- newIORef []
  runContT (sequence [do liftIO $ putStrLn $ "round: " ++ show i
                         w <- playGame gg (position $ game gg) fststrat sndstrat
                         liftIO $ putStrLn $ "winner: " ++ show w
                         liftIO $ putStrLn $ "~~~~~~~~~~~~"
                         liftIO $ putStrLn $ show w
                         liftIO $ return w
                     | (gg,i) <- zip ggs [1 ..]])
           (writeIORef wsRef)
  GLUT.mainLoop
  ws <- readIORef wsRef
  return ws
  where
  playGame :: Position p => GraphicalGame p -> p -> (p -> ContT () IO p) -> (p -> ContT () IO p) -> ContT () IO Winner
  playGame gg pos fststrat sndstrat = do
    display $ renderBoard gg pos
    yield
    case terminal pos of
      False -> do
        pos' <- fststrat pos
        playGame gg pos' sndstrat fststrat
      True -> do
        quit $ winner pos

-- it would have been nice to implement 
-- runGraphicalTournament using runGraphicalGame, but 
runGraphicalGame :: (Position p) => GraphicalGame p -> (p -> ContT () IO p) -> (p -> ContT () IO p) -> IO Winner
runGraphicalGame gg fststrat sndstrat = do
  [w] <- runGraphicalTournament [gg] fststrat sndstrat
  return w

-- ~~~~~ some support functions ~~~~~~~~~~~~~~~~~~~~~~~
-- (re-inversion of control of GLUT
--  this allows the above code to avoid using a lot of
--  messy state variables.)
--
--  Credit for this idea goes out to Dan Piponi, a.k.a. sigfpe.

-- quit with a return value
quit :: a -> ContT () IO a
quit ret = liftIO $ do
  GLUT.idleCallback GLUT.$= (Just $ GLUT.leaveMainLoop)
  return ret

yield :: ContT () IO ()
yield = ContT $ \f -> GLUT.idleCallback GLUT.$= (Just $ (GLUT.idleCallback GLUT.$= Nothing) >> f ())


-- wait for input
yieldInput :: ContT () IO (GLUT.Key, GLUT.KeyState, GLUT.Modifiers, GLUT.Position)
yieldInput = do
  yield
  ContT $ \f -> do
    GLUT.keyboardMouseCallback GLUT.$=
      (Just $ \key st mod pos -> do
         GLUT.keyboardMouseCallback GLUT.$= Nothing --unhook
         f (key, st, mod, pos) )


display :: GLUT.DisplayCallback -> ContT () IO ()
display f = do
  liftIO $ GLUT.displayCallback GLUT.$=
    (do GLUT.clear [GLUT.ColorBuffer] >> f >> GLUT.flush)
  liftIO $ GLUT.postRedisplay Nothing