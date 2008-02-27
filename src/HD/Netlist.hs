module HD.Netlist (netlist) where
import Control.Monad.State
import qualified Control.Monad.Trans
import HD.Types
import HD.HDSignal
import HD.Ref
import qualified Data.Traversable as DT (Traversable(mapM)) 

-- FIXME: give credit to Lava
----------------------------------------------------------------
-- netlist

-- v is the type of the tag generated per node of the graph
-- s is a state with which will be forwarded while traversing the graph


--FIXME: 1) why IO and not ST?
--       2) make a stateless version of netlist:
--       3) The interface should be somwthing like
--          DT.Traversable f =>
--         (State s (S HDPrimSignal) -> State s  v ) -> -- new 
--         (State s (Type,v) -> S v -> State s ())   -> -- define
--         State s (f HDPrimSignal) ->                  -- the graph
--         IO s 
--         perhaps State s () should be barely s

netlist :: DT.Traversable f =>
           (StateT s IO HDPrimSignal -> StateT s IO v ) ->        -- new 
           (StateT s IO (HDSignalType,v) -> S v -> StateT s IO ()) -> -- define
           -- FIXME: why is the graph inside the monad?
           StateT s IO (f HDPrimSignal) ->                        -- the graph
           StateT s IO () 

-- Generates a netlist given:
--  new: generates the new (and normally unique) tag of every node given
--       the iteration state which is updated as well.
--  define: given the tag of a node, 
--          current iteration state, its type, and the tag of its children, 
--          generates the netlist of that node, updating the iteration state
--  pSignals: the graph itself, a traversable collection of root 
--            signals including the initial state of the iteration  

-- It returns the final iteration state and the tags of outputs 
-- (root primitivesignals)

netlist new define pSignals =
  do container <-  pSignals
     tab       <-  lift table     
     let -- gather :: State s HDPrimSignal  -> StateT s IO v
         gather sm =
            do signal@(HDPrimSignal t node) <- sm 
               visited <- lift (find tab node) 
               case visited of
                 Just v  -> return v
                 Nothing -> do v'  <- new $ return signal
                               lift (extend tab node v')
                               s <- DT.mapM (gather.return) (deref node)
                               define (return (t,v')) s
                               return v'
           
      in DT.mapM (gather.return) container >> return()


