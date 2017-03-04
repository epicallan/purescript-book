module Chapter11 where

import Data.Tuple
import Control.Bind (pure)
import Control.Monad.Reader (Reader, ask)
import Control.Monad.State (State, execState, runState)
import Control.Monad.State.Class (modify)
import Data.Foldable (traverse_)
import Data.Traversable (sequence)
import Prelude (bind, Unit, (+))

sumArray :: Array Number -> State Number Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

runSum :: Number
runSum = execState ( do
          sumArray [1.0, 2.0, 3.0]
          sumArray [4.0, 5.0]
          sumArray [6.0]) 0.0

runSumState :: Tuple Unit Number
runSumState = runState ( do
                  sumArray [1.0, 2.0, 3.0]
                  sumArray [4.0, 5.0]
                ) 0.0

type Level = Int
type Doc = Reader Level String

-- line :: String -> Doc
-- line string = do
--   level <- ask
--   pure (level string)

-- ident :: Doc -> Doc
-- indent = local (add (1))
--
-- cat :: Array Doc -> Doc
-- cat = sequence ident
