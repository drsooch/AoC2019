module IntCode (Program, IMachine, createIM, runMachine, injectValues, getMemValue, (!>)) where

import           Control.Monad.State
import           Data.IntMap.Strict  as M

type Memory = M.IntMap Int

-- IntMachine representation
data IMachine = IM { ip  :: Address      -- Instruction Pointer Location
                   , mem :: Memory   -- Mapping memory addresses with their values
                   } deriving (Show)

-- IntMachine Operation types
data Op = Add
        | Mult
        | End
        deriving (Show)

type Address = Int

-- Encodes an Instruction to map its src and dest registers
data Instruction = Inst { opCode :: Op    -- Op Code for this instruction
                        , ipl    :: Address   -- Instruction Pointer Location
                        , src1   :: Address   -- src address
                        , src2   :: Address   -- src address
                        , dest   :: Address   -- dest address
                        } deriving (Show)

-- type alias State
type Program = State IMachine ()

-- create an IntCode Machine from a list of memory values
createIM :: [Int] -> IMachine
createIM xs = IM {ip = 0, mem = mem'}
  where
    mem' = M.fromAscList $ zip [0..] xs


-- run until the Machine halts i.e. memory address holds value 99
runMachine :: Program
runMachine = do
  p0 <- get
  unless (M.lookup (ip p0) (mem p0) == Just 99) $
    do runStep
       runMachine

-- runs one instruction and updates the State
runStep :: Program
runStep = do
  im <- get
  let (ip', im') = execInst im
  put im { ip = ip', mem = im' }


-- execute an instruction
execInst :: IMachine -> (Int, Memory)
execInst im = (newIp, newMem)
  where
    inst = createInst im                                     -- instruction
    val1 = im !> src1 inst                                   -- value from src1
    val2 = im !> src2 inst                                   -- value from src2
    result = computeInst (opCode inst) val1 val2             -- result of computeInst
    newMem = M.insert (mem im ! dest inst) result (mem im)   -- new memory with mutated value
    newIp = incIP (opCode inst) (ip im)                      -- new instruction pointer based on instruction type

-- create an instruction
createInst :: IMachine -> Instruction
createInst im = case M.lookup (ip im) (mem im) of
                  Just 1  -> Inst Add ip' (ip' + 1) (ip' + 2) (ip' + 3)
                  Just 2  -> Inst Mult ip' (ip' + 1) (ip' + 2) (ip' + 3)
                  Just 99 -> Inst End ip' ip' ip' ip'
                  x       -> error $ "Invalid Instruction" ++ show x
  where
    ip' = ip im

-- compute the instructions action
computeInst :: Op -> Int -> Int -> Int
computeInst Add x y  = x + y
computeInst Mult x y = x * y
computeInst _ _ _    = -1               -- Never reaches, keeps GHC happy

-- increment the inst pointer based on operation
incIP :: Op -> Address -> Address
incIP Add ip_  = ip_ + 4
incIP Mult ip_ = ip_ + 4
incIP _ ip_    = ip_

-- Used in Day 2 part 1
-- takes a list of Address Value pairs to alter the current memory
injectValues :: [(Address, Int)] -> IMachine -> IMachine
injectValues  [] im           = im
injectValues ((r, v) : xs) im = injectValues xs (injectValue r v im)

-- inject a single value used in injectValues
injectValue :: Address -> Int -> IMachine -> IMachine
injectValue r v im = im {mem = newMem}
  where
    newMem = M.insert r v (mem im)

-- Exposed function to get a value at specific Memory Address
getMemValue :: Address -> IMachine -> Int
getMemValue addr im = mem im ! addr

-- Indirect Lookup for an IntMap
-- Takes an address and gets the value from the memory
-- then uses this value as an address 
-- taken from: https://git.njae.me.uk/?p=advent-of-code-19.git;a=blob;f=advent02/src/advent02.hs;h=6eddc0a7375f5d71b7886517f9beed0b4ea6e988;hb=af70adae9b90e29d42fb10e32d0cfd040bd4e354
(!>) :: IMachine -> Address -> Int
im !> k = m ! (m ! k)
  where
    m = mem im
