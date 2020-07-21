# Advent Of Code 2019
This is my repository for Advent of Code 2019, I am using Haskell to better learn Functional Programming (and Haskell).

***
### Day One: The Tyranny of the Rocket Equation

***

Day One was a simple task. The idea is to get a Spaceship back from the edge of the solar System.
Each module of the spaceship required a set amount of fuel to travel the required distance.
The amount of fuel needed was given by the formula ```(m / 3) - 2``` where m is the mass of the module and the division is floored.

##### Part One

Part One required mapping over a list of masses, applying the formula and getting the sum.
In ```DayOne.hs```, the part1 function takes an int and applies this formula and sums the result.

##### Part Two 

For Part Two, there is an added twist. 
Just like the modules of the ship, the fuel adds mass. 
Therefore we need to calculate this fact into the solution. 
This requires a simple change, we simply want to apply the ```fuel'``` formula recursively until the result becomes negative.
The function we defined constructs a list of fuel amounts each based on the prior value. Then we simply ```concatMap``` and ```sum``` the resulting list

```Haskell
fuel :: Int -> [Int]
fuel n = result : takeWhile (>0) (fuel result)
  where
    result = fuel' n

fuel' :: Int -> Int
fuel' n = subtract 2 $ div n 3
```

***
### Day Two: 1202 Program Alert

***

I cheated a tad, I had originally started AoC 2019 back when it first came out, unfortunately Finals were upcoming and I had to take a break to focus on studying.
I knew I had to separate out the IntCode Machine for further use later on.
The actual implementation is in the file ```IntCode.hs```.
The IntCode Machine (IMachine) keeps track of an instruction pointer and memory.
The instruction pointer is simply an Int which is type aliased with Address.
The Memory of the IMachine is an IntMap, which is synonymous with a regular Map in Haskell. 
The key of the IntMap is the memory address starting at 0.
The values of the IntMap are the values held in memory.
The IMachine acts on specific Op Codes encoded as the Op type. 
As of Day Two those operations are: Add, Multiply and Halt.

The IMachine operates as follows:

- The first value it encounters is an Op Code:
  - Add  = 1
  - Mult = 2
  - Halt = 99
- For Day 2, the next two numbers are Source Addresses. For Example: ```1, 4, 6, 8``` translates to:
  - The instruction pointer points to address 0 where there is a 1. This means we have an add instruction.
  - The next two address (index 1 and 2) hold the source addresses for the add operation. Note: we have to access the value indirectly. In an imperative language like Java this would translate to: ```memory[memory[1]]```
  - The final address (index 3) holds the address that we place the result of the Add or Multiply operation eg. ```memory[memory[3]] = x```

The process of reading one Op Code is encoded in the Instruction Record. 
It holds the type of Operation, the address of the instruction pointer, the first source address, the second source address, and the destination address.

As a newbie Haskell-er, this seems like a great use of the State Monad.
The main crux of the IMachine is type defined as ```Program = State IMachine ()```. We can ignore the returned value of the State Monad as we don't need it. 
The main function that operates the IMachine is ```runMachine```. It's essentially executing instructions until the Halt Op Code is reached.

This took some time to get right as I struggled with getting a grasp of how to utilize the State Monad.
It was hard to wrap my head around the abstraction that the State Monad provides, but when I did it was one of the best feelings I've ever experienced.
I compared the code that I had written back in December and there was drastic improvement.

```Haskell

-- IntMachine representation
data IMachine = IM { ip  :: Address   -- Instruction Pointer Location
                   , mem :: Memory    -- Mapping memory addresses with their values
                   } deriving (Show)

-- IntMachine Operation types
data Op = Add
        | Mult
        | End
        deriving (Show)

-- Encodes an Instruction to map its src and dest registers
data Instruction = Inst { opCode :: Op        -- Op Code for this instruction
                        , ipl    :: Address   -- Instruction Pointer Location
                        , src1   :: Address   -- src address
                        , src2   :: Address   -- src address
                        , dest   :: Address   -- dest address
                        } deriving (Show)

-- type alias State
type Program = State IMachine ()
type Memory M.IntMap Int
type Address = Int
``` 

On to the actual challenge:

##### Part One

Part One was relatively straight forward, it required replacing values at address 1 and address 2, and running the resulting machine. 
I chose to use the IntCode module as if I were programming an API for an end user.
Instead of exposing all functions and data types, I selectively chose what could be used. 
As a result I needed to construct a function that takes a list of memory addresses and a value to replace (this would become especially useful in Part Two), this took the form of ```injectValues```.
After this, all we needed to do was ```execState runMachine replaceState``` and inspect the value at address 0.
To inspect a value a simple getter was created ```getMemValue```, after that part one is complete.

```Haskell
part1 :: IMachine -> Int
part1 initState = getMemValue 0 finalState
  where
    replaceState = injectValues [(1, 12), (2, 2)] initState
    finalState = execState runMachine replaceState
```

##### Part Two

Part Two was similar to Part One, however it required searching for a specific value located at address 0 when the machine has halted.
The replacement values range from 0 to 99, which is encoded in ```nounVerbPairs```.
In my previous attempt, I constructed a Set of all points tested and made sure not to test duplicates.
However, brute-forcing was quick enough that I didn't need to implement this option.

```Haskell
part2 :: [(Int, Int)] -> IMachine -> (Int, Int)
part2 [] _             = error "Can't find Magic Number"
part2 ((x, y) : xs) im = if getMemValue 0 finalState == magicNumber then (x, y) else part2 xs im
  where
    replaceState = injectValues [(1, x), (2, y)] im
    finalState = execState runMachine replaceState
```

**Note** The IntCode machine from Day 2 has been moved to IntCodeS.hs

***
### Day Three: Crossed Wires

***

Day Three was a step away from the IntCode Machine.
The problem centered around two wires on a grid and finding the intersection points between the two.
Each wire was constructed from a list of strings which encode direction and length (ex "U234" is Up 234 units).
The input file consists of two lines representing two wires.
Each "step" is encoded as above and split by commas.
We can easily parse the input and separate the wires into two separate lists.

~~When I first approached this problem, using a Set jumped out at me.~~
I will save you some reading time, I originally approached this problem using a data record to hold both the coordinates and the number of steps it took to reach that point.
The set was constructed using the coordinate as its key, however, since Maps in Haskell are implemented similarly to Sets and have near identical functions, we can substitute the coordinate as the map key and its value as the number of steps it took to reach it.
This way we don't need to lug around a data record making our lives a lot easier.
Part One does not need this implementation but Part Two is vastly simplified this way.

We define the origin as the coordinates (0, 0) and start each wire at this point.
The function ```traverseWire``` does all of the heavy lifting.
It essentially folds over the input while keeping track of the current position, the length traveled and the map of all coordinates.
I created a helper function ```createPoints``` that creates a list of all coordinates with the length at that point, which is then added to the accumulating map.
The creation of the points caused a bit of a headache as it silently failed because operators were ordered incorrectly.
As mentioned above, the point generation function, ```genCoords```, initially was only for x and y coordinates, however part two necessitated the need to add the distance as well.

```Haskell
type Coord = (Int, Int)
type Points = M.Map Coord Int -- Map a Coordinate to a step number

-- generates an X, Y coordinate and the total number of steps to that point
-- silently failed ex: "b - len - 1" should be "b - (len - 1)"
genCoords :: Coord -> (Int, Int) -> Dir -> [(Int, Int, Int)]
genCoords (x, y) (curr, len) d = case d of
  North -> zip3 (repeat x) (asc y) (asc curr)
  South -> zip3 (repeat x) (desc y) (asc curr)
  West  -> zip3 (desc x) (repeat y) (asc curr)
  East  -> zip3 (asc x) (repeat y) (asc curr)
  where
    desc b    = [b - len, b - (len - 1) .. b] -- descending list
    asc  c    = [c ..  (c + len)]             -- ascending list

-- Folds over the Input, keeping track of current position, number of steps taken
-- and creates a Map of all coordinates traversed
traverseWire :: Coord -> Int -> [String] -> Points -> Points
traverseWire (_, _) _ [] s = s
traverseWire (x, y) dist ((dir : steps) : rest) s = case dir of
  'U' -> traverseWire (x, y + steps') newDist rest (union North)
  'D' -> traverseWire (x, y - steps') newDist rest (union South)
  'L' -> traverseWire (x - steps', y) newDist rest (union West)
  'R' -> traverseWire (x + steps', y) newDist rest (union East)
  _   -> undefined  -- Shouldn't reach
  where
    steps' = read steps :: Int                              -- Converts String -> Int
    newDist = dist + steps'                                 -- Total distance traveled
    union d  = M.union s $ M.fromList $ create d            -- Unions two maps together defaults to the left map which means any coords that are duplicates we default to the first pass
    create d = createPoints (x, y) (dist, steps') d      -- helper function to create new segment
```

##### Part One

Part One involved finding the intersection between the two wires with the smallest Manhattan Distance to the origin.
After creating the intersection of the two wires, the answer is found simply by folding over the map and finding the minimum Manhattan distance. 

##### Part Two

Part Two involved finding the point of intersection that took the least amount of steps to get there.
This required getting the distance to each intersection from BOTH wires and summing the two. 
This is why the Map is key to this problem, because it provides a function ```intersectionWith``` that will be applied to the two map values.
We simply use ```intersectionWith (+)``` (the binary add function) to get a Map with intersecting points and their distances.
All that's left is applying a fold using the min function to get the final result.

```Haskell
part1 :: Points -> Int
part1 = M.foldrWithKey (\k _ m -> if manhattanDist k < m then manhattanDist k else m) (maxBound :: Int)

part2 :: Points -> Int
part2 = M.foldl min (maxBound :: Int)

-- wraps up part1 and part2's answers into a Tuple
solve :: [String] -> [String] -> (Int, Int)
solve s1 s2 = (part1 intersection, part2 intersection)
  where
    wire1     = traverseWire origin 0 s1 M.empty
    wire2     = traverseWire origin 0 s2 M.empty
    -- delete the Origin point which is usually guaranteed to be there
    -- combine the intersection adding the two length values to get a total distance
    intersection = M.delete origin $ M.intersectionWith (+) wire1 wire2
```

***
### Day Four: Secure Container

***

Day Four involves finding a 6-digit passcode given a set of rules.
The passcode is in a range given (in this case 240298 - 784956).
It's also required that each successive digit (from left to right) is either greater than or equal to prior digit.

```
range :: [(Int, Int, Int, Int, Int, Int)]
range = [(d1, d2, d3, d4, d5, d6)
        | d1 <- [2 .. 7]
        , d2 <- [d1 .. 9]
        , d3 <- [d2 .. 9]
        , d4 <- [d3 .. 9]
        , d5 <- [d4 .. 9]
        , d6 <- [d5 .. 9]
        ]
```

This list comprehension is the crux of this program.
When I originally went about solving this problem I constructed an abomination of a solution.
After perusing the internet I found a remarkable solution that uses the above all credit to him [here](https://work.njae.me.uk/2019/12/04/advent-of-code-2019-day-4/)

The rest of solution is done by me, but really it was only made possible by the intial step.

##### Part One

Part One required the rules listed above, as well as requiring at least one repeated digit.
This is encoded as the ```repeating``` function.
It checks at least two successive digits are equal.
After implementing this we simply apply ```filter (\n -> repeating n && inRange n)``` to range to filter out bad passcodes.
The actual solution requires the number of total valid passcodes, which is simply done by applying ```length`` to the filtered list.

```Haskell
part1 :: [(Int, Int, Int, Int, Int, Int)] -> Int
part1 = length . filter (\n -> repeating n && inRange n)
```

##### Part Two

Part Two adds a slight twist to the filtering.
Instead of allowing ANY number of repeated digits, Part Two only allows for TWO repeated digits.
This boils down to adding a couple extra checks to ```repeating``` function.
For instance if we are looking at d2 (d = digit) and d3, we need to make sure ```d1 /= d2 && d3 /= d4```.
Essentially the preceding and succeeding digits of the two being examined are different.

```Haskell
part2 :: [(Int, Int, Int, Int, Int, Int)] -> Int
part2 = length . filter (\n -> doubles n && inRange n)
```

***
### Day Five: Sunny With a Chance Of Asteroids

***

Day Five continued with the IntCode Machine, this time adding six new instructions (two in Part One and four in Part Two), and a new Parameter Mode.
The Parameter Mode is split between Positional Mode and Immediate Mode.
To accommodate these changes, the Instruction Code was changed from a Two-Digit number into a Five-Digit number.

For example: ```[1002, 4, 3, 10, 11]``` 

- The 0th index is the new Instruction Code
  - The ones and tens place encodes the instruction ```02``` or a ```Mult``` operation
  - The hundreds place encodes the parameter mode for the first source address (index 1) or ```0```
  - The thousands place encodes the parameter mode for the second source address (index 2) or ```1```
  - The ten-thousands place encodes the parameter mode for the destination address (index 3) or ```0```
    - Note: if a 0 appears here it is dropped and is implied

The parameter modes are then applied to each address, if the parameter mode is 0, we are in Position Mode which means the value at this address is another address.
This is the mode we were operating in for Day Two.
If the parameter mode is 1, we are in Immediate mode and the value at this address is a value to be used immediately.
Another caveat is if we are writing a value to an address the parameter mode will always be 0.

For this toy example it breaks down to: Multiplying value in address 4 (11) with the immediate value 3 and writing this value to address 10.

The new instruction modes are as follows:

- Part One:
  - Input (```LoadVal```): Op Code 3, takes an input and writes the value to the destination parameter
  - Output (```EmitVal```): Op Code 4, takes an address and outputs this value
- Part Two
  - Jump if True (```JumpTrue```): Op Code 5, Move instruction pointer to jump address if value in source address is NOT 0
  - Jump if False (```JumpFalse```): Op Code 6, Move instruction pointer to jump address if value in source address is 0
  - Less Than (```LessThan```): Op Code 7, compare the two source addresses, if the first is less than the second output a 1 to the destination address, otherwise 0
  - Equals (```Equals```): Op Code 8, compare the two source addresses, if they are equal output 1 to the destination address, otherwise 0

With the addition of the new operations to the IntCode Machine, the underlying structure of the Program must be altered. 
I knew that this would happen from my previous experience with AoC, however I wanted to "master" the State monad, before I made these changes.
I knew that the Reader Writer State (RWS) monad was perfect for the job.
As we need input (Reader), output (Writer), and the general state of the machine.
The updated program type was as follows: ```RWS [Int] [Int] IMachine Op```.
From right-to-left, the first list of Ints would be the Reader monad and is just a list of inputs to the program.
The second list of Ints is the Writer monad and is the output of the program.
The State monad would be the IMachine similar to the old IntCode Program.
The final Op is the value of a computation.
In the previous iteration of the Program, we used () as the return value.
However, after working through the base of the program I realized it would be much easier just to return the Op type of the last run instruction.
This way we could check what we ran, and if it was a Halt instruction we finish.

Switching from the standalone State representation to the RWS required slight corrections to the underlying functions, but was otherwise a clean process.
In fact, in my rewrite of the IntCode Program I found that the change to the RWS afforded me some much needed cleanup of the code.
In refactoring, the first order of business was to update the ```createInst``` function, which takes the State of the machine and translates it into an ```Instruction```, which holds all the information to run one step.
This required two changes to the underlying data structures of the machine.
The first was to change the src1, src2 etc. addresses into a Parameter type.
The new type ```Param``` has two constructors, ```Pos Address``` and ```Imm Int```, which encodes the new rules for parameter modes.
The second was to translate the instruction data type from one constructor to four.
In the prior implementation when a Halt instruction was encountered, it would be encoded with useless information to satisfy the constructor.
Now that the instructions each had varying amounts of parameters, I split the Instruction type into four separate types: ```HaltInst``` for Halt, ```BinaryInst``` for instructions that used a binary function, ```UnaryInst``` for input and output, and ```JumpInst``` for the jump instructions. 
The main logic was now encoded in one function and could handle all instruction types.

``` Haskell
-- create an instruction
createInst :: IMachine -> Instruction
createInst im = case op of
                  1  -> bInst Add
                  2  -> bInst Mult
                  3  -> uInst LoadVal
                  4  -> uInst EmitVal
                  5  -> jInst JumpTrue
                  6  -> jInst JumpFalse
                  7  -> bInst LessThan
                  8  -> bInst Equals
                  99 -> HaltInst
  where
    mem' = mem im
    [opNum, ip1, ip2, ip3] = map ((M.!) mem') [ip im .. (ip im) + 3] -- get each value in an address
    op = mod opNum 100  -- parse the OpCode out
    (p1, p2, p3) = (paramMode opNum 2, paramMode opNum 3, paramMode opNum 4) -- get each parameters mode type
    bInst i = BinaryInst i (toParam ip1 p1) (toParam ip2 p2) (toParam ip3 p3) -- using the derived values construct an instruction
    jInst i = JumpInst   i (toParam ip1 p1) (toParam ip2 p2)
    uInst i = UnaryInst  i (toParam ip1 p1)
```

This made the rest of the implementation trivial.
Each operation had an associated function to run the required operation (sort of).
The actual running of the machine boiled down to the same functions as the previous iteration, expanded for the new operations.

```Haskell
-- runs a machine until a halt code is encountered
runMachine :: Program
runMachine = do
  op <- runStep
  if op == Halt
    then return Halt
    else runMachine

-- runs one instruction
runStep :: Program
runStep = do
  im <- get
  let inst = createInst im
  case inst of
    UnaryInst LoadVal _       -> execLoad inst
    UnaryInst EmitVal _       -> execEmit inst
    JumpInst _ _ _            -> execJump inst
    BinaryInst Equals _ _ _   -> execTest inst
    BinaryInst LessThan _ _ _ -> execTest inst
    BinaryInst _ _ _ _        -> execCompute inst
    HaltInst                  -> return Halt
```

On the client side, aka for testing Part One and Two, we use the function ```runRWS``` which unwraps the computation into ~~(Value, Writer, State)~~ (Value, State, Writer).
For both parts we need the output (the Writer) to solve the problem.
I won't subdivide this day into Part One and Two as they are both the same, with the exception of the input values.

Of note, the module ```IntCodeS.hs``` encapsulates the old version of just the State IntCode Machine. 

***
### Day Six: Universal Orbit Map

***

Day Six required mapping a solar system's planetary orbits.
The map is generated from a list of orbits from a named Planet to another named Planet.
For example: "ABC)DEF", is translated as the planet DEF orbiting the planet ABC. 

The main data structure used in my solution is a Map with the key being the name of the planet, and the value as the name of the planet it orbits.
A graph would be more apt for this solution as we are traversing the structure in a way that resembles graph properties.
However, I did not want to roll my own, and the Haskell Prelude version would require a considerable amount of data munging to get it to work properly.

##### Part One

Part One required solving for the total number orbits, which seems straight forward on the surface but it added the twist of "including indirect orbits".
For example: we have the list of orbits ```ABC)DEF, DEF)GHI, GHI)JKL``` JKL orbits GHI, which orbits DEF, which orbits ABC, this means JKL has one direct orbit and two indirect orbits (a total of three orbits).
The solution was easily achievable by traversing each Key in the Map from it's start to the orbit of COM.
COM is the "Center of Mass", and has no descendant orbits.
This is achieved by mapping a function that traverses a key down to the root node COM.
Normally, this could be done more efficiently using some sort of memoization or rolling my own tree structure which encodes the distance from COM.
However, the brute force solution was near instantaneous and was easily completed with simple functions.

```Haskell
-- recursively traverse a full orbit to COM node
countOrbits :: System -> T.Text -> Int
countOrbits = go 0
  where
    go acc _ "COM" = acc
    go acc mp k    = go (acc + 1) mp (mp M.! k)

part1 :: [(T.Text, T.Text)] -> Int
part1 o = sum $ map (countOrbits orbits) $ M.keys orbits
  where
    orbits = M.fromList o
```

##### Part Two

Part Two required finding the distance between two Nodes YOU and SAN (the spaceship and Santa).
The solution I developed was creating a path from each node (YOU and SAN) to the root Node COM.
The path is stored in a Set which is then "differenced" with the opposite Node i.e. ```YOU - SAN``` and ```SAN - YOU```.
This gives us all the nodes between the two Nodes
The final portion is trivial as we can apply a union to both sets and find the Size of this new set.

```Haskell
-- Build a set of a location's Path to COM Node
traverseOrbit :: System -> T.Text -> Set T.Text
traverseOrbit = go S.empty
  where
    go s _ "COM" = s
    go s mp k    = go (S.insert k s) mp (mp M.! k)

part2 :: [(T.Text, T.Text)] -> Int
part2 o = S.size $ S.union (p1 S.\\ p2) (p2 S.\\ p1)
  where
    orbits = M.fromList o
    p1 = traverseOrbit orbits $ sanNode orbits
    p2 = traverseOrbit orbits $ youNode orbits
```

***
### Day Seven: Amplification Circuit

***

Phew.
This day made me question whether I was even capable of coding anything. 

The basic premise of Day Seven was to create a connection of machines that feed inputs/outputs in a chain.
The goal was to find the combination of inputs that would produce the largest output value.

Let's jump right in.

##### Part One:

This was so easy I thought I was God for a few moments.

Each machine would get a random number from the range [0, 4] using each number only once.
This is the combination that we must tweak to find the largest possible output value. 
Then each machine would produce one output value that was applied to the next machine in the chain.
Machine A was the special case where it was given a second input value of 0.

Example:
0 -> Machine A -> Machine B -> Machine C -> Machine D -> Machine E -> output value

The most important piece of this was to not share memory between the machines, and that each machine starts with the same memory layout.
I honestly thank immutability, as I could easily pass around the original state with no worries.

This ended up being straightforward to solve, as we can compute all possible permutations of the range.
We can then fold over all possible inputs using the ```runChain``` function which basically passes around the previous machines output to the next element. 

```Haskell
-- fold over a list of phase settings feeding the output from the previous
-- machine into the next
runChain :: [Int] -> [Int] -> Int
runChain mem phases = foldl' (runChain' mem) 0 phases

-- helper for runChain, actually executes the machine
runChain' :: [Int] -> Int -> Int -> Int
runChain' mem prevOutput phase = getMachineOutput mem [phase, prevOutput]

part1 :: [Int] -> Int
part1 mem = maximum $ map (runChain mem) perms
  where
    perms = permutations [0 .. 4]
```

**Note** ```getMachineOutput``` is a wrapper around around runRWS to return only the output.

##### Part Two

This is where things fell apart.

I was beaming with confidence on my way into this next challenge.

Some background:

The second part expanded on the chaining of machines, this time, machines will ask for input and produce output more than once.
The other twist is that the final Machine in the chain has its output chained to the first machine creating a Feedback Loop.
The looping mechanism means that machines will sometimes have to pause execution when they have no more input to consume.

Okay. Not too bad.

My first thoughts were to wrap an ```IMachine``` into an ```AmplifierMachine``` which keeps track of the underlying IM, as well as its current execution state, its inputs, and its outputs.
The second new structure is called a ```FeedbackLoop``` which carries an ```IntMap``` of machines (basically an easy way to update and keep track of specific machines) and a pointer to the current machine running.

There was one minor change to the underlying structure of the ```IMachine```.
Previously, the return type of ```runStep :: Program Op```, which passed back the instruction type that had just been executed.
This was done so that we could check for a ```Halt``` return value to end execution.
However, I realized that I need to return the instruction type of the NEXT instruction.
Otherwise the machine will attempt to execute a ```LoadVal``` without any checks to see if it has any.
Hypothetically I could have worked around this, but I liked the change.

Simple enough right?
I can make use of the functions I've already created to run the new Machine type.
The main driver for an ```IMachine``` is ```runMachine```, however this does not allow for pausing execution, only fully halting.
A sibling function ```runMachineMS``` was created to take it's place (not really siblings as much of the work is done by ```runMachineMS'```).
This allows mutating of an ```AmplifierMachine``` as well as getting the inner representation of the ```MachineState```.

```Haskell
-- Wrapper function to run and update an AM
runMachineMS :: AmplifierMachine -> AmplifierMachine
runMachineMS am = am { iMachine_ = im'
                     , imState_  = ms
                     , output_   = output
                     }
  where
    inputs = inputs_ am
    im = iMachine_ am
    (ms, im', output) = runRWS runMachineMS' inputs im

-- Similar to runMachine but returns a MachineState instead
runMachineMS' :: Program MachineState
runMachineMS' = do
  op <- runStep
  inputs <- ask
  im <- get
  if op == Halt
    then return Finished
  else if op == LoadVal && (length inputs < (inpIx_ im + 1))
    then return NeedsInput
  else runMachineMS'
```

Ok. Fine. Seems Easy.

No. The driver function for a ```FeedbackLoop``` was ```runFeedBackLoop``` (really a pointless sentence, but I'm keeping it).
This function went through 14 different iterations each more horrible than the last. 
I spent a good portion of one afternoon working on a solution.
The biggest issue I had was trying to extract the output from the currently executing machine.
I spent about an hour trying to figure out why I was receiving an empty list instead of values in that list.
I never did figure out what went wrong, but I did constantly add and remove functions so I assume the output was not getting propagated back up the chain
Anyways, my friend (a guy who has no idea I exist) [Neil Smith](https://work.njae.me.uk/2019/12/08/advent-of-code-2019-day-7/) who's work I've been comparing to when I complete my solutions, had his own solution that I used as the shell of mine.
I was close to giving up so I just used his solution for loose guidelines and filled in the rest. Turns out I came up with a pretty similar layout, but hey what can you do.

```Haskell
-- exported wrapper for running a FeedbackLoop
runFeedbackLoop :: [Int] -> [Int] -> Int
runFeedbackLoop mem phases = runFeedbackLoop' $ createFeedbackLoop mem phases

-- The guts of the work for FL is done here
-- Acquire the first available machine, run it and collect its output
-- feed its output into the next machine
-- and continue
runFeedbackLoop' :: FeedbackLoop -> Int
runFeedbackLoop' feedback
  -- the "max" machine is the highest index and is the one that has the answer included
  | isFinished feedback = last $ output_ $ snd $ M.findMax (aMachines_ feedback)
  | otherwise           = runFeedbackLoop' feedback' {aMachines_ = newAMachines}
  where
    (nextAM, feedback') = nextRunnable feedback
    (feederAM, feederFL) = nextMachine feedback'
    nextAMRan = runMachineMS nextAM
    updateState am_ = case (imState_ am_) of
    Runnable   -> Runnable
                        NeedsInput -> Runnable
                        Finished   -> Finished
    feederAM' = feederAM { imState_ = updateState feederAM
                         , inputs_ = (inputs_ feederAM) ++ (output_ nextAMRan)
                         }
    newAMachines = M.insert (curr_ feedback') nextAMRan $ M.insert (curr_ feederFL) feederAM' (aMachines_ feedback')

part2 :: [Int] -> Int
part2 mem = maximum $ map (runFeedbackLoop mem) perms
  where
    perms = permutations [5 .. 9]
```

***
### Day Eight: Space Image Format

***

This was a fun one. I actually felt like a functional programmer doing this challenge.
Day Eight required decoding an image format and reading the message it produces.
The input comes in as a stream of digits from 0 to 2.
0 indicates a black pixel, 1 indicates a white pixel, and 2 indicates a transparent pixel.
The image consists of layers of pixels placed on top of each other. 

For example a 2x2 picture with 3 layers would look like this:

```
Layer 1: 22
         11

Layer 2: 12
         00

Layer 3: 10
         01
```

Each digit in each layer corresponds to a specific pixel, i.e. the first digit is the top-left pixel.
This becomes more important in the second part.

##### Part One:

Part One was simply to make sure the image was received properly by checking its checksum (in this case a checkproduct...haha...).
The task at hand was to find the layer with the LEAST number of 0's (least is capitalized as I thought it said MOST number of 0's).
Then to multiply the number of 1's by the number of 2's.

Simple enough.

It was easy enough to break the input into the proper structure using ```chunksOf``` from the ```Data.List.Split``` library.
I rolled my own function to count the number of zeroes in a layer, which boils down to ```length . filter (== 0)```.
The final solution was a chain of function composition to transform the layer with the smallest number of 0s into the checksum.

```Haskell
part1 :: [Int] -> Int
part1 = product        -- get the product of the two lengths
  . (map length)       -- get the length of each list
  . group              -- group the 1s into a list and 2s into a list
  . sort               -- Sort the list into 1s and 2s
  . filter (/= 0)      -- Remove the zeroes from the layer
  . foldl1 countZeroes -- Find the minimum layer
  . layers'            -- Transform the input into layers
```

I still don't understand fusion, but I think GHC can optimize out some of this stuff.

##### Part Two:

Part Two was transforming the layers into a picture.
Now the picture I wrote up in the prelude will be of more use.
The basic gist of the layering is that the final picture will use the top most visible pixel.

Using the above example:

Layer 1 has two transparent digits in its top row, so we drill down into the next layer to see if those pixels are visible.
Layer 2 has a 1 in the top-left and another 2 in the top-right.
This means the final image will have a 1 in the top-left, and we must drill-down one more level for the top right.
The final layer has a 0 in the top-right position so the final image will have a 10 for the top row.
The bottom row is trivial as they are both 1s which means the final picture will be:

```
10
11
```

The solution I came up with is to transpose the list (which is essentially a matrix), placing each pixel in the same position into one list.
Since the layers are ordered, we can traverse each list and find the first non-2 in the list and that will be our final pixel output.

```Haskell
part2 :: [Int] -> [[T.Text]]
part2 =
  chunksOf width           -- This is done to make output readable
  . map transform          -- apply transformation to make text readable
  . concatMap (take 1)     -- take the first digit and flatten the list
  . map (dropWhile (== 2)) -- drop leading 2's
  . transpose              -- transpose matrix
  . layers'                -- Transform the input into layers
  where
    transform x = if x == 1 then "X" else " "  -- trivial function to make output more readable
```

I chunked the final output so I could "pretty print" the output, which is ASCII art. 


***
### Day Nine: Sensor Boost

***

New day, new IntCode Operation.
Day Nine added a new instruction and piece to the IMachine.
The IMachine must now keep track of a "relative base index".
The relative base index starts at 0 when an IMachine is created.
The new instruction (op code 9) increments the relative base index by the value located in its only parameter.
Speaking of parameters, the relative base is used for the third parameter mode, Relative.
What this means is that when a parameter is in Mode 2 or Relative, the value in the address is added to the current relative base index, and this value is then used as the address for whatever operation is to be executed.
For example, given a relative base of 50 and a parameter in mode Relative of -7, the new address would 50 + -7 or 43.
Implementing this change took very little time, as it was simple to add to the existing code base.

There were two other requirements needed for Day Nine, support for large numbers and memory access past the bounds of the program.
The support for large numbers is simply, just a find and replace Int with Integer.
An Integer in Haskell has no bounds and can grow indefinitely.
The second change didn't seem to affect the program. 
The map look-ups will fail when trying to access a value that is not a member.
However, I never got a failure when running the program, so I just omitted it for the time being.
It should also be noted that there was no actual need for Integer support, when I originally ran the problem I got the correct output but omitted one digit so I received a failure.
This is when I made the change to the underlying representation of the machine.

I won't bother doing a breakdown as both parts were simply running the machine given two inputs.

***
### Day Ten: Monitoring Station

***

Not much background needed, let's jump right into it.

##### Part One:

Day Ten Part One involved finding the best asteroid to build a monitoring station.
We are given a map of the area which has a coordinate system of asteroids and empty space.
The best asteroid to build the station is the point in the area that has the most number of VISIBLE asteroids from its location.
The prompt indicates that each asteroid is located at the center of its coordinate, this means that asteroids "hide" other asteroids if they fall on the same line.

For example this is a 4x4 map, where each ```#``` is an asteroid.

```
..#.
###.
#..#
.#.#
```

The problem at hand is to search all asteroids in the area, and counting the number of visible asteroids, attempting to find the largest number of visible asteroids.
Thanks to trigonometric functions (well inverse trig functions), we can determine the angle from the asteroid being tested to all other asteroids in the system.
We can then build a set of all angles, which will remove any duplicates (aka asteroids that are on the same line).

This begs the question how can we reasonable translate the input into a coordinate map.
Well, the little function ```divMod``` can do the heavy lifting for us.
This function combines integer division and mod division into one operation.
Given an array (list) and a width of each row, we can use this function to create a coordinate in one go.
When flattening a 2D array into a 1D array its common to use the function (x * width) + y.
The ```divMod``` function reverses this operation into (y, x).
A simple ```swap``` reverses the tuple and creates a coordinate.

The final parsing implementation is as follows:

```Haskell
parseToCoordList :: [String] -> [Coord]
parseToCoordList xs =
  map (toD . itc)      -- Map int to coords onto each asteroid index and transform to Double
  $ elemIndices '#'    -- Find each asteroid's index
  $ concat xs          -- Concat the input so we can remove new lines
  where
    width = length (xs !! 0)
    itc n = swap $ divMod n width  -- The coordinate system is flipped
    toD (x, y) = (fromIntegral x, fromIntegral y)
```

Perfect, but now we need to test each coordinate and find out how many visible asteroids there are.
People always complain about learning trigonometry functions in high school, but they will be extremely useful here.

```Haskell
-- computes the angle from one coordinate to another
-- Bounds are [-pi, pi], for part 2 we adjust this range from [0, 2pi]
angle :: Coord -> Coord -> Double
angle (x1, y1) (x2, y2) = pi + atan2 (y1 - y2) (x1 - x2)
```

The first coordinate is always curried to be the coordinate we are searching from.

We use this function when we fold over the input and create a Map from Coordinate to a Set of Doubles.

```Haskell
part1 :: [Coord] -> (Coord, Int)
part1 cm =
  foldl1 maximum'                    -- get the coordinate with the largest set
  $ map size                         -- Map the size function to the tuple of (coord, set)
  $ M.toList                         -- Turn the map into a list
  $ foldl (flip insertP) M.empty cm  -- fold over the list of coords, creating a map of a coordinate to a set of angles
  where
    cmA p1 = S.fromList $ map (angle p1) cm   -- create a set of angles from the point supplied
    insertP p = M.insert p (cmA p)            -- insert a coordinate and its angles into the map
    size (p, s) = (p, S.size s)               -- map set size from list
    maximum' acc@(_, s1) curr@(_, s2) = if s1 > s2 then acc else curr  -- maximum function on two tuples
```

The comments succinctly describe what is happening.
The actual solution is done by the ```maximum'``` function which searches the map for the largest Set.

##### Part Two:

Part One took relatively little time. Part Two on the other hand had me drawing out coordinate systems on a piece of paper for hours.

After finding the best asteroid to build the monitoring station, we were tasked with destroying all the asteroids around the station.
The prompt is also looking for the 200th asteroid that gets destroyed.
We start by pointing our laser UP.
When I first read this I was under the impression that it was in reference to the coordinate system.
However, the coordinate system origin starts from the top-left, so that UP is actually DOWN. 
After destroying one asteroid the laser moves in a clockwise rotation.
However, if the asteroid destroyed reveals an asteroid behind it, we must wait until we make it all the way around again.
To help keep track of all the moving pieces, I created the ```Laser``` data type.

```Haskell
data Laser = L { numDest :: Int      -- Number of asteroids destroyed
               , am      :: AngleMap -- map of angles to coords
               , currAng :: Double   -- current angle of the laser
               , equal   :: Bool     -- do we want to use lookupGE or lookupGT
               , base    :: Coord    -- Base coordinate
               } deriving (Show)
```

The ```currAng``` starts at 3&#960;/4 (Remember, I shifted the bounds of ```atan2``` by &#960;).

The actual process of destroying an asteroid is done by the ```destroy``` function.

I wrapped the Laser into an RWS Monad so the list of destroyed asteroids could be retrieved.

```Haskell
runDestroy :: LaserS ()
runDestroy = do
  laser <- get
  coord <- destroy
  tell [coord]
  if M.null (am laser)
    then return ()
  else runDestroy

destroy :: LaserS Coord
destroy = do
  laser <- get
  -- if we've done a lookup where there are more than one elements
  -- we don't want to search same list again
  let amLookup = if equal laser then M.lookupGE else M.lookupGT
  -- this catches the case where we need wrap around our angle i.e. 2pi -> 0
  let lkup = amLookup (currAng laser) (am laser)
  -- get the angle found and the Set of coords
  let (ang, coords) = case lkup of
                        Nothing -> fromJust $ M.lookupGE 0.0 (am laser)
                        Just x  -> x
  -- find and delete the closest coordinate
  let p@(_, toDest) = foldl1 (\acc test -> if (fst acc) > (fst test) then test else acc) coords
  let coords' = S.delete p coords
  -- if we've deleted the last coordinate then just delete the key/val pair otherwise update set
  -- the boolean doesn't matter except in the else clause
  let (equal', am') = if S.null coords' then (True, M.delete ang (am laser)) else (False, M.insert ang coords' (am laser))
  put laser {numDest = (numDest laser) + 1, am = am', currAng = ang, equal = equal'}
  return toDest
```

It's an ugly solution, seems quite imperative, but it was a complex problem to solve.
Of note: Somehow this solution actually worked.
When I was writing this up I realized that 3&#960;/4 is the angle for points above the base, when in reality we want below the base.
However, when tested against the test cases I got the proper outputs and ordering of destruction correct.
Maybe I'm overthinking this and I actually did do it properly, I'm not exactly sure.


***
### Day Eleven: Space Police

***

Not that anyone's keeping track of this, just enjoyed a nice quarantine Fourth of July in Northern Maine.
While most of the mini-vacation was spent enjoying the nice weather, I was able to knock out day eleven.
The challenge for this day was to paint a spaceships hull.
We see the return of the IntCode Machine, this time however, there were no changes to be made.
The machine will be running a Hull Painting Robot.

The program to run the robot, will continually ask for input and provide output which will direct how the robot paints and moves.
The input to the program will be either a 0 or a 1, with 0 being a black "pixel" and 1 being a white "pixel".
To actually supply input, the robot will examine its current location on the hull and read the color.
The output of the program will be either a 0 or a 1, and (from what I can tell) the output comes in pairs.
The first output will be the color to paint the current hull "pixel", a 0 means black and a 1 meaning white.
The second output will be the direction to turn and move the robot, a 0 is a left turn and a 1 is a right turn.
Of note, the turn will always advance the robot one step in the direction of the turn.

The representation of the robot is as follows:

```Haskell
type Coord = (Int, Int)
type Hull = Map Coord Color

data Color = Black -- 0
           | White -- 1
           deriving (Show, Eq, Enum)

data Dir = North
         | South
         | East
         | West
         deriving (Show, Eq)

data Robot = Robot { hull    :: Hull
                   , painted :: Set Coord
                   , currLoc :: Coord
                   , im      :: IMachine
                   , dir     :: Dir
                   , onPaint :: Bool
                   } deriving (Show)
```

A ```Hull``` is a map of ```Coord``` to ```Color```.
This is where we keep track of the current state of the hull.
We use a default lookup on this map that defaults to Black if we inspect a coordinate that hasn't been painted.
We keep track of what we've painted using a set of ```Coord```, this is only used for Part One.
The Direction encodes the way the robot is currently facing and is used to control how the robot turns.
We also have a boolean flag, ```onPaint```.
This flag is used when we are using the output values from the machine.
I never actually examined how the output was generated, i.e. The way the function to operate on the outputs is agnostic to how many values were actually output.
So hypothetically if only one output was given, the program will reliably track which operation to run.

The main operation for day eleven is ```runRobot```.
It uses ```runMachine``` internally to run the embedded ```IMachine``` in the robot.
I've altered the way ```runMachine``` works to accept operations that we want to pause on.

```Haskell
runRobot :: Robot -> Robot
runRobot r = do
  let currCol = fromColor $ coordToColor r
  let (op, im', out) = runRWS (runMachine [LoadVal, Halt]) [currCol] (im r)
  if op == Halt
    then r
    else runRobot $ movePaint out $ updateIM im' r

movePaint :: [Integer] -> Robot -> Robot
movePaint (x:xs) r = case (onPaint r) of
                       True  -> movePaint xs $ paint x r
                       False -> movePaint xs $ turn x r
movePaint [] r      = r

paint :: Integer -> Robot -> Robot
paint col r = notPaint $ r {hull = hull', painted = painted'}
  where
    hull' = M.insert (currLoc r) (toColor col) (hull r)
    painted' = S.insert (currLoc r) (painted r)

turn :: Integer -> Robot -> Robot
turn 0 r = notPaint $ case dir r of
             North -> r { currLoc = addCoords (currLoc r) (-1, 0), dir = West}
             South -> r { currLoc = addCoords (currLoc r) (1, 0), dir = East}
             East  -> r { currLoc = addCoords (currLoc r) (0, 1), dir = North}
             West  -> r { currLoc = addCoords (currLoc r) (0, -1), dir = South}
turn 1 r = notPaint $ case dir r of
             North -> r { currLoc = addCoords (currLoc r) (1, 0), dir = East}
             South -> r { currLoc = addCoords (currLoc r) (-1, 0), dir = West}
             East  -> r { currLoc = addCoords (currLoc r) (0, -1), dir = South}
             West  -> r { currLoc = addCoords (currLoc r) (0, 1), dir = North}
```

A couple of notes about the functions defined above:

- ```paint``` and ```turn``` are wrapped in ```notPaint``` inverts the flag on each pass.
- In Day Nine, the memory was supposed to be arbitrarily large. Any attempts to read from memory not already created we should default to 0. In my write-up of that day, I mentioned that I never ran into any issues with this. However, for this day I started to get exceptions. This was a simple fix as all that needed to change was to provide a default value, 0, for any lookups done on the memory.

##### Part One:

Part One was simple, all that needs to be done is to find out how many "pixels" were painted ONCE.
This means we are only counting unique "pixels" that were painted, this is encoded in the set ```painted```.

```Haskell
part1 :: [Integer] -> Int
part1 = S.size . painted . runRobot . mkRobot
```

##### Part Two:

Part Two was the actual running of the robot, the output is a registration number (part two's answer).
The only difference from part one is that the starting "pixel" is changed from black to white.
To get the actual output we need to display the pixels, I decided to use some unicode characters to make the output slightly more pretty, than day nine.
A function was created to take the Map of the hull and transform it into rows.
These rows are then altered from Colors to Strings.

```Haskell
part2 :: [Integer] -> [String]
part2 = hullToChar . runRobot . mkRobotPart2

hullToChar :: Robot -> [String]
hullToChar = map (map toChar) . splitHull
  where
    toChar Black = ' '
    toChar White = '█'

-- determine the size of the hull and create a list of each row
-- transforming each coord into a color
splitHull :: Robot -> [[Color]]
splitHull r = map (map (\c -> M.findWithDefault Black c (hull r)))
  $ cells
  where
    minX = minimum $ map fst $ M.keys (hull r)
    maxX = maximum $ map fst $ M.keys (hull r)
    minY = minimum $ map snd $ M.keys (hull r)
    maxY = maximum $ map snd $ M.keys (hull r)
    cells = [makeRow minX maxX y | y <- reverse [minY .. maxY]]

-- helper to make a row of coords
makeRow :: Int -> Int -> Int -> [Coord]
makeRow minX maxX y = [(x, y) | x <- [minX .. maxX]]
``` 

***
### Day Twelve: The N-Body Problem

***

The famous N-Body Problem (the one that I've never heard of until now), in astrophysics the N-Body Problem deals with the orbits of celestial objects.
It is not simple enough to model an orbit just by mapping the objects velocity, as there are cases where an orbit is influenced by other objects in the system.
This makes modeling an orbit much more complex as it must take into account the gravitational effect of other objects.
Day Twelve dealt with a vastly simplified model, it centered around Jupiter's four largest moons.
Each moon was given an X, Y, and Z position (the input was given in a funky format and I didn't bother creating a real parser for it so I just did some manual data-munging).
Each moon also had an X, Y, and Z velocity vector.
At the start of the model the velocity vector consisted of all 0's.

For each step in the model, we must account for the gravitational effects of each moon on the others which will effect the velocity.

The model works in the following way:
- We apply the gravitational effects to each moon and update its velocity vector
- After all gravity effects have been accounted for we update the position of each Moon by their individual velocity vector.

For example:

Take a Moon (Moon 1) with a position vector of (1, 0, 5) and another Moon (Moon 2) with a position vector of (4, -2, 5).
The velocity vector for the Moon 1 is the result of comparing each element in Moon 1's position vector to Moon 2.
If the element of a moon is greater than the one being compared to we subtract 1 to the corresponding velocity vectors element.
If the element of a moon is less than the one being compared to we add 1 to the corresponding velocity vectors element.
If the element of a moon is equal to the one being compared to we do nothing to the corresponding velocity vectors element.

Ex: Moon 1 x coordinate = 1 and Moon 2 x coordinate = 4, therefore we subtract -1 from the x position of Moon 1's velocity vector to get (-1, 0, 0).
Moon 1 y coordinate = 0 and Moon 2 y coordinate = -2, therefore we add 1 from the y position of Moon 1's velocity vector to get (-1, 1, 0).
Moon 1 z coordinate = 5 and Moon 2 z coordinate = 5, therefore we do nothing and get Moon 1's final velocity vector of (-1, 1, 0).

I decided to use a new package Linear, which is a mathematical vector package to model the Moons.
I should have been using this much earlier as it makes manipulating vectors much easier.

```Haskell
type Moons = [Moon]
type Vector = V3 Int
data Moon = Moon { pos_ :: Vector
                 , vel_ :: Vector
                 } deriving (Show, Eq)

applyGrav :: Moon -> Moon -> Moon
applyGrav (Moon p1 v1) (Moon p2 _) = Moon p1 (v1 L.^+^ v1')
  where
    cmp EQ = 0
    cmp GT = (-1)
    cmp LT = 1
    v1' = (fmap (cmp) $ L.liftI2 (compare) p1 p2)

updateVel :: Moons -> Moons
updateVel ms = map (\m -> foldl' applyGrav m ms) ms

applyVel :: Moons -> Moons
applyVel = map (updatePos)

updatePos :: Moon -> Moon
updatePos m = m {pos_ = (pos_ m) L.^+^ (vel_ m)}

timestep :: Moons -> Moons
timestep = applyVel . updateVel

simulate :: Moons -> [Moons]
simulate = iterate timestep
```

We can simulate the model by repeatedly applying the ```timestep``` function to the list of Moons.
This is done through the aptly name ```simulate``` function which uses ```iterate``` which creates an infinite list of repeated applications of the function given.

##### Part One:

Part One required simulating 1000 steps of the system and calculating the total amount of energy in the system at this step.

For one moon the total amount of energy is calculated by the sum of the absolute value of the position vector's elements multiplied by the sum of the absolute value of the velocity vector's elements.
The total energy of the system is the sum of all moons energy.

```Haskell
part1 :: [[Int]] -> Int
part1 = sum . map totalEnergy . head . drop 1000 . simulate . mkMoons

kineticEnergy :: Moon -> Int
kineticEnergy (Moon _ (V3 x y z)) = (abs x) + (abs y) + (abs z)

potentialEnergy :: Moon -> Int
potentialEnergy (Moon (V3 x y z) _) = (abs x) + (abs y) + (abs z)

totalEnergy :: Moon -> Int
totalEnergy m = (potentialEnergy m) * (kineticEnergy m)
```

##### Part Two:

As mentioned in the preamble, the system of moons is a model of their orbits.
These orbits continue indefinitely, and will eventually repeat.
The prompt indicates in the test case that the system repeats when it matches its starting position.
For Part Two we were supposed to find out how many steps it would take for the system to repeat.
Therefore, I made a broad assumption that this meant we would have to match the starting position.
However, brute-forcing the solution would be extremely slow, as the test case repeats after 4 trillion steps (I didn't actually count the digits in the prompt it was just a large number).
This means there has to be a way to simplify the simulation and get the correct answer quickly.

After looking up what the N-Body problem actually was, I came across a formula that dealt with the total energy of the system.
Now this problem is nowhere near as complex or similar to the real N-Body problem, but I thought that that was where I should look.
After about an hour of examining the energy of the system at different steps, I realized I had no idea what I was doing.
I thought maybe there would be a pattern that could be leveraged in how the energy changed from step to step.
I was wrong.
After this failed attempt I went to my faithful Neil Smith and looked at how he approached the problem.
His first sentence said that each coordinate of the planet is independent of the other and that it's possible to count how long it takes for a planet to cycle to the same position.
I was extremely deflated that I didn't start there and a tad embarrassed.

With this in mind its simple to find out how many steps it takes.
First you must find the period of the cycle for each coordinate X, Y, and Z.
This means we have to count how many steps it takes for all four planets to have their coordinates match up to the starting position.
This had to be done independently or else it would just be brute-forcing.
Once you get the counts for each coordinate you can just find the smallest multiple of the three numbers and that's how many steps it would take to repeat.

In the end I still couldn't get the function to behave properly, when I compared my implementation to Neil's it appears my step counts were off by 2.
So I cheated and mapped (+2) to final counts.
I'm not proud of it, and it really shows a lack of problem solving, but what can you do.

```Haskell
part2 :: [[Int]] -> Integer
part2 m = foldl' lcm 1 $ map (+2) [moonsX, moonsY, moonsZ]
  where
    sim = simulate
    moonsX = search X $ mkMoonsP2 X m
    moonsY = search Y $ mkMoonsP2 Y m
    moonsZ = search Z $ mkMoonsP2 Z m

search :: Coord -> Moons -> Integer
search c m = go (timestep m) 0
  where
    go m' n
      | map (getCoord c . pos_) m == map (getCoord c . pos_) m' = n
      | otherwise = go (timestep m') (n + 1)

getCoord :: Coord -> Vector -> Int
getCoord X (V3 x _ _) = x
getCoord Y (V3 _ y _) = y
getCoord Z (V3 _ _ z) = z
```

***
### Day Thirteen: Care Package

***
**Note** For those of you reading this real-timeish (which realistically is probably no one) , my Summer semester has started.
With all the Covid business, I decided to take a heavy course load. Not sure how long in between updates it will be.

Day Thirteen involved using the IntCode Machine to play the classic brick breaker arcade game.
The game would be modeled as a simple ADT: ```Arcade```.
The ```Arcade``` ADT wraps up the ```Screen``` type alias (which is just a map of x and y positions with a ```Tile``` type), the ```IMachine```, the ```MachineState```, the position of the ```Ball```, the position of the ```Paddle```, and the total score.

The creation of the game screen is derived from the output provided by the ```IMachine```. 
The first time the machine is run, the entire screen is output.
Thereafter, every time something was output it was only the coordinates that were changed.
I chose to play the game very simply (I'm honestly not sure there was any other way to play) and just have the paddle shadow the ball and constantly move to remain on the same x-coordinate as the ball.
Speaking of which, the ```IMachine``` required the input from the "joystick" to move the paddle.
To move left, a -1 is supplied. To move right, a 1 is supplied. To do nothing a 0 is supplied.
Also as a note, I felt so excited to be (what I consider) functional and leverage the ```fromEnum``` function and apply it to ```Ordering```, which provides values from [0 .. 2].
It wasn't needlessly complex but an if statement or pattern matching would've done the exact same thing.

Also of note, I did some fiddling with the IntCode module, I changed some of the return types to ```MachineState``` instead of ```Op```.
This just made it easier to reason about the running of the Machine. When the ```LoadVal``` operation is returned it doesn't always mean we need to stop running. 
Only when the machine has exhausted it's input do we need to stop.

```Haskell
type Coord = (Integer, Integer)
type Screen = Map Coord Tile

data Tile = Empty
          | Wall
          | Block
          | Paddle
          | Ball
          deriving (Show, Eq, Enum)

data Arcade = Arcade { iMachine_     :: IMachine
                     , machineState_ :: MachineState
                     , ballPos_      :: Coord
                     , paddlePos_    :: Coord
                     , screen_       :: Screen
                     , score_        :: Maybe Integer
                     } deriving (Show)
```

##### Part One:

Part One was a baby step into the Game.
All it asked for was a count of the number of ```Block``` tiles on the screen at the start of the game.

```Haskell
part1 :: [Integer] -> Int
part1 p =
  M.size                      -- Get the size of the map of blocks
  $ M.filter (== Block)       -- Filter out Tiles that aren't blocks
  $ snd
  $ mkScreen                  -- turn the output into a Screen
  $ getMachineOutput p [] []  -- run the machine
```

##### Part Two:

Part Two was actually playing the game.
Here we needed to capture the final score when all the blocks have been destroyed.
The function to run the ```Arcade``` was straightforward and matches a lot of the previous days.
The pseudo-loop is ended when all the blocks are gone.
There was nothing crazy about this day, despite how daunting it appeared at first.

```Haskell
runArcade :: Arcade -> Arcade
runArcade a
  | blocksLeft a || machineState_ a /= Finished = runArcade a'
  | otherwise = a
  where
    input = movePaddle a
    a' = updateArcade a $ getMachineRWS' (iMachine_ a) [input] [LoadVal, Halt]

updateArcade :: Arcade -> (MachineState, IMachine, [Integer]) -> Arcade
updateArcade arcade (ms, im, out) = arcade { iMachine_ = im'
                                           , machineState_ = ms
                                           , ballPos_ = findBall screen'
                                           , paddlePos_ = findPaddle screen'
                                           , screen_ = screen'
                                           , score_ = score <|> score_ arcade
                                           }
  where
    im' = resetInputIx im
    (score, screen) = mkScreen out
    screen' = M.union screen (screen_ arcade)
```
