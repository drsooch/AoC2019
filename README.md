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
Machine A -> Machine B -> Machine C -> Machine D -> Machine E -> output value
 [1, 0]       [2, A]       [0, B]       [3, C]       [4, D]

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
