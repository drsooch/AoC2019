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
The instruction pointer is simply an Int which is type synonymed with Address.
The Memory of the IMachine is an IntMap, which is synonomous with a regular Map in haskell. 
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

As a newbie Haskeller, this seems like a great use of the State Monad.
The main crux of the IMachine is type defined as ```Program = State IMachine ()```. We can ignore the returned value of the State Monad as we don't need it. 
The main function that operates the IMachine is ```runMachine```. It's essentially executing instructions until the Halt Op Code is reached.

This took some time to get right as I struggled with getting a grasp of how to utilize the State Monad.
It was hard to wrap my head around the abstraction that the State Monad provides, but when I did it was one of the best feelings I've ever experienced.
I compared the code that I had written back in December and there was drastic improvement.

On to the actual challenge:

##### Part One

Part One was relatively straight forward, it required replacing values at address 1 and address 2, and running the resulting machine. 
I chose to use the IntCode module as if I were programming an API for an end user.
Instead of exposing all functions and data types, I selectively chose what could be used. 
As a result I needed to construct a function that takes a list of memory addresses and a value to replace (this would become especially useful in Part Two), this took the form of ```injectValues```.
After this, all we needed to do was ```execState runMachine replaceState``` and inspect the value at address 0.
To inspect a value a simple getter was created ```getMemValue```, after that part one is complete.

##### Part Two

Part Two was similar to Part One, however it required searching for a specific value located at address 0 when the machine has halted.
The replacement values range from 0 to 99, which is encoded in ```nounVerbPairs```.
In my previous attempt, I constructed a Set of all points tested and made sure not to test duplicates.
However, brute-forcing was quick enough that I didn't need to implement this option.

***
### Day Three: Crossed Wires

***

Day Three was a step away fom the IntCode Machine.
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
```traverseWire``` has a helper function ```createPoints``` that creates a list of all coordinates with the length at that point, which is then added to the accumulating map.
The creation of the points caused a bit of a headache as it silently failed because operators were oredered incorrectly.
As mentioned above, the point generation function, ```genCoords```, initially was only for x and y coordinates, however part two necessitated the need to add the distance as well.

##### Part One

Part One involved finding the intersection between the two wires with the smallest Manhattan Distance to the origin.
After creating the intersection of the two wires, the answer is found simply by folding over the map and finding the minimum manhattan distance. 

##### Part Two

Part Two involved finding the point of intersection that took the least amount of steps to get there.
This required getting the distance to each intersection from BOTH wires and summing the two. 
This is why the Map is key to this problem, because it provides a funciton ```intersectionWith``` that will be applied to the two map values.
We simply use ```intersectionWith (+)``` (the binary add function) to get a Map with intersecting points and their distances.
All thats left is appling a fold using the min function to get the final result.

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
It checks atleast two successive digits are equal.
After implementing this we simply apply ```filter (\n -> repeating n && inRange n)``` to range to filter out bad passcodes.
The actual solution requires the number of total valid passcodes, which is simply done by applying ```length`` to the filtered list.

##### Part Two

Part Two adds a slight twist to the filtering.
Instead of allowing ANY number of repeated digits, Part Two only allows for TWO repeated digits.
This boils down to adding a couple extra checks to ```repeating``` function.
For instance if we are looking at d2 (d = digit) and d3, we need to make sure ```d1 /= d2 && d3 /= d4```.
Essentially the preceding and succeding digits of the two being examined are different.

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

On the client side, aka for testing Part One and Two, we use the function ```runRWS``` which unwraps the computation into (Value, Writer, State).
For both parts we need the output (the Writer) to solve the problem.
I won't subdivide this day into Part One and Two as they are both the same, with the exception of the input values.

Of note, the module ```IntCodeS.hs``` encapsulates the old version of just the State IntCode Machine. 

***
### Day Six: TODO

***
