# Advent Of Code 2019
This is my repository for Advent of Code 2019, I am using Haskell to better learn Functional Programming.

***
### Day One: The Tyranny of the Rocket Equation

***
Day One was a simple task. The idea is to get a Spaceship back from the edge of the solar System.
Each module of the space required a set amount of fuel to travel the required distance it was given by the formula ```(m / 3) - 2``` where m is the mass of the module and the division is floored.
Part One required mapping over a list of masses, applying the formula and getting the sum.

##### Part One
In ```DayOne.hs```, the part1 function takes an int and applies this formula and sums the result.

##### Part Two 
For Part Two, there is an added twist. 
Just like the modules of the ship, the fuel adds mass. 
Therefore we need to calculate this fact into the solution. 
This requires a simple change, we simply want to apply the ```fuel'``` formula recursively until the result becomes negative.
The function we defined constructs a list of fuel amounts each based on the prior value. Then we simply ```concatMap``` and ```sum``` the resulting list

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
  - The next two address (1 and 2) hold the source addresses for the add operation. Note: we have to access the value indirectly. In an imperative language like Java this would translate to: ```memory[memory[1]]```
  - The final address (3) holds the address that we place the result of the Add or Multiply operation eg. ```memory[memory[3]] = x```

The process of reading one Op Code is encoded in the Instruction Record. 
It holds the type of Operation, the address of the instruciton pointer, the first source address, the second source address, and the destination address.

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
As mentioned above, the point generation function (```genCoords```) initially was only for x and y coordinates, however part two necessitated the need to add the distance as well.

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
### Day Five: TODO

***
