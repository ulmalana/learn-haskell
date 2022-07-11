# Chapter 23 - State

* **State**: data that exists **in addition to the inputs and outputs** of our
  functions and **may change** after each function is evaluated.
* State type in Haskell is a way of expressing state which **may change without
  resorting to mutation**.

## Random numbers

* Haskell has `random` library that can be accessed in `System.Random` to
  generate pseudorandom values.
* Inside `System.Random`, there is `StdGen` datatype that consist of two
  `Int32` values (ex: 475647687647 847546376). Both values are the seed to
generate the next random number.
* `random` function has type `random :: (RandomGen g, Random a) => a -> g -> (a,
  g)` and is used to generate **random values of any types**.
    * `random` will **always produce the same random value**, **unless it is feed different `StdGen` value**.
* `next` function **is DEPRECATED**.
```
	> import System.Random
	> mkStdGen 0
	StdGen {unStdGen = SMGen 0 16294208416658607535}
	> random (mkStdGen 0)
	(-7749304166575005736,StdGen {unStdGen = SMGen 16294208416658607535 16294208416658607535})
	> let sg = mkStdGen 0
	
	-- same StdGen always give the same result 
	> random sg
	(-7749304166575005736,StdGen {unStdGen = SMGen 16294208416658607535 16294208416658607535})
	> random sg
	(-7749304166575005736,StdGen {unStdGen = SMGen 16294208416658607535 16294208416658607535})
	
	-- feeding the new StdGen to next random function to produce new random value
	> random $ snd $ (random sg :: (Int, StdGen))
	(-531973508589804280,StdGen {unStdGen = SMGen 14141672759607663454 16294208416658607535})
	
	-- generate random value from a range of (4,10)
	> randomR (4,10) sg
	(4,StdGen {unStdGen = SMGen 16294208416658607535 16294208416658607535})

```
## State newtype

* `State` newtype **wraps a function** inside.
    * ```
        newtype State s a =
            State { runState :: s -> (a, s) }
      ```
* To wrap and unwrap a function inside `State`, we can use the following:
    * ```
        State :: (s -> (a, s)) -> State s a

        runState :: State s a -> s -> (a, s)
      ```

