# Chapter 14 - Testing

* **Relying on the compiler only will not prevent all the errors**.
    * runtime errors can still occur
    * well-typed program still may not performed as expected.
* Testing allow us to compare the expected value and the result of an
  operation.

## Testing categories

### Unit testing

* Test the smallest unit of software independently.
* check functions whether they perform as expected
* **spec testing**: newer, more human-readable version of unit testing.
* Haskell libs: `Hspec` for spec testing and `HUnit` for unit testing.
* Limitation: Cant verify if all pieces work together properly

### Property testing

* Pioneered in Haskell because of the type system.
* **Test the formal properties of programs without formal proofs**
    * check truth-valued, universally quantified function against randomly
      generated input.
* Haskell lib: `QuickCheck`
    * By default, generate 100 random input, and giving 100 results.
    * If one result failed, then the program has no specified property.
    * If all result succeded, it doesnt guarantee that the program will never
      fail. Needs to do more tests.
    
* Good for ensuring our programs meet the minimum requirements.
* Inappropriate for all programs.

## Conventional testing

* Use `Hspec` to demonstrate a simple test case
