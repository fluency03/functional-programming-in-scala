# CHAPTER 6: Purely functional state

- The key to recovering referential transparency is to make the state updates *explicit*. Don’t update the state as a side effect, but simply return the new state along with the value that we’re generating.
- In effect, we separate the concern of *computing* what the next state is from the concern of *communicating* the new state to the rest of the program.
- Aren’t imperative and functional programming opposites? Absolutely not.
- 












