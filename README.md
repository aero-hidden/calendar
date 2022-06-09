# calendar

## Refinement types

What we want is the capability to proove the conditions our refinement types state.

[Singletons](https://stackoverflow.com/questions/62001205/associate-a-type-level-optional-natural-number-maybe-nat-with-a-value)

### Hot reloading

Hot reloading instead of template haskell => HLS won't start crying as much.

[Hot reloading link](https://blog.jayway.com/2020/11/08/hot-reload-with-haskell/)

We're going to want to use either some statemonad configuring our programming
to continously get prooven. Or we are going to want some other entrypoint.

## What to proove

Predicate p1 , p2 , Functions f1 f2 , types T1 T2 T3

```Haskell
f1 :: T1 -> T2
f2 :: T2 -> T3
```

Want to proove that for p1 that means that p2 is true. Or t1 in T1 such that:
p2 = f1 t1
p1 t1 => p2 (f1 t1)

Shouldn't be that hard.

### How

We might need to proovide an edge case. Which id rather not have to.
This edge case will depend on lots of stuff. for example if the type is enumerated.

Edge cases are going to be difficult since their not all that obv. Comparative cases are, however prooving that something is even will be difficult. We are also going to list a bunch of ensurements if we dont have a smart way for the compiler of prooving it.

We could create some inferens. like that if a number is positive and even then its more than 1. and have thoose be invisible. However if we're smart about it
that might not be too hard? For example if a number is enumerated and there is 
a comparative case, we automatically know about the edgecases. some predicates wont have natural edgecases like thoose of even / odd.

When we chain predicates we might get into the problem of the previous predicate
not holding the requirement of the current function.

Going to have to think about passing some initial requuirement which handles not just the first function in the chain of functions. But that can be carried over to comming functions.
