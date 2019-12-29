# distributed-closure

[![Build status](https://badge.buildkite.com/6fa836ad49cc34388f0db7b65c59ee8a269c89d309b959c55b.svg?branch=master)](https://buildkite.com/tweag-1/distributed-closure)

Leverage the `-XStaticPointers` [extension][staticpointers-extension]
from GHC 7.10 onwards for distributed programming using lightweight
serializable closures. This package implements a *serializable
closure* abstraction on top of static pointers. Serializable closures
can be shipped around a computer cluster. This is useful for
implementing intuitive and modular distributed applications. See
[this blog post][ocharles-static-pointers] for a hands on introduction
and [this paper][ch-paper] for the original motivation.

[staticpointers-extension]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#static-pointers
[ocharles-static-pointers]: https://ocharles.org.uk/blog/guest-posts/2014-12-23-static-pointers.html
[ch-paper]: http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf

# Example

In GHC 8 and above, remoting a computation on another node using this
library goes along the lines of

```Haskell
data NodeId

-- Assume we're given a spaw primitive.
spawn :: NodeId -> Closure (IO ()) -> IO ()

-- A computation to remote on another node.
hello :: String -> IO ()
hello name = putStrLn ("Hello, " ++ name)

main = do
  name <- getLine
  spawn "someAddress" (static hello `cap` name)
```

An example of sending static pointers and closures
through a communication channel is provided under
[examples/ClientServer.hs](examples/ClientServer.hs)
in the source repository.

`distributed-closure` does not implement sending/receiving/spawning
closures - that's left for higher-level frameworks. Only closure
creation, composition and (de)serialization.
