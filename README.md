Malice
======

Malice is a Haskell library that provides abstractions and protocol-specific attacks for intercepting and modifying network traffic. It leverages the power of Haskell's type system to define an embedded DSL for describing the actions of Eve and Mallory. In effect, it allows a man or woman in the middle to write code that is as close to pure evil as possible.

Malice is inspired by [Mallory](https://github.com/CarveSystems/mallory) and [mitmproxy](https://github.com/mitmproxy/mitmproxy).

## Core Abstractions

### Monad Transformers

At the core of Malice lie four monad transformers:

#### `EndpointT`

`EndpointT` provides a buffered interface to a stream endpoint (e.g. `recv` on a socket) suitable for use with Haskell's great parser combinator and serialization libraries such as [attoparsec](https://hackage.haskell.org/package/attoparsec) and [cereal](https://hackage.haskell.org/package/cereal-0.5.4.0).
It also provides a means of throwing and catching pure protocol-related exceptions.

#### `VertexT`

`VertexT` extends `EndpointT` with a stream origination point (e.g. `send` on a socket).
It can serve as an abstraction for Alice (a node for bidirectional communication, such as a client or a server), or for Mallory in one direction (a node that modifies a unidirectional stream).
These two uses are depicted below:

```
           v
           |
           |
  o   OR   o
 / \       |
|   |      |
^   v      v
```

#### `EveT`

`EveT` is just like `EndpointT`, except it provides buffered interfaces for two stream endpoints.
This is an abstraction for a passive man in the middle, and is depicted below.

```
      Bob
       v
       |
       |
       v
  o    o
  ^
  |
  |
  ^
Alice
```

Give a specific side of the communication that an `EveT` action sits between (Alice or Bob), an `EndpointT` action can be hoisted into an `EveT` action.
Here, the `o`'s represent effective `EndpointT` contexts.

#### `MalT`

`MalT` (Mal is short for Mallory) extends `EveT` with two stream origination points.
`MalT` is to `EveT` as `VertexT` is to `EndpointT`, and `MalT` is to `VertexT` as `EveT` is to `EndpointT`.
Like `VertexT`, `MalT` can be viewed in two different ways.

```
      Bob        Bob
     v   ^       ^  v
     |   |       |  |
      \ /        |  |
  o    o    OR   o  o
 / \             |  |
|   |            |  |
^   v            ^  v
Alice            Alice
```

Here, the `o`'s represent effective `VertexT` contexts.
Accordingly, given a source side and destination side, a `VertexT` action can be hoisted into a `MalT` action.

### Monad Classes

These monad transformers come with [mtl](https://hackage.haskell.org/package/mtl)-compatible monad classes.
They work like one would expect.
The diagram below depicts how the relationships described above apply to these classes:

```
MonadEve       ==implies=>  MonadMal

   ^                           ^
   |                           |
 hoistFrom                   hoistFromTo
   |                           |

MonadEndpoint  ==implies=>  MonadVertex
```

Here are the main operations within each class (keep in mind the hierarchy):

```
data Side = Alice | Bob

instance Awaitable Parser
instance Awaitable Get
...

instance Yieldable B.ByteString
instance Yieldable L.ByteString
instance Yieldable Builder
...

type instance InnerEndpoint (EveT m) = EndpointT m
type instance InnerEndpoint (MalT m) = EndpointT m
type instance InnerVertex (MalT m) = VertexT m

await :: (MonadEndpoint e m, Awaitable f) => f a -> m a
raise :: MonadEndpoint e m => e -> m a
except :: MonadEndpoint e m => m a -> (e -> m a) -> m a

yield :: (MonadVertex e m, Yieldable a) => a -> m ()

hoistFrom :: MonadEve e m => Side -> (InnerEndpoint m) a -> m a

hoistFromTo :: MonadMal e m => Side -> Side -> (InnerVertex m) a -> m a
```

All of these relationship make more sense when seen in action.
A well-commented demonstration of flipping images over HTTP and HTTPS (without any external HTTP protocol logic) can be found at [demo/flip-images/FlipImages.hs](demo/flip-images/FlipImages.hs). The **Demo** section below describes how to run the example.

Furthermore, for a nice example for just `MonadVertex`, see `Mal.Middle.Socks5`.

## Protocol Support

The only protocol-specific attack included right now is a basic SSL-sniff-style key swap that requires your TLS client to trust Malice's certs (which `scripts/new-root` can generate for you).
Thanks to Vincent Hanquez's wonderful [tls](https://hackage.haskell.org/package/tls) library, this required only 150 lines of code (found in `Mal.Protocol.TLS`).

Other protocols will be added as needed. SSH will probably be next.

## Routing Traffic through Malice

Currently, Malice contains a SOCK5 server and a tranparent TCP proxy that uses netfilter.

## Demo

`./demo` contains a simple example program called `flip-images`, which flips JPEG's and PNG's passing through HTTP and HTTPS.
It demonstrates the ease with which one can modify structured streams using Malice, and some basic ways to situate Malice between Alice and Bob.

It's usage is as follows:

```
flip-images [-t|--transparent] [-p|--port PORT] CERT PRIV
```

Where `CERT` is a PEM-encoded CA certificate and `PRIV` is the corresponding PEM-encoded RSA private key.
`./scripts/new-root` can generate these for you.
You will need to install this certificate as a root in your browser.

By default, `flip-images` runs as a SOCKS5 server.
The `-t` flag causes it to instead run as a transparent TCP proxy, using netfilter to forward connections along to their intended destinations.
With this option, you will need to direct all of Bob's traffic to the proxy port.

This iptables rule is probably the easiest way to test out transparent mode on a single machine (where Malice runs as `$uid`, which must be different from that of Bob):

```
iptables -t nat -A OUTPUT -p tcp -m multiport --dports 80,443 -m owner ! --uid-owner $uid -j REDIRECT --to-port $port
```

And to restore:

```
iptables -t nat -L --line-numbers
iptables -t nat -D OUTPUT $n
```
