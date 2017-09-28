Malice
======

Malice is a Haskell library that provides abstractions and protocol-specific attacks for intercepting and modifying network traffic. It leverages the power of Haskell's type system to define an embedded DSL for describing the actions of Eve and Mallory. In effect, it allows a man or woman in the middle to write code that is as close to pure evil as possible.

Malice is inspired by [mallory](https://github.com/CarveSystems/mallory) and [mitmproxy](https://github.com/mitmproxy/mitmproxy).

## How It Works

At the core of Malice lie four Monad transformers:

- `EndpointT`
- `VertexT`
- `EveT`
- `MalT`

```
MonadEve       ==implies=>  MonadMal

   ^                           ^
 hoistFrom                   hoistFromTo
   |                           |

MonadEndpoint  ==implies=>  MonadVertex
```

*More documentation coming soon.*

## Demo

`./demo` contains a simple example program called `flip-images`, which flips JPEG's and PNG's passing through HTTP and HTTPS.
It demonstrates the ease with which one can modify structured streams using Malice, and some basic ways to situate Malice between Alice and Bob.

It's usage is as follows:

```
flip-images [-t|--transparent] [-p|--port PORT] CERT_IN PRIV_IN
```

Where `CERT_IN` is a PEM-encoded CA certificate and `PRIV_IN` is the corresponding PEM-encoded RSA private key.
`./scripts/new-root` can generate these for you.
You will need to install this certificate as a root in your browser.

By default, `flip-images` runs as a SOCKS5 server.
The `-t` flag causes it to instead run as a transparent TCP proxy, using netfilter to forward connections along to their intended destinations.
With this option, you will need to direct all of Bob's traffic to the proxy port.

This iptables rule is probably the easiest way to test out transparent mode (where Malice runs as `$uid`, which must be different from that of Bob):

```
iptables -t nat -A OUTPUT -p tcp -m multiport --dports 80,443 -m owner ! --uid-owner $uid -j REDIRECT --to-port $port
```

And to restore:

```
iptables -t nat -L --line-numbers
iptables -t nat -D OUTPUT $n
```
