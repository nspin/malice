malice
======

A Haskell library and tool for rapid prototyping on behalf of Alice, Bob, Eve, and Mallory.

This iptables rule is probably the easiest way to test out transparent mode:
```
iptables -t nat -A OUTPUT -p tcp -m multiport --dports 80,443 -m owner ! --uid-owner $uid -j REDIRECT --to-port $port
```
And to restore:
```
iptables -t nat -L --line-numbers
iptables -t nat -D OUTPUT $n
```

*More documentation coming soon*
