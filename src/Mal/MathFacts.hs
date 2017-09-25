module Mal.MathFacts
    ( MathFact(..)
    ) where

import Crypto.PubKey.RSA
import Crypto.PubKey.DH as DH
import Crypto.PubKey.ECC.DH as ECDH

-- | A fact about the integers.
--
-- If one TLS endpoint logs MathFacts, these MathFacts can be used to decrypt
-- the TLS session after the fact (e.g. from a pcap) without any information
-- specific to that session.  In other words, logging MathFacts gives you
-- integer factorization and DLP, etc for all N's, logarithms, etc relevant to
-- that session.
data MathFact = RSA Integer Integer Integer -- ^ N, p, q
              | DH Integer Integer DH.PrivateNumber DH.PublicNumber -- ^ p, g, a, g^a
              | ECDH Curve ECDH.PrivateNumber ECDH.PublicPoint -- ^ (C, P), m, mP
