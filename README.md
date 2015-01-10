# [Ledger (backend)][1]

[![Build Status][2]][3]
<a href="https://assembly.com/ledger/bounties">
  <img height="20" src="https://asm-badger.herokuapp.com/ledger/badges/tasks.svg">
</a>

Track your expenses.

This is a product being built by the Assembly community. You can help push this idea forward by visiting <https://assembly.com/ledger>.

This is the backend for [ledger-react][4].

- [Installation](#installation)
- [Configuration](#configuration)
- [Deployment](#deployment)

## Installation

To install ledger-backend, you'll need [The Haskell Platform][5] version 2014.2.0.0.

``` sh
$ git clone https://github.com/asm-products/ledger-backend.git
$ cd ledger-backend
$ cabal update
$ cabal sandbox init
$ cabal install
$ cabal run
# http://localhost:8080
```

## Configuration

To configure ledger-backend, create a [Configurator][6] file.

``` cfg
# tmp/ledger-backend.cfg
warp { port = "8888" }
acid-state { directory = "state/ledger-backend" }
```

``` sh
$ cabal run tmp/ledger-backend.cfg
# http://localhost:8888
```

For a complete list of options, check out [the default configuration][7].

## Deployment

To deploy ledger-backend, create an [OpenShift][8] account.

``` sh
$ rhc app create ledgerbackend http://www.accursoft.com/cartridges/yesod.yml
$ cd ledgerbackend
$ rhc ssh
```

``` sh
$ echo 'warp {
  host = "$(OPENSHIFT_HASKELL_IP)"
  port = "$(OPENSHIFT_HASKELL_PORT)"
}
acid-state {
  directory = "$(OPENSHIFT_DATA_DIR)/state/ledger-backend"
}' > $OPENSHIFT_DATA_DIR/ledger-backend.cfg
$ exit
```

``` sh
$ git remote add github https://github.com/asm-products/ledger-backend.git
$ git pull github master
$ git push origin github/master:master
```

[1]: https://github.com/asm-products/ledger-backend
[2]: https://img.shields.io/travis/asm-products/ledger-backend/master.svg?style=flat
[3]: https://travis-ci.org/asm-products/ledger-backend
[4]: https://github.com/tfausak/ledger-react
[5]: https://www.haskell.org/platform/
[6]: https://github.com/bos/configurator
[7]: data/ledger-backend.cfg
[8]: https://www.openshift.com
