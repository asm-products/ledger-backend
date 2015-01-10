# [Ledger (backend)][1]

<a href="https://assembly.com/ledger/bounties">
  <img height="20" src="https://asm-badger.herokuapp.com/ledger/badges/tasks.svg">
</a>

Track your expenses.

This is a product being built by the Assembly community. You can
help push this idea forward by visiting <https://assembly.com/ledger>.

This is the backend for [ledger-frontend][2].

- [Installation](#installation)
- [Configuration](#configuration)
- [Deployment](#deployment)

## Installation

To install ledger-backend, you'll need [The Haskell Platform][3] version 2014.2.0.0.

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

To configure ledger-backend, create a [Configurator][4] file.

``` cfg
# tmp/ledger-backend.cfg
warp { port = "8888" }
acid-state { directory = "state/ledger-backend" }
```

``` sh
$ cabal run tmp/ledger-backend.cfg
# http://localhost:8888
```

For a complete list of options, check out [the default configuration][5].

## Deployment

To deploy ledger-backend, create an [OpenShift][6] account.

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
[2]: https://github.com/asm-products/ledger-frontend
[3]: https://www.haskell.org/platform/
[4]: https://github.com/bos/configurator
[5]: data/ledger-backend.cfg
[6]: https://www.openshift.com
