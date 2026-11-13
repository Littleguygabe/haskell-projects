# haskell-projects

Collection of standalone Haskell projects and experiments:

- **`nn-dsl/`** — Neural network library built with `hmatrix` (`Numeric.LinearAlgebra`), featuring Xavier-initialized weight matrices, `tanh` activations, forward propagation, MSE loss, and recursive backpropagation.
- **`order-book/`** — Limit order book and matching engine built with `containers` and `mtl`, including market interaction handlers, matching logic, and test cases.
- **`uni-exchange/`** — Haskell matching engine paired with a FastAPI/Uvicorn Python API and stress-testing client scripts.
- **`volatility-surfaces/`** — Implied volatility surface data ingestion (`DataReader.hs`) and plotting (`Graphing.hs`), alongside Python `yfinance` data scripts.
- **`genetic-progs/`** — Genetic programming experiments in Haskell.
- **`generative-tree/`** — Procedural/generative tree experiments in Haskell.
- **`trading-comp/`** — Trading competition server in Haskell.
- **`warp-servant/`** — HTTP API prototype using `warp`, `servant-server`, and `aeson`.
- **`donut/`, `bin-sudoku/`, `randoms/`** — Smaller standalone Haskell exercises (ASCII 3D donut renderer, binary sudoku solver, etc.).
