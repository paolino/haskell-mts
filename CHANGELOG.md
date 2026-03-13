# Changelog for mts

## [1.0.0](https://github.com/lambdasistemi/haskell-mts/compare/v0.3.2...v1.0.0) (2026-03-13)


### ⚠ BREAKING CHANGES

* MerkleTreeStore now takes a mode type parameter. Downstream consumers add 'Full to their types. csmtMerkleTreeStore and mpfMerkleTreeStore now return IO.

### Features

* add CBOR proof serialization, root export, and RocksDB MPF tests ([8d97b9a](https://github.com/lambdasistemi/haskell-mts/commit/8d97b9a3c0304400f0ad150479d7ddb02cb8a5cf))
* add completeness proofs to shared MTS interface ([828ee3f](https://github.com/lambdasistemi/haskell-mts/commit/828ee3ffb1563e207e6f419f37574e922263ecd6))
* add CSMT pop (minimum extraction) test vectors ([542af6d](https://github.com/lambdasistemi/haskell-mts/commit/542af6d3bfa0b59ce806d27513de5fe728fbb940)), closes [#81](https://github.com/lambdasistemi/haskell-mts/issues/81)
* add csmt-test-vectors executable for Aiken test generation ([1c6ab33](https://github.com/lambdasistemi/haskell-mts/commit/1c6ab33456b326120742d4270c606b70a558b323))
* add CSMT.Fifo module with counterToKey encoding ([ccf9d49](https://github.com/lambdasistemi/haskell-mts/commit/ccf9d49eec8702bac66094300b17f0427c3edcc1))
* add FIFO test vectors and FifoSpec ([abc7c7d](https://github.com/lambdasistemi/haskell-mts/commit/abc7c7d8bf48f1a22b3fa3ce54be547ede028620))
* add hexTreePrefix to MPF FromHexKV ([f9ab8ae](https://github.com/lambdasistemi/haskell-mts/commit/f9ab8ae126d79c5ac8935038a5ede335f86f0499))
* add keyToByteString inverse of byteStringToKey ([b8d1be5](https://github.com/lambdasistemi/haskell-mts/commit/b8d1be55d6ce27b5cb80f12a8f88b3aae1decfb3))
* add MPF (Merkle Patricia Forest) implementation ([aa60ceb](https://github.com/lambdasistemi/haskell-mts/commit/aa60ceb8fad53e64af2776372a763fd22d3c2164))
* add MTS constructors for CSMT and MPF ([85c7f11](https://github.com/lambdasistemi/haskell-mts/commit/85c7f11d065d43b0ef3da4fde07107aeacf5b6d4))
* add mts:rollbacks Haskell sub-library skeleton ([1a7396f](https://github.com/lambdasistemi/haskell-mts/commit/1a7396fbc93ff6002b3228cf879a5a19c9dee892))
* add mts.cabal and shared property test ([9773a07](https://github.com/lambdasistemi/haskell-mts/commit/9773a07eb0eea689cdb701f73befdaba9b7081a3))
* add pop_max, push_min, push_max test vectors ([090596f](https://github.com/lambdasistemi/haskell-mts/commit/090596f870f3f71d16370dcb8f5e5d587a79e2d9))
* add prefix-scoped CSMT operations for dynamic namespaces ([b7696e8](https://github.com/lambdasistemi/haskell-mts/commit/b7696e8d24a9d5c62efdd653d3031e77fd968116))
* add prefix-scoped MPF operations for dynamic namespaces ([d1a88e6](https://github.com/lambdasistemi/haskell-mts/commit/d1a88e67a32255e920a71daa845c7a5d8cca31b0))
* add RollbackColumn GADT ([da84fd4](https://github.com/lambdasistemi/haskell-mts/commit/da84fd4196034a1417da5153ad2a1396a3929209))
* add shared MTS interface and QuickCheck properties ([192b51e](https://github.com/lambdasistemi/haskell-mts/commit/192b51e968d7760bbf395c8fd77e29faf290d3d8))
* add split-mode MTS with KVOnly bootstrap and journal replay ([965db27](https://github.com/lambdasistemi/haskell-mts/commit/965db279deb3943797e6d89e118a9631510de25a))
* add Transaction-level rollback operations ([23e7351](https://github.com/lambdasistemi/haskell-mts/commit/23e7351c5b4c7275224c8dcf6524b3cfe3754a0d))
* add treePrefix field to FromKV for secondary indexing ([8a3d055](https://github.com/lambdasistemi/haskell-mts/commit/8a3d0556d0cbd21469f66569950d346ba6819742))
* add WithOrigin and RollbackPoint to Types ([7391914](https://github.com/lambdasistemi/haskell-mts/commit/73919142c8c7fee1dfbd80dd2175d6ae7a20cef5))
* anchor proofs to their root hash ([6b01145](https://github.com/lambdasistemi/haskell-mts/commit/6b011452055b6f0278808d93051b90d2f6f9e977)), closes [#63](https://github.com/lambdasistemi/haskell-mts/issues/63)
* export packHexKey, nibbleBytes, merkleProof from MPF.Hashes ([7c1f9a8](https://github.com/lambdasistemi/haskell-mts/commit/7c1f9a8623bfe01c3a3e8e0d7d3d5ecd97847eb1))
* expose csmt-test-vectors in nix flake ([3a9df26](https://github.com/lambdasistemi/haskell-mts/commit/3a9df26eac1878e3a7aa92f3c9dea4815d8a9aea))
* formalize swap-partition model in Lean 4 ([5edd966](https://github.com/lambdasistemi/haskell-mts/commit/5edd9660752e75134dbf4f2af290868289a51fae))
* implement completeness proofs with prefix inclusion ([f565576](https://github.com/lambdasistemi/haskell-mts/commit/f565576f4eac3fa79638cdeafea2edb653412479)), closes [#58](https://github.com/lambdasistemi/haskell-mts/issues/58)
* prove rollback correctness (zero sorry) ([effc260](https://github.com/lambdasistemi/haskell-mts/commit/effc26052eb38913c8a7f9592cb72ae6d9919f16))


### Bug Fixes

* adapt to rocksdb-kv-transactions snapshot API ([80e41c8](https://github.com/lambdasistemi/haskell-mts/commit/80e41c8be145576d9ddc259b9973d8e193361a4c))
* add missing prefix argument to inserting, deleting, buildInclusionProof, root ([4cd37b8](https://github.com/lambdasistemi/haskell-mts/commit/4cd37b80248b218e8f4c2fda8fc6a817e396a407))
* add pop to CSMT test vectors import ([f0e2d22](https://github.com/lambdasistemi/haskell-mts/commit/f0e2d220bbdd448a3ae2bbae068455c54e446eb5))
* address hlint warning in Aiken parser ([4cd13f8](https://github.com/lambdasistemi/haskell-mts/commit/4cd13f802ccafcedc1d3c0944fd20f26eca21564))
* apply hlint suggestion in namespace test ([253ca2e](https://github.com/lambdasistemi/haskell-mts/commit/253ca2e7f0736c1740303162480d0fc9a77a9f68))
* CI failures and hlint warnings ([c2a8ddb](https://github.com/lambdasistemi/haskell-mts/commit/c2a8ddb1d20ec90a3c67701a7f3c958b6e979217))
* eta reduce storeRollbackPoint per hlint ([5c66021](https://github.com/lambdasistemi/haskell-mts/commit/5c6602136c7d3fc96945b2c976e94580f028e2b5))
* exclude storage prefix from pslNeighborKeyPath in proofs ([ff5d814](https://github.com/lambdasistemi/haskell-mts/commit/ff5d814844af5412ffbfeeb045a007042d7eea9c)), closes [#87](https://github.com/lambdasistemi/haskell-mts/issues/87)
* expose transactional MTS constructors for composable operations ([2289bac](https://github.com/lambdasistemi/haskell-mts/commit/2289bacbf0c327183a2c9911b1eeede6b511e1e6)), closes [#66](https://github.com/lambdasistemi/haskell-mts/issues/66)
* generalize cf/op phantoms in transactional MTS constructors ([106356f](https://github.com/lambdasistemi/haskell-mts/commit/106356f1e2db5d76a893da5e20b6e58ab313b939))
* generate Leaf/Fork proof steps matching Aiken format ([a610227](https://github.com/lambdasistemi/haskell-mts/commit/a610227e01c730b98517ce8c4950f83cc5f04859))
* handle prefix queries with path compression in collectValues and generateProof ([a93c00f](https://github.com/lambdasistemi/haskell-mts/commit/a93c00ff8f16dc97353c12991439a89171226412))
* migrate mkdocs gh-deploy to mkdocs-deploy wrapper ([4a294e6](https://github.com/lambdasistemi/haskell-mts/commit/4a294e6169d5eab7674de85431c629e6bbd8e584)), closes [#89](https://github.com/lambdasistemi/haskell-mts/issues/89)
* replace partial head with pattern match, fix CI format check ([7d5e6a6](https://github.com/lambdasistemi/haskell-mts/commit/7d5e6a62b81d1729b43644ed7e17ed909e96f714))
* replace stale haskell-csmt references with haskell-mts ([29c04ba](https://github.com/lambdasistemi/haskell-mts/commit/29c04bacfb9f6e4f63151f9a5d1ccbe21e66835c)), closes [#57](https://github.com/lambdasistemi/haskell-mts/issues/57)
* revert to bare cachix commands (requires infra PR [#58](https://github.com/lambdasistemi/haskell-mts/issues/58)) ([c763fd9](https://github.com/lambdasistemi/haskell-mts/commit/c763fd953cd0b378f977584650f8ad1c6a77563f))
* sanitize nix store paths in asciinema cast files ([e46ef73](https://github.com/lambdasistemi/haskell-mts/commit/e46ef731076cf0884d5ed0f047d0dd74e622aea8))
* use cachix-action instead of bare cachix command ([2d8d91d](https://github.com/lambdasistemi/haskell-mts/commit/2d8d91d137900cca65401edba09580d5d88112b1))
* use cachix-action with cachix from extraPackages ([857532c](https://github.com/lambdasistemi/haskell-mts/commit/857532ca402a4b8274850e31afe77bf8f2398f79))
* use format-check in CI and add hlint job ([c9436d7](https://github.com/lambdasistemi/haskell-mts/commit/c9436d703f7664ba9d754a8eda499550b5c2d3f3))

## [0.4.0.0](https://github.com/lambdasistemi/haskell-mts/compare/v0.3.2...v0.4.0.0) (2026-03-02)

### Features

* rename package from `csmt` to `mts` (Merkle Tree Store)
* introduce shared `MerkleTreeStore` record with type families (`MTS.Interface`)
* add 12 shared QuickCheck properties (`MTS.Properties`)
* add MPF (Merkle Patricia Forest) 16-ary trie implementation
* MPF batch, chunked, and streaming insertion modes
* MPF inclusion proofs with Aiken-compatible proof steps
* CSMT and MPF both provide `MerkleTreeStore` constructors (`csmtMerkleTreeStore`, `mpfMerkleTreeStore`)
* add completeness proofs to shared MTS interface
* restructure cabal file into `mts` (shared), `mts:csmt`, `mts:mpf` sub-libraries

## [0.3.2](https://github.com/lambdasistemi/haskell-mts/compare/v0.3.1...v0.3.2) (2026-02-02)

### Bug Fixes

* correct artifact copy commands for bundler outputs ([140e2eb](https://github.com/lambdasistemi/haskell-mts/commit/140e2eb33c6845bf6c0f8009bed7e23b9aa22438))
* handle bundler output directories in release upload ([eb3632e](https://github.com/lambdasistemi/haskell-mts/commit/eb3632e7ed908e1282484a67fb08a90afbe373eb))

## [0.3.1](https://github.com/lambdasistemi/haskell-mts/compare/v0.3.0...v0.3.1) (2026-02-02)

### Bug Fixes

* add workflow_dispatch to release workflow ([5bacf67](https://github.com/lambdasistemi/haskell-mts/commit/5bacf670c5d59a89b1614e70c7d94016baeb6dcf))
* read version from manifest instead of version.txt ([5b3b415](https://github.com/lambdasistemi/haskell-mts/commit/5b3b4154f57d0a9286210c0af3b18740b1192af9))
