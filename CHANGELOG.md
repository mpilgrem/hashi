# hashi

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
* This `CHANGELOG.md`.
* Package description files `package.yaml` and `hashi.cabal`.
* Stack project-level configuration file `stack.yaml`
* Different `README.md`.
* `22x28veryhard` example puzzle.
* Different `.gitignore`.
* Haddock and in-code documentation.
* `Hashi.Read.mapText` helper function.
* `Hashi.Types.isIsland`, to reduce code duplication.

### Changed
* `LICENSE` renamed `LICENSE.md`.
* `hashiheader.eps` bounding box reoriented from portrait to landscape.
* Code in `solve.hs` moved to `app/Main.hs`.
* Code in `Hashi.hs` distributed between `src/Hashi.hs`, `src/Hashi/Types.hs`,
  `src/Hashi/Read.hs`, `src/Hashi/State.hs` and `src/Hashi/Show.hs`; and
  reformatted.
* Use of `String` replaced by `Data.Text.Text` in `Hashi.Read` and `Hashi.Show`,
  including modification of `Hashi.Read.readProblemList`.
* `import Heredoc` replaced by a dependency on the `heredoc` package.
* `Hashi.Types.Bridges` renamed `Hashi.Types.BridgeSet`, with consequent
  changes.
* `Hashi.Read.readFile` modified to use `Data.Char.digitToInt`.
* `Hashi.Show.bridges` and `Hashi.Show.circle` derived from
  `Hashi.Show.showStateEPS`, with use of `Data.Text.unwords`.
* `Hashi.connectedComponents` modified to avoid partial functions by using
  `NonEmpty (Set Index)`.
* `solve` renamed `solveProblem`, `solve'` renamed `solveState` and
  `solve''` renamed `solveForIsland`.
* `Hashi.solveForIsland` modified to use `Foldable` instance of `Data.Set` in
  `unfinished`.
