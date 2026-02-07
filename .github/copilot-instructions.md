# Copilot Instructions

## Build & Test

```shell
dotnet build                # Build entire solution
dotnet run --project test/StarTrek.Tests/StarTrek.Tests.fsproj   # Run all tests (Expecto)
dotnet run --project test/StarTrek.Tests/StarTrek.Tests.fsproj -- --filter "test name"  # Run a single test
dotnet run --project src/StarTrek/StarTrek.fsproj                # Run the game
```

Tests use [Expecto](https://github.com/haf/expecto) (not xUnit/NUnit). Tests are run as an executable, not via `dotnet test`. Use `testCase` and `testList` to define tests, and `Expect.*` for assertions.

## Architecture

This is an F# port of the classic 1971 Star Trek text game, targeting .NET 9.

**StarTrek.Core** (`src/StarTrek.Core/`) — Pure domain logic, no I/O:
- `GameTypes.fs` — All domain types: `GameState`, `Sector` (discriminated union), `Position`, `DeviceStatus`, `QuadrantInfo`
- `Galaxy.fs` — Galaxy mechanics: coordinate validation, course vectors, sector encoding, map creation

**StarTrek** (`src/StarTrek/`) — Console application, depends on StarTrek.Core:
- `Display.fs` — Rendering (short-range scan display)
- `Commands.fs` — Command handlers (commands 0–7, most are stub implementations)
- `Program.fs` — Entry point, game loop, command dispatch

**StarTrek.Tests** (`test/StarTrek.Tests/`) — Tests for StarTrek.Core only.

## Conventions

- **F# file order matters.** Files are compiled in the order listed in `.fsproj` `<Compile>` items. A file can only reference types/modules from files listed above it.
- **Immutable state.** `GameState` is a record passed through and returned from command handlers. Commands have the signature `GameState -> GameState`.
- **Discriminated unions for sector contents.** The `Sector` type uses DU cases (`Empty | Star | Klingon of shieldPower: float | Starbase | Enterprise`).
- **Galaxy uses an 8×8 grid.** Both quadrant and sector maps are 8×8 2D arrays. Positions are 0-indexed internally.
- **Quadrant encoding.** Quadrant contents are encoded as a 3-digit int (hundreds=Klingons, tens=Starbases, units=Stars) via `encodeQuadrant`/`decodeQuadrant`.
- **Course system.** Courses are floats from 1.0 to <9.0 mapped to 8 compass directions. Fractional courses interpolate between adjacent directions.
