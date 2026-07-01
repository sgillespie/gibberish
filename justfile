# Get current stystem name (e.g. x86_64-linux)

system := `nix eval --impure --raw --expr builtins.currentSystem`

# Show available recipes
default:
    @just --list --unsorted

## Building and running

# Build executables
build:
    nix build \
      ".#gibberish:exe:gibber" \
      ".#gibberish:exe:gibber-gen-trigraph"

# Run main executable (`just run -- --help`)
run *args:
    nix run ".#gibberish:exe:gibber" -- {{ args }}

# Build release artifacts
dist:
    nix build \
      ".#x86_64-linux-static-dist" \
      ".#x86_64-windows-dist"

## Checks

# Run the static analyzers (statix, deadnix, hlint)
lint:
    nix build \
      ".#checks.{{ system }}.statix" \
      ".#checks.{{ system }}.deadnix" \
      ".#checks.{{ system }}.hlint"

# Check formatting without writing changes
fmt-check:
    nix build ".#checks.{{ system }}.treefmt"

# Run the test suites
test:
    nix build \
      ".#checks.{{ system }}.gibberish:test:spec" \
      ".#checks.{{ system }}.gibberish:test:golden"

# Run the benchmarks
bench:
    nix run ".#gibberish:bench:main"

# Run basic checks
check-light:
    nix build \
      ".#checks.{{ system }}.statix" \
      ".#checks.{{ system }}.deadnix" \
      ".#checks.{{ system }}.hlint" \
      ".#checks.{{ system }}.treefmt" \
      ".#checks.{{ system }}.gibberish:test:spec" \
      ".#checks.{{ system }}.gibberish:test:golden"

# Run the full flake check (every check, all systems)
check-full:
    nix flake check

## Other development tools

# Format the source tree in place
fmt:
    nix fmt
