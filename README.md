# Signals - Durable Reactive Effects for Nim

## Introduction

A *signal* is a variable that remembers who reads it and can notify those readers when its value changes. This library brings that idea to Nim with very little boilerplate. Write ordinary Nim objects, call `reactive(MyType)`, and keep using the fields as usual; each one transforms into signal.

Dependent values stay consistent, side effects run only when results really change, and the runtime handles dependency tracking, batching, undo, redo, time-travel, and optional JSON autosave. If you need more control you can still work directly with `Signal`, `computed`, or `effect`, but for most code the automatic wrapper is all you need.

All signals code is dedicated to the public domain or CC0 (your choice).

To use signals, just copy signals.nim into your project. It has no dependencies.

## Main features

* **Universal**: `reactive(MyType)` creates a transparent wrapper for abistrarily complex objects (which can contain sub-objects, `seq[T]` and `Table[K, V]`)
* **First class signals**: explicit `Signal[T]` values with `+=`, `*=` and equality helpers
* ***Computed* and *writable computed***: signals that cache results and can write back to sources
* `effect`, `watch`, and `memo` utilities for side effects, change observers, and cached functions
* `transaction` blocks and scheduler hooks with immediate, queued, or custom frame execution
* `undo`, `redo`, and `snapshot` support with an automatic history stack for time travel style state management
* persistent store and autosave that serializes any marked signal to JSON once per flush; signals can be created as stored signals, or registered later
* `ReactiveSeq` and `ReactiveTable` wrappers that follow both structural edits (cardinality, keys) and per element value edits
* `peek` and `effectOnce` helpers to read without tracking or to run code exactly once
* cleanup hooks and context disposal that stop reactions and write a final snapshot when the context ends
* context encapsulation to allow several reactive engines to co-exist, even with different scheduler models

## Documentation

Take a look at the [quick-start examples](/examples.nim) or read the **[full API reference here](https://turbo.github.io/signals.nim/signals.html)**.

## Development

* Runs `bench.nim` to get an idea of the overhead signals have

Run tests & generate docs:

```bash
./ci.sh
```