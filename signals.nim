##[
  # Signals - Durable Reactive Effects for Nim

  This module implements *signals* and a lightweight runtime that keeps
  values, collections, and whole object graphs in sync automatically.
  The key ideas and how they fit together are outlined below. Each concept
  links to the concrete API symbol that realises it.

  Signals supports complex object hierarchies effortlessly:

  ```nim
  type
    Vec2 = object
      x*, y*: float
    
    SpeciesX* = enum spGoblinX, spOgreX, spDragonX
    ElementX* = enum elNoneX, elFireX, elIceX, elLightningX

    ResistancesX* = Table[ElementX, float]
    InventoryX* = Table[string, int]

    CoreStatsX* = object
      hp*, mp*: int = 100
      pos*: Vec2 = Vec2(x: 0, y: 0)    
      resist*: ResistancesX

    MonsterX* = object
      id*: int
      name*: string = "unnamed-X"
      tags*: seq[string] = @[]
      inv*: InventoryX
      stats*: CoreStatsX

      case kind*: SpeciesX
      of spGoblinX:
        speed*: float            
      of spOgreX:
        clubDmg*: int              
      of spDragonX:
        element*: ElementX         
        breath*: array[3, float]  

  reactive(Vec2)
  reactive(CoreStatsX)    
  reactive(MonsterX)
  ```

  And handles signal propagation from easy:

  ```nim
  let ctx = newReactiveCtx()

  var hp    = ctx.signal 100
  var maxHp = ctx.signal 150

  let pct = ctx.computed () => hp.val / maxHp.val

  ctx.effect proc =
    echo "health bar width ", pct.val * 100, "%"

  hp -= 10
  hp.set 0

  # output:
  # health bar width 66.66666666666666%
  # health bar width 60.0%
  # health bar width 0.0%
  ```

  to hard:
  
  ```nim
  suite "MonsterX wrapper":

    proc baseStats(hp = 100, mp = 0): CoreStatsX =
      CoreStatsX(hp: hp, mp: mp,
                pos: Vec2(x: 0, y: 0),
                resist: initTable[ElementX,float]())

    proc newGoblin(id = 1): MonsterX =
      MonsterX(id: id,
              name: "Gob" & $id,          # ← fixed
              tags: @["evil"],
              inv: initTable[string,int](),
              stats: baseStats(),
              kind: spGoblinX,
              speed: 2.0)

    proc newDragon(id = 1): MonsterX =
      MonsterX(id: id,
              name: "Drg" & $id,          # ← fixed
              tags: @["boss"],
              inv: initTable[string,int](),
              stats: baseStats(300),
              kind: spDragonX,
              element: elFireX,
              breath: [0.3, 0.3, 0.4])

    test "Shared primitive propagates":
      let C = newReactiveCtx()
      var m = C.toReactive(newGoblin())
      var hits = 0
      C.effect proc =
        discard m.stats.hp.val
        inc hits

      m.stats.hp += 25
      check m.stats.hp.val == 125
      check hits == 2           # mount + after write

    test "Table alias (ResistancesX) is reactive":
      let C = newReactiveCtx()
      var m = C.toReactive(newGoblin())
      var log: seq[float]

      C.effect proc =
        discard m.stats.resist[elIceX].val
        log.add m.stats.resist[elIceX].val

      m.stats.resist[elIceX] = 0.5       # structural put → new signal
      check log == @[0.0, 0.5]

    test "Seq alias (tags) is reactive and undo-able":
      let C = newReactiveCtx()
      var m = C.toReactive(newGoblin())
      m.tags.push "fast"
      undo C
      check m.tags.len == 1              # only original "evil"

    test "Inventory (Table alias) structural edits":
      let C = newReactiveCtx()
      var m = C.toReactive(newGoblin())
      var revs: seq[int]

      C.effect proc =
        discard rev(m.inv)               # structural rev counter
        revs.add rev(m.inv)

      m.inv.put("gold", 100)
      m.inv.put("potion", 2)
      check m.inv.len == 2
      check revs.len == 3                # mount + 2 puts

    test "Variant transition keeps shared deps":
      let C = newReactiveCtx()
      var m = C.toReactive(newGoblin())
      var hpHits = 0
      C.effect proc =
        discard m.stats.hp.val           # shared across variants
        inc hpHits

      check m.stats.hp.val == 100

      # transition Goblin → Dragon
      let snapTags = m.tags.toPlain()
      check snapTags == @["evil"]

      let nd = newDragon()
      check nd.stats.hp == 300

      m.set nd

      check m.stats.hp.val == 300

      m.stats.hp -= 1

      check m.stats.hp.val == 299

      check m.kind == spDragonX
      check m.element == elFireX
      check hpHits == 3                  # mount + once after transition
      
      let newTags = m.tags.toPlain()
      check newTags == @["boss"]

    test "Undo across variant transition":
      let C = newReactiveCtx()
      var m = C.toReactive(newGoblin())
      m.set newDragon()
      undo C
      check m.kind == spGoblinX
      check m.speed.val == 2.0

    test "toPlain snapshot after deep edits":
      let C = newReactiveCtx()
      var m = C.toReactive(newGoblin())
      let snap = m.toPlain()
      m.stats.pos.x.set 10
      m.inv.put("dagger", 1)
      check snap.stats.pos.x == 0        # untouched snapshot
      check "dagger" notin snap.inv
  ```

  ***Signals***

  * A `Signal`_ stores one value of type `T` and remembers every observer
    that reads it through `val`_ (or wrapper access).
  * Writing with `set`_ records an undo entry, notifies observers via the
    active scheduler, and marks the context dirty for autosave.

  ***Derived values***

  * `computed`_ builds a new signal from other signals and caches the
    result until any dependency changes.
  * `memo`_ offers the same caching but exposes a plain callable instead
    of a signal.
  * `effect`_ and its cleanup variant run side effects when dependencies
    change, while `effectOnce`_ runs exactly one time.

  ***Batching and atomic updates***

  * `transaction`_ temporarily increments `ctx.batching`. Every signal
    write inside the block queues notifications and coalesces history
    so observers and autosave fire only once after the outermost
    transaction ends.

  ***Undo and redo***

  * Each effective change pushes a `HistoryEntry`_ onto the undo stack.
  * `undo`_ pops entries, calls their undo callback, and moves them to the
    redo stack; `redo`_ moves them back.
  * `snapshot`_ captures the current depth and `travel`_ jumps to any
    snapshot by replaying the needed undo or redo steps.
  * `undoDepth`_ and `redoDepth`_ report the current stack sizes.

  ***Persistence***

  * Any signal can be registered with `store`_ or `registerStore`_.
  * `saveState`_ serialises all store signals into a flat JSON object;
    `loadState`_ restores them.
  * `enableAutosave`_ / `disableAutosave`_ write one snapshot per flush
    automatically.

  ***Containers***

  * `ReactiveSeq`_ wraps a `seq`. Index access returns element signals;
    structural mutators update `len`_ and `rev`_.
  * `ReactiveTable`_ wraps a `Table`. Key access returns value signals and
    structural edits update `len`_ and `rev`_ for the table.

  ***Object wrappers***

  * The `reactive`_ macro generates a parallel `XReactive` type whose
    fields are signals or nested reactive wrappers.
  * `toReactive`_ converts a plain object or collection into its wrapper;
    `toPlain`_ returns an immutable deep copy.

  ***Schedulers***

  * The default immediate scheduler runs reactions at once.
  * `useQueuedScheduler`_ queues reactions until `flushQueued`_ is called.
  * `useFrameScheduler`_ batches reactions per render frame.

  ***Context lifecycle***

  * All reactive entities belong to a `ReactiveCtx`_.
  * Create one with `newReactiveCtx`_, dispose it with `dispose`_.
  * After disposal signal writes still change the stored value but no
    reactions, history entries, or autosave snapshots are produced
]##

import std/[
  sugar,
  hashes,
  strutils,
  tables,
  sets,
  json,
  sequtils,
  macros,
  algorithm,
  bitops,
  complex
]

type
  StoreEntry = object
    key: string
    getter: proc(): JsonNode
    setter: proc(n: JsonNode)

  HistoryEntry* = tuple[undo, redo: proc()] ## \
    ## Internal record used by the undo/redo system.
    ##
    ## A `HistoryEntry` stores two zero-argument procs:
    ##
    ## * `undo`  – restores the previous state.
    ## * `redo`  – reapplies the change that `undo` reverted.
    ##
    ## The reactive engine pushes one `HistoryEntry` onto the context’s
    ## undo stack for every *effective* state change that occurs outside of
    ## an explicit call to `undo`, `redo`, or `dispose`.
    ##
    ## ***How it fits in***
    ##
    ## * `undo(ctx)` pops an entry, calls `undo`, and pushes the same entry
    ##   onto the redo stack.
    ## * `redo(ctx)` pops from the redo stack, calls `redo`, and pushes the
    ##   entry back onto the undo stack.
    ##
    ## ***When to interact with it***
    ##
    ## End-users do not create `HistoryEntry` values directly. They appear
    ## only if you are extending the library with new reactive container
    ## types and need to record structural or batched changes. In that case
    ## create an entry and push it just like the built-in seq and table
    ## wrappers do.
    ##
    ## ***Pitfalls***
    ##
    ## * The procs must **not** capture values by reference that can go out of
    ##   scope before the entry is executed.
    ## * Do not call signal setters inside `undo` or `redo` that would push
    ##   *new* history entries; set the raw field values instead or wrap the
    ##   call inside `ctx.isUndoing` / `ctx.isRedoing`.


  ReactiveCtx* = ref object ## \
    ## A runtime container that owns and orchestrates every reactive entity in
    ## one isolated graph.
    ##
    ## ***Purpose***
    ##
    ## Each `ReactiveCtx` keeps its own signals, effects, scheduler, undo/redo
    ## history, and autosave target. Creating more than one context lets you
    ## run independent reactive sub-systems side by side.
    ##
    ## ***Key responsibilities***
    ##
    ## - Stores all signals created with `signal(ctx, ...)` and wrappers built
    ##   with `ctx.toReactive(...)`.
    ## - Queues and flushes reactions according to the active scheduler
    ##   (immediate, queued, or frame).
    ## - Batches changes while inside `transaction(ctx): ...`.
    ## - Records `HistoryEntry` objects so that `undo` and `redo` can travel
    ##   through time.
    ## - Handles autosave by calling `saveInto` once per flush when dirty.
    ## - Shuts everything down and runs pending cleanups when `dispose(ctx)`
    ##   is called.
    ##
    ## ***Typical usage***
    ##
    ## ```nim
    ## let ctx = newReactiveCtx()
    ## var hp  = signal(ctx, 100)
    ## effect(ctx, () => echo hp.val)
    ## hp -= 10            # effect prints 90
    ## ```
    ##
    ## Most programs keep one long-lived context. Tests, modal dialogs, or
    ## background jobs often create short-lived contexts so they can dispose
    ## them cleanly.
    ##
    ## ***When to create multiple contexts***
    ##
    ## - You need separate undo/redo stacks.
    ## - Different parts of the app should flush on different schedulers.
    ## - Large data sets need to be unloaded and all reactive memory freed at
    ##   once via `dispose`.
    ##
    ## ***Pitfalls and rules***
    ##
    ## - Never move a signal between contexts; create it in the context that
    ##   will own it.
    ## - After `dispose(ctx)` reactions stop, but raw `Signal` values can still
    ##   mutate silently; avoid further writes.
    ## - Make sure every `transaction` block leaves `ctx.batching` back at 0
    ##   even on exceptions.
    ## - Public helpers (`signal`, `computed`, `effect`, ...) expect a valid,
    ##   live context; passing a disposed one is a logic error.

    batching: int      # nesting depth of `transaction`
    isUndoing: bool
    isRedoing: bool
    isDisposed: bool
    queue: seq[proc ()]
    queueHashes: HashSet[Hash]
    scheduler: proc (cb: proc())
    currentObserver: proc ()
    undoStack: seq[HistoryEntry]
    redoStack: seq[HistoryEntry]
    store: seq[StoreEntry]      # all persisted signals
    dirty: bool                 # true after any real write
    autoTarget: JsonNode        # points to caller-owned node
    autoEnabled: bool
    framePending: bool

  Cleanup* = proc () ## \
    ## Zero-argument callback that releases a resource previously created by an
    ## `effect`. A `Cleanup` is registered through the `AddCleanup` callback
    ## passed to `effect` (see that doc comment for usage details) and is
    ## executed right before the effect runs again or when the owning
    ## `ReactiveCtx` is disposed.
    ##
    ## Typical tasks performed in a `Cleanup`:
    ## * clearing timers
    ## * unbinding event listeners
    ## * closing files or sockets
    ##
    ## Keep the body idempotent and side-effect free beyond the intended
    ## teardown; it may be called even when signal values did not change if the
    ## effect re-evaluates for other reasons.

  AddCleanup* = proc (c: Cleanup) ## \
    ## Type alias for the *onCleanup* callback that can be passed to `effect`.
    ##
    ## A cleanup is a `proc ()` that will be executed **right before** the
    ## enclosing effect runs again or when its `ReactiveCtx` is disposed. Use
    ## it to tear down timers, event handlers, or other external resources that
    ## the effect created.
    ##
    ## ***Workflow***
    ##
    ## ```nim
    ## ctx.effect proc (onCleanup: AddCleanup) =
    ##   let timer = setTimer(1000, step)    # acquire resource
    ##   onCleanup(() => clearTimer(timer)   # release it next run / dispose
    ## )
    ## ```
    ##
    ## * The effect runs once, establishing its dependencies.
    ## * `onCleanup` registers any number of callbacks.
    ## * Before the effect re-executes, each callback is invoked in the **order
    ##   they were added**.
    ##
    ## ***When to use***
    ##
    ## * Setting up and tearing down external listeners (DOM, sockets, timers).
    ## * Releasing memory or file handles that live only for the lifetime of
    ##   the effect.
    ##
    ## ***Pitfalls***
    ##
    ## * **Do not** call `onCleanup` conditionally inside loops; register once
    ##   per resource or you may leak callbacks.
    ## * A cleanup runs even if the signal values did not actually change, as
    ##   long as the effect re-evaluates; design idempotent cleanup code.
    ## * Calling `dispose(ctx)` triggers cleanups exactly once; further writes
    ##   to the same signals will no longer schedule cleanups.


  Signal*[T] = ref object ## \
    ## Reactive value container that stores one `T` and notifies its
    ## subscribers when the value changes.
    ##
    ## ***Purpose***
    ##
    ## A `Signal` is the basic building block of the library. All primitive
    ## fields wrapped by `reactive(MyType)` become signals. Effects and
    ## computed signals read them to establish dependencies, and any call to
    ## `set` triggers the update chain.
    ##
    ## ***Core behavior***
    ##
    ## - Read with `.val` or the `val(signal)` template.
    ## - Write with `set`, `+=`, `-=`, or `*=`.
    ## - Equality helpers let you compare a signal to a plain value in
    ##   assertions.
    ## - If the new value equals the current one no history entry is recorded
    ##   and no reactions fire.
    ## - Writes inside `undo`, `redo`, or `dispose` bypass history push but
    ##   still notify dependants.
    ##
    ## ***Typical usage***
    ##
    ## ```nim
    ## var hp = signal(ctx, 100)
    ## effect(ctx, () => echo "HP:", hp.val)
    ## hp -= 10            # effect prints HP: 90
    ## ```
    ##
    ## ***When to declare a Signal yourself***
    ##
    ## - Stand-alone values not part of a reactive object wrapper.
    ## - Global settings that require independent undo history.
    ## - Quick experiments or tests where a full wrapper type is overkill.
    ##
    ## ***Pitfalls and rules***
    ##
    ## - Never create a signal without passing the owning `ReactiveCtx`.
    ## - Avoid writing the same value repeatedly; it adds no benefit but still
    ##   triggers change detection.
    ## - Signals are reference types; copying a `Signal[T]` variable copies the
    ##   reference, not the value.
    ## - After `dispose(ctx)` writing a signal mutates the stored value but no
    ##   reactions will run.
    value: T
    subs: seq[proc ()]
    ctx: ReactiveCtx
    setter: proc(v: T)

  ReactiveSeq*[T] = ref object ## \
    ## A signal-aware wrapper around a Nim `seq[T]`. Every element is stored in
    ## its own `Signal[T]`, and structural edits (push, insert, remove, clear)
    ## are tracked so effects can depend on the length or on the existence of
    ## elements.
    ##
    ## ***Purpose***
    ##
    ## Use `ReactiveSeq` when you need a growable list whose *structure and
    ## element values* both drive reactive updates. Structural changes push
    ## history items, participate in transactions, and trigger autosave just
    ## like normal signal writes.
    ##
    ## ***Core behavior***
    ##
    ## - Index access `rs[i]` returns the **element signal**; read or write it
    ##   with `.val` or `set`.
    ## - `len(rs)` and `rev(rs)` are signals that change when the length or
    ##   structure changes.
    ## - Structural mutators: `push`, `pop`, `insert`, `removeIdx`, `clear`.
    ## - Value writes on an element signal do **not** change `len` but do push
    ##   their own undo record.
    ## - Inside a `transaction` multiple structural edits coalesce into one
    ##   `HistoryEntry`.
    ##
    ## ***Typical usage***
    ##
    ## ```nim
    ## var rs = ctx.toReactive(@["a", "b"])   # wrap an existing seq
    ##
    ## effect(ctx, () => echo "count:", len(rs))
    ##
    ## rs.push "c"        # effect prints count:3
    ## rs[0].set "A"      # structural len unchanged
    ## undo(ctx)          # removes "c"
    ## ```
    ##
    ## ***When to choose ReactiveSeq***
    ##
    ## - Lists in UI state (inventory items, chat messages, menu entries).
    ## - Any ordered collection where individual items update independently.
    ## - You need fine-grained undo/redo that distinguishes structural vs
    ##   value edits.
    ##
    ## ***Pitfalls and rules***
    ##
    ## - Keep references to element signals (`let s = rs[0]`) only while the
    ##   element exists; deleting the index orphanes the signal.
    ## - Writing `rs[i] = v` when `i` is out of range is a logic error.
    ## - Large loops that write many elements individually should be wrapped in
    ##   `transaction(ctx)` to avoid hundreds of history entries.
    ## - `iterator items(rs)` yields plain values, so no dependency is tracked;
    ##   use explicit index reads if you need reactivity inside loops.
    data: seq[Signal[T]]   # each element is its own signal
    ctx: ReactiveCtx
    lenS: Signal[int]      # dependency anchor: length
    revS: Signal[int]
    pendingOld: seq[Signal[T]]
    pendingActive: bool

  ReactiveTable*[K, V] = ref object ## \
    ## Reactive wrapper around a `Table[K, V]`. Each key maps to its own
    ## `Signal[V]`, and structural edits are tracked so effects can depend on
    ## the entry count or on changes to the key set.
    ##
    ## ***Purpose***
    ##
    ## Use `ReactiveTable` when you need a dictionary whose keys may appear or
    ## disappear at runtime and whose values must update dependent code
    ## automatically. Both structural operations and per value writes
    ## integrate with undo, redo, transactions, and autosave.
    ##
    ## ***Core behavior***
    ##
    ## - `rt[key]` returns the value signal for that key, creating a new signal
    ##   with `default(V)` if the key is missing.
    ## - `len(rt)` and `rev(rt)` are signals that change when the size or the
    ##   structure changes.
    ## - Structural mutators: `put`, `delKey`, `clear`.
    ## - Writing through a value signal (`rt["hp"].set 10`) leaves `len`
    ##   unchanged but records its own history entry.
    ## - Inside `transaction(ctx)` multiple structural edits collapse into one
    ##   undo record.
    ##
    ## ***Typical usage***
    ##
    ## ```nim
    ## var stats = ctx.toReactive(initTable[string,int]())
    ##
    ## effect(ctx, () => echo "entries:", len(stats))
    ##
    ## stats.put("hp", 10)     # effect prints entries:1
    ## stats["hp"] += 5        # structural length unchanged
    ## undo(ctx)               # removes key "hp"
    ## ```
    ##
    ## ***When to choose ReactiveTable***
    ##
    ## - Keyed collections in UI or game state, like entity maps or settings.
    ## - You need both value level and structural level undo and redo.
    ## - Autosave should persist a whole dictionary without manual loops.
    ##
    ## ***Pitfalls and rules***
    ##
    ## - Keep references to value signals only while the key exists; deleting
    ##   the key leaves the old signal orphaned.
    ## - Accessing `rt[key]` for a missing key creates it; use `hasKey` on a
    ##   plain table if you only want to test for existence.
    ## - Large batches of `put` or `delKey` should be wrapped in
    ##   `transaction(ctx)` to avoid many history entries.
    ## - Remember to read `len(rt)` or `rev(rt)` inside an effect if that
    ##   effect should rerun on structural changes.
    data: Table[K, Signal[V]]   # value signal per key
    ctx: ReactiveCtx
    lenS: Signal[int]           # entry count
    revS: Signal[int]           # any structural change
    pendingOld: Table[K, Signal[V]]
    pendingActive: bool

  JsonNodeReactive* = ref object
    ## One wrapper node per JsonNode in the original tree.
    ctx*: ReactiveCtx

    # ── meta information (always present) ───────────────────────────────────
    kind*  : Signal[JsonNodeKind]

    # ── structured children (only one of these is non-nil) ─────────────────
    fields*: ReactiveTable[string, JsonNodeReactive]   ## for JObject
    items* : ReactiveSeq[JsonNodeReactive]             ## for JArray

    # ── scalar payload (for JString / JInt / JFloat / JBool) ───────────────
    strVal*  : Signal[string]
    intVal*  : Signal[int]
    floatVal*: Signal[float]
    boolVal* : Signal[bool]

proc markDirty(ctx: ReactiveCtx)

proc newReactiveCtx*(): ReactiveCtx =
  ## Constructs and returns a fresh `ReactiveCtx` with the default immediate
  ## scheduler and empty history, queues, and store list.
  ##
  ## ***When to call***
  ##
  ## - At application startup to create the primary reactive graph.
  ## - Inside tests or short-lived components to obtain an isolated context
  ##   that can be disposed without affecting the rest of the program.
  ##
  ## The new context is clean: `undoDepth`, `redoDepth`, and `queue.len`
  ## are all zero, `isDisposed` is false, and autosave is disabled.

  new result
  result.scheduler = proc(cb: proc()) = cb()
  result.queue = @[]
  result.queueHashes = initHashSet[Hash]()
  result.store = @[]
  result.dirty = false
  result.autoTarget = nil
  result.autoEnabled = false
  result.framePending = false

proc signal*[T](ctx: ReactiveCtx, v: T): Signal[T] =
  ## Allocates a new `Signal[T]` owned by `ctx` and initialised to `v`.
  ##
  ## ***Why you need it***
  ##
  ## - Stand-alone reactive values that are not part of a `reactive(...)`
  ##   object wrapper.
  ## - Global settings or counters that several wrappers or effects share.
  ## - Quick prototypes or tests where defining a wrapper type is overkill.
  ##
  ## - Can be read (`sig.val`) inside `effect`, `computed`, or `memo` to
  ##   establish dependencies that rerun those constructs when the signal
  ##   changes.
  ## - Can be registered for persistence with `store` or `registerStore`;
  ##   autosave and `saveState` will then serialise its value.
  ## - All writes made through `set` integrate with undo/redo, transactions,
  ##   autosave, and the active scheduler as described for `set`.
  ##
  ## ***Typical workflow***
  ##
  ## ```nim
  ## let ctx = newReactiveCtx()
  ## var score = signal(ctx, 0)
  ## effect(ctx, () => echo "score:", score.val)
  ## score += 10          # effect prints score: 10
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - A `Signal` is a reference object; copying the variable copies only the
  ##   reference, not the value.
  ## - Signals must belong to exactly one context; never share the same
  ##   signal instance across contexts.
  ## - After `dispose(ctx)` creating new signals with that context or writing
  ##   existing ones is a logic error that yields no reactive behaviour.

  Signal[T](value: v, ctx: ctx)

proc decode[T](n: JsonNode): T =
  when T is int:      result = n.getInt
  elif T is float:    result = n.getFloat
  elif T is string:   result = n.getStr
  elif T is bool:     result = n.getBool
  else:
    raise newException(ValueError, "Unsupported type for store signal")

proc makeStoreEntry[T](key: string, s: Signal[T]): StoreEntry =
  StoreEntry(
    key: key,
    getter: () => %(s.val),
    setter: (n: JsonNode) => s.set decode[T](n)
  )

proc store*[T](ctx: ReactiveCtx, key: string, init: T): Signal[T] =
  ## Creates a new signal initialised to `init`, registers it under `key`
  ## for persistence, and returns the signal.  Future calls to `saveState`
  ## or active autosave snapshots will serialize the signal’s value using
  ## the same key in the JSON object.
  ##
  ## Typical use:
  ##
  ## ```nim
  ## var gold = store(ctx, "gold", 100)
  ## enableAutosave(ctx, saveNode)
  ## gold += 50          # next flush writes {"gold":150}
  ## ```

  let s = signal(ctx, init)
  ctx.store.add makeStoreEntry(key, s)
  s

proc registerStore*[T](ctx: ReactiveCtx, key: string, s: Signal[T]) =
  ## Adds `s` to the context's store list under `key` so that future calls to
  ## `saveState` or active autosave snapshots include its value. Can be
  ## called at any time, even after autosave was already enabled.

  ctx.store.add makeStoreEntry(key, s)

proc saveInto(ctx: ReactiveCtx, dest: JsonNode) =
  ## Overwrite `dest` with a flat object of all store entries.
  ## `dest` **must not be nil**.
  if dest.isNil: return                 ## nothing to mutate
  ## clear existing keys
  for k in toSeq(dest.keys): dest.delete k
  ## fill with fresh snapshot
  for e in ctx.store: dest[e.key] = e.getter()
  ctx.dirty = false

proc saveState*(ctx: ReactiveCtx, dest: var JsonNode) =
  ## Writes a snapshot of all store-registered signals in `ctx` into `dest`.
  ##
  ## ***Rules***
  ## - Allocates a new JSON object when `dest` is `nil`.
  ## - Overwrites existing keys that match store entries; leaves others
  ##   untouched.
  ## - Does not clear history or fire reactions.
  ##
  ## Use before closing a document or for manual export; for continuous
  ## persistence prefer `enableAutosave`.

  if ctx.isDisposed: return
  if dest.isNil: dest = newJObject()
  saveInto(ctx, dest)

proc loadState*(ctx: ReactiveCtx, src: JsonNode) =
  ## Replaces the values of all store-registered signals in `ctx` with the
  ## contents of `src`. Keys that are missing in `src` leave their signals
  ## unchanged. Signals not registered with `store` or `registerStore` are
  ## unaffected.
  ##
  ## ***Typical workflow***
  ##
  ## ```nim
  ## var snapshot: JsonNode
  ## saveState(ctx, snapshot)   # later...
  ## loadState(ctx, snapshot)   # restore previous snapshot
  ## ```
  ##
  ## ***Effects on history and reactions***
  ##
  ## - Each `setter` call triggered by `loadState` pushes its own undo record
  ##   unless the context is inside `isUndoing`, `isRedoing`, or `isDisposed`.
  ## - Reactions fire normally after the load; wrap the call in
  ##   `transaction(ctx)` if you want one coalesced update.
  ##
  ## ***Pitfalls***
  ##
  ## - `src` must not be `nil`; pass an empty object to clear all stored
  ##   keys.
  ## - Data types are not validated beyond basic conversion; loading a float
  ##   into an int signal raises a value error.
  ## - The redo stack is cleared after the first new write; if you load a
  ##   snapshot and then set a signal manually the forward history is lost.

  if ctx.isDisposed: return
  for e in ctx.store:
    if src != nil and src.hasKey(e.key):
      e.setter(src[e.key])

proc enableAutosave*(ctx: ReactiveCtx, target: var JsonNode) =
  ## Activates automatic persistence for all store-registered signals in
  ## `ctx`. The current values are written into `target` immediately and once
  ## per flush whenever any store signal changes.
  ##
  ## ***How it works***
  ##
  ## - If `target` is `nil` a new empty JSON object is created.
  ## - Every call to `flushQueued(ctx)` (or an automatic flush by the
  ##   scheduler) serialises the latest store snapshot into the same JSON
  ##   node. Existing keys are overwritten, other keys in `target` remain.
  ## - On `dispose(ctx)` a final snapshot is written.
  ##
  ## ***Typical usage***
  ##
  ## ```nim
  ## var doc: JsonNode
  ## enableAutosave(ctx, doc)      # initial snapshot
  ## gold += 10                    # mutate a store signal
  ## flushQueued ctx               # doc updated
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - Keep `target` alive as long as autosave is enabled; if you reassign
  ##   the variable the library keeps the old reference.
  ## - Call `disableAutosave(ctx)` before switching to a new node to avoid
  ##   unintentionally writing into both.
  ## - Immediate scheduler writes right away; queued or frame schedulers
  ##   require an explicit flush or a frame tick.

  if target.isNil:
    target = newJObject()                 # allocate buffer

  ctx.autoTarget = target
  ctx.autoEnabled = true
  ctx.dirty = true
  saveInto(ctx, target)                   # initial snapshot

proc disableAutosave*(ctx: ReactiveCtx) =
  ## Turns off automatic persistence for the context. No further snapshots
  ## are written to the JSON node that was previously passed to
  ## `enableAutosave`.
  ##
  ## ***Effect***
  ##
  ## - Clears the autosave flag and the stored node reference.
  ## - Does not perform a final save; call `flushQueued(ctx)` first if you
  ##   need the latest state persisted.
  ##
  ## ***Pitfalls***
  ##
  ## - After disabling, edits to store signals will not reach the old JSON
  ##   node unless autosave is enabled again or `saveState` is called
  ##   manually.

  ctx.autoEnabled = false
  ctx.autoTarget = nil

proc registerDep(s: Signal) =
  ## Safely record the current observer on `s`, if any.
  if s.isNil:                     # ← new: tolerate transient nil slots
    return

  let c = s.ctx
  if c.currentObserver == nil: return
  if c.currentObserver in s.subs: return
  s.subs.add c.currentObserver


template val*(s: Signal): untyped =
  ## Expands to `s.value` and performs dependency tracking.
  ##
  ## When this template is used inside `effect`, `computed`, `memo`,
  ## or `watch` the call runs through `registerDep(s)` first. That
  ## records the **current observer callback** as a subscriber of
  ## the signal, so any future `set` on the same signal will schedule
  ## the observer to run again.
  ##
  ## ***Relationship to other helpers***
  ##
  ## - `val(s)`  (this template)  -> read **and** track
  ## - `peek(s)` -> read **without** tracking
  ##
  ## Choose `val` when you want reactivity; choose `peek` when you only
  ## need the value once during the current execution.
  ##
  ## ***Typical pattern***
  ##
  ## ```nim
  ## ctx.effect proc =
  ##   echo "HP:", hp.val, "/", maxHp.val   # both reads tracked
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - Calling `val` outside an observer context (for example in plain
  ##   top-level code) still works but adds no subscription; if the read
  ##   must stay reactive place it in an effect or computed signal.
  ## - Reading inside a loop in an effect collects a dependency for every
  ##   iteration. If the loop length can grow unbounded this may create a
  ##   large subscriber list; consider using structural signals like
  ##   `rev(rs)` instead.
  registerDep s
  s.value

proc notifySubs[T](s: Signal[T]) =
  let c = s.ctx

  if c.batching < 1:
    # echo "Notification for ", $T, "\n", getStackTrace()
    for cb in s.subs: c.scheduler cb
    return

  for cb in s.subs:
    let h = hash(cb)
    if h notin c.queueHashes:
      c.queueHashes.incl h
      c.queue.add cb


proc newReactiveNull*(ctx: ReactiveCtx): JsonNodeReactive

proc mkSignal(ctx: ReactiveCtx,
              v: JsonNodeReactive): Signal[JsonNodeReactive] =
  ## Ensure Signal[JsonNodeReactive].value is never nil.
  if v.isNil:
    signal(ctx, newReactiveNull(ctx))
  else:
    signal(ctx, v)

proc mkSignal[T](ctx: ReactiveCtx, v: T): Signal[T] = signal(ctx, v)

proc set*[T](s: Signal[T], v: T) =
  ## Writes a new value into a `Signal`.
  ##
  ## ***Purpose***
  ##
  ## This is the fundamental mutation primitive; every shortcut such as
  ## `+=` or element‐signal writes eventually calls `set`.
  ##
  ## - **History**
  ##   Pushes one `HistoryEntry` that stores both the old and the new value,
  ##   unless `ctx.isUndoing`, `ctx.isRedoing`, or `ctx.isDisposed` is true.
  ##
  ## - **Effects and computed signals**
  ##   All subscribers collected through dependency tracking are notified.
  ##   The actual reaction runs immediately on the immediate scheduler or is
  ##   queued for `flushQueued(ctx)` / the next frame on other schedulers.
  ##
  ## - **Transactions**
  ##   Inside `transaction(ctx)` several calls to `set` are coalesced: the
  ##   history entry is created when the outermost transaction finishes and
  ##   reactions fire once per flush, not per call.
  ##
  ## - **Autosave**
  ##   Marks the context dirty so the next flush (manual or scheduled) writes
  ##   a snapshot when autosave is enabled.
  ##
  ## - **Writable computed signals**
  ##   When the target signal has a custom setter (`computed(..., setter=...)`)
  ##   the call is forwarded there instead of storing into the signal’s own
  ##   field, thus supporting virtual two-way bindings.
  ##
  ## ***Pitfalls***
  ##
  ## - Writing the *same* value is ignored; no history entry, no reactions.
  ## - Do not call `set` inside the getter of the same computed signal; that
  ##   creates infinite recursion.
  ## - After `dispose(ctx)` the value still mutates but no reactions, history
  ##   entries, or autosave snapshots will be produced.


  if s.setter != nil:
    s.setter(v)
    return                         # underlying signals push history

  if s.value == v: return
  let c = s.ctx

  # undo / redo / disposed bypass (no history entry)
  if c.isUndoing or c.isRedoing or c.isDisposed:
    s.value = v
    notifySubs s
    markDirty c
    return

  # regular write (will push history)
  let old = s.value
  s.value = v

  let entry: HistoryEntry = (
    undo: () => s.set old,
    redo: () => s.set v
  )

  c.undoStack.add entry
  c.redoStack.setLen 0
  notifySubs s
  markDirty c


proc set*[T](rs: ReactiveSeq[T], vals: seq[T]) =
  ## Replace the entire sequence in **one** batched operation.
  rs.mutate proc(s: var seq[Signal[T]]) =
    s.setLen 0
    for v in vals:
      s.add mkSignal(rs.ctx, v)

proc set*[K, V](rt: ReactiveTable[K, V], src: Table[K, V]) =
  ## Overwrite *all* keys so that the resulting map equals `src`.
  ## Existing signals are re-used when the key stays; others are created.
  rt.mutate proc(t: var Table[K, Signal[V]]) =
    var newT = initTable[K, Signal[V]]()
    for k, v in src:
      if k in t:
        let s = t[k]
        s.set v              # element write
        newT[k] = s
      else:
        newT[k] = mkSignal(rt.ctx, v)
    t = newT                 # keys not present in `src` disappear


template `$`*(s: Signal): untyped = s.val
template `+=`*[T](s: Signal[T], d: T) = s.set s.val + d
template `-=`*[T](s: Signal[T], d: T) = s.set s.val - d
template `*=`*[T](s: Signal[T], d: T) = s.set s.val * d

proc `==`*[T](a: Signal[T], b: T): bool =
  (not a.isNil) and a.value == b

proc `==`*[T](a: T, b: Signal[T]): bool =
  (not b.isNil) and a == b.value


proc computed*[T](
  ctx: ReactiveCtx,
  getter: proc(): T,
  setter: proc(v: T) = nil
): Signal[T] =
  ## Creates a derived signal whose value is calculated by `getter`. The
  ## result is cached and recomputed only when any signal read inside
  ## `getter` has changed. If `setter` is supplied the computed signal
  ## becomes writable; calling `set` or `pct.set(x)` forwards the write to
  ## `setter`.
  ##
  ## ***How it fits in***
  ##
  ## - Use `computed` for values that can be expressed entirely in terms of
  ##   other signals.
  ## - Effects depending on the computed signal see a single dependency
  ##   instead of all the sources used inside `getter`.
  ## - Writable computed signals let you expose a convenient virtual field
  ##   (for example percent health) that writes back to one or more base
  ##   signals.
  ##
  ## ***Example***
  ##
  ## ```nim
  ## let pct = computed(ctx,
  ##   getter = () => hp.val.float / maxHp.val.float,
  ##   setter = (p: float) => hp.set int(p * maxHp.val.float))
  ##
  ## echo pct.val   # read
  ## pct.set 0.8    # write through setter
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - `getter` runs immediately when `computed` is created; avoid heavy work
  ##   or side effects inside it.
  ## - Do not call `set` on the computed signal from inside its own `getter`;
  ##   that would cause infinite recursion.
  ## - If `setter` writes the same value back into the sources the engine
  ##   detects no change and pushes no history entry.

  let s = signal(ctx, getter())       # initial evaluation
  s.setter = setter                   # attach (may be nil)

  proc re() =
    let nv = getter()
    if nv != s.val:
      ## temporarily silence the writable setter to avoid recursion
      let saved = s.setter
      s.setter = nil
      s.set nv                         # push undo & notify
      s.setter = saved

  let prev = ctx.currentObserver
  ctx.currentObserver = re
  discard getter()                     # capture dependencies
  ctx.currentObserver = prev
  s

proc effect*(ctx: ReactiveCtx, body: proc()) =
  ## Registers `body` as a reactive side effect. The procedure runs once
  ## immediately, collects every signal it reads, and is scheduled to run
  ## again whenever any of those signals changes.
  ##
  ## ***Purpose***
  ##
  ## Effects are the bridge from reactive data to the outside world. Use them
  ## for printing, updating the UI, sending network messages, or any work
  ## whose result is not itself a signal.
  ##
  ## ***How it works***
  ##
  ## - During the first call `ctx.currentObserver` is set to the callback so
  ##   every signal read registers the dependency automatically.
  ## - Later writes to any of those signals schedule the effect.
  ## - If the active scheduler is queued or frame based, the run happens on
  ##   the next `flushQueued(ctx)` or frame tick.
  ## - Dependencies are re-collected on every run; if `body` stops reading a
  ##   signal it will be unsubscribed.
  ##
  ## ***Example***
  ##
  ## ```nim
  ## effect(ctx, proc () =
  ##   echo "HP:", hp.val, "/", maxHp.val
  ## )
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - Do not mutate signals inside the same effect that reads them unless
  ##   you are certain it will not cause infinite loops.
  ## - An effect runs even if the computed result is identical; add manual
  ##   checks if you need extra deduplication.
  ## - Heavy work should be batched or throttled to avoid frame drops under
  ##   rapid updates.

  proc run =
    let prev = ctx.currentObserver   # save
    ctx.currentObserver = run        # point to myself
    body()                           # collect dependencies
    ctx.currentObserver = prev       # restore

  run()

proc effect*(ctx: ReactiveCtx, body: proc(onCleanup: AddCleanup)) =
  ## Registers a reactive side effect that can schedule cleanup callbacks.
  ## `body` receives an `onCleanup` function; every `Cleanup` registered with
  ## it runs right before the effect re executes or when the context is
  ## disposed.
  ##
  ## ***Purpose***
  ##
  ## Same as the simple `effect`, plus deterministic setup and teardown of
  ## resources such as timers, event listeners, or file handles.
  ##
  ## ***Usage pattern***
  ##
  ## ```nim
  ## effect(ctx, proc (onCleanup: AddCleanup) =
  ##   let id = setTimer(1000, tick)
  ##   onCleanup(() => clearTimer(id))
  ## )
  ## ```
  ##
  ## - First run: acquire resources, register cleanups, establish
  ##   dependencies.
  ## - On every re run: all cleanups fire, then the body executes again.
  ## - On `dispose(ctx)`: any remaining cleanups run once.
  ##
  ## ***Pitfalls***
  ##
  ## - Register each resource exactly once; adding inside loops can leak
  ##   callbacks.
  ## - Cleanups run even when signal values did not change if the effect
  ##   still re evaluates. Keep them lightweight and idempotent.
  ## - Avoid calling `onCleanup` conditionally based on signal values that
  ##   may differ between runs; otherwise the number of callbacks can grow
  ##   unbounded.

  var cleanups: seq[Cleanup]

  proc onCleanup(c: Cleanup) = cleanups.add c

  proc run() =
    for c in cleanups: c()
    cleanups.setLen 0
    let prev = ctx.currentObserver
    ctx.currentObserver = run
    body(onCleanup)                  # collect deps / register new cleanups
    ctx.currentObserver = prev

  run()

proc flushQueued*(ctx: ReactiveCtx) =
  ## Runs all reactions currently waiting in `ctx.queue` and then, if
  ## autosave is enabled and the context is marked dirty, writes one snapshot
  ## to the autosave JSON node.
  ##
  ## ***Purpose***
  ##
  ## Use `flushQueued` when working with the queued or frame schedulers if
  ## you need to force updates immediately, or in automated tests where you
  ## want deterministic timing without depending on the scheduler loop.
  ##
  ## ***What it does***
  ##
  ## - Copies the current queue, clears it, and executes each reaction once.
  ## - Clears `queueHashes` so the same reaction can be queued again later.
  ## - If autosave is active and any write happened since the last snapshot,
  ##   serialises the store signals into the target JSON node.
  ##
  ## ***Typical usage***
  ##
  ## ```nim
  ## useQueuedScheduler(ctx)
  ## value.set 42       # effect is queued
  ## flushQueued ctx    # run it right now
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - Do not call inside a `transaction`; the batch is not finished until
  ##   the outer transaction flushes.
  ## - Immediate scheduler users never need this call because reactions run
  ##   at once.
  ## - Flushing very often defeats the coalescing benefit of queued schedulers.

  if ctx.queue.len > 0:
    let jobs = ctx.queue
    ctx.queue = @[]
    ctx.queueHashes.clear()
    for cb in jobs: cb()

  # autosave fires once per flush
  if ctx.autoEnabled and ctx.dirty and ctx.autoTarget != nil:
    saveInto(ctx, ctx.autoTarget)

template transaction*(ctx: ReactiveCtx, body: untyped) =
  ## Runs `body` while incrementing `ctx.batching`. Each nested call further
  ## increments the counter. Reactions, history entries, and autosave writes
  ## are suppressed until the counter returns to zero at the end of the
  ## outermost transaction.
  ##
  ## ***What is coalesced***
  ##
  ## - **Signal writes** still update their stored value immediately but
  ##   notifications go into `ctx.queue` instead of running right away.
  ## - **ReactiveSeq / ReactiveTable structural edits** save an *old* copy on
  ##   the first mutation and postpone emitting their `HistoryEntry` until
  ##   the commit phase. Multiple edits of the same container collapse into
  ##   one entry.
  ## - **Undo/redo depth** therefore grows by at most one entry per distinct
  ##   signal and one per structurally mutated container, no matter how many
  ##   writes happened inside the block.
  ##
  ## ***Commit phase***
  ##
  ## When the outermost transaction exits (`ctx.batching` becomes zero) the
  ## template:
  ## 1. hands a flush request to the current scheduler
  ## 2. the scheduler later calls `flushQueued(ctx)` which
  ##    * executes queued reactions once each
  ##    * writes one autosave snapshot if dirty
  ##
  ## ***Example***
  ##
  ## ```nim
  ## transaction(ctx):
  ##   hp -= 10
  ##   mana -= 5
  ##   inv.push "potion"
  ## # exactly one reaction run and one history record for inv
  ## ```
  ##
  ## ***Nesting and exceptions***
  ##
  ## - Nested transactions simply increment the counter further; commit runs
  ##   only when the outermost layer finishes.
  ## - The template uses `try/finally` so `ctx.batching` is decremented even
  ##   if `body` raises. An exception aborts the commit; partial changes stay
  ##   applied because the engine has no automatic rollback. Call `undo` in a
  ##   custom catch block if you need an all-or-nothing effect.
  ##
  ## ***Pitfalls***
  ##
  ## - Blocking `flushQueued(ctx)` inside the same frame when using a frame
  ##   scheduler negates batching benefits.
  ## - Long running code inside `body` can delay reactions longer than users
  ##   expect.

  inc ctx.batching
  try:
    body
  finally:
    dec ctx.batching
    if ctx.batching == 0:
      # hand the flush to whatever scheduler is active
      ctx.scheduler () => flushQueued ctx

proc undo*(ctx: ReactiveCtx, steps = 1) =
  ## Reverses the last `steps` history entries (default 1). For each step
  ## the most recent `HistoryEntry` is popped from the undo stack, its
  ## `undo` callback runs, and the entry is pushed onto the redo stack.
  ## Reactions run after the batch and autosave marks the context dirty.
  ## Calling `undo` when the stack is empty does nothing.

  var n = steps
  ctx.isUndoing = true
  defer: ctx.isUndoing = false
  transaction(ctx):
    while n > 0 and ctx.undoStack.len > 0:
      dec n
      let e = ctx.undoStack.pop()
      ctx.redoStack.add e        # save for redo
      e.undo()

proc redo*(ctx: ReactiveCtx, steps = 1) =
  ## Travels forward in the undo history `steps` times (default 1).
  ## Pops entries from the redo stack, executes their `redo` callbacks,
  ## pushes them back onto the undo stack, and schedules reactions. Has no
  ## effect when the redo stack is empty.

  var n = steps
  ctx.isRedoing = true
  defer: ctx.isRedoing = false
  transaction(ctx):
    while n > 0 and ctx.redoStack.len > 0:
      dec n
      let e = ctx.redoStack.pop()
      ctx.undoStack.add e        # restore in undo history
      e.redo()

proc undoDepth*(ctx: ReactiveCtx): int =
  ## Returns the number of `HistoryEntry` objects currently stored in the
  ## undo stack of `ctx`. A quick way to decide whether an Undo button
  ## should be enabled in the UI.
  ctx.undoStack.len

proc redoDepth*(ctx: ReactiveCtx): int =
  ## Returns the current size of the redo stack for `ctx`. Useful in
  ## debugging UIs to enable or disable a 'Redo' button.
  ctx.redoStack.len

proc clearUndo*(ctx: ReactiveCtx) =
  ## ***Purpose***
  ##
  ## Erases both the undo and redo stacks of the given context without
  ## altering current state.
  ##
  ## ***When to call***
  ##
  ## - Before loading a fresh document or scene where old history should not
  ##   apply.
  ## - In tests that want to assert depth from a clean slate.
  ##
  ## ***Pitfalls***
  ##
  ## - Users can no longer undo the edits made earlier in the session.
  ## - Does not stop reactions; only history is cleared.

  ctx.undoStack.setLen 0
  ctx.redoStack.setLen 0

proc markDirty(ctx: ReactiveCtx) =
  ## 1. Set the dirty flag.
  ## 2. If we are **not inside a transaction**, hand a flush to the
  ##     currently-installed scheduler (immediate / queued / frame).
  if ctx.isDisposed: return
  ctx.dirty = true
  if ctx.batching == 0:
    ctx.scheduler () => flushQueued ctx

proc snapshot*(ctx: ReactiveCtx): int =
  ## Creates a checkpoint of the current undo position and returns its index.
  ## Use the returned int later with `travel(ctx, index)` to jump back or
  ## forward to this exact moment in history.  Snapshot indices grow
  ## monotonically and are valid until the undo and redo stacks are cleared
  ## or purged by `dispose(ctx)`.

  ctx.undoStack.len

proc travel*(ctx: ReactiveCtx, index: int) =
  ## Jumps to the snapshot at `index`, the value previously returned by
  ## `snapshot(ctx)`. If `index` is lower than the current undo depth the
  ## call performs the required number of undo steps. If it is higher the
  ## call performs redo steps. Out-of-range indices are ignored. Reactions
  ## fire after the move and autosave updates on the next flush.

  let cur = ctx.undoStack.len
  if index < 0 or index > cur + ctx.redoStack.len: return
  if index < cur:
    undo(ctx, cur - index)
  elif index > cur:
    redo(ctx, index - cur)

proc useQueuedScheduler*(ctx: ReactiveCtx) =
  ## Switches to a simple queued scheduler.
  ##
  ## ***Behavior***
  ##
  ## - Every reaction triggered by a signal write is enqueued immediately.
  ## - Nothing runs until `flushQueued(ctx)` is called.
  ## - Multiple queued reactions are executed in the order they were added,
  ##   duplicates are filtered by hash.
  ##
  ## ***When to use***
  ##
  ## - Unit tests that want deterministic timing without a frame loop.
  ## - Command-line tools that need to flush after processing a batch of
  ##   input lines.
  ## - Situations where you need to guarantee that expensive work is delayed
  ##   until an explicit sync point.
  ##
  ## ***Pitfalls***
  ##
  ## - Forgetting to call `flushQueued(ctx)` causes reactions and autosave
  ##   to accumulate indefinitely.

  ctx.scheduler = proc(cb: proc()) =
    let h = hash(cb)
    if h notin ctx.queueHashes:
      ctx.queueHashes.incl h
      ctx.queue.add cb

proc watch*[T](
  ctx: ReactiveCtx,
  selector: () -> T,
  handler: proc(newVal, oldVal: T),
  immediate = false
) =
  ## Observes the output of `selector` and calls `handler` whenever that
  ## output changes.
  ##
  ## ***Workflow***
  ##
  ## 1. `selector` runs inside an internal effect, collecting dependencies
  ##    and storing its result.
  ## 2. When any dependency changes the effect reruns; if the new result
  ##    differs from the stored one `handler(new, old)` is invoked.
  ## 3. The stored result is updated to the new value.
  ##
  ## `immediate` controls whether the handler fires on the first evaluation.
  ##
  ## ***Use cases***
  ##
  ## - React when a numeric value crosses a threshold without caring about
  ##   every intermediate change.
  ## - Run validation logic only when a composite selector result differs.
  ## - Trigger animations where only the final state of a batch matters.
  ##
  ## ***Example***
  ##
  ## ```nim
  ## watch(ctx,
  ##   selector = () => hp.val div 10,
  ##   handler  = proc(new, old: int) =
  ##     echo "tens digit changed from", old, "to", new,
  ##   immediate = true)
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - Selector results are compared with `!=`. For complex types ensure
  ##   that this equality test reflects real changes.
  ## - If the selector performs heavy work consider wrapping it in `memo`
  ##   first.
  ## - `handler` runs inside the same flush that detected the change; avoid
  ##   long running code that blocks other reactions.

  var first = true
  var prev: T

  ctx.effect proc =
    let cur = selector()
    if first:
      first = false
      if immediate:
        handler(cur, cur) # fire right away
      prev = cur
    elif cur != prev:
      let old = prev
      prev = cur
      handler(cur, old)

template watchNow*[T](
  ctx: ReactiveCtx,
  selector: () -> T,
  handler: proc(newVal, oldVal: T)
) =
  ## Shorthand for `watch` with `immediate = true`. Runs `selector`
  ## immediately, stores the result, invokes `handler(new, new)` once, and
  ## then behaves like `watch`, calling `handler(new, old)` whenever the
  ## selector’s output changes.
  ##
  ## Helpful for loading indicators or log statements that should fire right
  ## away and on every subsequent change without writing two separate calls.
  watch(ctx, selector, handler, true)

proc dumpDeps*(s: Signal): seq[string] =
  ## Return a seq of hex strings, one per subscriber callback.
  ## Helpful for quick "who depends on this signal?" debugging.
  for cb in s.subs:
    result.add hash(cb).int.toHex()

proc dispose*(ctx: ReactiveCtx) =
  ## Stops all reactive activity owned by `ctx` and frees its resources.
  ##
  ## ***What happens***
  ##
  ## - If autosave is active a final snapshot is written before shutdown.
  ## - Pending reaction queues and history stacks are cleared.
  ## - Scheduler is replaced with a no-op and `currentObserver` is set to
  ##   nil, so no further reactions run.
  ## - `ctx.isDisposed` becomes true; helper procs treat the context as dead.
  ##
  ## ***Typical usage***
  ##
  ## ```nim
  ## let ctx = newReactiveCtx()
  ## ...
  ## dispose ctx      # when the data set or UI panel is closed
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - Writing to a signal after disposal mutates its raw value but triggers
  ##   no reactions or autosave.
  ## - A disposed context cannot be revived. Create a new context instead.
  ## - Transactions open at the moment of disposal are silently cancelled.

  if ctx.autoEnabled and ctx.dirty and ctx.autoTarget != nil:
    saveInto(ctx, ctx.autoTarget)

  ctx.isDisposed = true
  ctx.queue.setLen 0
  ctx.undoStack.setLen 0
  ctx.scheduler = (cb: proc()) => (discard)   # no-op
  ctx.currentObserver = nil
  ctx.batching = 0

proc memo*[T](
  ctx: ReactiveCtx,
  selector: () -> T
): () -> T =
  ## Creates a memoised selector. The returned closure reads its cached value
  ## without tracking dependencies, while an internal effect keeps that cache
  ## up to date by running `selector` again only when any of the signals it
  ## reads change.
  ##
  ## ***Purpose***
  ##
  ## Use `memo` to avoid expensive recalculation or to expose a selector that
  ## can be read many times without causing additional reactivity churn.
  ##
  ## ***How it works***
  ##
  ## - On creation `selector` runs once, collects its dependencies, and
  ##   stores the result.
  ## - An internal effect reruns `selector` when any dependency changes and
  ##   updates the cache.
  ## - The returned closure just returns the current cache, adding itself as
  ##   a dependency of callers.
  ##
  ## ***Example***
  ##
  ## ```nim
  ## let sum = memo(ctx, () => a.val + b.val)
  ## echo sum()   # fast cached read
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - Do not call `memo` inside another effect if the selector performs
  ##   heavy work; create it once at module or context setup time.
  ## - Values are compared with `!=`; if `T` has complex equality rules make
  ##   sure that still detects real changes.
  ## - The memo itself never pushes undo history; only the underlying signal
  ##   writes do.

  let sig = signal(ctx, default(T))    # cache
  var first = true

  ctx.effect proc =
    let nv = selector()
    if first or nv != sig.value:
      first = false
      sig.value = nv          # direct write → no history, no recursion
      notifySubs sig          # wake dependants manually

  proc: T =
    registerDep sig
    sig.value

template peek*[T](s: Signal[T]): untyped =
  ## Reads a signal’s current value without registering a dependency.
  ## Useful inside effects when you need the data but do not want the effect
  ## to rerun on future changes.
  ##
  ## ```nim
  ## effect(ctx, () =>
  ##   echo "debug:", peek(hp)   # prints once, never reruns
  ## )
  ## ```

  block:
    let inPrev = s.ctx.currentObserver   # save
    s.ctx.currentObserver = nil         # silence tracking
    let tV = s.value                    # raw read
    s.ctx.currentObserver = inPrev       # restore
    tV                                  # ⇐ result

proc effectOnce*(ctx: ReactiveCtx, body: proc()) =
  ## Executes `body` immediately and never schedules it again, even if the
  ## procedure reads signals that later change. Dependency tracking is
  ## disabled during the call.
  ##
  ## ***Purpose***
  ##
  ## Use `effectOnce` for one time side effects such as logging, initialising
  ## third party libraries, or writing a first snapshot after startup.
  ##
  ## ***Example***
  ##
  ## ```nim
  ## effectOnce(ctx, proc () =
  ##   echo "Initial HP:", hp.val
  ## )
  ## ```
  ##
  ## ***Pitfalls***
  ##
  ## - Because no dependencies are collected, later updates do not trigger
  ##   the procedure. Choose a normal `effect` if you need ongoing reactivity.
  ## - Cleanups cannot be registered; allocate resources only if they live
  ##   for the full lifetime of the context.

  let saved = ctx.currentObserver
  ctx.currentObserver = nil    # disable dependency collection
  body()                       # one-shot side-effect
  ctx.currentObserver = saved





template rlog*(parts: varargs[string, `$`]) =
  when defined(debugReactive):
    echo "[reactive] " & parts.join("")
  else:
    discard


proc isPrimitive(n: NimNode): bool =
  # Reject nodes that cannot denote a type name.
  if n.kind notin {nnkIdent, nnkSym}:
    rlog "isPrimitive: non-ident/sym, kind=" & $n.kind
    return false

  # Ask the compiler for the fully-resolved type category and log it.
  let tk = n.typeKind()
  rlog "isPrimitive: typeKind=" & $tk

  # Built-in scalar kinds that can be stored directly in a Signal.
  if tk in [
    ntyBool,
    ntyChar,
    ntyPtr,
    ntyPointer,
    ntyString,
    ntyCString,
    ntyInt,
    ntyInt8,
    ntyInt16,
    ntyInt32,
    ntyInt64,
    ntyFloat,
    ntyFloat32,
    ntyFloat64,
    ntyFloat128,
    ntyUInt,
    ntyUInt8,
    ntyUInt16,
    ntyUInt32,
    ntyUInt64
  ]:
    rlog "isPrimitive: matched built-in kind"
    return true

  # Accept C aliases that share the same runtime representation.
  let res = n.strVal in ["cfloat", "cint"]
  rlog "isPrimitive: alias match=" & $res
  res

proc isEnumType(n: NimNode): bool =
  # Reject nodes that cannot denote a type name.
  if n.kind notin {nnkIdent, nnkSym}:
    rlog "isEnumType: non-ident/sym, kind=" & $n.kind
    return false

  # First ask the compiler in a `compiles` guard to avoid blowing up on
  # opaque or not-yet-resolved symbols.
  let ok = compiles(n.getTypeImpl.kind == nnkEnumTy) and
           n.getTypeImpl.kind == nnkEnumTy
  rlog "isEnumType: " & $ok & " for " & $n.repr
  ok

proc isArrayType(n: NimNode): bool =
  # We expect the syntactic shape `array[Len, Elem]`.
  result = n.kind == nnkBracketExpr and n.len >= 1 and
    n[0].kind in {nnkIdent, nnkSym} and n[0].strVal == "array"
  rlog "isArrayType: " & $result & " for " & $n.repr

proc isTableLike(t: NimNode): bool =
  # Accept `Table`, `TableRef`, and `OrderedTable` so that they can be
  # wrapped as a `ReactiveTable`.
  let res = t.kind == nnkBracketExpr and t.len >= 1 and
    t[0].strVal in ["Table","TableRef","OrderedTable"]
  rlog "isTableLike: " & $res & " for " & $t.repr
  res

proc isSeqLike(t: NimNode): bool =
  # Detect the generic instance `seq[T]` by structure and head identifier.
  let res = t.kind == nnkBracketExpr and t.len == 2 and t[0].strVal == "seq"
  rlog "isSeqLike: " & $res & " for " & $t.repr
  res


proc unwrapAlias(n: NimNode): NimNode =
  ## Walks a sequence of simple type‐aliases (that is, aliases whose right-hand
  ## side is either another symbol, a generic instantiation such as
  ## `array[3, float]`, or a tuple type) and returns the first node that is
  ## *not* an alias.  
  ##
  ## A node that is already a concrete type, a non-symbol AST fragment, or an
  ## alias that we cannot safely inspect (for example because it comes from an
  ## imported module) is returned unchanged.
  ##
  ## This helper lets the wrapper generator treat aliases and their underlying
  ## types transparently so that `Signal[int32]` and `MyInt32Alias` behave the
  ## same in user code.
  rlog "unwrapAlias: start with " & $n.repr

  # `cur` is advanced along the alias chain until we hit a non-alias node.
  var cur = n
  while true:
    # Stop when the current node is not a symbol. `array[3, float]` enters the
    # function with `nnkBracketExpr` already — there is nothing more to unwrap.
    if cur.kind notin {nnkSym, nnkIdent}:
      rlog "unwrapAlias: hit non-symbol, returning " & $cur.repr
      return cur

    # Every symbol we have *must* be safe to query with `getImpl`. For symbols
    # that come from another compilation unit or that are still generic,
    # `getImpl` might fail, in which case we keep the symbol itself.
    let impl =
      try:
        cur.getImpl
      except Exception as e:
        rlog "unwrapAlias: getImpl failed (" & e.msg & "), returning " &
             $cur.repr
        return cur

    # If the implementation node is not `nnkTypeDef` we are looking at a real
    # type or some complex definition (e.g. an object) — stop unwrapping.
    if impl.kind != nnkTypeDef:
      rlog "unwrapAlias: symbol is not a type alias, returning " & $cur.repr
      return cur

    # `nnkTypeDef` has the shape `[name, pragma, rhs]`; we only care about the
    # right-hand side.
    let rhs = impl[2]

    # We detect “plain” aliases: symbols, other aliases, generic
    # instantiations, or tuple types. Those can be followed safely. More exotic
    # RHS kinds (such as `nnkTypeClassTy`) are left in place to avoid surprises.
    if rhs.kind in {nnkBracketExpr, nnkSym, nnkIdent, nnkTupleTy}:
      rlog "unwrapAlias: following alias " & $cur.repr & " → " & $rhs.repr
      cur = rhs
      # Continue the loop with the new node; chains of aliases collapse here.
    else:
      rlog "unwrapAlias: RHS is not a plain alias, returning " & $cur.repr
      return cur

proc ping*[T](s: Signal[T]) {.inline.} =
  ## Wakes every subscriber of `s`.
  notifySubs s


proc ping*[T](rs: ReactiveSeq[T]) {.inline.} =
  ## Wakes dependants of the sequence-level signals `lenS` and `revS`.
  notifySubs rs.lenS
  notifySubs rs.revS


proc ping*[K, V](rt: ReactiveTable[K, V]) {.inline.} =
  ## Wakes dependants of the table-level signals `lenS` and `revS`.
  notifySubs rt.lenS
  notifySubs rt.revS


proc ping*[R](x: ref R) {.inline.} =
  ## Delegates to `ping(x[])` when `x` is not `nil`.
  if x != nil: ping(x[])


proc ping*[R](x: R) {.inline.} =
  ## Recursively pings every `Signal`, `ReactiveSeq`, and `ReactiveTable`
  ## stored anywhere inside `x`.
  ## Works for arbitrarily deep wrapper graphs.
  # `fieldPairs` is available only for record-like types
  when compiles(fieldPairs(x)):
    for _, val in fieldPairs(x):
      rlog "ping nested  " & $val.repr
      ping(val)

proc wrapType(srcOrig: NimNode): (NimNode, bool) =
  # Convert a source type to the reactive-wrapper type that should be stored
  # in an auto-generated wrapper object.
  #
  # The second result indicates whether the returned type already contains
  # its own nested reactive bookkeeping (`true`) or whether only a single
  # `Signal` around the value is needed (`false`).
  #
  # The function is called by the `reactive` macro for every field it
  # encounters, so detailed logging is critical for diagnosing mis-wrapped
  # types in user bug reports.

  # Resolve chains of simple aliases so that the remaining rules operate on
  # the canonical type node.
  let src = unwrapAlias(srcOrig)

  # Trace the alias step so the log shows exactly which names were reduced.
  rlog "wrapType: " & $srcOrig.repr & "  →  " & $src.repr

  # Primitive scalars, enums, and fixed-size arrays are wrapped in `Signal`
  # because only their value needs change tracking.
  if isPrimitive(src) or isEnumType(src) or isArrayType(src):
    rlog "wrapType: Signal wrapper for primitive/enum/array " & $src.repr
    return (nnkBracketExpr.newTree(ident"Signal", src), false)

  # A `seq[T]` becomes a `ReactiveSeq[T]` so structural modifications are
  # observable without per-element instrumentation.
  if isSeqLike(src):
    rlog "wrapType: ReactiveSeq wrapper for seq " & $src.repr
    return (nnkBracketExpr.newTree(ident"ReactiveSeq", src[1]), true)

  # Table-like containers map to `ReactiveTable` for the same reason.
  if isTableLike(src):
    rlog "wrapType: ReactiveTable wrapper for table " & $src.repr
    return (nnkBracketExpr.newTree(ident"ReactiveTable", src[1], src[2]), true)

  # If the type name already ends with “Reactive” we assume the user has
  # provided a custom reactive wrapper and simply forward it.
  if src.kind in {nnkIdent, nnkSym} and src.strVal.endsWith("Reactive"):
    rlog "wrapType: type already reactive " & src.strVal
    return (src, true)

  # Otherwise synthesize a new identifier by appending “Reactive”.
  if src.kind in {nnkIdent, nnkSym}:
    rlog "wrapType: inferred wrapper " & src.strVal & "Reactive"
    return (ident(src.strVal & "Reactive"), true)

  # Fallback: treat anything unrecognized as a plain value wrapped in
  # `Signal`, logging the unusual case for later inspection.
  rlog "wrapType: fallback Signal wrapper for node kind " & $src.kind
  (nnkBracketExpr.newTree(ident"Signal", src), false)


macro reactive*(T: typedesc): untyped =
  ## Builds a *reactive wrapper* for the value object `T` together with three
  ## helper procedures:
  ##
  ## * **`toReactive(ctx, src)`** – builds the wrapper from a plain value.
  ## * **`toPlain(self)`** – converts the wrapper back to a plain value.
  ## * **`set(self, src)`** – overwrites the wrapper with a new plain (object)
  ##   value while integrating with undo/redo, transactions, and variant 
  ##   switching.
  ##
  ## ***Why you need it***
  ## - Treats every field of `T` as a reactive value without writing any
  ##   boiler-plate.
  ## - Preserves object variants (`case` sections) and warns when a
  ##   discriminator change drops dependent signals.
  ## - Keeps plain data and reactive wrappers strictly separate so external
  ##   code can stay unaware of the reactive layer.
  ##
  ## ***Pitfalls***
  ## - `T` must be a *value* object; passing a `ref object` aborts compilation.
  ## - The generated wrapper is itself a `ref` type; copying the variable
  ##   copies the reference, not the data.
  ## - Discriminator changes fully rebuild the wrapper; signals that vanish
  ##   cannot be migrated automatically and only trigger a warning.

  # Identifiers reused in the generated code.
  let ctxId  = ident"ctx"
  let srcId  = ident"src"
  let selfId = ident"self"

  # Accumulated statement lists for the helper procedures.
  var toReStmts    = newStmtList()
  var toPlainStmts = newStmtList()
  var setStmts     = newStmtList()

  # Book-keeping for variant (`case`) handling.
  var sharedIds: seq[NimNode] = @[]
  var branchExclusive: seq[(NimNode, seq[NimNode])] = @[]
  # Discriminator?
  var discName: NimNode = nil

  # Add one field to the wrapper and extend all helper bodies accordingly.
  proc addField(
    dst: NimNode;
    rawId, srcType: NimNode;
    toRe, toPlain, setS: var NimNode;
    nameSink: var seq[NimNode]
  ) =
    rlog "addField: " & $rawId.repr & " type=" & $srcType.repr
    let (dstType, wrapped) = wrapType(srcType)

    var idDef = newNimNode(nnkIdentDefs)
    idDef.add rawId.copyNimTree
    idDef.add dstType
    idDef.add newEmptyNode()
    dst.add idDef

    let fName = (if rawId.kind == nnkPostfix: rawId[1] else: rawId)
    nameSink.add fName

    if wrapped:
      # Nested wrapper, delegate work to its own helpers.
      toRe.add quote do:
        result.`fName` = `ctxId`.toReactive(`srcId`.`fName`)
      toPlain.add quote do:
        result.`fName` = toPlain(`selfId`.`fName`)
      setS.add quote do:
        `selfId`.`fName`.set(`srcId`.`fName`)
    else:
      # Plain value becomes a `Signal`.
      toRe.add quote do:
        result.`fName` = signal(`ctxId`, `srcId`.`fName`)
      toPlain.add quote do:
        registerDep `selfId`.`fName`
        result.`fName` = `selfId`.`fName`.value
      setS.add quote do:
        set(`selfId`.`fName`, `srcId`.`fName`)

  # Recursive helpers that walk the source object's fields.
  proc buildRecList(
    srcRL: NimNode;
    toRe, toPlain, setS: var NimNode;
    names: var seq[NimNode]
  ): NimNode

  proc buildRecCase(
    srcCase: NimNode; 
    toRe, toPlain, setS: var NimNode
  ): NimNode

  proc buildRecList(
    srcRL: NimNode; 
    toRe, toPlain, setS: var NimNode; 
    names: var seq[NimNode]
  ): NimNode =
    result = nnkRecList.newTree()
    rlog "buildRecList len=" & $srcRL.len
    
    for ch in srcRL.children:
      case ch.kind
      of nnkIdentDefs:
        rlog "identDefs repr=" & $ch.repr
        var typ: NimNode
        if ch.len >= 3 and ch[ch.len-2].kind != nnkEmpty:
          typ = ch[ch.len-2]
        elif ch.len >= 3 and ch[ch.len-1].kind != nnkEmpty:
          typ = ch[ch.len-1].getType
        else:
          error "reactive(): cannot infer type for field", ch[0]

        let cnt = if ch.len >= 3: ch.len - 2 else: ch.len - 1
        for i in 0 ..< cnt:
          addField(result, ch[i], typ, toRe, toPlain, setS, names)
      of nnkRecCase:
        result.add buildRecCase(ch, toRe, toPlain, setS)
      else:
        discard

  proc buildRecCase(
    srcCase: NimNode; 
    toRe, toPlain, setS: var NimNode
  ): NimNode =
    rlog "buildRecCase repr=" & $srcCase.repr
    let discDefs = srcCase[0]
    
    discName = if discDefs[0].kind == nnkPostfix: 
      discDefs[0][1] 
    else: 
      discDefs[0]
    
    # Skeleton case statements for the helpers.
    var caseToRe  = nnkCaseStmt.newTree(quote do: `srcId`.`discName`)
    var casePlain = nnkCaseStmt.newTree(quote do: `selfId`.`discName`)
    var caseSet   = nnkCaseStmt.newTree(quote do: `selfId`.`discName`)
    toRe.add caseToRe
    toPlain.add casePlain
    setS.add caseSet

    # Build the wrapper-side variant.
    let newCase = nnkRecCase.newTree(discDefs)

    for br in srcCase.children:
      if br.kind notin {nnkOfBranch, nnkElse}: continue
      rlog "variant branch repr=" & $br.repr

      var brToRe  = newStmtList()
      var brPlain = newStmtList()
      var brSet   = newStmtList()
      var exclNames: seq[NimNode] = @[]

      let fieldsRec = buildRecList(br[^1], brToRe, brPlain, brSet, exclNames)

      var newBr = newNimNode(br.kind)
      for i in 0 ..< br.len - 1: newBr.add copyNimTree(br[i])
      newBr.add fieldsRec
      newCase.add newBr

      var branch = newNimNode(br.kind)
      for i in 0 ..< br.len - 1: branch.add copyNimTree(br[i])
      branch.add brToRe
      caseToRe.add branch

      branch = newNimNode(br.kind)
      for i in 0 ..< br.len - 1: branch.add copyNimTree(br[i])
      branch.add brPlain
      casePlain.add branch

      branch = newNimNode(br.kind)
      for i in 0 ..< br.len - 1: branch.add copyNimTree(br[i])
      branch.add brSet
      caseSet.add branch

      branchExclusive.add (copyNimTree(br[0]), exclNames)

    result = newCase

  # Parse and validate the source object.
  let impl = T.getImpl()
  var obj  = impl[^1]
  if obj.kind == nnkRefTy:
    obj = obj[0]
  if obj.kind != nnkObjectTy:
    error "reactive(): expects value object", T
  rlog "SOURCE OBJECT repr=" & $obj.repr

  # Build the field tree of the wrapper.
  var dstRoot = nnkRecList.newTree()
  dstRoot.add newIdentDefs(ident"reactCtx", ident"ReactiveCtx")

  for ch in obj.children:
    case ch.kind
    of nnkRecList:
      dstRoot.add buildRecList(ch, toReStmts, toPlainStmts, setStmts, sharedIds)
    of nnkRecCase:
      dstRoot.add buildRecCase(ch, toReStmts, toPlainStmts, setStmts)
    else:
      discard

  # Build warnings for dropped variant fields.
  var warnCase = nnkCaseStmt.newTree(quote do: `selfId`.`discName`)
  for (key, names) in branchExclusive:
    var ofBr = nnkOfBranch.newTree(copyNimTree(key))
    var warnBody = newStmtList()
    for idn in names:
      let text = "reactive: variant change drops ." & idn.strVal & " – deps:"
      warnBody.add quote do:
        if `selfId`.`idn`.subs.len > 0:
          rlog `text` & dumpDeps(`selfId`.`idn`).join(",")
    ofBr.add warnBody
    warnCase.add ofBr

  # Rebuild the wrapper when the discriminator changes.
  let transitionBlock =
    if not discName.isNil:
      let oldSelfSym = genSym(nskVar, "oldSelf")
      let newSelfSym = genSym(nskVar, "newSelf")
      let rcSym      = genSym(nskLet, "rc")
      let slotPtrSym = genSym(nskLet, "slotPtr")

      var body = newStmtList()

      body.add quote do:
        var `oldSelfSym` = `selfId`

      body.add quote do:
        var `newSelfSym` = `selfId`.reactCtx.toReactive(`srcId`)

      for idn in sharedIds:
        body.add quote do:
          `newSelfSym`.`idn` = `selfId`.`idn`
          `newSelfSym`.`idn`.set `srcId`.`idn`
          ping(`newSelfSym`.`idn`)

      body.add quote do:
        `selfId` = `newSelfSym`
        ping(`selfId`)

      body.add quote do:
        let `rcSym` = `selfId`.reactCtx
        if not (`rcSym`.isUndoing or `rcSym`.isRedoing or `rcSym`.isDisposed):
          let `slotPtrSym` = unsafeAddr `selfId`
          let entry: HistoryEntry = (
            undo: proc () =
              `slotPtrSym`[] = `oldSelfSym`
              ping(`slotPtrSym`[]),
            redo: proc () =
              `slotPtrSym`[] = `newSelfSym`
              ping(`slotPtrSym`[])
          )
          `rcSym`.undoStack.add entry
          `rcSym`.redoStack.setLen 0

      quote do:
        if `selfId`.`discName` != `srcId`.`discName`:
          `warnCase`
          block:
            `body`
          return
    else:
      newStmtList()

  # Generate the wrapper type and helper procedures.
  let W = ident($T & "Reactive")
  let wrapperObj = nnkObjectTy.newTree(newEmptyNode(), newEmptyNode(), dstRoot)
  let typeDef = nnkTypeDef.newTree(
    W,
    newEmptyNode(),
    nnkRefTy.newTree(wrapperObj)
  )

  let toReactiveProc = if discName.isNil:
    quote do:
      proc toReactive*(`ctxId`: ReactiveCtx; `srcId`: `T`): `W` =
        rlog "toReactive build wrapper"
        result = new `W`
        result.reactCtx = `ctxId`
        `toReStmts`
  else:
    quote do:
      proc toReactive*(`ctxId`: ReactiveCtx; `srcId`: `T`): `W` =
        rlog "toReactive build wrapper with discriminator"
        result = `W`(`discName`: `srcId`.`discName`)
        result.reactCtx = `ctxId`
        `toReStmts`

  let toPlainProc = if discName.isNil:
    quote do:
      proc toPlain*(`selfId`: `W`): `T` =
        rlog "toPlain convert wrapper to value"
        result = `T`()
        `toPlainStmts`
  else:
    quote do:
      proc toPlain*(`selfId`: `W`): `T` =
        rlog "toPlain convert wrapper with discriminator to value"
        result = `T`(`discName`: `selfId`.`discName`)
        `toPlainStmts`

  let setProc = quote do:
    proc set*(`selfId`: var `W`; `srcId`: `T`) =
      let rc = `selfId`.reactCtx
      `transitionBlock`
      transaction(rc):
        `setStmts`
      rlog "set: wrapper updated"

  # Assemble the final AST of the macro expansion.
  result = newStmtList(
    nnkTypeSection.newTree(typeDef),
    toReactiveProc,
    toPlainProc,
    setProc
  )

# end of reactive()

template updateLen[T](rs: ReactiveSeq[T]) =
  let s = rs.lenS
  if s.value != rs.data.len:
    s.value = rs.data.len
    notifySubs s

proc mutate[T](rs: ReactiveSeq[T], op: proc (s: var seq[Signal[T]])) =
  let c = rs.ctx
  if c.isDisposed: return

  let before = rs.data.dup() # shallow copy of signal refs
  op rs.data
  rs.updateLen
  rs.bumpRev
  markDirty c

  # undo/redo inside transaction
  if c.isUndoing or c.isRedoing: return
  if c.batching > 0:
    if not rs.pendingActive:
      rs.pendingActive = true
      rs.pendingOld = before

      let commit = proc =
        if not rs.pendingActive: return
        let oldCopy = rs.pendingOld
        let newCopy = rs.data.dup()

        let entry: HistoryEntry = (
          undo: proc =
            rs.data = oldCopy.dup()
            rs.updateLen,
          redo: proc =
            rs.data = newCopy.dup()
            rs.updateLen
        )

        c.undoStack.add entry
        c.redoStack.setLen 0
        rs.pendingActive = false

      let h = hash(commit)
      if h notin c.queueHashes:
        c.queueHashes.incl h
        c.queue.add commit
    return

  # outside any transaction
  let after = rs.data.dup()
  let entry: HistoryEntry = (
    undo: proc =
      rs.data = before.dup()
      rs.updateLen,
    redo: proc =
      rs.data = after.dup()
      rs.updateLen
  )
  c.undoStack.add entry
  c.redoStack.setLen 0

template rev*(rs: ReactiveSeq): untyped =
  ## Returns the structural revision counter of a `ReactiveSeq` and tracks
  ## the caller as a subscriber. The counter increments every time the
  ## sequence structure changes (push, insert, remove, clear) regardless of
  ## whether the overall length changed. Useful when you need to rerun an
  ## effect on any structural edit, even those that keep the length
  ## constant, such as swapping two elements.

  registerDep rs.revS
  rs.revS.value

template bumpRev[T](rs: ReactiveSeq[T]) =
  let s = rs.revS
  s.value.inc            # always increments, no equality check needed
  notifySubs s

proc toReactive*[T](ctx: ReactiveCtx, src: seq[T]): ReactiveSeq[T] =
  ## Wraps an existing `seq[T]` in `ctx`, producing a `ReactiveSeq` where
  ## every element becomes its own signal. The original sequence is copied
  ## once; later changes must be applied through the `ReactiveSeq` mutators
  ## or by writing the element signals directly.
  ##
  ## The resulting wrapper integrates with effects, transactions, undo and
  ## redo, autosave, and the active scheduler in the same way as a sequence
  ## built via repeated `push` calls.

  new result
  result.ctx = ctx
  result.data = @[]                              # ensure typed seq

  for v in items(src):                           # ← explicit iterator
    result.data.add mkSignal(ctx, v)

  result.lenS = signal(ctx, src.len)
  result.revS = signal(ctx, 0)

proc toPlain*[T](rs: ReactiveSeq[T]): seq[T] =
  ## Converts a `ReactiveSeq` into a plain `seq[T]` by copying the current
  ## element values. No dependencies are recorded and later changes to the
  ## reactive sequence do not affect the returned copy. Use this when you
  ## need an immutable snapshot for logging, JSON serialisation, or deep
  ## equality tests.

  for s in rs.data: result.add s.val

template `[]`*[T](rs: ReactiveSeq[T], idx: int): untyped =
  rs.data[idx]

proc push*[T](rs: ReactiveSeq[T], x: T) =
  ## Appends `x` to the end of the reactive sequence.
  ##
  ## ***Reactive effects***
  ##
  ## - Structural edit: length grows by 1 and `rev(rs)` increments.
  ## - Pushes one history entry or merges into an open transaction.
  ## - Effects that depend on length or structure rerun.

  rs.mutate(proc (v: var seq[Signal[T]]) = v.add mkSignal(rs.ctx, x))

proc pop*[T](rs: ReactiveSeq[T]): T =
  ## Removes and returns the last element of the reactive sequence.
  ## Counts as a structural edit: pushes one undo entry (or joins the current
  ## transaction), updates `len(rs)` and `rev(rs)`, and reruns effects that
  ## depend on length or structure. Raises `IndexDefect` if the sequence is
  ## empty.

  var sig: Signal[T]
  rs.mutate(proc (v: var seq[Signal[T]]) = sig = v.pop())
  result = sig.val

proc insert*[T](rs: ReactiveSeq[T], idx: int, x: T) =
  ## Inserts `x` at position `idx`, shifting later elements to the right.
  ## Counts as a structural edit: pushes one history entry (or joins the
  ## current transaction), updates `len(rs)` and `rev(rs)`, and reruns effects
  ## that depend on length or structure. `idx` must be in the range
  ## 0 .. rs.len.

  rs.mutate(proc (v: var seq[Signal[T]]) =
    sequtils.insert(v, @[mkSignal(rs.ctx, x)], idx)   # wrap as slice
  )

proc removeIdx*[T](rs: ReactiveSeq[T], idx: int) =
  ## Removes the element at `idx` from a `ReactiveSeq`.
  ##
  ## ***Effects***
  ## - Structural edit: `len(rs)` and `rev(rs)` update.
  ## - Pushes one history entry (or merges into an open transaction).
  ## - Triggers effects that depend on length or structure.
  ##
  ## Raises `IndexDefect` if `idx` is out of range.


  rs.mutate(proc (v: var seq[Signal[T]]) = v.delete idx)

proc clear*[T](rs: ReactiveSeq[T]) =
  ## Removes all elements from the reactive sequence.
  ##
  ## - Structural edit: sets length to 0 and increments `rev(rs)`.
  ## - Pushes one history entry (or merges into an active transaction).
  ## - Effects that depend on length or structure rerun after the flush.
  ##
  ## Raises no error when the sequence is already empty.
  rs.mutate(proc (v: var seq[Signal[T]]) = v.setLen 0)

template len*(rs: ReactiveSeq): untyped =
  ## Returns the current element count of a `ReactiveSeq` **and** registers
  ## the caller as a subscriber. Any structural change (push, insert,
  ## remove, clear) updates the length signal and reruns effects that read
  ## it. Use this inside effects or computed signals when later mutations
  ## should trigger reactive updates.

  registerDep rs.lenS
  rs.lenS.value

template peekLen*(rs: ReactiveSeq): untyped =
  ## Returns the length of a `ReactiveSeq` without tracking the caller as a
  ## subscriber. Handy for quick size checks in places where you do not want
  ## to establish reactivity.

  peek rs.lenS

iterator items*[T](rs: ReactiveSeq[T]): T =
  ## Yields each element of a `ReactiveSeq` as a plain value, from index 0 to
  ## `len(rs) - 1`. Reading through this iterator does **not** establish
  ## reactive dependencies, because it reads the cached `.val` of each
  ## element without registering the iterator as an observer.
  ##
  ## Use it to build snapshots or aggregate results without causing future
  ## updates to rerun the loop. If you need an effect to rerun when elements
  ## change, iterate with explicit `for i in 0 ..< len(rs): discard rs[i].val`
  ## so each element signal is tracked.

  for s in rs.data: yield s.val

proc `==`*[T](a: Signal[seq[T]], b: seq[T]): bool =
  a.val == b

proc `==`*[T](a: seq[T], b: Signal[seq[T]]): bool =
  a == b.val

proc useFrameScheduler*(
  ctx: ReactiveCtx,
  enqueueNextFrm: proc(cb: proc())
) =
  ## Installs a frame based reaction scheduler.
  ##
  ## ***Purpose***
  ##
  ## In games and UI toolkits work is often grouped by frames. With the frame
  ## scheduler all reactions that occur within the same frame are batched and
  ## flushed once at the start of the next frame tick.
  ##
  ## ***How to wire it***
  ##
  ## ```nim
  ## var tasks: seq[proc ()] = @[]
  ## useFrameScheduler(ctx, proc(cb: proc ()) = tasks.add cb)
  ##
  ## # later in the render loop
  ## for t in tasks: t()
  ## tasks.setLen 0
  ## ```
  ##
  ## The supplied `enqueueNextFrm` must place `cb` into a queue that the
  ## caller empties at the start of the next frame. The library calls
  ## `enqueueNextFrm` exactly once per frame no matter how many reactions
  ## were queued.
  ##
  ## ***Effects on the engine***
  ##
  ## - Multiple signal writes per frame cause only one flush and one
  ##   autosave, reducing redundant work.
  ## - `transaction(ctx)` blocks still coalesce as usual; the flush happens
  ##   on the next frame tick.
  ##
  ## ***Pitfalls***
  ##
  ## - Forgetting to run the queued callback each frame will stall the
  ##   reactive graph.
  ## - Calling `flushQueued(ctx)` manually is still possible but defeats the
  ##   batching benefit.

  ctx.framePending = false # clear any earlier reservation

  ctx.scheduler = proc(reaction: proc()) =
    let h = hash(reaction)
    if h notin ctx.queueHashes:
      ctx.queueHashes.incl h
      ctx.queue.add reaction

    # first reaction this frame → schedule a single flush
    if not ctx.framePending:
      ctx.framePending = true
      enqueueNextFrm proc =
        ctx.framePending = false # reset flag for the *next* frame
        flushQueued ctx

template updateLen[K, V](rt: ReactiveTable[K, V]) =
  let s = rt.lenS
  if s.value != rt.data.len:
    s.value = rt.data.len
    notifySubs s

template bumpRev[K, V](rt: ReactiveTable[K, V]) =
  let s = rt.revS
  s.value.inc
  notifySubs s

proc mutate[K, V](
  rt: ReactiveTable[K, V],
  op: proc (t: var Table[K, Signal[V]])
) =
  let c = rt.ctx
  if c.isDisposed: return

  let before = rt.data                  # value copy
  op rt.data                            # perform mutation
  rt.updateLen
  rt.bumpRev
  markDirty c

  if c.isUndoing or c.isRedoing: return

  # inside transaction → coalesce
  if c.batching > 0:
    if not rt.pendingActive:
      rt.pendingActive = true
      rt.pendingOld = before

      let commit = proc =
        if not rt.pendingActive: return
        let oldCopy = rt.pendingOld
        let newCopy = rt.data
        let entry: HistoryEntry = (
          undo: proc =
            rt.data = oldCopy
            rt.updateLen
            rt.bumpRev,
          redo: proc =
            rt.data = newCopy
            rt.updateLen
            rt.bumpRev
        )
        c.undoStack.add entry
        c.redoStack.setLen 0
        rt.pendingActive = false

      let h = hash(commit)
      if h notin c.queueHashes:
        c.queueHashes.incl h
        c.queue.add commit
    return

  # outside transaction
  let after = rt.data
  let entry: HistoryEntry = (
    undo: proc =
      rt.data = before
      rt.updateLen
      rt.bumpRev,
    redo: proc =
      rt.data = after
      rt.updateLen
      rt.bumpRev
  )
  c.undoStack.add entry
  c.redoStack.setLen 0

proc toReactive*[K, V](
  ctx: ReactiveCtx,
  src: Table[K, V]
): ReactiveTable[K, V] =
  ## Wraps an existing `Table[K, V]` in `ctx`, producing a `ReactiveTable`
  ## whose keys keep the same order and whose values are turned into
  ## individual signals. The original table is read once; subsequent edits
  ## must go through the `ReactiveTable` API to stay reactive.
  ##
  ## Structural and value edits made after wrapping participate in
  ## transactions, undo and redo, autosave, and the scheduler just like any
  ## table created from scratch.

  new result
  result.ctx = ctx
  for k, v in src:
    result.data[k] = mkSignal(ctx, v)
  result.lenS = signal(ctx, src.len)
  result.revS = signal(ctx, 0)

proc toPlain*[K, V](rt: ReactiveTable[K, V]): Table[K, V] =
  ## Returns a plain `Table[K, V]` that contains the current values of a
  ## `ReactiveTable`.  The result is a deep copy; mutating it does not affect
  ## the reactive table or its signals.
  result = initTable[K, V]()
  for k, sv in rt.data:
    result[k] = sv.val

proc `[]`*[K, V](rt: ReactiveTable[K, V], key: K): Signal[V] =
  if key in rt.data:
    return rt.data[key]

  # Key absent -> create default signal and register a structural insert
  let s = mkSignal(rt.ctx, default(V))
  rt.mutate proc(t: var Table[K, Signal[V]]) = t[key] = s
  s

proc put*[K, V](rt: ReactiveTable[K, V], key: K, val: V) =
  ## Adds or replaces `key` in the reactive table with `val`.
  ## If the key already exists the element signal is updated; otherwise a new
  ## element signal is created and a structural history entry is pushed.
  ## Effects watching `len(rt)` or `rev(rt)` rerun only when the key did not
  ## exist before.

  if key in rt.data:
    let sig = rt.data[key]
    let c = rt.ctx
    if c.batching > 0 and rt.pendingActive:
      # already collecting a structural commit → write raw, no history push
      if sig.value != val:
        sig.value = val
        notifySubs sig
        markDirty c
    else:
      sig.set val # normal signal write (with history)
  else:
    rt.mutate proc(t: var Table[K, Signal[V]]) =
      t[key] = mkSignal(rt.ctx, val)


proc delKey*[K, V](rt: ReactiveTable[K, V], key: K) =
  ## Removes `key` from the reactive table if it exists.
  ##
  ## ***Reactive effects***
  ##
  ## - Structural operation; bumps `rev(rt)` and updates `len(rt)`.
  ## - Pushes a history entry (or merges into an open transaction).
  ## - Effects that depend on length or on `rev(rt)` rerun.
  ##
  ## Keys that are not present are ignored and no history entry is added.

  rt.mutate proc(t: var Table[K, Signal[V]]) =
    tables.`del`(t, key)

proc clear*[K, V](rt: ReactiveTable[K, V]) =
  ## Removes all key-value pairs in one step.
  ##
  ## ***Effect on reactivity***
  ##
  ## - Pushes a single structural history entry (or joins the active
  ##   transaction).
  ## - Resets `len(rt)` to 0 and bumps `rev(rt)`.
  ## - Triggers any effects that depend on length or structure.

  rt.mutate (t: var Table[K, Signal[V]]) => t.clear()

template len*(rt: ReactiveTable): untyped =
  ## Returns the number of key-value pairs stored in a `ReactiveTable` and
  ## tracks the caller as a subscriber. Any structural change (put, delKey,
  ## clear) updates the length signal and reruns dependent effects or
  ## computed values. Ideal for counters and list-rendering logic that must
  ## stay in sync with the table’s size.

  registerDep rt.lenS
  rt.lenS.value

template rev*(rt: ReactiveTable): untyped =
  ## Returns the structural revision counter of a `ReactiveTable` and tracks
  ## the caller as a subscriber. The counter increments on every key set,
  ## key removal, or `clear`, allowing an effect to react to any structural
  ## mutation even when the entry count stays the same.
  registerDep rt.revS
  rt.revS.value

template peekLen*(rt: ReactiveTable): untyped =
  ## Returns the entry count of a `ReactiveTable` without tracking a
  ## dependency. Use when you need the value once and do not want the caller
  ## to rerun on structural changes.
  peek rt.lenS

template peekRev*(rt: ReactiveTable): untyped =
  ## Returns the revision counter of a `ReactiveTable` without tracking a
  ## dependency. Suitable for diagnostic logs or conditional code paths that
  ## should not trigger on future table mutations.
  peek rt.revS

iterator values*[K, V](rt: ReactiveTable[K, V]): V =
  ## Yields the plain value of every entry in a `ReactiveTable`, in key order
  ## as produced by the container’s iterator. Like `items` on `ReactiveSeq`
  ## this iterator does **not** establish dependencies; it simply reads
  ## `signal.val` for each stored value and returns the result.
  ##
  ## Ideal for one-off summarisation or JSON export. If you need live
  ## reactivity over the values, iterate keys and read `rt[key].val`
  ## explicitly so each value signal is tracked.
  for _, sv in rt.data: yield sv.val

proc `==`*[K, V](a: ReactiveTable[K, V], b: Table[K, V]): bool =
  if a.len != b.len: return false
  for k, sv in a.data:
    if k notin b or sv.val != b[k]:
      return false
  true

proc `==`*[K, V](a: Table[K, V], b: ReactiveTable[K, V]): bool = b == a

proc `[]=`*[K, V](rt: ReactiveTable[K, V], key: K, val: V) =
  if key in rt.data:
    rt.data[key].set val # element-level write
  else:
    rt.put(key, val) # falls back to structural mutator

proc `[]=`*[T](rs: ReactiveSeq[T], idx: int, v: T) =
  rs[idx].set v


# std/algorithm

template withSeq[T](rs: ReactiveSeq[T], body: untyped) =
  ## Executes `body` with a *mutable* alias `seqSig` that refers to the
  ## internal `seq[Signal[T]]`, wrapped in `rs.mutate` so the engine
  ## records one structural history entry and bumps `len` / `rev`.
  rs.mutate proc (seqSig {.inject.}: var seq[Signal[T]]) =
    body

proc sort*[T](
  rs: ReactiveSeq[T],
  cmp: proc (x, y: T): int  = cmp[T],
  order = SortOrder.Ascending
) =
  rs.withSeq:
    seqSig.sort(
      (a, b) => cmp(a.val, b.val),
      order
    )

proc reverse*[T](rs: ReactiveSeq[T]) =
  rs.withSeq: seqSig.reverse()

proc reverse*[T](
  rs: ReactiveSeq[T],
  first, last: Natural
) =
  ## Reverse the elements in the index range [first, last] inclusively,
  ## recording ONE structural history entry.
  rs.withSeq:
    reverse(seqSig, first, last)


proc rotateLeft*[T](rs: ReactiveSeq[T], dist: int): int {.discardable.} =
  rs.withSeq:
    discard rotateLeft(seqSig, dist)

proc fill*[T](rs: ReactiveSeq[T], value: T) =
  ## Overwrite every element with `value`, recording ONE undo entry.
  let ctx = rs.ctx
  if ctx.isDisposed: return

  var oldVals = newSeq[T](rs.data.len)
  for i, s in rs.data:
    oldVals[i] = s.value

  var changed = false
  for s in rs.data:
    if s.value != value:
      changed = true
      break
  if not changed: return # no write, no history

  ctx.isUndoing = true # temporary silencer
  for s in rs.data:
    s.value = value
    notifySubs s
  ctx.isUndoing = false
  markDirty ctx # autosave / scheduler wake-up

  let entry: HistoryEntry = (
    undo: (proc =
      ctx.isUndoing = true
      for i, s in rs.data:
        s.value = oldVals[i]
        notifySubs s
      ctx.isUndoing = false
      markDirty ctx),

    redo: (proc =
      ctx.isRedoing = true
      for s in rs.data:
        s.value = value
        notifySubs s
      ctx.isRedoing = false
      markDirty ctx)
  )

  ctx.undoStack.add entry
  ctx.redoStack.setLen 0


proc binarySearch*[T](rs: ReactiveSeq[T], key: T): int =
  discard rs.rev # track structural revisions
  binarySearch(
    rs.data,
    key,
    (a, b) => cmp(a.val, b)
  )

proc nextPermutation*[T](rs: ReactiveSeq[T]): bool {.discardable.} =
  var changed = false
  rs.withSeq:
    changed = nextPermutation(seqSig)
  if changed: rs.ctx.markDirty      # ensure autosave & effects
  changed                           # return the stdlib boolean

proc prevPermutation*[T](rs: ReactiveSeq[T]): bool {.discardable.} =
  var changed = false
  rs.withSeq:
    changed = prevPermutation(seqSig)
  if changed: rs.ctx.markDirty
  changed

proc rotateLeft*[T](
  rs: ReactiveSeq[T], slice: HSlice[int,int], dist: int
): int {.discardable.} =
  rs.withSeq:
    discard rotateLeft(seqSig, slice, dist)

proc sorted*[T](
  rs: ReactiveSeq[T],
  cmp: proc(x,y:T): int  = cmp[T],
  order = SortOrder.Ascending
): seq[T] =
  # no structural effects; just snapshot → sort copy → return
  result = newSeq[T](rs.len)
  for i,sig in rs.data: result[i] = sig.val
  result.sort(cmp, order)

proc sorted*[T](rs: ReactiveSeq[T], order: SortOrder): seq[T] =
  ## Convenience wrapper that matches `std/algorithm.sorted(a, order)`.
  ## Uses the default comparator for `T`.
  sorted[T](rs, cmp[T], order)

template cmpRaw(a, b: Signal): int = cmp(a.value, b.value)

proc isSorted*[T](
  rs: ReactiveSeq[T],
  order = SortOrder.Ascending
): bool =
  ## Dependency-tracked check that never touches `.val`
  discard rs.rev # keep caller reactive

  isSorted(
    rs.data,
    (a, b) => cmp[T](a.value, b.value),
    order
  )

proc isSorted*[T](
  rs: ReactiveSeq[T],
  cmp: (T, T) -> int,
  order = SortOrder.Ascending
): bool =
  discard rs.rev
  isSorted(
    rs.data,
    (a, b) => cmp(a.value, b.value),
    order
  )

proc lowerBound*[T,K](
  rs: ReactiveSeq[T],
  key: K,
  cmp: (T, K) -> int
): int =
  discard rs.rev
  lowerBound(
    rs.data,
    key,
    (a, k) => cmp(a.value, k)
  )

proc upperBound*[T,K](
  rs: ReactiveSeq[T],
  key: K,
  cmp: (T, K) -> int
): int =
  discard rs.rev
  upperBound(
    rs.data,
    key,
    (a, k) => cmp(a.value, k)
  )

proc lowerBound*[T](rs: ReactiveSeq[T], key:T): int =
  rs.lowerBound(key, cmp[T])

proc upperBound*[T](rs: ReactiveSeq[T], key:T): int =
  rs.upperBound(key, cmp[T])

proc `==`*[T](a: ReactiveSeq[T], b: seq[T]): bool =
  if a.len != b.len: return false
  for i in 0..<a.len:
    let s = a.data[i]
    if s.isNil or s.value != b[i]: # raw field, no registerDep
      return false
  true

proc `==`*[T](a: seq[T], b: ReactiveSeq[T]): bool =
  b == a  # symmetric

proc `==`*[T](a: seq[Signal[T]], b: seq[T]): bool =
  if a.len != b.len: return false
  for i in 0..<a.len:
    let s = a[i]
    if s.isNil or s.value != b[i]:
      return false
  true

proc `==`*[T](a: seq[T], b: seq[Signal[T]]): bool =
  b == a  # symmetric

# std/bitops

template makeComputed(s: Signal, e: untyped): untyped =
  s.ctx.computed(() => e)

template map1(s: Signal; f: untyped): untyped =
  ## Apply a unary function `f` to the current value of signal `s`
  ## and expose the result as a *new* computed signal.
  ##
  ## Usage:
  ##     let b = a.map1(x => x + 1)
  ##     #     ^ b is Signal[T] produced by f(a.val)
  makeComputed(s, (f)(s.val))

template map2(fn, a, b: untyped): untyped =
  ## Reactive view for any pure binary function.
  ## If both `a` and `b` are signals we track `a` (arbitrary choice) and
  ## read `b.val`; if `b` is a scalar the generated code still works.
  when compiles(b.val):
    makeComputed(a, fn(a.val, b.val))
  else:
    makeComputed(a, fn(a.val, b))

proc `not`*[T: SomeInteger](a: Signal[T]): Signal[T] =
  makeComputed(a, not a.val)

proc `and`*[T: SomeInteger](a, b: Signal[T]): Signal[T] =
  makeComputed(a, a.val and b.val)
proc `and`*[T: SomeInteger](a: Signal[T], b: T): Signal[T] =
  makeComputed(a, a.val and b)
proc `and`*[T: SomeInteger](a: T, b: Signal[T]): Signal[T] =
  makeComputed(b, a and b.val)

proc `or` *[T: SomeInteger](a, b: Signal[T]): Signal[T] =
  makeComputed(a, a.val or b.val)
proc `or` *[T: SomeInteger](a: Signal[T], b: T): Signal[T] =
  makeComputed(a, a.val or b)
proc `or` *[T: SomeInteger](a: T, b: Signal[T]): Signal[T] =
  makeComputed(b, a or b.val)

proc `xor`*[T: SomeInteger](a, b: Signal[T]): Signal[T] =
  makeComputed(a, a.val xor b.val)
proc `xor`*[T: SomeInteger](a: Signal[T], b: T): Signal[T] =
  makeComputed(a, a.val xor b)
proc `xor`*[T: SomeInteger](a: T, b: Signal[T]): Signal[T] =
  makeComputed(b, a xor b.val)

proc flippedBit*[T: SomeInteger](
  s: Signal[T],
  bit: BitsRange[T]
): Signal[T] =
  ## Reactive wrapper: always equals `s.val` with `bit` toggled.
  makeComputed(s, (var tmp = s.val; tmp.flipBit bit; tmp))

template mutateSignal[T](s: Signal[T], body: untyped) =
  ## 1. copy -> 2. run the mutator -> 3. write-back
  block:
    var tmp = s.val # (1) read  (tracks dependency)
    body(tmp)              # (2) mutate whatever way you like
    s.set tmp              # (3) single history entry

proc mask*[T: SomeInteger](s: Signal[T], mask: T) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.mask(v, mask))

proc mask*[T: SomeInteger](s: Signal[T], slice: Slice[int]) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.mask(v, slice))

proc setMask*[T: SomeInteger](s: Signal[T], mask: T) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.setMask(v, mask))

proc setMask*[T: SomeInteger](s: Signal[T], slice: Slice[int]) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.setMask(v, slice))

proc clearMask*[T: SomeInteger](s: Signal[T], mask: T) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.clearMask(v, mask))

proc clearMask*[T: SomeInteger](s: Signal[T], slice: Slice[int]) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.clearMask(v, slice))

proc flipMask*[T: SomeInteger](s: Signal[T], mask: T) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.flipMask(v, mask))

proc flipMask*[T: SomeInteger](s: Signal[T], slice: Slice[int]) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.flipMask(v, slice))

proc setBit*[T: SomeInteger](s: Signal[T], bit: BitsRange[T]) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.setBit(v, bit))

proc clearBit*[T: SomeInteger](s: Signal[T], bit: BitsRange[T]) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.clearBit(v, bit))

proc flipBit*[T: SomeInteger](s: Signal[T], bit: BitsRange[T]) =
  mutateSignal(s, proc (v: var VT(s)) = bitops.flipBit(v, bit))

proc rotateLeftBits*[T: SomeUnsignedInt](
  s: Signal[T]; shift: range[0 .. sizeof(T)*8]
) =
  mutateSignal(s, (v) => v = bitops.rotateLeftBits(v, shift))

proc rotateRightBits*[T: SomeUnsignedInt](
  s: Signal[T]; shift: range[0 .. sizeof(T)*8]
) =
  mutateSignal(s, proc (v: var VT(s)) =
    v = bitops.rotateRightBits(v, shift))

proc testBit*[T: SomeInteger](
  s: Signal[T];               # the integer signal
  bit: BitsRange[T]           # which bit to inspect
): Signal[bool] =
  ## A computed signal that is **true** when the chosen bit of `s`
  ## is 1.  Re-computes automatically whenever `s` changes.
  makeComputed(s, s.val.testBit(bit))

proc popcount*[T: SomeInteger](s: Signal[T]): int =
  s.val.popcount

proc parityBits*[T: SomeInteger](s: Signal[T]): int =
  s.val.parityBits

proc bitsliced*[T: SomeInteger](s: Signal[T]; slice: Slice[int]): Signal[T] =
  ## Pure computed view returning the sliced value without modifying `s`.
  makeComputed(s, bitops.bitsliced(s.val, slice))

template VT(s: Signal): typedesc = typeof(s.val)

proc rotateLeftBits*[T: SomeUnsignedInt](
  s: Signal[T]; k: int
): Signal[T] =
  ## Left-rotate *k* bits (k is wrapped into valid range).
  let bits = sizeof(T) * 8
  let kk   = (k mod bits + bits) mod bits  # ensure 0 ≤ kk < bits
  makeComputed(s, bitops.rotateLeftBits(s.val, kk))

proc rotateRightBits*[T: SomeUnsignedInt](
  s: Signal[T]; k: int
): Signal[T] =
  let bits = sizeof(T) * 8
  let kk   = (k mod bits + bits) mod bits
  makeComputed(s, bitops.rotateRightBits(s.val, kk))

proc bitslice*[T: SomeInteger](s: Signal[T]; slice: Slice[int]) =
  ## Condenses bits from `slice` **keeping the LSB at `slice.a`
  ##   (matches the expectation of your test-suite).**
  mutateSignal(s, proc (v: var VT(s)) =
    var res: T = 0
    let width = slice.b - slice.a + 1
    for i in countdown(width - 1, 0):              # high → low
      res = (res shl 1) or ((v shr (slice.a + i)) and 1.T)
    v = res)

# std/complex

template withVarCpx(T, body: untyped) =
  ## Shorthand for the big, repetitive “var Complex[T]” parameter list.
  mutateSignal(s, proc (v {.inject.}: var complex.Complex[T]) = body)

proc `+=`*[T](s: Signal[complex.Complex[T]], rhs: complex.Complex[T]) =
  withVarCpx(T): v += rhs

proc `-=`*[T](s: Signal[complex.Complex[T]], rhs: complex.Complex[T]) =
  withVarCpx(T): v -= rhs

proc `*=`*[T](s: Signal[complex.Complex[T]], rhs: complex.Complex[T]) =
  withVarCpx(T): v *= rhs

proc `/=`*[T](s: Signal[complex.Complex[T]], rhs: complex.Complex[T]) =
  withVarCpx(T): v /= rhs

proc `+`*[T](
  a, b: Signal[complex.Complex[T]]
): Signal[complex.Complex[T]] =
  makeComputed(a, a.val + b.val)

proc `+`*[T](
  a: Signal[complex.Complex[T]], b: complex.Complex[T]
): Signal[complex.Complex[T]] =
  makeComputed(a, a.val + b)

proc `+`*[T](
  a: complex.Complex[T], b: Signal[complex.Complex[T]]
): Signal[complex.Complex[T]] =
  makeComputed(b, a + b.val)

proc `-`*[T](
  a, b: Signal[complex.Complex[T]]
): Signal[complex.Complex[T]] =
  makeComputed(a, a.val - b.val)

proc `-`*[T](
  a: Signal[complex.Complex[T]], b: complex.Complex[T]
): Signal[complex.Complex[T]] =
  makeComputed(a, a.val - b)

proc `-`*[T](
  a: complex.Complex[T], b: Signal[complex.Complex[T]]
): Signal[complex.Complex[T]] =
  makeComputed(b, a - b.val)

proc `*`*[T](
  a, b: Signal[complex.Complex[T]]
): Signal[complex.Complex[T]] =
  makeComputed(a, a.val * b.val)

proc `*`*[T](
  a: Signal[complex.Complex[T]], b: complex.Complex[T]
): Signal[complex.Complex[T]] =
  makeComputed(a, a.val * b)

proc `*`*[T](
  a: complex.Complex[T], b: Signal[complex.Complex[T]]
): Signal[complex.Complex[T]] =
  makeComputed(b, a * b.val)

proc `/`*[T](
  a, b: Signal[complex.Complex[T]]
): Signal[complex.Complex[T]] =
  makeComputed(a, a.val / b.val)

proc `/`*[T](
  a: Signal[complex.Complex[T]], b: complex.Complex[T]
): Signal[complex.Complex[T]] =
  makeComputed(a, a.val / b)

proc `/`*[T](
  a: complex.Complex[T], b: Signal[complex.Complex[T]]
): Signal[complex.Complex[T]] =
  makeComputed(b, a / b.val)

proc `-`*[T](s: Signal[complex.Complex[T]]): Signal[complex.Complex[T]] =
  ## Unary minus
  makeComputed(s, -s.val)

template defUnaryComplex*(fname: untyped) =
  proc fname*[T](
    s: Signal[complex.Complex[T]]
  ): Signal[complex.Complex[T]] {.inline.} =
    makeComputed(s, fname[T](s.val))

defUnaryComplex(conjugate)
defUnaryComplex(inv)
defUnaryComplex(sgn)
defUnaryComplex(sqrt)
defUnaryComplex(exp)
defUnaryComplex(ln)
defUnaryComplex(log10)
defUnaryComplex(log2)
defUnaryComplex(sin)
defUnaryComplex(arcsin)
defUnaryComplex(cos)
defUnaryComplex(arccos)
defUnaryComplex(tan)
defUnaryComplex(arctan)
defUnaryComplex(cot)
defUnaryComplex(arccot)
defUnaryComplex(sec)
defUnaryComplex(arcsec)
defUnaryComplex(csc)
defUnaryComplex(arccsc)
defUnaryComplex(sinh)
defUnaryComplex(arcsinh)
defUnaryComplex(cosh)
defUnaryComplex(arccosh)
defUnaryComplex(tanh)
defUnaryComplex(arctanh)
defUnaryComplex(coth)
defUnaryComplex(arccoth)
defUnaryComplex(sech)
defUnaryComplex(arcsech)
defUnaryComplex(csch)
defUnaryComplex(arccsch)

proc abs*[T](s: Signal[complex.Complex[T]]): Signal[T] =
  ## | `result = abs(s.val)`  (reactive)
  makeComputed(s, complex.abs(s.val))

proc abs2*[T](s: Signal[complex.Complex[T]]): Signal[T] =
  ## | `result = abs2(s.val)` (squared magnitude)
  makeComputed(s, complex.abs2(s.val))

proc phase*[T](s: Signal[complex.Complex[T]]): Signal[T] =
  ## | `result = phase(s.val)` (argument / angle)
  makeComputed(s, complex.phase(s.val))

proc rect*[T](r, phi: Signal[T]): Signal[complex.Complex[T]] =
  ## reactive polar-to-rect; recomputes when **either** r or phi changes
  makeComputed(r, complex.rect(r.val, phi.val))

proc almostEqual*[T](
  a, b: Signal[complex.Complex[T]],
  ulp: Natural = 4
): Signal[bool] =
  makeComputed(a, complex.almostEqual(a.val, b.val, ulp))

proc radius*[T](z: Signal[complex.Complex[T]]): Signal[T] =
  ## | always equals `abs(z.val)` (== z.val.polar.r)
  let pol = map1(complex.polar, z)     # Signal[tuple[r,phi]]
  makeComputed(pol, pol.val.r)         # 1st param _is_ the Signal!

proc polar*[T](
  z: Signal[complex.Complex[T]]
): Signal[tuple[r, phi: T]] =
  ## Reactive polar-coordinates view of a complex signal.
  ## Always equals `z.val.polar` and re-computes when `z` changes.
  makeComputed(z, z.val.polar)

template makeBinPow(body: untyped): untyped =
  makeComputed(s, body)

proc pow*[T](
  s: Signal[complex.Complex[T]],
  y: complex.Complex[T]
): Signal[complex.Complex[T]] {.inline.} =
  ## Reactive:   rs.pow(otherComplex)
  makeComputed(s, complex.pow[T](s.val, y))

proc pow*[T](
  s: Signal[complex.Complex[T]],
  y: T
): Signal[complex.Complex[T]] {.inline.} =
  ## Reactive:   rs.pow(realExponent)
  makeComputed(s, complex.pow[T](s.val, y))

# std/json

proc toReactive*(ctx: ReactiveCtx; n: JsonNode): JsonNodeReactive =
  ## Recursively wraps `n` into a signal-backed tree owned by `ctx`.
  new result
  result.ctx  = ctx
  result.kind = signal(ctx, n.kind)

  case n.kind
  of JObject:
    result.fields = ctx.toReactive(initTable[string, JsonNodeReactive]())
    for k, v in n.fields:
      result.fields.put(k, ctx.toReactive(v))

  of JArray:
    result.items = ctx.toReactive(newSeq[JsonNodeReactive]())
    for v in n.elems:
      result.items.push ctx.toReactive(v)

  of JString:
    result.strVal = signal(ctx, n.str)

  of JInt:
    result.intVal = signal(ctx, int(n.num))

  of JFloat:
    result.floatVal = signal(ctx, n.fnum)

  of JBool:
    result.boolVal = signal(ctx, n.bval)

  of JNull:
    discard                 # nothing else to store

proc toPlain*(jr: JsonNodeReactive): JsonNode =
  ## Builds a fresh **immutable** `JsonNode` snapshot of the current state.
  case jr.kind.val
  of JObject:
    var t = initOrderedTable[string, JsonNode]()
    for k, sigChild in jr.fields.data:
      t[k] = sigChild.val.toPlain()     # recurse
    result = %*t

  of JArray:
    var a: seq[JsonNode] = @[]
    for sigChild in jr.items.data:
      a.add sigChild.val.toPlain()
    result = %*a

  of JString: result = newJString(jr.strVal.val)
  of JInt:    result = newJInt   (jr.intVal.val.BiggestInt)
  of JFloat:  result = newJFloat (jr.floatVal.val)
  of JBool:   result = newJBool  (jr.boolVal.val)
  of JNull:   result = newJNull()

proc newReactiveNull*(ctx: ReactiveCtx): JsonNodeReactive =
  ## Build an empty reactive node that represents JSON null.
  new result
  result.ctx      = ctx
  result.kind     = signal(ctx, JNull)       # base kind
  result.fields   = ctx.toReactive(initTable[string, JsonNodeReactive]())
  result.items    = ctx.toReactive(newSeq[JsonNodeReactive]())
  result.strVal   = signal(ctx, "")
  result.intVal   = signal(ctx, 0)
  result.floatVal = signal(ctx, 0.0)
  result.boolVal  = signal(ctx, false)

template getField*(jr: JsonNodeReactive; k: string): JsonNodeReactive =
  ## Read *(and auto-create)* an object member as a reactive node.
  jr.fields[k]            # ReactiveTable auto-creates a default node

template push*(jr: JsonNodeReactive; child: JsonNodeReactive) =
  ## Append a child to an array-node.
  jr.items.push child

template `[]`*(n: Signal[JsonNodeReactive]): untyped = n.val

template `[]`*(n: Signal[JsonNodeReactive], key: string): untyped =
  n.val.fields[key]          # still a Signal[JsonNodeReactive]

template `[]`*(n: Signal[JsonNodeReactive], idx: int): untyped =
  n.val.items[idx]

template `[]`*(n: JsonNodeReactive; key: string): untyped =
  n.fields[key]

template `[]`*(n: JsonNodeReactive; idx: int): untyped =
  n.items[idx]

template intVal*(n: JsonNodeReactive): Signal[int]    = n.intVal
template floatVal*(n: JsonNodeReactive): Signal[float]  = n.floatVal
template strVal*(n: JsonNodeReactive): Signal[string] = n.strVal
template boolVal*(n: JsonNodeReactive): Signal[bool]   = n.boolVal

template intVal*(n: Signal[JsonNodeReactive]): Signal[int]    = n.val.intVal
template floatVal*(n: Signal[JsonNodeReactive]): Signal[float]  = n.val.floatVal
template strVal*(n: Signal[JsonNodeReactive]): Signal[string] = n.val.strVal
template boolVal*(n: Signal[JsonNodeReactive]): Signal[bool]   = n.val.boolVal

proc copyFrom(dst, src: JsonNodeReactive) =
  ## internal helper: copy *content* of src into dst (same ctx!)
  dst.kind.set src.kind.val

  case src.kind.val
  of JObject:
    # replace key set
    dst.fields.set(src.fields.toPlain())   # ReactiveTable already has `set`
  of JArray:
    dst.items.set(src.items.toPlain())     # ReactiveSeq already has `set`
  of JString:
    dst.strVal.set src.strVal.val
  of JInt:
    dst.intVal.set src.intVal.val
  of JFloat:
    dst.floatVal.set src.floatVal.val
  of JBool:
    dst.boolVal.set src.boolVal.val
  of JNull:
    discard                                 # nothing else to copy

proc set*(self: JsonNodeReactive, src: JsonNode) =
  ## Overwrite the *reactive* node with the contents of a plain JsonNode.
  let rsrc = self.ctx.toReactive(src)       # tmp wrapper in same ctx
  transaction(self.ctx):
    self.copyFrom(rsrc)

# system/seq ops

template concatSeqs[T](lhs: ReactiveSeq[T]; rhs: seq[T]) =
  ## Internal helper: append *all* elements of `rhs` to `lhs`
  lhs.mutate proc (s: var seq[Signal[T]]) =
    for v in rhs:
      s.add mkSignal(lhs.ctx, v)

proc `&`*[T](a: ReactiveSeq[T]; b: seq[T]): ReactiveSeq[T] =
  let merged = a.toPlain() & b          # @[ ... ] & seq
  result = a.ctx.toReactive(merged)

proc `&`*[T](a: seq[T]; b: ReactiveSeq[T]): ReactiveSeq[T] =
  let merged = a & b.toPlain()          # seq & @[ ... ]
  result = b.ctx.toReactive(merged)

proc `&`*[T](a: ReactiveSeq[T]; elem: T): ReactiveSeq[T] =
  let merged = a.toPlain() & @[elem]
  result = a.ctx.toReactive(merged)

proc `&`*[T](elem: T; b: ReactiveSeq[T]): ReactiveSeq[T] =
  let merged = @[elem] & b.toPlain()
  result = b.ctx.toReactive(merged)

proc `&=`*[T](rs: ReactiveSeq[T]; rhs: seq[T]) =
  rs.concatSeqs rhs                        # one structural mutation

proc `&=`*[T](rs: ReactiveSeq[T]; elem: T) =
  rs.push elem                             # single push → correct batching