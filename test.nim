import std / [
  unittest,
  math
]

include signals

type
  Player = object
    name*: string
    hp*  : int
    x*, y*: float

  Vec2 = object
    x*, y*: float

  StatsD = object
    hp*, mp*: int
    pos*     : Vec2

  PlayerD = object
    name* : string
    stats*: StatsD

  Complex = object
    id*  : int
    tags*: seq[string]
    data*: Table[string,int]
    pos* : Vec2

type
  ## 1)  “field-list” object (x*, y: float)
  MT = object
    x*, y: float              ## two fields in a single IdentDefs

  ## 2)  object with default-initialised field
  BT = object
    foo*: int = 42

  ## 3)  variant object referencing an enum
  ColorSpace* = enum csSRGB, csDisplayP3
  QT = object
    case space*: ColorSpace    ## variant discriminator
    of csSRGB:
      rgb*: array[3, float]
    of csDisplayP3:
      p3*: array[3, float]

reactive(MT)
reactive(BT)
reactive(QT)

reactive(Player)
reactive(Vec2)
reactive(StatsD)
reactive(PlayerD)
reactive(Complex)


# kitchen sink for reactive compiler macro

type
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

reactive(CoreStatsX)
reactive(MonsterX)


proc pushFront[T](rs: ReactiveSeq[T], x: T) = rs.insert(0, x)

let C = newReactiveCtx()

suite "reactive core without [] overloads":

  test "basic get / set / ==":
    var s = signal(C, 0)
    doAssert s == 0
    s.set 7
    doAssert s == 7

  test "Computed propagates":
    var base = signal(C, 3)

    let twice = computed(C, () =>
      base.val * 2)

    check twice == 6
    base.set 5
    check twice == 10

  test "Effect re-runs and batching coalesces":
    var x = signal(C, 0)
    var hits = 0
    effect(C, proc =
      registerDep x
      inc hits
    )

    check hits == 1                 # initial run

    x.set 1
    check hits == 2                 # immediate scheduler: runs again

    transaction(C):                   # two writes, one batch
      x.set 2
      x.set 3
    check hits == 3                 # only one more run

  test "Undo rolls back last writes":
    var hp = signal(C, 100)
    hp.set 50
    hp.set 20
    undo(C)            # one step
    check hp == 50
    undo(C, 1)         # another step
    check hp == 100

  test "Queued scheduler defers effects until flush":
    let Q = newReactiveCtx()
    var v = signal(Q, 0)
    var log: seq[int]

    effect(Q, proc =
      registerDep v
      log.add v.val
    )

    useQueuedScheduler(Q)

    v.set 1
    check log[^1] == 0              # still old value

    flushQueued Q
    check log[^1] == 1

  test "Watch runs only on change and respects batching":
    var a = signal(C, 0)
    var seen: seq[int] = @[]

    watch(C,
      () => a.val,       # selector  (arrow, single line)
      proc (n, o: int) = # handler   (proc =, multi-line)
        seen.add n
    )

    a.set 1
    check seen == @[1]

    a.set 1 # no actual change
    check seen == @[1]

    transaction(C): # two writes in one batch
      a.set 2
      a.set 3
    check seen == @[1, 3] # fired once with final value

  test "dumpDeps lists all subscribers":
    var a    = signal(C, 0)

    let dbl  = computed(C, () => a.val * 2)   # 1 subscriber

    effect(C, proc =                          # 2nd subscriber
      registerDep a
    )

    let deps = dumpDeps(a)
    check deps.len == 2
    echo "dumpDeps lists all subscribers: ", deps

  test "Effect cleanup runs before next execution":
    var tgt   = signal(C, 0)
    var fired = 0

    effect(C, proc (onCleanup: AddCleanup) =
      onCleanup(() => inc fired)   # register cleanup   (single-line lambda)
      registerDep tgt              # read signal → dependency
    )

    check fired == 0             # first mount → no cleanup yet

    tgt.set 1
    check fired == 1

    tgt.set 2
    check fired == 2

  test "Disposing context stops further reactions":
    let D = newReactiveCtx()
    var s    = signal(D, 0)
    var hits = 0

    effect(D, proc =
      registerDep s
      inc hits
    )

    check hits == 1          # initial run
    s.set 1
    check hits == 2          # reacted once

    dispose D                  # tear it down

    s.set 2
    check hits == 2          # no further reactions
    flushQueued D              # queue is already cleared
    check hits == 2

  test "Memo recomputes only on dependency change":
    var base     = signal(C, 1)
    var computes = 0

    let dbl = memo(C, proc: int =
      inc computes
      base.val * 2
    )

    check dbl() == 2
    check computes == 1

    discard dbl()
    check computes == 1

    base.set 2
    check dbl() == 4
    check computes == 2

suite "Undo-stack utilities":

  test "Depth grows with every write":
    let U = newReactiveCtx()
    var s = signal(U, 0)
    check undoDepth(U) == 0
    s.set 1
    check undoDepth(U) == 1
    s.set 2
    check undoDepth(U) == 2

  test "Depth does not grow on identical write":
    let U = newReactiveCtx()
    var s = signal(U, 1)
    s.set 1             # no change
    check undoDepth(U) == 0

  test "Undo reduces depth":
    let U = newReactiveCtx()
    var s = signal(U, 0)
    s.set 1; s.set 2
    undo(U)             # one step
    check undoDepth(U) == 1
    undo(U)             # another
    check undoDepth(U) == 0

  test "Undo more than stack is safe":
    let U = newReactiveCtx()
    var s = signal(U, 0)
    s.set 1
    undo(U, 5)          # exceeds depth
    check undoDepth(U) == 0
    check s == 0

  test "clearUndo wipes history":
    let U = newReactiveCtx()
    var s = signal(U, 0)
    s.set 1; s.set 2
    clearUndo(U)
    check undoDepth(U) == 0
    undo(U)             # no effect
    check s == 2

  test "Undo works inside transaction":
    let U = newReactiveCtx()
    var s = signal(U, 0)
    transaction(U):
      s.set 1
      s.set 2
    check undoDepth(U) == 2   # each write pushed
    undo(U, 2)
    check s == 0

  test "Undo stack is per-context":
    let A = newReactiveCtx()
    let B = newReactiveCtx()
    var sa = signal(A, 0)
    var sb = signal(B, 0)
    sa.set 1
    sb.set 1; sb.set 2
    check undoDepth(A) == 1
    check undoDepth(B) == 2

  test "Reads do not affect depth":
    let U = newReactiveCtx()
    var s = signal(U, 0)
    discard s.val
    check undoDepth(U) == 0

  test "Undo after dispose does nothing":
    let U = newReactiveCtx()
    var s = signal(U, 0)
    s.set 1
    dispose U
    undo(U)
    check s == 1
    check undoDepth(U) == 0

  test "Undo stack unaffected by memo reads":
    let U = newReactiveCtx()
    var s = signal(U, 1)
    let dbl = memo(U, () => s.val * 2)
    discard dbl()
    check undoDepth(U) == 0

  test "Depth grows only for first write to same value sequence":
    let U = newReactiveCtx()
    var s = signal(U, 0)
    s.set 1
    s.set 1          # identical: ignored
    s.set 1          # identical again
    check undoDepth(U) == 1

suite "Memo behaviour":

  test "Memo computes exactly once at creation":
    let M = newReactiveCtx()
    var src = signal(M, 1)
    var cnt = 0
    let mm  = memo(M, proc: int =
      inc cnt
      src.val)

    check cnt == 1            # initial evaluation done by effect

  test "Repeated reads do not recompute":
    let M = newReactiveCtx()
    var src = signal(M, 1)
    var cnt = 0
    let mm  = memo(M, proc: int =
      inc cnt
      src.val)

    discard mm()
    discard mm()
    check cnt == 1            # still cached

  test "Recompute only when value really changes":
    let M = newReactiveCtx()
    var src = signal(M, 1)
    var cnt = 0
    let mm  = memo(M, proc: int =
      inc cnt
      src.val)

    src.set 1                   # identical – ignored
    check cnt == 1
    src.set 2                   # change
    check cnt == 2

  test "Batched writes trigger one recompute":
    let M = newReactiveCtx()
    var src = signal(M, 0)
    var cnt = 0
    let mm  = memo(M, proc: int =
      inc cnt
      src.val)

    transaction(M):
      src.set 1
      src.set 2
    check cnt == 2            # exactly one extra evaluation

  test "Memo with two dependencies reacts to either":
    let M = newReactiveCtx()
    var a   = signal(M, 1)
    var b   = signal(M, 2)
    var cnt = 0
    let sum = memo(M, proc: int =
      inc cnt
      a.val + b.val)

    a.set 5
    check cnt == 2            # once more
    b.set 7
    check cnt == 3

  test "Nested memo cascades once":
    let M = newReactiveCtx()
    var src = signal(M, 1)
    var innerCnt = 0
    var outerCnt = 0

    let inner = memo(M, proc: int =
      inc innerCnt
      src.val * 2)

    let outer = memo(M, proc: int =
      inc outerCnt
      inner() + 1)

    src.set 2
    check innerCnt == 2       # inner recomputed once
    check outerCnt == 2       # outer recomputed once

  test "Dispose stops further memo recompute":
    let D = newReactiveCtx()
    var src = signal(D, 1)
    var cnt = 0
    let mm  = memo(D, proc: int =
      inc cnt
      src.val)

    dispose D
    src.set 2
    check cnt == 1            # no new evaluation after dispose

  test "Undo stack unaffected by memo reads":
    let U = newReactiveCtx()
    var src = signal(U, 1)
    let mm  = memo(U, () => src.val * 2)

    discard mm()
    check undoDepth(U) == 0

suite "Time-travel":
  test "Redo restores state after undo":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1; s.set 2
    undo(T)           # -> 1
    redo(T)           # -> 2
    check s == 2
    check redoDepth(T) == 0
    check undoDepth(T) == 2

  test "Redo cleared by new write":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1; s.set 2
    undo(T)           # -> 1
    s.set 3           # new branch
    check redoDepth(T) == 0

  test "Multiple redo steps":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1; s.set 2; s.set 3
    undo(T, 3)
    redo(T, 2)
    check s == 2

  test "Snapshot / travel backward":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1
    let snap = snapshot(T)
    s.set 2; s.set 3
    travel(T, snap)             # back to value 1
    check s == 1

  test "Travel forward again":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1
    let idx = snapshot(T)        # 1 write in history
    s.set 2
    travel(T, idx)               # back to 1
    travel(T, idx + 1)           # forward to 2
    check s == 2

  test "Travel beyond bounds is no-op":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1
    travel(T, -1)
    travel(T, 999)
    check s == 1
    check undoDepth(T) == 1

  test "Dispose prevents further undo/redo":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1
    dispose T
    undo(T)
    redo(T)
    check s == 1

  test "Undo then write clears redo stack":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1; s.set 2
    undo(T)                      # back to 1
    s.set 5                      # new write
    check redoDepth(T) == 0

  test "Undo/redo inside transaction":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    transaction(T):
      s.set 1
      s.set 2
    undo(T, 2)
    redo(T, 2)
    check s == 2

  test "Memo value follows time travel":
    let T = newReactiveCtx()
    var src = signal(T, 1)
    let dbl = memo(T, () => src.val * 2)
    src.set 2
    undo(T)                      # back to 1
    check dbl() == 2
    redo(T)                      # forward to 2
    check dbl() == 4

  test "dumpDeps unaffected by history ops":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    let _ = computed(T, () => s.val * 2)
    let before = dumpDeps(s)
    s.set 1
    undo(T); redo(T)
    check dumpDeps(s) == before

suite "Travel edge-cases":

  test "Travel to current index is a no-op":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1; s.set 2            # depth = 2, index 2
    let here = snapshot(T)
    travel(T, here)             # same spot
    check s == 2
    check undoDepth(T) == here
    check redoDepth(T) == 0

  test "Travel on fresh context does nothing":
    let T = newReactiveCtx()
    travel(T, 0)
    travel(T, 1)
    check undoDepth(T) == 0
    check redoDepth(T) == 0

  test "Travel forward then back adjusts redo depth":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1; s.set 2; s.set 3            # depth = 3
    travel(T, 1)                         # back to value 1
    check s == 1
    check redoDepth(T) == 2
    travel(T, 3)                         # forward to tip
    check s == 3
    check redoDepth(T) == 0

  test "Branch after travelling back clears redo":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1; s.set 2
    travel(T, 1)                         # back to 1
    s.set 42                             # new branch
    check redoDepth(T) == 0
    redo(T)                              # should do nothing
    check s == 42

  test "Travel index exactly at far forward bound":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1; s.set 2
    let bound = undoDepth(T)             # 2
    undo(T, 2)                           # back to 0
    travel(T, bound)                     # jump to newest
    check s == 2
    check redoDepth(T) == 0

  test "Travel inside transaction is applied after flush":
    let T = newReactiveCtx()
    var s = signal(T, 0)
    s.set 1; s.set 2; s.set 3            # depth 3
    transaction(T):
      travel(T, 1)                       # request back to value 1
    check s == 1
    check undoDepth(T) == 1
    check redoDepth(T) == 2

suite "Writable computed":

  test "Setting writable computed updates source":
    let W = newReactiveCtx()
    var hp    = signal(W, 50.0)          # current hit-points
    var maxHp = signal(W, 100.0)         # maximum

    let pct = computed(W,
      getter = () => hp.val / maxHp.val,
      setter = (p: float) => hp.set p * maxHp.val
    )

    check pct.val == 0.5
    pct.set 0.75                       # write through computed
    check hp == 75
    check pct.val == 0.75            # reflects the change

  test "Undo rolls back writable computed change":
    let W = newReactiveCtx()
    var hp    = signal(W, 30.0)
    var maxHp = signal(W, 100.0)

    let pct = computed(W,
      getter = () => hp.val / maxHp.val,
      setter = (p: float) => hp.set p * maxHp.val
    )

    pct.set 0.6                        # hp → 60
    check hp == 60
    undo(W)                            # time-travel one step
    check hp == 30
    check pct.val == 0.3

suite "Persistence":

  test "Manual save / load round-trip":
    let P = newReactiveCtx()
    var j: JsonNode
    var hp = store(P, "hp", 100)
    hp.set 42
    saveState(P, j)
    check j["hp"].getInt == 42

    hp.set 7               # mutate again
    loadState(P, j)        # restore → 42
    check hp == 42

  test "Autosave after flush":
    let P = newReactiveCtx()
    var doc: JsonNode
    var score = store(P, "score", 0)

    enableAutosave(P, doc)
    score.set 10
    flushQueued P

    check doc["score"].getInt == 10

  test "Autosave after batched transaction":
    let P = newReactiveCtx()
    var doc: JsonNode
    var s = store(P, "x", 0)
    enableAutosave(P, doc)

    transaction(P):
      s.set 5
      s.set 8                # final value

    check doc["x"].getInt == 8

  test "Dispose triggers final save":
    let P = newReactiveCtx()
    var doc: JsonNode
    var v = store(P, "val", 1)
    enableAutosave(P, doc)
    v.set 4
    dispose P

    check doc["val"].getInt == 4

  test "registerStore works for external signal":
    let P = newReactiveCtx()
    var doc: JsonNode
    var name = signal(P, "anon")
    registerStore(P, "player", name)
    enableAutosave(P, doc)

    name.set "alice"
    flushQueued P
    check doc["player"].getStr == "alice"

suite "Edge‑case combinatorial stress":
  #  1 ── autosave after enable→disable→enable again -------------------------
  test "Enable/disable/enable autosave updates target":
    let C = newReactiveCtx()
    var A, B: JsonNode
    var s = store(C, "v", 0)
    enableAutosave(C, A)
    s.set 1
    flushQueued C
    check A["v"].getInt == 1

    disableAutosave(C)
    s.set 2
    flushQueued C
    check A["v"].getInt == 1            # unchanged

    enableAutosave(C, B)                   # new target
    s.set 3
    flushQueued C
    check B["v"].getInt == 3
    check A["v"].getInt == 1            # old target untouched

  #  2 ── undo after autosave restores pre‑autosave value --------------------
  test "Undo reverses value saved by autosave":
    let C = newReactiveCtx()
    var doc: JsonNode
    var s = store(C, "hp", 10)
    enableAutosave(C, doc)
    s.set 20             # change → autosave
    flushQueued C
    check doc["hp"].getInt == 20

    undo(C)              # back to 10
    check s == 10
    flushQueued C        # autosave again
    check doc["hp"].getInt == 10

  #  3 ── loadState inside a transaction coalesces one history push ----------
  test "loadState inside transaction pushes once":
    let C = newReactiveCtx()
    var s = store(C, "score", 0)
    var snap: JsonNode
    s.set 5
    saveState(C, snap)        # { score: 5 }

    transaction(C):
      loadState(C, snap)      # should revert to 5 (already is)
    check undoDepth(C) == 1 # only the earlier set(5)

  #  4 ── writable computed + autosave + undo -------------------------------
  test "Writable computed integrates with autosave & undo":
    let C = newReactiveCtx()
    var storeDoc: JsonNode
    var val  = store(C, "base", 2)
    let wc = computed(C,
      getter = () => val.val * 2,
      setter = proc(x: int) = val.set x div 2)

    enableAutosave(C, storeDoc)

    wc.set 20                # base becomes 10
    flushQueued C
    check val == 10
    check wc.val == 20
    check storeDoc["base"].getInt == 10

    undo(C)
    check val == 2
    flushQueued C
    check storeDoc["base"].getInt == 2

  #  5 ── dispose during batched transaction still snapshots -----------------
  test "Dispose inside transaction takes final snapshot":
    let C = newReactiveCtx()
    var js: JsonNode
    var x = store(C, "x", 0)

    enableAutosave(C, js)
    transaction(C):
      x.set 99
      dispose C                 # should flush & snapshot
    check js["x"].getInt == 99

  #  6 ── loadState with missing keys leaves existing values -----------------
  test "loadState ignores unknown keys":
    let C = newReactiveCtx()
    var foo = store(C, "foo", 1)
    var j = %*{ "bar": 42 }
    loadState(C, j)
    check foo == 1

  #  7 ── registerStore twice for same key keeps latest state ----------------
  test "Double registration same key persists most recent":
    let C = newReactiveCtx()
    var doc: JsonNode
    var a = store(C, "k", 1)
    var b = signal(C, 99)
    registerStore(C, "k", b)        # b shadows a
    enableAutosave(C, doc)
    b.set 77
    flushQueued C
    check doc["k"].getInt == 77

  #  8 ── deep undo/redo after loadState snapshot ---------------------------
  test "Undo after loadState toggles between snapshots":
    let C = newReactiveCtx()
    var n = store(C, "num", 0)
    var j: JsonNode

    n.set 5
    saveState(C, j)            # snapshot at 5
    n.set 9                     # advance to 9

    loadState(C, j)            # back to 5 via setters
    check n == 5
    undo(C)                    # should go to 9
    check n == 9

  #  9 ── autosave with immediate scheduler when no batch -------------------
  test "Autosave fires even if queue empty":
    let C = newReactiveCtx()
    var doc: JsonNode
    var s = store(C, "z", 1)
    enableAutosave(C, doc)
    flushQueued C              # nothing changed yet → still 1
    check doc["z"].getInt == 1

  # 10 ── time travel after autosave-----------------------------------------
  test "Travel adjusts autosave correctly":
    let C = newReactiveCtx()
    var js: JsonNode
    var s = store(C, "v", 0)
    s.set 1; s.set 2
    enableAutosave(C, js)
    flushQueued C
    check js["v"].getInt == 2

    undo(C, 2)                 # back to 0
    flushQueued C
    check js["v"].getInt == 0

    redo(C, 1)                 # to 1
    flushQueued C
    check js["v"].getInt == 1

  # 11 ── loadState followed by redo is no‑op -------------------------------
  test "Redo after loadState has no effect":
    let C = newReactiveCtx()
    var s = store(C, "p", 0)
    s.set 5; loadState(C, newJObject())  # loads nothing → p unchanged
    redo(C)
    check s == 5                       # redo depth should be 0

  # 12 ── create many store signals and bulk save ---------------------------
  test "Bulk save 50 keys":
    let C = newReactiveCtx()
    for i in 0 ..< 50:
      discard store(C, $i, i)
    var obj: JsonNode
    saveState(C, obj)
    for i in 0 ..< 49:
      check obj[$i].getInt == i

  # 13 ── high frequency writes inside one frame ---------------------------
  test "Hundreds of writes still single autosave":
    let C = newReactiveCtx()
    var doc: JsonNode
    var s = store(C, "cnt", 0)
    enableAutosave(C, doc)
    for i in 1 .. 500:
      s.set i
    flushQueued C                  # one flush
    check doc["cnt"].getInt == 500

  # 14 ── disposing twice is safe ------------------------------------------
  test "Double dispose safe":
    let C = newReactiveCtx()
    dispose C
    dispose C                      # no crash

  # 15 ── loadState on disposed ctx is ignored ------------------------------
  test "loadState after dispose does nothing":
    let C = newReactiveCtx()
    var s = store(C, "w", 1)
    dispose C
    var jj = %*{ "w": 9 }
    loadState(C, jj)
    check s == 1

  # 16 ── autosave disabled then dispose -> no final save -------------------
  test "No snapshot when autosave disabled before dispose":
    let C = newReactiveCtx()
    var doc: JsonNode
    var s = store(C, "k", 2)

    enableAutosave(C, doc)
    disableAutosave(C)
    s.set 5
    dispose C
    check doc.isNil or not doc.hasKey("k") or doc["k"].getInt == 2


  # 19 ── registerStore after first save adds new key next flush -----------
  test "Late store registration shows up next autosave":
    let C = newReactiveCtx()
    var doc: JsonNode
    enableAutosave(C, doc)
    flushQueued C                 # snapshot at start (empty)
    var s = signal(C, 8)
    registerStore(C, "later", s)
    s.set 9
    flushQueued C
    check doc["later"].getInt == 9

  # 20 ── watch selector updates autosave only when changed -----------------
  test "Watch plus autosave only serialises on change":
    let C = newReactiveCtx()
    var js: JsonNode
    var base = store(C, "b", 1)   # odd → selector() == 1
    enableAutosave(C, js)
    var hits = 0

    watch(C,
      () => base.val mod 2,        # selector
      (n,o: int) => inc hits)   # handler

    base.set 2     # parity changes 1 → 0  (handler fires: hits = 1)
    base.set 4     # parity stays 0        (no more fires)
    flushQueued C

    check hits == 1
    check js["b"].getInt == 4


  # 21 ── snapshot/index travel after autosave updates node -----------------
  test "Travel index with autosave":
    let C = newReactiveCtx()
    var doc: JsonNode
    enableAutosave(C, doc)
    var s = store(C, "t", 0)

    s.set 11
    let idx = snapshot(C)
    s.set 22
    flushQueued C
    check doc["t"].getInt == 22

    travel(C, idx)   # back to 11
    flushQueued C
    check doc["t"].getInt == 11

  # 22 ── dispose immediately after enableAutosave captures initial state ---
  test "Enable then dispose without writes":
    let C = newReactiveCtx()
    var node: JsonNode
    var s = store(C, "q", 7)
    enableAutosave(C, node)   # initial snapshot should already contain q
    dispose C
    check node["q"].getInt == 7

  # 23 ── autosave reuses same node object (no replace) ---------------------
  test "saveInto mutates not replaces JsonNode":
    let C = newReactiveCtx()
    var node = newJObject()
    enableAutosave(C, node)
    let origAddr = addr node
    var v = store(C, "k", 1)
    v.set 2
    flushQueued C
    check (addr node) == origAddr

  # 24 ── high‑volume undo with autosave ------------------------------------
  test "500 undo steps with autosave stable":
    let C = newReactiveCtx()
    var node: JsonNode
    var s = store(C, "n", 0)
    enableAutosave(C, node)
    for i in 1..500:
      s.set i
    flushQueued C
    check node["n"].getInt == 500

    undo(C, 500)
    flushQueued C
    check node["n"].getInt == 0

  # 25 ── undo/redo after disposing autosave node reference -----------------
  test "Node alias retains last snapshot even after ctx disposed":
    let C = newReactiveCtx()
    var node: JsonNode
    var s = store(C, "z", 3)
    enableAutosave(C, node)
    s.set 4
    dispose C
    check node["z"].getInt == 4

suite "peek / untracked read":

  test "peek returns value but does not track":
    let C = newReactiveCtx()
    var a = signal(C, 0)
    var hits = 0

    effect(C, proc =
      discard peek(a)        # <<< untracked
      inc hits
    )

    check hits == 1        # initial mount

    a.set 42                 # would retrigger if tracked
    check hits == 1        # still 1 → no dependency captured

  test "peek value equals normal read":
    let C = newReactiveCtx()
    var s = signal(C, 7)
    check peek(s) == s.val

suite "effectOnce":

  test "effectOnce executes only once despite dependency changes":
    let C = newReactiveCtx()
    var s = signal(C, 0)
    var hits = 0

    effectOnce(C, proc =
      discard s.val        # read signal, but untracked
      inc hits
    )

    check hits == 1      # initial call

    s.set 1                # would re-run a normal effect
    check hits == 1      # still only once

suite "watchNow":

  test "watchNow fires handler on first evaluation":
    let C = newReactiveCtx()
    var a = signal(C, 7)
    var firstSeen = -1
    watchNow(C,
      () => a.val,                   # selector
      proc(n, o: int) = firstSeen = n)

    check firstSeen == 7           # handler ran immediately

  test "Immediate flag false keeps old behaviour":
    let C = newReactiveCtx()
    var a = signal(C, 0)
    var seen: seq[int] = @[]
    watch(C,
      () => a.val,
      (n,o:int) => seen.add n,       # immediate defaults to false
    )
    check seen.len == 0            # no first fire
    a.set 1
    check seen == @[1]

suite "Reactive object wrapper":

  test "Signal‑like field semantics":
    let C = newReactiveCtx()
    var p = C.toReactive(Player(name: "hero", hp: 100, x: 0, y: 0))

    var hits = 0
    effect(C, proc =
      discard p.hp.val   # establishes dependency
      inc hits
    )

    p.hp += 50                        # write via '+=' sugar
    flushQueued C
    check p.hp.val == 150
    check hits == 2                 # first mount + after change

  test "toPlain snapshot is decoupled":
    let C = newReactiveCtx()
    var p = C.toReactive(Player(name: "H", hp: 1, x: 1, y: 2))
    let snap = p.toPlain()
    check snap.hp == 1
    p.hp += 9
    check snap.hp == 1              # unchanged – deep copy

  test "Undo restores wrapper field":
    let C = newReactiveCtx()
    var p = C.toReactive(Player(name: "h", hp: 10, x: 0, y: 0))
    p.hp.set 20
    undo C
    check p.hp.val == 10

  test "Wrapper fields integrate with persistence":
    let C = newReactiveCtx()
    var doc: JsonNode
    var p = C.toReactive(Player(name: "a", hp: 5, x: 0, y: 0))

    registerStore(C, "player_hp", p.hp)   # store just one field
    enableAutosave(C, doc)

    p.hp += 5
    flushQueued C
    check doc["player_hp"].getInt == 10

  test "Multiple fields batched in one transaction":
    let C = newReactiveCtx()
    var p = C.toReactive(Player(name: "b", hp: 0, x: 0, y: 0))
    var hits = 0

    C.effect proc =
      discard p.x.val          # establish dependency on x
      discard p.y.val          # establish dependency on y
      inc hits


    transaction(C):
      p.x += 3.0
      p.y += 4.0
    check hits == 2                 # initial + once after batch
    check p.x.val == 3.0
    check p.y.val == 4.0

suite "Reactive deep‑nest regression suite (D‑types)":

  test "Primitive field wiring":
    let C = newReactiveCtx()
    var v = C.toReactive(Vec2(x: 0, y: 0))
    var hits = 0
    effect(C, () => (discard v.x.val; inc hits))

    v.x += 2.5
    flushQueued C
    check v.x.val == 2.5
    check hits == 2

  test "Nested object propagates":
    let C = newReactiveCtx()
    var s = C.toReactive(StatsD(hp: 10, mp: 5, pos: Vec2(x: 1, y: 1)))
    var logs: seq[int]
    effect(C, () => (discard s.pos.y.val; logs.add s.pos.y.val.int))

    s.pos.y += 3
    flushQueued C
    check logs == @[1, 4]

  test "Undo works across depths":
    let C = newReactiveCtx()
    var p = C.toReactive(PlayerD(name: "bob", stats: StatsD(hp: 20, mp: 0, pos: Vec2(x:0,y:0))))
    p.stats.hp += 15
    undo C
    check p.stats.hp.val == 20

  test "toPlain snapshot full depth":
    let C = newReactiveCtx()
    var p = C.toReactive(PlayerD(name: "ela", stats: StatsD(hp: 5, mp: 5, pos: Vec2(x:2,y:3))))
    let snap = toPlain p
    p.stats.pos.x += 10
    check snap.stats.pos.x == 2

  test "Batched deep writes fire once":
    let C = newReactiveCtx()
    var p = C.toReactive(PlayerD(name:"z", stats: StatsD(hp:3,mp:2,pos:Vec2(x:0,y:0))))
    var cnt = 0
    effect(C, () => (discard p.stats.hp.val; discard p.stats.mp.val; inc cnt))

    transaction(C):
      p.stats.hp += 1
      p.stats.mp += 1
    flushQueued C
    check cnt == 2   # mount + batch flush

  test "Travel with nested object state":
    let C = newReactiveCtx()
    var p = C.toReactive(PlayerD(name:"t", stats: StatsD(hp:10, mp:0, pos:Vec2(x:0,y:0))))
    p.stats.hp += 30
    let snap = snapshot(C)
    p.stats.hp -= 15
    travel(C, snap)
    check p.stats.hp.val == 40

  test "Writable nested computed field":
    let C = newReactiveCtx()
    var s = C.toReactive(StatsD(hp:50, mp:100, pos: Vec2(x:0,y:0)))

    let pctHp = computed(C,
      getter = () => s.hp.val.float / 100.0,
      setter = (p: float) => s.hp.set (p * 100.0).int)

    pctHp.set 0.75
    check s.hp.val == 75
    undo C
    check s.hp.val == 50

suite "Wrapper edge-cases":
  test "field-list converted to two independent signals":
    let C = newReactiveCtx()
    var v = C.toReactive(MT(x: 1.0, y: 2.0))
    v.x += 1.0
    check v.x.val == 2.0
    check v.y.val == 2.0            # unchanged

  test "implicit-typed field works and is undo-able":
    let C = newReactiveCtx()
    var b = C.toReactive(BT())      # foo = 42 by default
    b.foo += 8
    check b.foo.val == 50
    undo C
    check b.foo.val == 42

  test "field list becomes two signals":
    let C = newReactiveCtx()
    var m = C.toReactive(MT(x: 1.0, y: 2.0))
    var cnt = 0
    C.effect proc =
      discard m.x.val; discard m.y.val
      inc cnt
    m.x.set 5.0
    m.y.set 6.0
    check cnt == 3            # mount + both edits

  test "default-initialised field copied correctly":
    let C = newReactiveCtx()
    var b = C.toReactive(BT())        # no explicit init → foo = 42
    check b.foo.val == 42
    b.foo.set 10
    check b.foo.val == 10

  test "variant object updates branch signals":
    let C = newReactiveCtx()
    var q = C.toReactive(QT(space: csSRGB, rgb: [0.1, 0.2, 0.3]))
    check q.rgb.val[1] == 0.2
    # change the whole variant in one go
    q.set QT(space: csDisplayP3, p3: [0.4, 0.5, 0.6])
    check q.space == csDisplayP3
    check q.p3.val[2] == 0.6

suite "MonsterX wrapper":

  proc baseStats(hp = 100, mp = 0): CoreStatsX =
    CoreStatsX(hp: hp, mp: mp,
               pos: Vec2(x: 0, y: 0),
               resist: initTable[ElementX,float]())

  proc newGoblin(id = 1): MonsterX =
    MonsterX(id: id,
            name: "Gob" & $id,
            tags: @["evil"],
            inv: initTable[string,int](),
            stats: baseStats(),
            kind: spGoblinX,
            speed: 2.0)

  proc newDragon(id = 1): MonsterX =
    MonsterX(id: id,
            name: "Drg" & $id,
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

suite "Extended combinatorial tests (added)":

  test "Nested autosave after sequential edits and undo":
    let C = newReactiveCtx()
    var node: JsonNode
    var p = C.toReactive(PlayerD(name:"aa", stats: StatsD(hp:10, mp:0, pos: Vec2(x:0,y:0))))
    registerStore(C, "ply", p.stats.hp)   # store nested signal explicitly
    enableAutosave(C, node)
    p.stats.hp += 5        # 15
    p.stats.hp += 10       # 25
    flushQueued C
    check node["ply"].getInt == 25
    undo C                 # back to 15
    flushQueued C
    check node["ply"].getInt == 15

  test "Effect cleanup fires for nested signal":
    let C = newReactiveCtx()
    var v = C.toReactive(Vec2(x:0,y:0))
    var cleanups = 0
    effect(C, proc(onCleanup: AddCleanup) =
      onCleanup(proc() = cleanups.inc)
      discard v.y.val)
    v.y += 1
    check cleanups == 1

  test "Memo chain across nested objects":
    let C = newReactiveCtx()
    var s = C.toReactive(StatsD(hp:2, mp:0, pos: Vec2(x:0,y:0)))
    let dblHp = memo(C, () => s.hp.val * 2)
    let tripleHp = memo(C, () => dblHp() * 3)
    s.hp += 1
    check tripleHp() == (3 * 2) * 3   # (hp=3) → dbl=6 → triple=18

  test "Disable → re‑enable autosave keeps old node untouched":
    let C = newReactiveCtx()
    var A, B: JsonNode
    var hp = store(C, "hp", 10)
    enableAutosave(C, A)
    hp.set 20
    flushQueued C
    disableAutosave(C)
    hp.set 30
    flushQueued C
    enableAutosave(C, B)
    hp.set 40
    flushQueued C
    check A["hp"].getInt == 20
    check B["hp"].getInt == 40

  test "dumpDeps on nested field lists subscribers":
    let C = newReactiveCtx()
    var p = C.toReactive(PlayerD(name:"cc", stats: StatsD(hp:1, mp:1, pos: Vec2(x:0,y:0))))
    effect(C, () => (discard p.stats.hp.val))
    let deps = dumpDeps(p.stats.hp)
    check deps.len == 1

  test "clearUndo then new write resets depth":
    let C = newReactiveCtx()
    var s = signal(C, 0)
    s.set 1; s.set 2
    clearUndo C
    check undoDepth(C) == 0
    s.set 3
    check undoDepth(C) == 1

  test "EffectOnce with nested read remains single execution":
    let C = newReactiveCtx()
    var v = C.toReactive(Vec2(x:1,y:1))
    var calls = 0
    effectOnce(C, proc() = discard v.x.val; calls.inc)
    v.x += 1
    check calls == 1

  test "Transaction inside transaction correctly nests batching":
    let C = newReactiveCtx()
    var s = signal(C, 0)
    var hits = 0
    effect(C, () => (discard s.val; hits.inc))
    transaction(C):
      s += 1
      transaction(C):
        s += 1
      s += 1
    check hits == 2   # mount + one after outer flush

  test "Multiple snapshots and travel chain":
    let C = newReactiveCtx()
    var hp = signal(C, 5)
    hp.set 10
    let i1 = snapshot(C)
    hp.set 15
    let i2 = snapshot(C)
    hp.set 20
    travel(C, i1)    # back to 10
    check hp.val == 10
    travel(C, i2)    # forward to 15
    check hp.val == 15

  test "Snapshot, travel, undo/redo round-trip on nested signal":
    ## 1. set-up -------------------------------------------------------------
    let C  = newReactiveCtx()
    var pj = C.toReactive(PlayerD(name: "pj", stats: StatsD(hp: 5)))

    ## 2. take baseline snapshot (depth = 0) ---------------------------------
    let snap0 = snapshot(C)

    ## 3. mutate nested field ------------------------------------------------
    pj.stats.hp += 20            # hp -> 25   (undo depth = 1)

    ## 4. take second snapshot (depth = 1) -----------------------------------
    let snap1 = snapshot(C)

    ## 5. travel back to baseline -------------------------------------------
    travel(C, snap0)             # hp should revert to 5
    check pj.stats.hp.val == 5

    ## 6. travel forward again ----------------------------------------------
    travel(C, snap1)             # hp should be 25
    check pj.stats.hp.val == 25

    ## 7. undo / redo sanity -------------------------------------------------
    undo(C)                      # back to 5
    check pj.stats.hp.val == 5

    redo(C)                      # forward to 25
    check pj.stats.hp.val == 25

  test "Nested signal persists & rolls back":
    ## 1. context, wrapper instance and store --------------------------------
    let C  = newReactiveCtx()
    var doc: JsonNode
    var pj = C.toReactive(PlayerD(name: "pj", stats: StatsD(hp: 5)))

    ## Register **the nested field** for persistence
    registerStore(C, "hp", pj.stats.hp)

    ## Enable autosave and trigger first snapshot
    enableAutosave(C, doc)       # doc = { "hp": 5 } immediately
    flushQueued C
    check doc["hp"].getInt == 5

    ## 2. mutate nested field -------------------------------------------------
    pj.stats.hp += 12            # hp -> 17
    flushQueued C                # autosave after write
    check doc["hp"].getInt == 17

    ## 3. undo – autosave should follow after flush ---------------------------
    undo(C)                      # hp -> 5
    flushQueued C
    check pj.stats.hp.val == 5
    check doc["hp"].getInt == 5

    ## 4. redo – autosave tracks forward state --------------------------------
    redo(C)                      # hp -> 17
    flushQueued C
    check pj.stats.hp.val == 17
    check doc["hp"].getInt == 17

suite "ReactiveSeq":

  test "push triggers effect and undo / redo":
    let C = newReactiveCtx()
    var rs = C.toReactive(@["a", "b"])        # len == 2

    var hits = 0
    effect(C, proc =
      discard rs.len          # depend on length
      inc hits)

    check hits == 1
    rs.push "c"               # len -> 3
    check rs.len == 3
    check hits == 2

    undo C                    # remove "c"
    check rs.len == 2
    check hits == 3

    redo C                    # re-add "c"
    check rs.len == 3
    check hits == 4

  test "multiple mutators inside transaction coalesce":
    let C  = newReactiveCtx()
    var rs = C.toReactive(@[1,2,3])
    var fires = 0
    effect(C, () => (discard rs.len; fires.inc))

    transaction(C):
      rs.push 4
      rs.push 5
      rs.removeIdx 0          # delete first
    check rs.len == 4       # 3 + 2 pushes – 1 delete
    check fires == 2        # mount + one batch flush
    check undoDepth(C) == 1 # one history entry

  test "clear then undo restores entire sequence":
    let C  = newReactiveCtx()
    var rs = C.toReactive(@["x","y","z"])
    rs.clear()
    check rs.len == 0
    undo C
    check rs.len == 3
    check rs.data == @["x","y","z"]

  test "peekLen does not create dependency":
    let C  = newReactiveCtx()
    var rs = C.toReactive(@[10])
    var runs = 0
    effectOnce(C, proc =
      discard peekLen(rs)     # untracked
      runs.inc)
    rs.push 11
    check runs == 1         # did NOT retrigger

suite "ReactiveSeq concat operators":

  test "&= with seq is one structural change":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1, 2])
    var lenHits = 0
    C.effect proc =
      discard rs.len
      inc lenHits

    rs &= @[3, 4]          # in-place append
    flushQueued C
    check rs == @[1,2,3,4]
    check lenHits == 2     # mount + one structural update

  test "& with seq builds new ReactiveSeq":
    let C = newReactiveCtx()
    var rs1 = C.toReactive(@[1])
    let rs2 = rs1 & @[2, 3]
    check rs1 == @[1]
    check rs2 == @[1,2,3]

  test "&= with single element":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[10])
    rs &= 20
    flushQueued C
    check rs == @[10, 20]

  test "elem & ReactiveSeq prepends element":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[5,6])
    let rs2 = 4 & rs
    check rs2 == @[4,5,6]

suite "Frame scheduler":

  test "Reactions run on next frame only once":
    let C  = newReactiveCtx()
    var frameTasks: seq[proc()] = @[]
    useFrameScheduler(C, proc(cb: proc()) = frameTasks.add cb)

    var s = signal(C, 0)
    var hits = 0
    effect(C, () => (discard s.val; hits.inc))

    check hits == 1           # mount

    s.set 10                    # schedule reaction
    check hits == 1           # not yet flushed
    check frameTasks.len == 1

    # ─ next frame ─
    for t in frameTasks: t()
    frameTasks.setLen 0
    check hits == 2           # flushed

    # second write in same frame re-uses existing flush
    s.set 20
    s.set 30
    check frameTasks.len == 1 # still one pending flush

  test "Transaction batches and flushes once next frame":
    let C  = newReactiveCtx()
    var tasks: seq[proc()] = @[]
    useFrameScheduler(C, proc(cb: proc()) = tasks.add cb)

    var x = signal(C, 0)
    var fires = 0
    effect(C, () => (discard x.val; fires.inc))

    transaction(C):
      x.set 1
      x.set 2                    # two writes, still same frame
    check fires == 1           # not flushed yet
    check tasks.len == 1

    tasks[0]()                   # run flush
    check fires == 2           # exactly once more

  test "Autosave integrates with frame scheduler":
    let C  = newReactiveCtx()
    var queued: seq[proc()] = @[]
    useFrameScheduler(C, proc(cb: proc()) = queued.add cb)

    var doc: JsonNode
    var score = store(C, "score", 0)
    enableAutosave(C, doc)       # initial snapshot   (score = 0)

    score.set 42                 # mark dirty
    check (doc["score"].getInt == 0)     # not flushed yet

    queued[0]()                  # frame flush
    check doc["score"].getInt == 42

suite "ReactiveTable":

  test "put / undo / redo and effect on len":
    let C = newReactiveCtx()
    var raw = initTable[string,int]()
    raw["a"] = 1
    var rt = C.toReactive(raw)

    var hits = 0
    effect(C, proc =
      discard rt.len
      hits.inc)

    check rt.len == 1
    check hits == 1              # mount

    rt.put("b", 2)                 # +1 entry
    check rt.len == 2
    check hits == 2

    undo C                         # remove "b"
    check rt.len == 1
    check hits == 3

    redo C
    check rt.len == 2
    check hits == 4

  test "batched mutations coalesce into one history entry":
    let C = newReactiveCtx()
    var rt = C.toReactive(initTable[int,string]())
    var fires = 0
    effect(C, () => (discard rt.rev; fires.inc))

    transaction(C):
      rt.put(1, "x")
      rt.put(2, "y")
      rt.delKey(1)
    check rt.len == 1
    check fires == 2             # mount + one batch flush
    check undoDepth(C) == 1

  test "clear then undo restores full content":
    let C = newReactiveCtx()
    var tbl = initTable[int,int]()
    tbl[1] = 1; tbl[2] = 2
    var rt = C.toReactive(tbl)
    rt.clear()
    check rt.len == 0
    undo C
    check rt.len == 2
    redo C
    check rt.len == 0

  test "peekLen does not capture dependency":
    let C = newReactiveCtx()
    var rt = C.toReactive(initTable[string,int]())
    rt.put("a", 1)
    var runs = 0
    effectOnce(C, proc =
      discard peekLen(rt)
      runs.inc)
    rt.put("b", 2)
    check runs == 1              # effect did not re-run

suite "ReactiveSeq element-signals":

  test "element signal updates effect without length change":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[10, 20])
    var seen: seq[int]
    effect(C, () => (discard rs[1].val; seen.add rs[1].val))
    rs[1] += 5               # 20 → 25   (no structural change)
    check rs[1].val == 25
    check rs.len == 2
    check seen == @[20, 25]

  test "undo / redo on element value":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1])
    rs[0] += 9               # -> 10
    undo C
    check rs[0].val == 1
    redo C
    check rs[0].val == 10

  test "autosave persists element value":
    let C = newReactiveCtx()
    var doc: JsonNode
    var rs = C.toReactive(@[3])
    registerStore(C, "first", rs[0])   # store the element signal
    enableAutosave(C, doc)
    rs[0] += 7                         # -> 10
    flushQueued C
    check doc["first"].getInt == 10

  test "insert keeps previous signals alive":
    let C = newReactiveCtx()
    var rs = C.toReactive(@["a", "c"])
    let sigA = rs[0]         # hold reference to first element
    rs.insert(1, "b")        # now ["a","b","c"]
    sigA.set "z"
    check rs[0].val == "z"   # the same signal was updated
    check rs.len == 3

  test "iterator yields plain values":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1,2,3])
    var sum = 0
    for v in rs: sum += v
    check sum == 6

suite "ReactiveTable element-signals":

  test "value signal updates effect without len change":
    let C = newReactiveCtx()
    var base = initTable[int,string]()
    base[1] = "orc"
    var rt = C.toReactive(base)

    var hits = 0
    effect(C, () => (discard rt[1].val; hits.inc))

    check hits == 1
    rt[1] = "ogre"          # element write
    check hits == 2
    check rt.len == 1

  test "undo / redo on element value":
    let C = newReactiveCtx()
    var rt = C.toReactive(initTable[string,int]())
    rt.put("hp", 10)
    rt["hp"] += 5           # -> 15
    undo C
    check rt["hp"].val == 10
    redo C
    check rt["hp"].val == 15

  test "structural and value edits coalesce correctly":
    let C = newReactiveCtx()
    var rt = C.toReactive(initTable[int,int]())
    var fires = 0
    effect(C, () => (discard rt.rev; fires.inc))

    transaction(C):
      rt.put(1, 10)         # structural
      rt.put(1, 12)         # value-only
      rt.put(2, 20)         # structural
    check fires == 2      # mount + one flush
    check undoDepth(C) == 1

  test "autosave nested value":
    let C = newReactiveCtx()
    var doc: JsonNode
    var rt = C.toReactive(initTable[string,int]())
    registerStore(C, "hp", rt["hp"])   # store the element
    enableAutosave(C, doc)
    rt.put("hp", 8)
    flushQueued C
    check doc["hp"].getInt == 8

suite "Complex combinatorial integration":

  test "Mixed seq + table edits coalesce (4-step history)":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[0])
    var rt = C.toReactive(initTable[int,int]())

    var fires = 0
    effect(C, () => (discard rs.len; discard rt.rev; fires.inc))

    transaction(C):
      rs.push 2               # structural
      rt.put(1, 10)           # structural
      rs[0] += 3              # value-only
      rt[1] += 4              # value-only

    check rs == @[3, 2]
    check rt[1].val == 14
    check fires == 2                  # mount + one flush
    check undoDepth(C) == 4           # 2 value + 2 structural entries

    undo(C, 4)                          # roll back the entire tx
    check rs == @[0]
    check rt.len == 0
    redo(C, 4)                          # forward again
    check rs == @[3, 2]
    check rt[1].val == 14

  # 2 ── autosave in frame-scheduler path with structural+value edits ───────
  test "Frame scheduler + autosave deep mix":
    let C = newReactiveCtx()
    var queue: seq[proc()] = @[]
    useFrameScheduler(C, proc(cb: proc()) = queue.add cb)

    var doc: JsonNode
    var rs = C.toReactive(@[1])
    var rt = C.toReactive(initTable[string,int]())
    registerStore(C, "first", rs[0])     # element signal
    registerStore(C, "cnt",   rt.lenS)   # structural len signal
    enableAutosave(C, doc)               # snapshot at start

    # mutate – should schedule ONE frame flush
    rs[0] += 4
    rt.put("kills", 3)
    check queue.len == 1
    queue[0]()                           # next frame

    check doc["first"].getInt == 5     # 1+4
    check doc["cnt"].getInt   == 1

  # 3 ── time-travel through mixed structural + value history ---------------
  test "Travel across structural+value snapshots":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[10,20])
    var rt = C.toReactive(initTable[int,string]())
    rt.put(1, "a")                       # structural
    let snap1 = snapshot(C)              # depth = 1

    rs[1] += 5                           # value-only
    let snap2 = snapshot(C)              # depth = 2

    rt[1] = "b"                          # value-only
    check undoDepth(C) == 3

    travel(C, snap1)                     # back to state after first insert
    check rs == @[10,20]
    check rt[1].val == "a"

    travel(C, snap2)                     # forward one
    check rs == @[10,25]
    check rt[1].val == "a"

  # 4 ── registerStore on non-existent element then mutate ------------------
  test "Late key accessor safe for persistence":
    let C = newReactiveCtx()
    var doc: JsonNode
    var rt = C.toReactive(initTable[string,int]())  # empty
    registerStore(C, "hp", rt["hp"])     # accessor auto-creates key
    enableAutosave(C, doc)
    flushQueued C                        # first snapshot (hp = 0)
    rt["hp"] += 9                        # value write
    flushQueued C
    check doc["hp"].getInt == 9

  # 5 ── deep dispose mid-transaction still snapshots -----------------------
  test "Dispose inside nested batch with seq+table":
    let C = newReactiveCtx()
    var js: JsonNode
    var rs = C.toReactive[:int](@[])
    var rt = C.toReactive(initTable[int,int]())
    enableAutosave(C, js)

    transaction(C):
      rs.push 7
      rt.put(1, 99)
      dispose C                  # inside batch – should flush once

    # lenS not stored
    check not js.hasKey("cnt")
    check not js.hasKey("score")

    # but rs structural signal created; autosave captured new seq length
    check rs.len == 1

suite "Memo advanced behaviour":

  test "Memo does NOT recompute if selector result unchanged":
    let C = newReactiveCtx()
    var src  = signal(C, 1)
    var tick = 0
    let even = memo(C, proc: bool =
      inc tick
      (src.val mod 2) == 0)

    check even() == false
    src.set 3          # selector returns false again
    check even() == false
    check tick == 2  # exactly one extra evaluation

  test "Memo ignores structural change with identical elements":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1, 2])
    var runs = 0
    let sum = memo(C, proc: int =
      inc runs
      rs[0].val + rs[1].val)

    rs.push 0          # length change, but memo does NOT read len
    check sum() == 3
    check runs == 1  # no recompute

  test "Memo survives deep undo/redo chain":
    let C   = newReactiveCtx()
    var src = signal(C, 1)
    var dbl = memo(C, () => src.val * 2)
    src.set 2
    src.set 3
    undo(C, 2)         # back to 1
    check dbl() == 2
    redo(C, 2)         # forward to 3
    check dbl() == 6

suite "ReactiveSeq effect reactivity":

  test "Effect sees element edits after structural push (immediate)":
    let C   = newReactiveCtx()
    var rs  = C.toReactive(@["a"])
    var log : seq[string]

    effect(C, proc() =
      discard rs.len
      log = rs.toPlain)           # store a snapshot

    rs.push "b"                   # structural change
    rs[1].set "B"                 # element change
    check log == @["a", "B"]

  test "Effect sees edits with queued scheduler":
    let C   = newReactiveCtx()
    C.useQueuedScheduler
    var rs  = C.toReactive(@[0])
    var snap: seq[int]

    effect(C, proc() =
      discard rs.len
      snap = rs.toPlain)

    rs.push 1
    rs[0].set 9
    rs[1].set 8
    flushQueued C
    check snap == @[9, 8]

  test "One flush per transaction despite mixed edits":
    let C   = newReactiveCtx()
    var rs  = C.toReactive(@[1])
    var hits = 0
    effect(C, proc() =
      discard rs.len
      discard rs[0].val
      hits.inc)

    transaction(C):
      rs.push 2           # structural
      rs[0].set 5         # value
      rs[1].set 6         # value
    check hits == 2     # mount + one batch flush
    check rs.toPlain == @[5, 6]

suite "Context isolation":

  test "Writes in one context never leak into another":
    let A = newReactiveCtx()
    let B = newReactiveCtx()

    var a = signal(A, 1)
    var b = signal(B, 2)

    var hitsA = 0
    var hitsB = 0

    effect(A, () => (discard a.val; hitsA.inc))
    effect(B, () => (discard b.val; hitsB.inc))

    check (hitsA, hitsB) == (1, 1)   # initial mounts

    a.set 10
    check (a.val, b.val) == (10, 2)
    check (hitsA, hitsB) == (2, 1)    # only A reacted

    b.set 20
    check (hitsA, hitsB) == (2, 2)    # now B reacted

  test "Undo/redo confined to originating context":
    let C1 = newReactiveCtx()
    let C2 = newReactiveCtx()
    var s1 = signal(C1, 0)
    var s2 = signal(C2, 0)

    s1.set 7            # history C1 depth = 1
    s2.set 13           # history C2 depth = 1
    undo(C1)
    check (s1.val, s2.val) == (0, 13)
    redo(C1)
    check (s1.val, s2.val) == (7, 13)

suite "Scheduler interaction":

  test "Queued scheduler defers *and* survives nested transactions":
    let C = newReactiveCtx()
    useQueuedScheduler(C)

    var s = signal(C, 1)
    var log: seq[int]
    effect(C, () => (discard s.val; log.add s.val))
    check log == @[1]            # mount

    transaction(C):                # inner writes will queue exactly once
      s += 1
      transaction(C):
        s += 2
      s += 3                       # net +6 → 7

    check log[^1] == 1           # not flushed yet
    flushQueued C
    check log[^1] == 7           # single reaction flush

  test "Frame scheduler flush composes with queued autosave":
    let C = newReactiveCtx()
    var frameTasks: seq[proc()] = @[]
    useFrameScheduler(C, (cb: proc()) => frameTasks.add cb)

    var doc: JsonNode
    var hp = store(C, "hp", 10)
    enableAutosave(C, doc)         # initial snapshot hp=10

    hp.set 42                      # mark dirty, schedule frame‑flush
    check doc["hp"].getInt == 10   # not flushed yet
    check frameTasks.len == 1

    frameTasks[0]()                # execute next‑frame flush
    check doc["hp"].getInt == 42

suite "ReactiveSeq permutations":

  test "Element signal identity intact after mixed insert/delete":
    let C  = newReactiveCtx()
    var rs = C.toReactive(@["a","b","c","d"])  # index: 0 1 2 3

    let sigB = rs[1]             # capture reference to element "b"
    let sigD = rs[3]             # capture reference to element "d"

    rs.insert(1, "X")           # a X b c d
    rs.removeIdx 2               # remove old "b" (now at idx 2)
    rs.push "Y"                  # a X c d Y
    rs.pushFront "Z"            # Z a X c d Y

    check sigB.val == "b"      # orphaned but still holds value
    check sigD.val == "d"      # same instance (now deeper)
    check rs.toPlain == @["Z","a","X","c","d","Y"]

  test "All length‑3 permutations of two writes coalesce correctly":
    let C = newReactiveCtx()
    var a = signal(C, 0)
    var b = signal(C, 0)
    let sum = computed(C, () => a.val + b.val)

    proc incA() = a += 1
    proc incB() = b += 1

    let ops = @[incA, incB]

    for i in 0 ..< 8:             # 3‑bit permutation space
      a.set 0; b.set 0
      transaction(C):
        for pos in 0 .. 2:
          if ((i shr pos) and 1) == 0: ops[0]() else: ops[1]()
      check sum.val == 3

suite "ReactiveTable key life‑cycle":

  test "Old value‑signal becomes orphan after delKey + put":
    let C = newReactiveCtx()
    var rt = C.toReactive(initTable[string,int]())
    rt.put("hp", 10)
    let sigOld = rt["hp"]
    check undoDepth(C) == 1      # ensure history push occurred

    rt.delKey "hp"
    rt.put("hp", 99)              # new signal created & inserted

    check rt["hp"].val == 99
    check sigOld.val     == 10
    sigOld.set 77
    check rt["hp"].val == 99
    check sigOld.val     == 77

  test "Structural delete/undo/redo restores same signal instance":
    let C = newReactiveCtx()
    var rt = C.toReactive(initTable[int,int]())
    rt.put(1, 1)
    let sig1 = rt[1]
    check undoDepth(C) == 1       # depth check

    rt.delKey 1
    undo C                          # bring key back
    check rt[1] == sig1
    redo C                          # delete again
    rt.put(1, 42)                   # re‑create
    check rt[1] != sig1

suite "Memo – watch interaction":

  test "Watch on memo output fires exactly on distinct outputs":
    let C = newReactiveCtx()
    var raw = signal(C, 1)
    var seen: seq[int]

    let parity = memo(C, () => raw.val mod 2)

    watch(C, selector = parity, handler = (n, o: int) => seen.add n, immediate=true)
    check seen == @[1]

    raw.set 3
    raw.set 6
    raw.set 8
    raw.set 9

    check seen == @[1, 0, 1]

  test "Effect reading watch selector inside memo chain not doubly counted":
    let C = newReactiveCtx()
    var base = signal(C, 2)
    var outerHits = 0

    let dbl    = memo(C, () => base.val * 2)
    let triple = memo(C, () => dbl() * 3)

    effect(C, () => (discard triple(); outerHits.inc))
    check outerHits == 1

    base.set 4
    check outerHits == 2

suite "Reentrancy & flush‑during‑flush":

  test "Effect scheduling new write inside queued flush gets processed same cycle":
    let C = newReactiveCtx()
    useQueuedScheduler(C)

    var a = signal(C, 0)
    var b = signal(C, 0)

    effect(C, proc =
      discard a.val
      if b.val == 0:
        b.set 1)                    # schedule inside current flush

    a.set 1
    flushQueued C                  # should process both
    check b.val == 1

suite "Snapshot chain (length 5)":

  test "Travel arbitrarily backward/forward through checkpoints":
    let C = newReactiveCtx()
    var s = signal(C, 0)
    var chkpts: seq[int]

    for i in 1..5:
      s.set i * 10
      chkpts.add snapshot(C)

    travel(C, chkpts[2])     # 30
    check s == 30
    travel(C, chkpts[4])     # 50
    check s == 50
    travel(C, chkpts[0])     # 10
    check s == 10
    travel(C, chkpts[3])     # 40
    check s == 40

suite "Cascade depth chain":

  test "Single leaf write propagates through 4‑layer chain exactly once":
    let C = newReactiveCtx()
    var base = signal(C, 1)

    let cmp  = computed(C, () => base.val + 1)
    let mm   = memo(C, () => cmp.val * 2)

    var hits = 0
    effect(C, () => (discard mm(); hits.inc))

    check hits == 1
    base.set 5
    check hits == 2

suite "Writable computed recursion guard":

  test "Setter that updates source once does not recurse infinitely":
    let C = newReactiveCtx()
    var src = signal(C, 1)
    var chkpts = 0

    let twice = computed(C,
      getter = () => src.val * 2,
      setter = (v: int) => (if src.val * 2 != v: src.set v div 2))

    twice.set 10            # src becomes 5
    chkpts.inc         # test completed
    check (src.val, twice.val) == (5, 10)
    check chkpts == 1

suite "Reactive object wrapper (Complex)":

  test "Nested seq + table fields all propagate in one outer tx":
    let C = newReactiveCtx()
    var obj = C.toReactive(Complex(
      id: 1,
      tags: @["alpha"],
      data: initTable[string,int](),
      pos: Vec2(x: 0, y: 0)
    ))

    var fires = 0
    effect(C, () => (
      discard obj.tags.len
      discard obj.data.len
      discard obj.pos.x.val
      fires.inc))

    transaction(C):
      obj.tags.push "beta"
      obj.data.put("y", 2)
      obj.pos.x += 3

    flushQueued C
    check fires == 2
    check obj.tags.toPlain == @["alpha", "beta"]
    check obj.data["y"].val == 2
    check obj.pos.x.val == 3

  test "Undo chain traverses writable computed nested setter":
    let C = newReactiveCtx()
    var obj = C.toReactive(Complex(
      id: 1,
      tags: @[],
      data: initTable[string,int](), pos: Vec2(x:0,y:0)
    ))

    let dist = computed(C,
      getter = () => sqrt(obj.pos.x.val*obj.pos.x.val + obj.pos.y.val*obj.pos.y.val),
      setter = (d: float) => (obj.pos.x.set d; obj.pos.y.set 0))

    dist.set 5
    check (obj.pos.x.val, obj.pos.y.val) == (5.0, 0.0)
    undo C
    check dist.val == 0.0

suite "Autosave toggling":

  test "Swapping nodes mid‑flight leaves originals untouched":
    let C = newReactiveCtx()
    var A, B: JsonNode
    var score = store(C, "score", 0)

    enableAutosave(C, A)
    score.set 10
    flushQueued C
    check A["score"].getInt == 10

    disableAutosave C
    enableAutosave(C, B)
    score.set 22
    flushQueued C
    check A["score"].getInt == 10
    check B["score"].getInt == 22

  test "Autosave snapshot survives context disposal then further JSON edits":
    let C = newReactiveCtx()
    var node: JsonNode
    var hp = store(C, "hp", 5)
    enableAutosave(C, node)
    hp.set 7
    dispose C

    node["external"] = newJInt(99)
    check node["hp"].getInt == 7

suite "Large history stress (1000)":

  test "Undo/redo 1000 entries across seq & table":
    ## Structural: 500 pushes + 500 puts  → 1000 history entries.
    ## Value edits: Double (seq) / triple (table) each element.  Index 0 remains
    ## unchanged when multiplied, so *two* value writes are no-ops, producing
    ## 998 additional history entries.  Total expected depth = 1998.
    let C = newReactiveCtx()
    var rs = C.toReactive(newSeq[int]())
    var rt = C.toReactive(initTable[int,int]())

    for i in 0 ..< 500:
      rs.push i
      rt.put(i, i)
    for i in 0 ..< 500:
      rs[i].set rs[i].val * 2
      rt[i]  = rt[i].val * 3

    check undoDepth(C) == 1998   # 1000 structural + 998 value entries

    undo(C, 1998)
    flushQueued C                      # ensure any queued len/structural updates apply
    check rs.len == 0
    check rt.len == 0

    redo(C, 1998)
    flushQueued C
    check rs.len == 500
    check rt.len == 500
    check rs[123].val == 246 and rt[123].val == 369

suite "Dispose mid‑frame flush":

  test "Context disposed inside own frame flush gracefully aborts further work":
    let C = newReactiveCtx()
    var frame: seq[proc()] = @[]
    useFrameScheduler(C, (cb: proc()) => frame.add cb)

    var s = signal(C, 0)
    var hits = 0

    effect(C, proc =
      discard s.val
      hits.inc
      if s.val == 1:
        dispose C)                 # dispose while reacting

    check hits == 1              # initial mount

    s.set 1                        # schedule reaction via frame scheduler
    frame[0]()                     # run flush – effect runs, disposes ctx
    frame.setLen 0                 # clear processed task
    check hits == 2

    s.set 2                        # write after dispose should **not** trigger effects
    check s.val == 2             # value still mutates silently
    check frame.len == 0         # scheduler is now no‑op
    flushQueued C                  # nothing to flush
    check hits == 2              # no additional reactions

suite "watchNow with nested transaction":

  test "Immediate fires once, nested tx merges subsequent changes":
    let C = newReactiveCtx()
    var hp = signal(C, 100)
    var log: seq[int]

    watchNow(C,
      () => hp.val div 10,
      (n, o: int) => log.add n)

    transaction(C):
      hp -= 1
      transaction(C):
        hp -= 30
      hp += 5

    check log == @[10, 7]  # first immediate, second after outer tx flush

suite "std/algorithm interop":

  test "sort, reverse, rotateLeft push one structural history entry":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[3, 1, 4, 2])
    var revHits = 0
    effect(C, () => (discard rs.rev; revHits.inc))

    rs.sort()                          # 3 → 1 history entry
    check rs.toPlain == @[1,2,3,4]
    check undoDepth(C) == 1
    check revHits == 2               # mount + after sort

    rs.reverse()
    check rs.toPlain == @[4,3,2,1]
    check undoDepth(C) == 2

    rs.rotateLeft(2)
    check rs.toPlain == @[2,1,4,3]
    check undoDepth(C) == 3

    undo(C, 3)
    check rs.toPlain == @[3,1,4,2]

  test "element signal survives reorder":
    let C = newReactiveCtx()
    var rs = C.toReactive(@["a","b","c"])
    let sigB = rs[1]                   # "b"
    rs.sort()
    check sigB.val == "b"            # same instance, moved index

  test "binarySearch & isSorted stay reactive":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1, 2, 3])
    var idxLog: seq[int]

    effect(C, () => idxLog.add rs.binarySearch(3))
    check idxLog == @[2]          # initial mount

    rs.pushFront 2            # « instead of 0 »
    check idxLog[^1] == 3   # 3 moved to index 3

    check rs.isSorted == false
    rs.sort()
    check rs.isSorted == true


  test "fill batches inside transaction":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[0,0,0,0])
    var hits = 0
    effect(C, () => (discard rs[0].val; hits.inc))  # depend on element 0

    rs.fill(9)
    check rs.toPlain == @[9,9,9,9]
    check undoDepth(C) == 1          # single transaction entry
    check hits == 2

  test "lowerBound / upperBound reactive":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1, 2, 3, 5, 7])   # already sorted
    var lbLog, ubLog: seq[int]

    effect(C, () => lbLog.add rs.lowerBound(4))
    effect(C, () => ubLog.add rs.upperBound(4))

    check lbLog == @[3]      # first ≥ 4 is 5, at index 3
    check ubLog == @[3]      # first > 4  is 5, at index 3

    rs.insert(3, 4)            # keep sequence sorted

    # effect fired again → logs got a second entry
    check lbLog == @[3, 3]   # 4 itself is now at 3, so lb index unchanged
    check ubLog == @[3, 4]   # first > 4 moved one slot to the right


  test "nextPermutation pushes single history":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1,2,3])
    rs.nextPermutation()
    check rs.toPlain == @[1,3,2]
    check undoDepth(C) == 1
    undo C
    check rs.toPlain == @[1,2,3]

  test "slice rotateLeft mutates once":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[0,1,2,3,4])
    rs.rotateLeft(1 .. 3, 2)   # [0,3,1,2,4]
    check rs.toPlain == @[0,3,1,2,4]
    check undoDepth(C) == 1

  test "sorted snapshot unaffected by later edits":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[3, 1, 2])

    let snap = rs.sorted()
    check snap == @[1, 2, 3]     # copy is returned *already sorted*

    rs.sort()                      # mutate the reactive sequence
    check rs.toPlain == @[1, 2, 3]
    check snap == @[1, 2, 3]     # independent copy, unchanged

suite "Advanced std/algorithm inter-op":

  test "nextPermutation fires exactly one effect and pushes one history":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1,2,3])
    var hits = 0
    effect(C, () => (discard rs.rev; hits.inc))

    rs.nextPermutation()            # -> 1,3,2
    check rs.toPlain == @[1,3,2]
    check hits == 2               # mount + reorder
    check undoDepth(C) == 1
    undo C
    check rs.toPlain == @[1,2,3]

  test "prevPermutation then nextPermutation round-trips":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[0, 1, 2])     # already the first permutation

    # prevPermutation should fail and keep the sequence unchanged:
    check rs.prevPermutation() == false
    check rs.toPlain == @[0, 1, 2]

    # nextPermutation succeeds and advances once:
    check rs.nextPermutation() == true
    check rs.toPlain == @[0, 2, 1]

  test "slice rotateLeft mutates structurally and tracks history":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[0,1,2,3,4,5])
    var revHit = 0
    effect(C, () => (discard rs.rev; revHit.inc))

    rs.rotateLeft(2 .. 4, 1)        # rotate mini-window [2,3,4] -> 3,4,2
    check rs.toPlain == @[0,1,3,4,2,5]
    check revHit == 2
    check undoDepth(C) == 1
    undo C
    check rs.toPlain == @[0,1,2,3,4,5]

  test "lower/upperBound react with custom descending cmp":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[9,7,5,3,1])   # descending
    proc desc(a,b:int):int = system.cmp(b,a)
    var lbLog: seq[int]
    effect(C, () => lbLog.add rs.lowerBound(6, desc))

    check lbLog == @[2]           # 5 is first ≤ 6 in descending order

    rs.insert(2, 6)                 # maintain descending sorting
    check lbLog == @[2, 2]        # index of first ≤6 still 2 (new element)

  test "sorted(…, Descending) returns deep copy unaffected by further edits":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[2,5,1])
    let snap = rs.sorted(Descending)   # @[5,2,1]
    rs.push 6
    check snap == @[5,2,1]
    check rs.toPlain == @[2,5,1,6]

  test "multiple permutation calls inside transaction coalesce":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1,2,3])
    transaction(C):
      rs.nextPermutation()
      rs.nextPermutation()
      rs.prevPermutation()
    check undoDepth(C) == 1         # one structural entry

  test "memo over lowerBound output updates once per structural change":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[0, 2, 4, 6])
    let lbMemo = memo(C, () => rs.lowerBound(3))
    var fires = 0

    watchNow(C, lbMemo, (n, o: int) => fires.inc)
    check fires == 1                  # immediate fire (index = 2)

    rs.insert(1, 1)                     # 0,1,2,4,6  →  index becomes 3
    check fires == 2                  # handler ran once

    rs.insert( rs.upperBound(5), 5 )    # 0,1,2,4,5,6 → index still 3
    check fires == 2                  # no extra fire

  test "isSorted tracking survives unsort → sort toggle":
    let C = newReactiveCtx()
    var rs = C.toReactive(@[1,2,3])
    var log: seq[bool]
    effect(C, () => log.add rs.isSorted())

    rs.insert(0, 4)                   # 4,1,2,3 -> unsorted
    rs.sort()                         # back to sorted
    check log == @[true,false,true]

suite "ReactiveSeq == std/algorithm consistency":

  template mkPair(body: untyped): untyped =
    let C = newReactiveCtx()
    var plain {.inject.}: seq[int] = body
    var rs    {.inject.} = C.toReactive(plain)

  test "sort parity (asc/desc/custom)":
    mkPair(@[5, 1, 4, 2, 3])

    plain.sort()
    rs.sort()
    check rs.toPlain == plain

    plain.sort(Descending)
    rs.sort(order = Descending)
    check rs.toPlain == plain

    proc oddEven(a,b:int):int =
      let ra = (a and 1); let rb = (b and 1)
      if ra != rb: return cmp(ra, rb)
      cmp(a, b)
    plain.sort(oddEven)
    rs.sort(oddEven)
    check rs.toPlain == plain

  test "reverse parity":
    mkPair(@[0,1,2,3,4,5])

    plain.reverse()
    rs.reverse()
    check rs.toPlain == plain

    plain.reverse(1,4)
    rs.reverse(1,4)
    check rs.toPlain == plain

  test "rotateLeft parity":
    mkPair(@[0,1,2,3,4])

    discard plain.rotateLeft(2)
    discard rs.rotateLeft(2)
    check rs.toPlain == plain

    discard plain.rotateLeft(1..3, 1)
    discard rs.rotateLeft(1..3, 1)
    check rs.toPlain == plain

  test "next/prevPermutation parity":
    mkPair(@[1,2,3])

    check plain.nextPermutation() == rs.nextPermutation()
    check rs.toPlain == plain

    check plain.prevPermutation() == rs.prevPermutation()
    check rs.toPlain == plain

  test "lower/upperBound parity after structural change":
    mkPair(@[1,3,5,7])

    check plain.lowerBound(4) == rs.lowerBound(4)

    plain.insert(3, 4)
    rs.insert(3, 4)

    check plain.lowerBound(4) == rs.lowerBound(4)
    check plain.upperBound(4) == rs.upperBound(4)

  test "isSorted parity through unsort → sort":
    mkPair(@[1,2,3])

    check rs.isSorted == plain.isSorted

    plain.insert(4, 0)
    rs.insert(0, 4)
    check rs.isSorted == plain.isSorted

    plain.sort()
    rs.sort()
    check rs.isSorted == plain.isSorted

suite "bitops / signal integration":

  test "binary bit-and reactivity":
    let C = newReactiveCtx()
    var a = C.signal 0b1100'u8
    var b = C.signal 0b1010'u8
    let c = a and b                 # 0b1000
    var hits = 0
    C.effect proc = (discard c.val; inc hits)

    check c.val == 0b1000'u8
    a.set 0b0110                    # now 0b0010
    check c.val == 0b0010'u8
    check hits == 2                 # re-ran exactly once

  test "and/or/xor with scalar rhs":
    let C = newReactiveCtx()
    var x = C.signal 0b0101'u8
    let m = x and 0b0011'u8
    let n = x or  0b1000'u8
    let p = x xor 0b1111'u8
    check (m.val, n.val, p.val) == (0b0001'u8, 0b1101'u8, 0b1010'u8)
    x.set 0b1111'u8
    check (m.val, n.val, p.val) == (0b0011'u8, 0b1111'u8, 0b0000'u8)

  test "unary not":
    let C = newReactiveCtx()
    var f = C.signal uint8 0b0000_1111
    let g = not f
    check g.val == 0b1111_0000'u8
    f.set 0b0101_0101
    check g.val == 0b1010_1010'u8

  test "rotateLeft / rotateRight wrappers":
    let C = newReactiveCtx()
    var v = C.signal uint8 0b0110_1001
    let l = rotateLeftBits(v, 4)
    let r = rotateRightBits(v, 4)
    check (l.val, r.val) == (0b1001_0110'u8, 0b1001_0110'u8)
    v.set 0b0011_1100
    check (l.val, r.val) == (0b1100_0011'u8, 0b1100_0011'u8)

  test "testBit over signal":
    let C = newReactiveCtx()
    var flags = C.signal uint8 0b0001_0000
    let bit4  = flags.testBit(4'u8)
    var log: seq[bool]
    C.effect proc = (log.add bit4.val)
    check log == @[true]
    flags.flipBit 4'u8
    check log[^1] == false

  test "mask / unmask range":
    let C = newReactiveCtx()
    var n = C.signal 0b1111_0000'u8
    n.mask(0 .. 3)
    check n.val == 0'u8
    n.setMask(1 .. 2)
    check n.val == 0b0000_0110
    n.flipMask(1 .. 2)
    check n.val == 0b0000_0000

  test "flipMask and undo":
    let C = newReactiveCtx()
    var flags = C.signal 0b1111_1111'u8
    flags.flipMask(4 .. 6)       # -> 0b1000_1111
    check flags.val == 0b1000_1111'u8
    undo C
    check flags.val == 0b1111_1111'u8

  test "bitslice condenses value":
    let C = newReactiveCtx()
    var x = C.signal 0b1011_0100'u8       # 0xB4
    x.bitslice(2 .. 6)
    check x.val == 0b01101'u8

suite "Reactive vs std/bitops parity":

  # If mkPair is already defined elsewhere just remove the proc below.
  proc mkPair[T](init: T): (Signal[T], T) =
    let C = newReactiveCtx()
    (C.signal init, init)

  test "mask / unmask parity":
    let (r, _) = mkPair(uint8 0b1100_0011)   # r is a Signal[uint8]
    var plain  = uint8 0b1100_0011

    # mask range 2..5
    plain.mask(2 .. 5)
    r.mask(2 .. 5)
    check r.val == plain

    # unmask (clearMask) same range
    plain.clearMask(2 .. 5)
    r.clearMask(2 .. 5)
    check r.val == plain

  test "set / clear / flip single bits":
    let (r, _) = mkPair(uint8 0)
    var plain  = uint8 0

    for bit in 0u8 ..< 8u8:
      plain.setBit bit
      r.setBit   bit
      check r.val == plain

      plain.clearBit bit
      r.clearBit   bit
      check r.val == plain

      plain.flipBit bit
      r.flipBit   bit
      check r.val == plain

  test "bitslice parity (all byte‑slices)":
    let (r0, _) = mkPair(uint8 0b1011_0100)
    let plain0  = uint8 0b1011_0100

    for lo in 0 .. 7:
      for hi in lo .. 7:
        # create an *independent* signal each turn
        var r = C.signal(uint8 0b1011_0100)
        var plain = uint8 0b1011_0100
        r.bitslice(lo .. hi)
        plain.bitslice(lo .. hi)
        check r.val == plain

  test "rotateLeftBits / rotateRightBits parity":
    let (r0, _) = mkPair(uint16 0b0011_1100_1100_0011)
    let plain0  = uint16 0b0011_1100_1100_0011

    for rot in 0 ..< 16:
      var r     = r0
      var plain = plain0

      r.set rotateLeftBits(r.val, rot)
      plain = rotateLeftBits(plain, rot)
      check r.val == plain

      r.set rotateRightBits(r.val, rot)
      plain = rotateRightBits(plain, rot)
      check r.val == plain

  test "testBit parity & reactivity":
    let C = newReactiveCtx()
    var flags = C.signal uint8 0b0101_0001
    var plain = uint8 0b0101_0001

    let bit2 = flags.testBit(2'u8)
    let bit6 = flags.testBit(6'u8)

    var log2, log6: seq[bool]
    C.effect(proc() = log2.add bit2.val) # false
    C.effect(proc() = log6.add bit6.val) # true

    # initial parity
    check bit2.val == plain.testBit(2'u8)
    check bit2.val == false
    check bit6.val == plain.testBit(6'u8)
    check bit6.val == true

    # flip both bits
    flags.flipBit 2'u8 # false -> true
    flags.flipBit 6'u8 # true -> false

    plain.flipBit 2'u8 # false -> true
    plain.flipBit 6'u8 # true -> false

    # parity again
    check bit2.val == plain.testBit(2'u8)
    check bit2.val == true
    check bit6.val == plain.testBit(6'u8)
    check bit6.val == false

    # effect logs prove reactivity
    check log2 == @[false, true]
    check log6 == @[true, false]

  test "mask / unmask parity":
    var (r, plain) = mkPair(uint8 0b1100_0011)
    for sl in @[0..3, 2..5, 1..6]:
      r.mask(sl);     plain.mask(sl);        check r.val == plain
      r.setMask(sl);  plain.setMask(sl);     check r.val == plain
      r.clearMask(sl);plain.clearMask(sl);   check r.val == plain
      r.flipMask(sl); plain.flipMask(sl);    check r.val == plain

  test "set / clear / flip single bits":
    var (r, plain) = mkPair(uint8 0)
    for b in 0u8 .. 7u8:
      r.setBit(b);    plain.setBit(b);    check r.val == plain
      r.flipBit(b);   plain.flipBit(b);   check r.val == plain
      r.clearBit(b);  plain.clearBit(b);  check r.val == plain

  test "bitslice parity (all byte-slices)":
    for a in 0 .. 6:
      for b in a .. 7:
        var (r, plain) = mkPair(uint8 0b1011_0100)
        r.bitslice(a..b);   plain.bitslice(a..b)
        check r.val == plain

suite "Signal-based complex helpers":
  let C     = newReactiveCtx()
  var s1    = C.signal(complex.complex64(1, 2))
  let other = complex.complex64(3, -4)

  test "in-place +=":
    var s1 = C.signal complex.complex64(1, 2)
    s1 += other
    # (1 + 2i) + (3 – 4i)  ==>  4 – 2i
    check almostEqual(s1.val, complex.complex64(4, -2))

  test "computed addition":
    var s2  = C.signal complex.complex64(1, 2)
    let sum = s2 + other        # initial: 4 – 2i
    check almostEqual(sum.val, complex.complex64(4, -2))

    s2 += other                 # s2 becomes 4 – 2i
    # sum should now be (4 – 2i) + (3 – 4i) = 7 – 6i
    check almostEqual(sum.val, complex.complex64(7, -6))

  test "abs / phase reactive":
    let z   = C.signal(complex.complex64(3, 4))   # |z| = 5
    let az  = z.abs
    check abs(az.val - 5.0) < 1e-6
    z += complex.complex64(0, 1)                  # now 3+5i  -> |.| ≈ 5.830951
    check abs(az.val - 5.830951) < 1e-4

suite "Reactive vs std/complex parity":
  const z0 = complex.complex64(3, -4)   # |z0| = 5
  const w0 = complex.complex64(-2, 1.5) # a second operand

  proc newPair[T](x: complex.Complex[T]): (
    Signal[complex.Complex[T]],
    complex.Complex[T]
  ) =
    let C = newReactiveCtx()
    (C.signal(x), x)

  template checkSame(op: untyped; a, b: untyped) =
    check almostEqual(op(a), op(b))

  test "+, -, *, /":
    var (rs, p) = newPair(complex64(1, 2))
    let q       = complex64(3, -4)
    rs += q
    p  += q
    check rs.val.almostEqual(p)

  test "sqrt / exp / ln":
    let (rs, p) = newPair(complex64(0.3, 1.2))
    checkSame(sqrt, rs.val, p)
    checkSame(exp,  rs.val, p)
    checkSame(ln,   rs.val, p)

  test "polar / rect round-trip":
    let (rs, _) = newPair(complex64(2, -2))
    let pol = rs.polar()
    let back = rect(
      pol.map1((p: typeof pol.val) => p.r),      # radius    λ
      pol.map1((p: typeof pol.val) => p.phi)     # phase     λ
    )
    check back.val.almostEqual(rs.val)

  test "sin reactive dependency":
    let C  = newReactiveCtx()
    var z  = C.signal complex64(1, 1)
    let s  = z.sin
    var log: seq[Complex64]
    C.effect proc () = log.add s.val
    z += complex64(1, 0)
    check log.len == 2          # fired again
    check log[1].almostEqual(sin(z.val))

  test "abs / abs2 / phase":
    let (rs, p) = newPair(z0)
    check rs.abs.val  == abs(p)
    check rs.abs2.val == abs2(p)
    check rs.phase.val.almostEqual phase(p)

  test "conjugate / inverse":
    let (rs, p) = newPair(z0)
    check rs.conjugate.val == conjugate(p)
    check rs.inv.val.almostEqual inv(p)

  test "sqrt / exp / ln":
    let (rs, p) = newPair(z0)
    check rs.sqrt.val.almostEqual sqrt(p)
    check rs.exp.val.almostEqual  exp(p)
    check rs.ln.val.almostEqual   ln(p)

  test "trigonometric functions":
    let (rs, p) = newPair(z0)
    check rs.sin.val.almostEqual  sin(p)
    check rs.cos.val.almostEqual  cos(p)
    check rs.tan.val.almostEqual  tan(p)

  test "hyperbolic functions":
    let (rs, p) = newPair(z0)
    check rs.sinh.val.almostEqual sinh(p)
    check rs.cosh.val.almostEqual cosh(p)
    check rs.tanh.val.almostEqual tanh(p)

  test "polar / rect round-trip (again)":
    let (rs, _) = newPair(w0)
    let pol  = rs.polar()                # Signal[(r,phi)]
    let back = rect(
      pol.map1((x: typeof pol.val) => x.r),
      pol.map1((p: typeof pol.val) => p.phi)
    )
    check back.val.almostEqual(rs.val)

  test "binary ops & pow":
    var (rs, p) = newPair(z0)

    # +  –  *  /
    check (rs + w0).val.almostEqual(p + w0)
    check (rs - w0).val.almostEqual(p - w0)
    check (rs * w0).val.almostEqual(p * w0)
    check (rs / w0).val.almostEqual(p / w0)

    # pow with complex and real exponents
    let cExp  = complex.complex64(0.3, 2)
    check (rs.pow cExp).val.almostEqual pow(p, cExp)
    check (rs.pow 2'f64).val.almostEqual pow(p, 2'f64)

  test "in-place mutators (+=, -=, *=, /=)":
    var (rs, p) = newPair(w0)

    rs += z0;  p += z0
    check rs.val.almostEqual p

    rs -= z0;  p -= z0
    check rs.val.almostEqual p

    rs *= z0;  p *= z0
    check rs.val.almostEqual p

    rs /= z0;  p /= z0
    check rs.val.almostEqual p

suite "Reactive JSON tree":

  test "scalar change triggers effect":
    let C  = newReactiveCtx()
    var jr = C.toReactive(parseJson("""{ "x": 1 }"""))

    var log: seq[int]
    C.effect proc =
      log.add jr["x"].intVal.val      # read → dependency

    jr["x"].intVal.set 7
    flushQueued C
    check log == @[1, 7]

  test "adding a new key updates len & effect":
    let C  = newReactiveCtx()
    var jr = C.toReactive(parseJson("""{}"""))

    var lens: seq[int]
    C.effect proc =
      lens.add len(jr.fields)         # reactive length read

    jr["a"].intVal.set 1
    flushQueued C
    check lens == @[0, 1]

  test "array element reactive read/write":
    let C  = newReactiveCtx()
    var jr = C.toReactive(parseJson("""{ "arr": [4, 5, 6] }"""))

    var seen: seq[int]
    C.effect proc =
      seen.add jr["arr"][1].intVal.val

    jr["arr"][1].intVal.set 99
    flushQueued C
    check seen == @[5, 99]

  test "deep nested object propagates once per transaction":
    let C  = newReactiveCtx()
    var jr = C.toReactive(parseJson("""{ "p": { "x": 1, "y": 2 } }"""))
    var hits = 0

    C.effect proc =
      discard jr["p"].val["x"].intVal.val
      discard jr["p"].val["y"].intVal.val
      inc hits

    transaction(C):
      jr["p"].val["x"].intVal.set 10
      jr["p"].val["y"].intVal.set 20
    flushQueued C
    check hits == 2             # mount + once for the batch

  test "undo / redo across JSON edits":
    let C  = newReactiveCtx()
    var jr = C.toReactive(parseJson("""{ "v": 3 }"""))

    jr["v"].intVal.set 8
    jr["v"].intVal.set 13
    undo(C)
    check jr["v"].intVal.val == 8
    redo(C)
    check jr["v"].intVal.val == 13

  test "bulk set replaces entire JSON wrapper in one step":
    let C  = newReactiveCtx()
    var jr = C.toReactive(parseJson("""{ "a": 1 }"""))
    var hits = 0

    C.effect proc =
      discard jr["b"].intVal.val or 0   # first run -> key absent -> 0
      inc hits

    jr.set parseJson("""{ "b": 42 }""")
    flushQueued C

    check len(jr.fields) == 1
    check jr["b"].intVal.val == 42
    check hits == 2                    # mount + one change









# EOF