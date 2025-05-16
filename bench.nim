import std/[random, os, sequtils]
import benchy           # std/benchmarks helper

include signals

randomize()
const
  Nsignals  = 10_000
  Nwrites   = 10_000
  Ntravels  = 1_000

proc makeSignals(ctx: ReactiveCtx; n = Nsignals): seq[Signal[int]] =
  newSeqWith(n, signal(ctx, 0))

timeIt "raw signal write (immediate)":
  let ctx = newReactiveCtx()
  var s   = signal(ctx, 0)
  for i in 0 ..< Nwrites:
    s.set i

timeIt "signal write inside single transaction":
  let ctx = newReactiveCtx()
  var s   = signal(ctx, 0)
  transaction(ctx):
    for i in 0 ..< Nwrites:
      s.set i

timeIt "batched writes to 10k distinct signals":
  let ctx   = newReactiveCtx()
  var sigs  = makeSignals(ctx)
  transaction(ctx):
    for i, sg in sigs:
      sg.set i

timeIt "effect firing 10k times (no batching)":
  let ctx  = newReactiveCtx()
  var sigs = makeSignals(ctx)
  var hits = 0

  for sg in sigs:
    let sig = sg                     # copy; no longer lent
    effect(ctx, proc =
      registerDep sig                # multi-line lambda style
      inc hits
    )

  for sg in sigs:
    sg.set sg.val + 1


timeIt "memo read vs computed read":
  let ctx = newReactiveCtx()
  var base = signal(ctx, 1)
  let cpt  = computed(ctx, () => base.val * 2)
  let m    = memo(ctx, () => base.val * 2)
  var s = 0
  for i in 0 ..< Nwrites:
    s += cpt.val                 # computed recalculates each time
    s += m()                     # memo cached

timeIt "undo → redo ping-pong (1k steps)":
  let ctx = newReactiveCtx()
  var s   = signal(ctx, 0)
  for i in 0 ..< Ntravels:
    s.set i
  undo(ctx, Ntravels)
  redo(ctx, Ntravels)

timeIt "snapshot / travel jumping around":
  let ctx = newReactiveCtx()
  var s   = signal(ctx, 0)
  for i in 1 .. 100:
    s.set i
  let snaps = (0..10).mapIt(snapshot(ctx))
  for sn in snaps:
    travel(ctx, sn)

timeIt "queued scheduler flush cost (10k callbacks)":
  let ctx = newReactiveCtx()
  useQueuedScheduler(ctx)
  var s = signal(ctx, 0)
  for i in 0 ..< Nwrites:
    s.set i
  flushQueued ctx

timeIt "dispose context with 10k signals":
  let ctx = newReactiveCtx()
  discard makeSignals(ctx)
  dispose ctx

