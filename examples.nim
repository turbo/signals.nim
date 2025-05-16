import std/[sugar, strformat, json, tables]
import signals

proc ex_1 =
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

proc ex_2 =
  let ctx = newReactiveCtx()
  var score = ctx.signal 0

  ctx.effect proc =
    echo "score changed to ", score.val

  score += 10   # triggers effect once
  score.set 20  # triggers again

  # output
  # score changed to 0
  # score changed to 10
  # score changed to 20

proc ex_3 =
  let ctx = newReactiveCtx()
  var hp     = ctx.signal 60
  var maxHp  = ctx.signal 100

  let percent = ctx.computed () => hp.val / maxHp.val

  ctx.effect proc =
    echo "percent is ", percent.val

  hp += 20

  # output
  # percent is 0.6
  # percent is 0.8

proc ex_4 =
  let ctx = newReactiveCtx()
  var hp  = ctx.signal 30
  var max = ctx.signal 100

  let pct = ctx.computed(
    getter = () => hp.val / max.val,
    setter = (p: float) => hp.set int(p * max.val.float)
  )

  pct.set 0.75
  echo pct.val # 0.75
  echo hp.val # 75

proc ex_5 =
  let ctx = newReactiveCtx()
  var ammo = ctx.signal 3

  ctx.effect proc(onCleanup: AddCleanup) =
    if ammo.val == 0:
      echo "reloading..."
      ammo.set 10
    onCleanup () => echo "ready to fire"

  ammo.set 2
  ammo.set 1
  ammo.set 0
  echo ammo.val

proc ex_6 =
  let ctx = newReactiveCtx()
  var x = ctx.signal 0

  ctx.effect proc =
    echo "x is ", x.val

  transaction(ctx):
    x += 1
    x += 1

  # output:
  # x is 0
  # x is 2

proc ex_7 =
  let ctx = newReactiveCtx()
  var hp = ctx.signal 10

  hp.set 5
  let mark = ctx.snapshot    # depth = 1
  hp.set 2                    # depth = 2

  ctx.travel mark
  echo hp.val # 5
  ctx.redo
  echo hp.val # 2

proc ex_8 =
  let ctx = newReactiveCtx()
  var doc: JsonNode

  var gold = ctx.store("gold", 100)
  ctx.enableAutosave doc

  gold += 50
  ctx.flushQueued # immediate scheduler, so nothing queued
  echo doc        # {"gold":150}


type Vec2 = object
  x, y: float

type Player = object
  name: string
  pos:  Vec2
  hp:   int

reactive(Vec2)
reactive(Player)

proc ex_9 =
  let ctx = newReactiveCtx()
  var p = ctx.toReactive Player(
    name: "Zed",
    pos: Vec2(x: 0, y: 0),
    hp: 100
  )

  ctx.effect () => echo &"HP: {p.hp.val}, Position: {p.pos.toPlain}"
  ctx.transaction:
    p.hp -= 30
    p.pos.x.set 20
    p.pos.y.set 123

proc ex_10 =
  let ctx = newReactiveCtx()
  # ctx.useQueuedScheduler

  var inv = ctx.toReactive(@["sword"])

  ctx.effect proc =
    discard inv.len # to react to .push() structural updates
    echo &"Current player inventory: {inv.toPlain}"

  inv.push "shield"

  inv[0].set "axe"
  inv[1].set "bettershield"
  inv[0].set "betteraxe"

  # output:
  # Current player inventory: @["sword"]
  # Current player inventory: @["sword", "shield"]
  # Current player inventory: @["axe", "shield"]
  # Current player inventory: @["axe", "bettershield"]
  # Current player inventory: @["betteraxe", "bettershield"]

proc ex_11 =
  let ctx = newReactiveCtx()
  var stats = ctx.toReactive(initTable[string,int]())

  ctx.effect proc =
    echo "hp is ", stats["hp"].val

  stats.put("hp", 10)          # new key
  stats["hp"] += 5             # element write

  # output
  # hp is 0
  # hp is 10
  # hp is 15

proc ex_12 =
  let ctx  = newReactiveCtx()
  var hp   = signal(ctx, 10)

  ctx.watch(
    selector = () => hp.val,
    handler  = proc(newVal, oldVal: auto) =
      echo "hp changed from ", oldVal, " to ", newVal
  )

  hp += 5

  # output
  # (note: no effect run on mount!)
  # hp changed from 10 to 15

for demo in [
  ex_1, ex_2, ex_3, ex_4, ex_5, ex_6, ex_7, ex_8, ex_9, ex_10, ex_11, ex_12
]:
  demo()
  echo "----------------"

