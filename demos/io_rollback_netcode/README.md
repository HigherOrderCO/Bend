# Rollback netcode in Bend

Input-synced rollback netcode for Bend games over the UDP effect kit.
You write an offline game as a pure `step(state, inputs) -> state` with a
text codec for its inputs, and it plays online: every client folds the
same stream of server-stamped inputs through the same step, so every
client computes the same state. Your own inputs apply at once as
predictions; a late input rolls the state back and replays.

    netcode.bend        the library: the Client (a Game record in, the
                        local and remote states out) and the Server
    server.bend         runs the relay                   PORT=7777
    walkers_demo.bend   the demo: colored squares on a torus, in a window
    walkers_test.bend   the demo played headless by a script: the test

## Run

Build and run each file. The relay prints one
line and waits. The demo opens a 512 x 512 window per player; run it
again for another player. Your square appears at once, where your id
puts it; WASD or the arrows move it; Esc quits. The id is the clock at
launch (or PID); SERVER=127.0.0.1 PORT=7777 ROOM=lobby are the
defaults.

    bend demos/io_rollback_netcode/server.bend -o relay && ./relay
    bend demos/io_rollback_netcode/walkers_demo.bend -o walkers && ./walkers

Headless test: two players that must print the same three lines.

    bend demos/io_rollback_netcode/walkers_test.bend -o bot
    PID=1 ROOM=t ./bot & PID=2 ROOM=t ./bot; wait
    # posts=64 tick=900 hash=...     (the same three lines from each)
    # posts=64 tick=1200 hash=...
    # posts=64 tick=2400 hash=...

A bot drives the demo's own `Play.tick` with key events at 16 ms a
frame: every 30th frame it presses a key (W, S, A, D in turn, offset by
PID) and releases it 15 frames later, for FRAMES (480) frames, idles
IDLE (150) more, then prints the room's post count and the hash of the
confirmed world at ticks past the run. Run bots on two machines with
SERVER pointing at the relay to test the clock sync too; a bot built to
`.js` runs with `bun`.

## Writing a game

```python
import ./netcode.bend as Net

def My.game() -> Net.Game<Input, State>:
  Net.Game{My.step, Input.read, Input.show, 60n, 200n}  # rate, tolerance ms
```

`Input` and `State` are `Data`. The client:

- `Net.Client.open(~Input, ~State, ~My.game(), init, host, port, room, me)`
  binds a port, joins the room and waits for the relay's first answer
  (two seconds, then it dies with a message); `me` names the player.
- `Net.Client.frame(~.., c)` once per frame answers `c & local & remote`:
  the state now with your predictions, and a past state no late input
  can change. Draw yourself from local and the others from remote
  (`World.blend` in walkers_demo.bend). Post your inputs before the
  frame, so they show in it.
- `Net.Client.post(~.., c, input)` predicts an input and sends it.
- `Net.Client.close(Input, State, c)`.

The game's functions are template arguments (`~`), so the fold that
replays ticks is compiled per game.

## Design

The time model: a post lands on
`tick(max(client_time, server_time - tolerance))`, identical on every
client, so an input applies when pressed if it reached the server within
the tolerance. The relay gives a room its birth time, and `tick(born)`
is the room's first tick: a client is anchored by the first Pong, before
any post. The engine keeps one folded base state plus the pending posts;
the relay's Pong (every half second) says how many posts the room held
at its clock, and a client holding them all folds every tick below
`pong_time - tolerance` into the base and drops what landed there, its
own stale predictions included. A missing post is asked again with
`Join{room, next}`, which is also how a late joiner gets the history.
The remote view stays `tolerance + ping/2` behind, plus one tick.

Choices: one `step(state, inputs)` handles ticks and posts alike; the
relay has no timer (the Pong is the checkpoint and the clock sample);
messages are text lines, not a bit packer; there is no state-hash ring:
the bot hashes states instead. The relay keeps rooms in memory only.

The effect kit's `UDP.try_recv_from(sock, max, 0)` is `recv_from`
without the wait, `Wait{}` when nothing is queued, so a frame drains its
socket and goes on. The demo runs its own window loop instead of base's
`App.run`: `App.turn` drops a frame with a `!`, and that mark makes a binary
compile its whole program for the GPU at launch (a minute for this one).
