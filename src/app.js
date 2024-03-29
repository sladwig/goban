import { get, set } from 'idb-keyval';
import ActionCable from 'actioncable';
import { Elm } from './Main.elm';

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer")

  ElmDebugTransform.register({
    simple_mode: true
  })
}

var App = Elm.Main.init({ flags: { moves: [] } });

App.ports.enterRoom.subscribe((game) => {
  BroadCast.createChannelFor(game);

  Games.get(game).then((sgf) => {
    App.ports.loadGame.send([game, sgf || '']);
  });
});

App.ports.updateGame.subscribe(([game, sgf]) => {
  Games.set(game, sgf).then(() => BroadCast.to(game, sgf));
});

App.ports.confirmReset.subscribe((game) => {
  const reseting  = window.confirm('Reset game?')
  App.ports.resetGame.send([game, true]);
});

App.cable = ActionCable.createConsumer(window.cableUrl);

var connectedRooms = new Set();
var BroadCast = {
  isConnected: (game) => connectedRooms.has(game),
  onData: (game, received) => ({
    connected: () => connectedRooms.add(game),
    disconnected: () => connectedRooms.remove(game),
    received,
  }),
  retryStack: [],
  triggeredNotify: false,

  retryLater: () => {
    if (BroadCast.triggeredNotify) return;

    BroadCast.triggeredNotify = true;
    setTimeout(() => {
      BroadCast.triggeredNotify = false;
      BroadCast.notify();
    }, 3000);
  },
  notify: () => {
    var toTry = BroadCast.retryStack.shift();

    if (!toTry) return; // nothing to do

    var [game, sgf] = toTry;

    if (!game || !sgf) return; // incomplete

    if (!BroadCast.isConnected(game)) return BroadCast.retryLater();

    if (BroadCast.forGame(game).send({ sgf })) return BroadCast.notify(); // next

    BroadCast.retryStack.unshift([game, sgf]);
    BroadCast.retryLater();
  },

  channels: new Map(),
  createChannelFor: (game) => {
    BroadCast.channels.set(
      game,
      App.cable.subscriptions.create(
        { channel: 'GameChannel', game },
          BroadCast.onData(game, ({ sgf }) => {
          App.ports.loadGame.send([game, sgf]);
          Games.set(game, sgf);
        }),
      ),
    );
  },

  forGame: (game) => {
    if (!BroadCast.channels.has(game)) {
      BroadCast.createChannelFor(game);
    }
    return BroadCast.channels.get(game);
  },
  to: (game, sgf) => {
    BroadCast.retryStack.push([game, sgf]);
    BroadCast.notify();
  },
};

const Games = {
  archiveFor: (game) => 'archived-' + game,
  active: (game) => 'active-' + game,

  keepVersions: 10,

  set: (game, sgf) =>
    set(Games.active(game), sgf).finally(() => Games.saveVersion(game, sgf)),
  get: (game) => get(Games.active(game)),

  saveVersion: (game, sgf) => {
    get(Games.archiveFor(game)).then((versions = []) => {
      if (Games.keepVersions < versions.push(sgf)) versions.shift();

      set(Games.archiveFor(game), versions);
    });
  },

  loadLast: (game) => {
    get(Games.archiveFor(game)).then((versions = []) => {
      const lastVersion = versions.pop();
      if (!lastVersion) return;

      App.ports.updateRoom.send(game, lastVersion);
    });
  },
};
